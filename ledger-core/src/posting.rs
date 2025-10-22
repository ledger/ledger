//! Posting/entry representation within transactions

use chrono::{NaiveDate, NaiveDateTime};
use compact_str::CompactString;
use ledger_math::{format_amount, Commodity, FormatConfig, FormatFlags};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::sync::Arc;

use crate::account::AccountRef;
use crate::strings::{AccountName, PayeeName};
use crate::transaction::{Position, TagData};
use ledger_math::amount::Amount;

// Posting flags matching C++ post_t flags
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct PostingFlags: u16 {
        const NORMAL = 0x0000;
        const VIRTUAL = 0x0010;         // account specified with (parens)
        const MUST_BALANCE = 0x0020;    // posting must balance in transaction
        const CALCULATED = 0x0040;      // posting's amount was calculated
        const COST_CALCULATED = 0x0080; // posting's cost was calculated
        const COST_IN_FULL = 0x0100;    // cost specified using @@
        const COST_FIXATED = 0x0200;    // cost is fixed using = indicator
        const COST_VIRTUAL = 0x0400;    // cost is virtualized: (@)
        const ANONYMIZED = 0x0800;      // a temporary, anonymous posting
        const DEFERRED = 0x1000;        // account specified with <angles>
        const IS_TIMELOG = 0x2000;      // the posting is a timelog entry
    }
}

/// Posting status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PostingStatus {
    /// Uncleared
    #[default]
    Uncleared,
    /// Cleared (*)
    Cleared,
    /// Pending (!)
    Pending,
}

/// Extended data for posting calculations and reporting
#[derive(Debug, Clone)]
pub struct PostingExtData {
    pub visited_value: Option<Amount>,
    pub compound_value: Option<Amount>,
    pub total: Option<Amount>,
    pub count: usize,
    pub date: Option<NaiveDate>,
    pub value_date: Option<NaiveDate>,
    pub datetime: Option<NaiveDateTime>,
    pub reported_account: Option<AccountRef>,
    pub sort_values: SmallVec<[AccountName; 4]>, // Most postings have few sort values
    pub flags: u16,
}

/// Represents a posting (line item) within a transaction
#[derive(Debug, Clone)]
pub struct Posting {
    /// Reference to the account for this posting
    pub account: AccountRef,
    /// Amount posted (can be None until finalization)
    pub amount: Option<Amount>,
    /// Optional amount expression (for calculated amounts) - optimized for short expressions
    pub amount_expr: Option<CompactString>,
    /// Optional cost (for commodity conversions)
    pub cost: Option<Amount>,
    /// Original cost as given (before any calculations)
    pub given_cost: Option<Amount>,
    /// Amount assigned through balance assertion
    pub assigned_amount: Option<Amount>,
    /// Check-in time for timelog entries
    pub checkin: Option<NaiveDateTime>,
    /// Check-out time for timelog entries
    pub checkout: Option<NaiveDateTime>,
    /// Optional payee override (different from transaction payee) - optimized memory usage
    pub payee: Option<PayeeName>,
    /// Optional note/comment - optimized memory usage
    pub note: Option<CompactString>,
    /// Posting flags
    pub flags: PostingFlags,
    /// Posting status
    pub status: PostingStatus,
    /// Source position information
    pub pos: Option<Position>,
    /// Metadata tags
    pub metadata: HashMap<String, TagData>,
    /// Sequence number for ordering
    pub sequence: usize,
    /// Extended data (lazy allocation for reporting)
    pub xdata: Option<PostingExtData>,
}

impl Default for PostingFlags {
    fn default() -> Self {
        PostingFlags::NORMAL
    }
}

impl Default for PostingExtData {
    fn default() -> Self {
        Self {
            visited_value: None,
            compound_value: None,
            total: None,
            count: 0,
            date: None,
            value_date: None,
            datetime: None,
            reported_account: None,
            sort_values: SmallVec::new(),
            flags: 0,
        }
    }
}

impl Posting {
    /// Create a new posting with required account
    pub fn new(account: AccountRef) -> Self {
        Self {
            account,
            amount: None,
            amount_expr: None,
            cost: None,
            given_cost: None,
            assigned_amount: None,
            checkin: None,
            checkout: None,
            payee: None,
            note: None,
            flags: PostingFlags::default(),
            status: PostingStatus::default(),
            pos: None,
            metadata: HashMap::new(),
            sequence: 0,
            xdata: None,
        }
    }

    /// Create a posting with account and amount
    pub fn with_amount(account: AccountRef, amount: Amount) -> Self {
        let mut posting = Self::new(account);
        posting.amount = Some(amount);
        posting
    }

    /// Check if posting must balance in the transaction
    pub fn must_balance(&self) -> bool {
        !self.flags.intersects(PostingFlags::VIRTUAL | PostingFlags::IS_TIMELOG)
            || self.flags.contains(PostingFlags::MUST_BALANCE)
    }

    /// Check if posting is virtual (enclosed in parentheses)
    pub fn is_virtual(&self) -> bool {
        self.flags.contains(PostingFlags::VIRTUAL)
    }

    /// Check if posting is deferred (enclosed in angle brackets)
    pub fn is_deferred(&self) -> bool {
        self.flags.contains(PostingFlags::DEFERRED)
    }

    /// Check if posting amount was calculated
    pub fn is_calculated(&self) -> bool {
        self.flags.contains(PostingFlags::CALCULATED)
    }

    /// Check if posting cost was calculated
    pub fn is_cost_calculated(&self) -> bool {
        self.flags.contains(PostingFlags::COST_CALCULATED)
    }

    /// Check if posting is a timelog entry
    pub fn is_timelog(&self) -> bool {
        self.flags.contains(PostingFlags::IS_TIMELOG)
    }

    /// Check if posting is anonymized (temporary)
    pub fn is_anonymized(&self) -> bool {
        self.flags.contains(PostingFlags::ANONYMIZED)
    }

    /// Get the effective date for this posting (from transaction or posting-specific)
    pub fn date(&self, transaction_date: NaiveDate, use_aux_date: bool) -> NaiveDate {
        if use_aux_date {
            if let Some(ref xdata) = self.xdata {
                if let Some(value_date) = xdata.value_date {
                    return value_date;
                }
            }
        }

        if let Some(ref xdata) = self.xdata {
            if let Some(date) = xdata.date {
                return date;
            }
        }

        transaction_date
    }

    /// Get the payee for this posting (posting-specific or inherited)
    pub fn effective_payee<'a>(&'a self, transaction_payee: &'a str) -> &'a str {
        self.payee.as_ref().map(|s| s.as_str()).unwrap_or(transaction_payee)
    }

    /// Check if posting has a specific tag
    pub fn has_tag(&self, tag: &str, inherit_from_account: bool) -> bool {
        if self.metadata.contains_key(tag) {
            return true;
        }

        if inherit_from_account {
            // Would need to check account metadata - simplified for now
            false
        } else {
            false
        }
    }

    /// Get a tag value
    pub fn get_tag(&self, tag: &str, inherit_from_account: bool) -> Option<&TagData> {
        if let Some(tag_data) = self.metadata.get(tag) {
            return Some(tag_data);
        }

        if inherit_from_account {
            // Would need to check account metadata - simplified for now
            None
        } else {
            None
        }
    }

    /// Set a tag with optional value
    pub fn set_tag(&mut self, tag: String, value: Option<String>, inherited: bool) {
        self.metadata.insert(tag, TagData { value, inherited });
    }

    /// Get account ID for sorting/indexing
    pub fn account_id(&self) -> usize {
        self.account.borrow().account_id
    }

    /// Get account name
    pub fn account_name(&self) -> String {
        self.account.borrow().fullname_immutable()
    }

    /// Set the reporting account (different from actual account for certain reports)
    pub fn set_reported_account(&mut self, account: AccountRef) {
        if self.xdata.is_none() {
            self.xdata = Some(PostingExtData::default());
        }
        if let Some(ref mut xdata) = self.xdata {
            xdata.reported_account = Some(account);
        }
    }

    /// Get the reported account (for display purposes)
    pub fn reported_account(&self) -> AccountRef {
        if let Some(ref xdata) = self.xdata {
            if let Some(ref reported) = xdata.reported_account {
                return reported.clone();
            }
        }
        self.account.clone()
    }

    /// Initialize extended data if not present
    pub fn ensure_xdata(&mut self) {
        if self.xdata.is_none() {
            self.xdata = Some(PostingExtData::default());
        }
    }

    /// Clear extended data to save memory
    pub fn clear_xdata(&mut self) {
        self.xdata = None;
    }

    /// Check if extended data is present
    pub fn has_xdata(&self) -> bool {
        self.xdata.is_some()
    }

    /// Get a description of this posting
    pub fn description(&self) -> String {
        if let Some(ref pos) = self.pos {
            format!("posting at line {}", pos.beg_line)
        } else {
            "generated posting".to_string()
        }
    }

    /// Check if this posting is valid
    pub fn valid(&self) -> bool {
        // Basic validation - must have an account
        !self.account.borrow().name.is_empty()
    }

    /// Set posting flags
    pub fn set_flags(&mut self, flags: PostingFlags) {
        self.flags = flags;
    }

    /// Add posting flags
    pub fn add_flags(&mut self, flags: PostingFlags) {
        self.flags.insert(flags);
    }

    /// Remove posting flags
    pub fn remove_flags(&mut self, flags: PostingFlags) {
        self.flags.remove(flags);
    }

    /// Check if posting has specific flags
    pub fn has_flags(&self, flags: PostingFlags) -> bool {
        self.flags.contains(flags)
    }

    /// Set posting status
    pub fn set_status(&mut self, status: PostingStatus) {
        self.status = status;
    }

    /// Mark as calculated
    pub fn mark_calculated(&mut self) {
        self.flags.insert(PostingFlags::CALCULATED);
    }

    /// Mark cost as calculated
    pub fn mark_cost_calculated(&mut self) {
        self.flags.insert(PostingFlags::COST_CALCULATED);
    }

    /// Set amount from calculation
    pub fn set_calculated_amount(&mut self, amount: Amount) {
        self.amount = Some(amount);
        self.mark_calculated();
    }

    /// Set cost from calculation
    pub fn set_calculated_cost(&mut self, cost: Amount) {
        self.cost = Some(cost);
        self.mark_cost_calculated();
    }

    /// Parse tags from a string (simplified version)
    pub fn parse_tags(&mut self, tags_str: &str, overwrite_existing: bool) {
        // Simple implementation - in practice would need more sophisticated parsing
        for tag_pair in tags_str.split(',') {
            let tag_pair = tag_pair.trim();
            if let Some(colon_pos) = tag_pair.find(':') {
                let (tag, value) = tag_pair.split_at(colon_pos);
                let value = value[1..].trim(); // Remove the ':'
                if overwrite_existing || !self.metadata.contains_key(tag) {
                    self.set_tag(tag.to_string(), Some(value.to_string()), false);
                }
            } else if overwrite_existing || !self.metadata.contains_key(tag_pair) {
                self.set_tag(tag_pair.to_string(), None, false);
            }
        }
    }

    /// Compare postings by date and sequence for sorting
    pub fn compare_by_date_and_sequence(
        &self,
        other: &Posting,
        base_date: NaiveDate,
    ) -> std::cmp::Ordering {
        let self_date = self.date(base_date, false);
        let other_date = other.date(base_date, false);

        match self_date.cmp(&other_date) {
            std::cmp::Ordering::Equal => self.sequence.cmp(&other.sequence),
            other_ord => other_ord,
        }
    }

    /// Format a posting into the given writer, for display within a transaction.
    pub fn write(
        &self,
        writer: &mut impl std::io::Write,
        max_account_width: usize,
        journal_commodities: &HashMap<String, Arc<Commodity>>,
    ) -> Result<(), std::io::Error> {
        let account_name = self.account.borrow().fullname_immutable();

        let status = match self.status {
            PostingStatus::Uncleared => "",
            PostingStatus::Cleared => " *",
            PostingStatus::Pending => " !",
        };

        // align status + account name to 36 chars + 2 for the account/amount separator
        let account_width =
            max_account_width.max(36).max(account_name.len()).saturating_sub(status.len()) + 2;

        if let Some(amount) = &self.amount {
            let price = amount
                .commodity()
                .and_then(|c| {
                    c.annotation().price().as_ref().map(|price| {
                        let commodity =
                            price.commodity().and_then(|c| journal_commodities.get(c.symbol()));
                        let config = FormatConfig::from_amount(price, &commodity);
                        format!(" {{{}}}", format_amount(price, &config))
                    })
                })
                .unwrap_or_default();

            let cost = self
                .cost
                .as_ref()
                .map(|cost| {
                    let commodity =
                        cost.commodity().and_then(|c| journal_commodities.get(c.symbol()));
                    let config = FormatConfig::from_amount(cost, &commodity);
                    let a = format!(" @ {}", format_amount(cost, &config));
                    a
                })
                .unwrap_or_default();

            let default_amount_width = 10;
            let amount = {
                let commodity =
                    amount.commodity().and_then(|c| journal_commodities.get(c.symbol()));
                let precision = commodity.map(|c| c.precision()).unwrap_or(amount.precision());

                let config = FormatConfig::from_amount(amount, &commodity)
                    .with_flags(FormatFlags::RIGHT_JUSTIFY)
                    .with_precision(precision)
                    .with_width(default_amount_width, None);
                format_amount(amount, &config) + price.as_str() + cost.as_str()
            };

            // FIXME: what is this +2 about?
            if amount.len() > (default_amount_width + 2) {
                write!(writer, "    {status}{account_name:account_width$}{amount}")?
            } else {
                write!(
                    writer,
                    "    {status}{account_name:account_width$}{amount:>default_amount_width$}",
                )?
            }
        } else {
            write!(writer, "    {status}{account_name}")?
        }

        Ok(())
    }

    /// Format a posting into a String, for display within a transaction
    pub fn format(
        &self,
        max_account_width: usize,
        journal_commodities: &HashMap<String, Arc<Commodity>>,
    ) -> String {
        let mut buffer = Vec::new();
        self.write(&mut buffer, max_account_width, journal_commodities).expect("writing to string");
        String::from_utf8(buffer).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::parser::parse_posting;

    #[test]
    fn test_parse_and_format_posting() {
        let (_, posting) =
            parse_posting("Actif:BC                               -340,00 €").unwrap();
        insta::assert_snapshot!(
            posting.format(0, &HashMap::default()),
            @r#"    Actif:BC                               -340,00 €"#
        );
    }

    #[test]
    fn test_parse_and_format_posting_with_extra_precision() {
        let (_, posting) =
            parse_posting("Actif:SSB                           125,0000 STK").unwrap();
        insta::assert_snapshot!(
            posting.format(0, &HashMap::default()),
            @r#"    Actif:SSB                             125,0000 STK"#
        );
    }

    #[test]
    fn test_parse_and_format_posting_with_cost() {
        let (_, posting) =
            parse_posting("Actif:SV                              1,0204 MFE @ 333,20 €").unwrap();
        insta::assert_snapshot!(
            posting.format(0, &HashMap::default()),
            @r#"    Actif:SV                              1,0204 MFE @ 333,20 €"#
        );
    }
}
