//! Journal data structure for storing transactions

use crate::account::{Account, AccountRef};
use crate::transaction::{Transaction, TransactionStatus};
use ledger_math::commodity::Commodity;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

/// Main journal containing all transactions
#[derive(Debug, Default, Clone)]
pub struct Journal {
    /// List of transactions
    pub transactions: Vec<Transaction>,
    /// Account registry
    pub accounts: HashMap<String, AccountRef>,
    /// Commodity registry
    pub commodities: HashMap<String, Arc<Commodity>>,
    /// Next account ID
    next_account_id: usize,
}

impl Journal {
    /// Create a new empty journal
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a transaction to the journal
    pub fn add_transaction(&mut self, transaction: Transaction) {
        self.transactions.push(transaction);
    }

    /// Find an account by name
    pub fn find_account(&self, name: &str) -> Result<AccountRef, String> {
        self.accounts.get(name).cloned().ok_or_else(|| format!("Account not found: {}", name))
    }

    /// Get or create an account by name
    pub fn get_or_create_account(&mut self, name: &str) -> AccountRef {
        if let Some(account) = self.accounts.get(name) {
            account.clone()
        } else {
            let account_id = self.next_account_id;
            self.next_account_id += 1;
            let account = Rc::new(RefCell::new(Account::new(name.into(), None, account_id)));
            self.accounts.insert(name.to_string(), account.clone());
            account
        }
    }

    /// Add an account to the journal
    pub fn add_account(&mut self, account: AccountRef) -> Result<(), String> {
        let name = account.borrow().name().to_string();
        if self.accounts.contains_key(&name) {
            return Err(format!("Account already exists: {}", name));
        }
        self.accounts.insert(name, account);
        Ok(())
    }

    /// Add a commodity to the journal
    pub fn add_commodity(&mut self, commodity: Arc<Commodity>) -> Result<(), String> {
        let symbol = commodity.symbol().to_string();
        if self.commodities.contains_key(&symbol) {
            return Err(format!("Commodity already exists: {}", symbol));
        }
        self.commodities.insert(symbol, commodity);
        Ok(())
    }

    /// Merge another journal into this one
    pub fn merge(&mut self, other: Journal) -> Result<(), String> {
        // Merge transactions
        self.transactions.extend(other.transactions);

        // Merge accounts
        for (name, account) in other.accounts {
            self.accounts.entry(name).or_insert(account);
        }

        // Merge commodities
        for (symbol, commodity) in other.commodities {
            self.commodities.entry(symbol).or_insert(commodity);
        }

        Ok(())
    }

    /// Format all transactions and write them to the given writer.
    pub fn write_transactions(
        &self,
        writer: &mut impl std::io::Write,
    ) -> Result<(), std::io::Error> {
        let transactions = &self.transactions;

        for (i, transaction) in transactions.iter().enumerate() {
            if i != 0 {
                writeln!(writer)?; // Empty line between transactions
            }

            let status = match transaction.status {
                TransactionStatus::Uncleared => "",
                TransactionStatus::Cleared => " *",
                TransactionStatus::Pending => " !",
            };

            writeln!(
                writer,
                "{}{status} {}",
                transaction.date.format("%Y/%m/%d"),
                &transaction.payee
            )?;

            let postings = &transaction.postings;
            // TODO: use --account-width?
            let longest_account_name = transaction
                .postings
                .iter()
                .map(|p| p.account.borrow_mut().fullname().len())
                .max()
                .unwrap_or(0);

            for posting in postings {
                posting.write(writer, longest_account_name, &self.commodities)?;
                writeln!(writer)?;
            }
        }

        Ok(())
    }

    /// Format all transactions and return them as a String
    pub fn format_transactions(&self) -> String {
        let mut buffer = Vec::new();
        self.write_transactions(&mut buffer).expect("writing to string");
        String::from_utf8(buffer).unwrap()
    }
}

#[cfg(test)]
mod tests {

    use insta::assert_snapshot;

    use crate::parser::JournalParser;

    #[test]
    fn test_parse_and_format_journal() {
        let input = textwrap::dedent(
            "
            1999/11/01 * Achat
                Actif:SSB      125 STK
                Actif:SSB            -1672,42 $

            1999/11/04 * Vente
                Actif:SSB        -125 STK
                Dépense:SSB:Commissions       55,07 $
                Actif:SSB             1821,54 $
            ",
        );
        let mut parser = JournalParser::new();
        let journal = parser.parse_journal(&input).unwrap();

        assert_snapshot!(journal.format_transactions(), @r#"
            1999/11/01 * Achat
                Actif:SSB                                125 STK
                Actif:SSB                             -1672,42 $

            1999/11/04 * Vente
                Actif:SSB                               -125 STK
                Dépense:SSB:Commissions                  55,07 $
                Actif:SSB                              1821,54 $
        "#);
    }
}
