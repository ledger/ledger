//! Commodity module providing currency and commodity type support
//!
//! This module implements the Commodity type for tracking currencies and other
//! commodities associated with amounts, including support for annotations
//! such as lot prices, dates, and tags.

use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use bitflags::bitflags;
use chrono::NaiveDate;

use crate::Amount;

/// Precision type for commodity display settings
pub type Precision = u16;

/// Date type for annotations
pub type Date = NaiveDate;

/// Placeholder for expression type - will be implemented later
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expression {
    text: String,
}

impl Expression {
    pub fn new(text: String) -> Self {
        Self { text }
    }

    pub fn text(&self) -> &str {
        &self.text
    }
}

/// Reference-counted commodity for safe sharing
pub type CommodityRef = Arc<Commodity>;

/// Represents a commodity (currency or other tradeable item)
///
/// This is a simplified version of the C++ commodity_t class.
/// It will need to be expanded to support all the features like
/// annotations, price history, display formatting flags, etc.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Commodity {
    /// The symbol or name of this commodity (e.g., "$", "USD", "AAPL")
    symbol: String,

    /// Annotation details for this instance
    annotation: Annotation,

    /// Display precision for this commodity (number of decimal places)
    precision: Precision,

    /// Various flags controlling display behavior
    flags: CommodityFlags,
}

impl Commodity {
    /// Create a new commodity with the given symbol
    pub fn new(symbol: impl Into<String>) -> Self {
        Self {
            symbol: symbol.into(),
            annotation: Annotation::default(),
            precision: 0,
            flags: CommodityFlags::STYLE_DEFAULTS,
        }
    }

    /// Create a new commodity with symbol and annotation
    pub fn with_annotation(symbol: impl Into<String>, annotation: Annotation) -> Self {
        Self {
            symbol: symbol.into(),
            annotation,
            precision: 0,
            flags: CommodityFlags::STYLE_DEFAULTS,
        }
    }

    /// Create a new commodity with symbol and precision
    pub fn with_precision(symbol: impl Into<String>, precision: Precision) -> Self {
        Self {
            symbol: symbol.into(),
            annotation: Annotation::default(),
            precision,
            flags: CommodityFlags::STYLE_DEFAULTS,
        }
    }

    /// Get the symbol of this commodity
    pub fn symbol(&self) -> &str {
        &self.symbol
    }

    /// Get the annotation of this commodity
    pub fn annotation(&self) -> &Annotation {
        &self.annotation
    }

    /// Get the mutable annotation of this commodity
    pub fn annotation_mut(&mut self) -> &mut Annotation {
        &mut self.annotation
    }

    /// Check if this commodity has annotations
    pub fn has_annotation(&self) -> bool {
        !self.annotation.is_empty()
    }

    /// Set the annotation for this commodity
    pub fn set_annotation(&mut self, annotation: Annotation) {
        self.annotation = annotation;
    }

    /// Get the display precision of this commodity
    pub fn precision(&self) -> Precision {
        self.precision
    }

    /// Set the display precision of this commodity
    pub fn set_precision(&mut self, precision: Precision) {
        self.precision = precision;
    }

    /// Get the flags of this commodity
    pub fn flags(&self) -> CommodityFlags {
        self.flags
    }

    /// Check if this commodity has specific flags set
    pub fn has_flags(&self, flags: CommodityFlags) -> bool {
        self.flags.contains(flags)
    }

    /// Add flags to this commodity
    pub fn add_flags(&mut self, flags: CommodityFlags) {
        self.flags |= flags;
    }

    /// Remove flags from this commodity
    pub fn drop_flags(&mut self, flags: CommodityFlags) {
        self.flags &= !flags;
    }

    /// Set display format for this commodity
    pub fn set_format(&mut self, _format: String) {
        // TODO: Implement format parsing and storage
        // For now, this is a placeholder
    }

    /// Set the no-market flag for this commodity
    pub fn set_no_market(&mut self, no_market: bool) {
        if no_market {
            self.add_flags(CommodityFlags::NOMARKET);
        } else {
            self.drop_flags(CommodityFlags::NOMARKET);
        }
    }
}

impl fmt::Display for Commodity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol)?;

        // Write annotation details
        if !self.annotation.is_empty() {
            write!(f, " {{")?;
            let mut first = true;

            if let Some(ref _price) = self.annotation.price {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "price")?; // TODO: Display actual price when Amount is available
                first = false;
            }

            if let Some(ref date) = self.annotation.date {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "{}", date.format("%Y-%m-%d"))?;
                first = false;
            }

            if let Some(ref tag) = self.annotation.tag {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "({})", tag)?;
            }

            write!(f, "}}")?;
        }

        Ok(())
    }
}

// Commodity flag constants (matching C++ commodity.h)
bitflags! {
    /// Commodity style and behavior flags matching C++ commodity.h
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct CommodityFlags: u32 {
        const STYLE_DEFAULTS = 0x000;
        const STYLE_SUFFIXED = 0x001;
        const STYLE_SEPARATED = 0x002;
        const STYLE_DECIMAL_COMMA = 0x004;
        const STYLE_THOUSANDS = 0x008;
        const NOMARKET = 0x010;
        const BUILTIN = 0x020;
        const WALKED = 0x040;
        const KNOWN = 0x080;
        const PRIMARY = 0x100;
        const SAW_ANNOTATED = 0x200;
        const SAW_ANN_PRICE_FLOAT = 0x400;
        const SAW_ANN_PRICE_FIXATED = 0x800;
        const STYLE_TIME_COLON = 0x1000;
        const STYLE_NO_MIGRATE = 0x2000;
    }
}

// Annotation flag constants (matching C++ annotate.h)
bitflags! {
    /// Annotation flags for tracking calculation states
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct AnnotationFlags: u32 {
        const PRICE_CALCULATED = 0x01;
        const PRICE_FIXATED = 0x02;
        const PRICE_NOT_PER_UNIT = 0x04;
        const DATE_CALCULATED = 0x08;
        const TAG_CALCULATED = 0x10;
        const VALUE_EXPR_CALCULATED = 0x20;
    }
}

/// Annotation information for commodities (lot prices, dates, tags)
/// Matches the behavior of C++ annotation_t
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Annotation {
    /// Lot price annotation (price paid for this lot)
    price: Option<Amount>,

    /// Date annotation (acquisition date)
    date: Option<Date>,

    /// Tag annotation (lot identifier)
    tag: Option<String>,

    /// Value expression for complex pricing
    value_expr: Option<Expression>,

    /// Flags tracking calculation state
    flags: AnnotationFlags,
}

impl Annotation {
    /// Create a new empty annotation
    pub fn new() -> Self {
        Self {
            price: None,
            date: None,
            tag: None,
            value_expr: None,
            flags: AnnotationFlags::empty(),
        }
    }

    /// Create annotation with price
    pub fn with_price(price: Amount) -> Self {
        Self {
            price: Some(price),
            date: None,
            tag: None,
            value_expr: None,
            flags: AnnotationFlags::empty(),
        }
    }

    /// Create annotation with date
    pub fn with_date(date: Date) -> Self {
        Self {
            price: None,
            date: Some(date),
            tag: None,
            value_expr: None,
            flags: AnnotationFlags::empty(),
        }
    }

    /// Create annotation with tag
    pub fn with_tag(tag: String) -> Self {
        Self {
            price: None,
            date: None,
            tag: Some(tag),
            value_expr: None,
            flags: AnnotationFlags::empty(),
        }
    }

    /// Create full annotation
    pub fn with_all(
        price: Option<Amount>,
        date: Option<Date>,
        tag: Option<String>,
        value_expr: Option<Expression>,
    ) -> Self {
        Self { price, date, tag, value_expr, flags: AnnotationFlags::empty() }
    }

    /// Check if annotation has any values set
    pub fn is_empty(&self) -> bool {
        self.price.is_none()
            && self.date.is_none()
            && self.tag.is_none()
            && self.value_expr.is_none()
    }

    /// Get price annotation
    pub fn price(&self) -> &Option<Amount> {
        &self.price
    }

    /// Set price annotation
    pub fn set_price(&mut self, price: Amount) {
        self.price = Some(price);
    }

    /// Get date annotation
    pub fn date(&self) -> &Option<Date> {
        &self.date
    }

    /// Get tag annotation
    pub fn tag(&self) -> &Option<String> {
        &self.tag
    }

    /// Get value expression
    pub fn value_expr(&self) -> &Option<Expression> {
        &self.value_expr
    }

    /// Get annotation flags
    pub fn flags(&self) -> AnnotationFlags {
        self.flags
    }

    /// Set annotation flags
    pub fn set_flags(&mut self, flags: AnnotationFlags) {
        self.flags = flags;
    }

    /// Add annotation flags
    pub fn add_flags(&mut self, flags: AnnotationFlags) {
        self.flags |= flags;
    }
}

impl Default for Annotation {
    fn default() -> Self {
        Self::new()
    }
}

impl Eq for Annotation {}

impl PartialOrd for Annotation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Annotation {
    fn cmp(&self, other: &Self) -> Ordering {
        // Compare price first
        match (&self.price, &other.price) {
            (Some(_), None) => return Ordering::Greater,
            (None, Some(_)) => return Ordering::Less,
            (Some(a), Some(b)) => {
                if let Some(ord) = a.partial_cmp(b) {
                    return ord;
                }
            }
            (None, None) => {}
        }

        // Compare date
        match self.date.cmp(&other.date) {
            Ordering::Equal => {}
            ord => return ord,
        }

        // Compare tag
        self.tag.cmp(&other.tag)
    }
}

/// Controls which annotation details to keep when stripping annotations
/// Matches the behavior of C++ keep_details_t
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeepDetails {
    /// Keep price annotations
    pub keep_price: bool,

    /// Keep date annotations
    pub keep_date: bool,

    /// Keep tag annotations
    pub keep_tag: bool,

    /// Only keep actual (non-calculated) annotations
    pub only_actuals: bool,
}

impl KeepDetails {
    /// Keep no details
    pub fn none() -> Self {
        Self { keep_price: false, keep_date: false, keep_tag: false, only_actuals: false }
    }

    /// Keep all details
    pub fn all() -> Self {
        Self { keep_price: true, keep_date: true, keep_tag: true, only_actuals: false }
    }

    /// Keep only actual details
    pub fn actuals_only() -> Self {
        Self { keep_price: true, keep_date: true, keep_tag: true, only_actuals: true }
    }

    /// Check if we should keep all details
    pub fn keep_all(&self) -> bool {
        self.keep_price && self.keep_date && self.keep_tag && !self.only_actuals
    }

    /// Check if we should keep any details
    pub fn keep_any(&self) -> bool {
        self.keep_price || self.keep_date || self.keep_tag
    }
}

impl Default for KeepDetails {
    fn default() -> Self {
        Self::none()
    }
}

/// An annotated commodity with additional lot information
/// Matches the behavior of C++ annotated_commodity_t
#[derive(Debug, Clone)]
pub struct AnnotatedCommodity {
    /// Reference to the underlying commodity
    commodity: CommodityRef,

    /// Annotation details for this instance
    annotation: Annotation,
}

impl AnnotatedCommodity {
    /// Create a new annotated commodity
    pub fn new(commodity: CommodityRef, annotation: Annotation) -> Self {
        Self { commodity, annotation }
    }

    /// Get the underlying commodity
    pub fn commodity(&self) -> &CommodityRef {
        &self.commodity
    }

    /// Get the commodity referent (dereference)
    pub fn referent(&self) -> &Commodity {
        &self.commodity
    }

    /// Get the annotation
    pub fn annotation(&self) -> &Annotation {
        &self.annotation
    }

    /// Get mutable annotation
    pub fn annotation_mut(&mut self) -> &mut Annotation {
        &mut self.annotation
    }

    /// Check if this commodity has annotations
    pub fn has_annotation(&self) -> bool {
        !self.annotation.is_empty()
    }

    /// Strip annotations based on keep rules
    pub fn strip_annotations(&self, keep_details: &KeepDetails) -> CommodityRef {
        if !keep_details.keep_any()
            || (keep_details.only_actuals && self.has_calculated_annotations())
        {
            // Return the base commodity
            self.commodity.clone()
        } else {
            // Create new annotated commodity with filtered annotations
            let mut new_annotation = Annotation::new();

            if keep_details.keep_price && self.annotation.price.is_some() {
                new_annotation.price = self.annotation.price.clone();
            }
            if keep_details.keep_date && self.annotation.date.is_some() {
                new_annotation.date = self.annotation.date;
            }
            if keep_details.keep_tag && self.annotation.tag.is_some() {
                new_annotation.tag = self.annotation.tag.clone();
            }

            if new_annotation.is_empty() {
                self.commodity.clone()
            } else {
                Arc::new(Commodity::new("")) // TODO: Proper implementation needed
            }
        }
    }

    /// Check if annotations are calculated (not actual)
    fn has_calculated_annotations(&self) -> bool {
        let flags = self.annotation.flags();
        flags.contains(AnnotationFlags::PRICE_CALCULATED)
            || flags.contains(AnnotationFlags::DATE_CALCULATED)
            || flags.contains(AnnotationFlags::TAG_CALCULATED)
            || flags.contains(AnnotationFlags::VALUE_EXPR_CALCULATED)
    }

    /// Get the symbol (delegated to underlying commodity)
    pub fn symbol(&self) -> &str {
        self.commodity.symbol()
    }

    /// Get the precision (delegated to underlying commodity)
    pub fn precision(&self) -> Precision {
        self.commodity.precision()
    }
}

impl PartialEq for AnnotatedCommodity {
    fn eq(&self, other: &Self) -> bool {
        // Two annotated commodities are equal if their base commodities
        // and annotations are equal
        Arc::ptr_eq(&self.commodity, &other.commodity) && self.annotation == other.annotation
    }
}

impl Eq for AnnotatedCommodity {}

impl fmt::Display for AnnotatedCommodity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.commodity)?;

        // Write annotation details
        if !self.annotation.is_empty() {
            write!(f, " {{")?;
            let mut first = true;

            if let Some(ref _price) = self.annotation.price {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "price")?; // TODO: Display actual price when Amount is available
                first = false;
            }

            if let Some(ref date) = self.annotation.date {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "{}", date.format("%Y-%m-%d"))?;
                first = false;
            }

            if let Some(ref tag) = self.annotation.tag {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "({})", tag)?;
            }

            write!(f, "}}")?;
        }

        Ok(())
    }
}

/// Global null commodity reference for amounts without commodities
/// This matches the C++ null_commodity concept
pub fn null_commodity() -> CommodityRef {
    static NULL_COMMODITY: std::sync::OnceLock<CommodityRef> = std::sync::OnceLock::new();
    NULL_COMMODITY.get_or_init(|| Arc::new(Commodity::new(""))).clone()
}

/// Pool for managing commodity instances and their annotations
/// Matches the behavior of C++ commodity_pool_t
#[derive(Debug)]
pub struct CommodityPool {
    /// Map of commodity symbols to commodity instances
    commodities: HashMap<String, CommodityRef>,

    /// Map of annotated commodities keyed by (symbol, annotation)
    annotated_commodities: HashMap<(String, Annotation), Arc<AnnotatedCommodity>>,

    /// Default commodity for operations
    default_commodity: Option<CommodityRef>,

    /// Whether to keep base commodities without annotations
    keep_base: bool,
}

impl CommodityPool {
    /// Create a new commodity pool
    pub fn new() -> Self {
        Self {
            commodities: HashMap::new(),
            annotated_commodities: HashMap::new(),
            default_commodity: None,
            keep_base: false,
        }
    }

    /// Find or create a commodity by symbol
    pub fn find_or_create(&mut self, symbol: &str) -> CommodityRef {
        if let Some(commodity) = self.commodities.get(symbol) {
            commodity.clone()
        } else {
            let commodity = Arc::new(Commodity::new(symbol));
            self.commodities.insert(symbol.to_string(), commodity.clone());
            commodity
        }
    }

    /// Find an existing commodity by symbol
    pub fn find(&self, symbol: &str) -> Option<CommodityRef> {
        self.commodities.get(symbol).cloned()
    }

    /// Create or find an annotated commodity
    pub fn find_or_create_annotated(
        &mut self,
        symbol: &str,
        annotation: Annotation,
    ) -> Arc<AnnotatedCommodity> {
        let key = (symbol.to_string(), annotation.clone());

        if let Some(annotated) = self.annotated_commodities.get(&key) {
            annotated.clone()
        } else {
            let base_commodity = self.find_or_create(symbol);
            let annotated = Arc::new(AnnotatedCommodity::new(base_commodity, annotation));
            self.annotated_commodities.insert(key, annotated.clone());
            annotated
        }
    }

    /// Find an existing annotated commodity
    pub fn find_annotated(
        &self,
        symbol: &str,
        annotation: &Annotation,
    ) -> Option<Arc<AnnotatedCommodity>> {
        let key = (symbol.to_string(), annotation.clone());
        self.annotated_commodities.get(&key).cloned()
    }

    /// Get all commodities in the pool
    pub fn commodities(&self) -> impl Iterator<Item = &CommodityRef> {
        self.commodities.values()
    }

    /// Get all annotated commodities in the pool
    pub fn annotated_commodities(&self) -> impl Iterator<Item = &Arc<AnnotatedCommodity>> {
        self.annotated_commodities.values()
    }

    /// Set the default commodity
    pub fn set_default_commodity(&mut self, commodity: CommodityRef) {
        self.default_commodity = Some(commodity);
    }

    /// Get the default commodity
    pub fn default_commodity(&self) -> Option<&CommodityRef> {
        self.default_commodity.as_ref()
    }

    /// Set keep base flag
    pub fn set_keep_base(&mut self, keep_base: bool) {
        self.keep_base = keep_base;
    }

    /// Get keep base flag
    pub fn keep_base(&self) -> bool {
        self.keep_base
    }

    /// Get the null commodity
    pub fn null_commodity(&self) -> CommodityRef {
        null_commodity()
    }

    /// Clear all commodities from the pool
    pub fn clear(&mut self) {
        self.commodities.clear();
        self.annotated_commodities.clear();
        self.default_commodity = None;
    }

    /// Get commodity count
    pub fn commodity_count(&self) -> usize {
        self.commodities.len()
    }

    /// Get annotated commodity count
    pub fn annotated_commodity_count(&self) -> usize {
        self.annotated_commodities.len()
    }
}

impl Default for CommodityPool {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_commodity_creation() {
        let usd = Commodity::new("USD");
        assert_eq!(usd.symbol(), "USD");
        assert_eq!(usd.precision(), 0);
    }

    #[test]
    fn test_commodity_with_precision() {
        let usd = Commodity::with_precision("$", 2);
        assert_eq!(usd.symbol(), "$");
        assert_eq!(usd.precision(), 2);
    }

    #[test]
    fn test_commodity_flags() {
        let mut usd = Commodity::new("$");
        assert!(!usd.has_flags(CommodityFlags::STYLE_THOUSANDS));

        usd.add_flags(CommodityFlags::STYLE_THOUSANDS);
        assert!(usd.has_flags(CommodityFlags::STYLE_THOUSANDS));

        usd.drop_flags(CommodityFlags::STYLE_THOUSANDS);
        assert!(!usd.has_flags(CommodityFlags::STYLE_THOUSANDS));
    }

    #[test]
    fn test_annotation_creation() {
        let annotation = Annotation::new();
        assert!(annotation.is_empty());

        let date = NaiveDate::from_ymd_opt(2023, 12, 31).unwrap();
        let annotation_with_date = Annotation::with_date(date);
        assert!(!annotation_with_date.is_empty());
        assert_eq!(annotation_with_date.date(), &Some(date));

        let annotation_with_tag = Annotation::with_tag("lot1".to_string());
        assert!(!annotation_with_tag.is_empty());
        assert_eq!(annotation_with_tag.tag(), &Some("lot1".to_string()));
    }

    #[test]
    fn test_annotation_comparison() {
        let date1 = NaiveDate::from_ymd_opt(2023, 12, 31).unwrap();
        let date2 = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();

        let ann1 = Annotation::with_date(date1);
        let ann2 = Annotation::with_date(date2);
        let ann3 = Annotation::with_date(date1);

        assert_ne!(ann1, ann2);
        assert_eq!(ann1, ann3);
        assert!(ann1 < ann2);
    }

    #[test]
    fn test_keep_details() {
        let keep_none = KeepDetails::none();
        assert!(!keep_none.keep_any());
        assert!(!keep_none.keep_all());

        let keep_all = KeepDetails::all();
        assert!(keep_all.keep_any());
        assert!(keep_all.keep_all());

        let keep_actuals = KeepDetails::actuals_only();
        assert!(keep_actuals.keep_any());
        assert!(!keep_actuals.keep_all()); // because only_actuals is true
    }

    #[test]
    fn test_annotated_commodity() {
        let commodity = Arc::new(Commodity::new("AAPL"));
        let date = NaiveDate::from_ymd_opt(2023, 12, 31).unwrap();
        let annotation = Annotation::with_date(date);

        let annotated = AnnotatedCommodity::new(commodity.clone(), annotation);
        assert!(annotated.has_annotation());
        assert_eq!(annotated.symbol(), "AAPL");
        assert_eq!(annotated.referent().symbol(), "AAPL");

        // Test stripping annotations
        let keep_none = KeepDetails::none();
        let stripped = annotated.strip_annotations(&keep_none);
        assert!(Arc::ptr_eq(&stripped, &commodity));
    }

    #[test]
    fn test_commodity_pool() {
        let mut pool = CommodityPool::new();

        // Test finding/creating commodities
        let usd1 = pool.find_or_create("USD");
        let usd2 = pool.find_or_create("USD");
        assert!(Arc::ptr_eq(&usd1, &usd2));
        assert_eq!(pool.commodity_count(), 1);

        // Test annotated commodities
        let date = NaiveDate::from_ymd_opt(2023, 12, 31).unwrap();
        let annotation = Annotation::with_date(date);
        let annotated1 = pool.find_or_create_annotated("USD", annotation.clone());
        let annotated2 = pool.find_or_create_annotated("USD", annotation);
        assert!(Arc::ptr_eq(&annotated1, &annotated2));
        assert_eq!(pool.annotated_commodity_count(), 1);

        // Test default commodity
        pool.set_default_commodity(usd1.clone());
        assert!(Arc::ptr_eq(pool.default_commodity().unwrap(), &usd1));
    }

    #[test]
    fn test_null_commodity() {
        let null = null_commodity();
        assert_eq!(null.symbol(), "");
    }
}
