//! Commodity definitions and management

use serde::{Deserialize, Serialize};
use crate::strings::{CommoditySymbol, intern_string};

/// Represents a commodity (currency, stock, etc.)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Commodity {
    /// Symbol or name of the commodity - optimized for short symbols like "USD", "EUR"
    pub symbol: CommoditySymbol,
    /// Display name - optimized memory usage
    pub name: Option<CommoditySymbol>,
    /// Default precision for display
    pub precision: u32,
}

impl Commodity {
    /// Create a new commodity
    pub fn new(symbol: String) -> Self {
        Self { 
            symbol: intern_string(&symbol), 
            name: None, 
            precision: 2 
        }
    }

    /// Create a new commodity with optimized string interning  
    pub fn new_interned(symbol: CommoditySymbol) -> Self {
        Self { 
            symbol, 
            name: None, 
            precision: 2 
        }
    }
}
