//! Basic usage example for ledger-core

use ledger_core::amount::Amount;
use ledger_core::balance::Balance;
use rust_decimal::Decimal;

fn main() {
    // Create some amounts with different commodities
    let usd_amount = Amount::with_commodity(Decimal::from(100), "USD".to_string());
    let eur_amount = Amount::with_commodity(Decimal::from(50), "EUR".to_string());
    
    // Add them to a balance
    let mut balance = Balance::new();
    balance.add_amount(usd_amount.clone());
    balance.add_amount(eur_amount.clone());
    
    println!("Created balance with {} commodities", balance.commodity_count());
    println!("USD amount: {}", usd_amount);
    println!("EUR amount: {}", eur_amount);
}