//! Basic usage example for ledger-core

use ledger_core::amount::Amount;
use ledger_core::balance::Balance;
use ledger_math::commodity::Commodity;
use rust_decimal::Decimal;
use std::sync::Arc;

fn main() {
    // Create some amounts with different commodities
    let usd_commodity = Some(Arc::new(Commodity::new("USD")));
    let eur_commodity = Some(Arc::new(Commodity::new("EUR")));
    let usd_amount = Amount::with_commodity(Decimal::from(100), usd_commodity);
    let eur_amount = Amount::with_commodity(Decimal::from(50), eur_commodity);

    // Add them to a balance
    let mut balance = Balance::new();
    balance.add_amount(&usd_amount).unwrap();
    balance.add_amount(&eur_amount).unwrap();

    println!("Created balance with {} commodities", balance.commodity_count());
    println!("USD amount: {}", usd_amount);
    println!("EUR amount: {}", eur_amount);
}
