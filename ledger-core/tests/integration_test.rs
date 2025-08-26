//! Integration tests for ledger-core

use ledger_core::amount::Amount;
use ledger_core::balance::Balance;
use ledger_math::commodity::Commodity;
use rust_decimal::Decimal;
use std::sync::Arc;

#[test]
fn test_amount_balance_integration() {
    let mut balance = Balance::new();
    let usd_commodity = Some(Arc::new(Commodity::new("USD")));
    let eur_commodity = Some(Arc::new(Commodity::new("EUR")));
    let amount1 = Amount::with_commodity(Decimal::from(100), usd_commodity);
    let amount2 = Amount::with_commodity(Decimal::from(50), eur_commodity);
    
    balance.add_amount(&amount1);
    balance.add_amount(&amount2);
    
    assert_eq!(balance.commodity_count(), 2);
}

#[test]
fn test_amount_display() {
    let usd_commodity = Some(Arc::new(Commodity::new("USD")));
    let amount = Amount::with_commodity(Decimal::from(100), usd_commodity);
    let display = format!("{}", amount);
    assert_eq!(display, "USD 100");
}