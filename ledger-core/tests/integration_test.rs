//! Integration tests for ledger-core

use ledger_core::amount::Amount;
use ledger_core::balance::Balance;
use rust_decimal::Decimal;

#[test]
fn test_amount_balance_integration() {
    let mut balance = Balance::new();
    let amount1 = Amount::with_commodity(Decimal::from(100), "USD".to_string());
    let amount2 = Amount::with_commodity(Decimal::from(50), "EUR".to_string());
    
    balance.add_amount(amount1);
    balance.add_amount(amount2);
    
    assert_eq!(balance.commodity_count(), 2);
}

#[test]
fn test_amount_display() {
    let amount = Amount::with_commodity(Decimal::from(100), "USD".to_string());
    let display = format!("{}", amount);
    assert_eq!(display, "USD 100");
}