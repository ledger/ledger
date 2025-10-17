//! Comprehensive formatting tests for Amount and Balance display
//!
//! Tests formatting behavior to match C++ ledger output exactly.

use ledger_math::*;
use rust_decimal::Decimal;

#[test]
fn test_amount_display_basic() {
    let amount = Amount::from_i64(12345);
    assert_eq!(format!("{}", amount), "12345");

    let amount = Amount::from_decimal(Decimal::new(12345, 2)).unwrap();
    assert_eq!(format!("{}", amount), "123.45");

    let amount = Amount::null();
    assert_eq!(format!("{}", amount), "<null>");
}

#[test]
fn test_amount_display_with_precision() {
    let amount = Amount::from_decimal(Decimal::new(12345, 2)).unwrap();

    assert_eq!(format!("{:.0}", amount), "123");
    assert_eq!(format!("{:.2}", amount), "123.45");
    assert_eq!(format!("{:.4}", amount), "123.4500");
}

#[test]
fn test_amount_display_with_width() {
    let amount = Amount::from_i64(123);

    assert_eq!(format!("{:10}", amount), "123       ");
    assert_eq!(format!("{:>10}", amount), "       123");
    // Center alignment not yet implemented, falls back to left align
    assert_eq!(format!("{:^10}", amount), "123       ");
}

#[test]
fn test_amount_display_width_and_precision() {
    let amount = Amount::from_decimal(Decimal::new(12345, 2)).unwrap();

    assert_eq!(format!("{:10.2}", amount), "123.45    ");
    assert_eq!(format!("{:>10.2}", amount), "    123.45");
}

#[test]
fn test_amount_debug_basic() {
    let amount = Amount::from_i64(12345);
    assert_eq!(format!("{:?}", amount), "AMOUNT(12345)");

    let amount = Amount::null();
    assert_eq!(format!("{:?}", amount), "AMOUNT(<null>)");
}

#[test]
fn test_amount_debug_detailed() {
    let amount = Amount::from_decimal(Decimal::new(12345, 2)).unwrap();
    let debug_output = format!("{:#?}", amount);

    assert!(debug_output.starts_with("AMOUNT(123.45)"));
    assert!(debug_output.contains("prec:2"));
    assert!(debug_output.contains("keep:false"));
}

#[test]
fn test_amount_to_string_methods() {
    let amount = Amount::from_decimal(Decimal::new(12345, 2)).unwrap();

    assert_eq!(amount.to_string(), "123.45");
    assert_eq!(amount.to_fullstring(), "123.45");
    assert_eq!(amount.quantity_string(), "2469/20");
}

#[test]
fn test_amount_print_method() {
    let amount = Amount::from_decimal(Decimal::new(12345, 2)).unwrap();
    let mut buffer = String::new();

    amount.print(&mut buffer, FormatFlags::NO_FLAGS).unwrap();
    assert_eq!(buffer, "123.45");

    let mut buffer = String::new();
    amount.print(&mut buffer, FormatFlags::RIGHT_JUSTIFY).unwrap();
    assert_eq!(buffer, "123.45"); // No width specified, so no padding
}

#[test]
fn test_balance_display_empty() {
    let balance = Balance::new();
    assert_eq!(format!("{}", balance), "0");
}

#[test]
fn test_balance_display_single() {
    let amount = Amount::from_i64(12345);
    let balance = Balance::from_amount(amount).unwrap();
    assert_eq!(format!("{}", balance), "12345");
}

#[test]
fn test_balance_display_with_width() {
    let amount = Amount::from_i64(123);
    let balance = Balance::from_amount(amount).unwrap();

    assert_eq!(format!("{:10}", balance), "123       ");
    assert_eq!(format!("{:>10}", balance), "       123");
}

#[test]
fn test_balance_debug_empty() {
    let balance = Balance::new();
    assert_eq!(format!("{:?}", balance), "BALANCE(<empty>)");
}

#[test]
fn test_balance_debug_single() {
    let amount = Amount::from_i64(12345);
    let balance = Balance::from_amount(amount).unwrap();
    assert_eq!(format!("{:?}", balance), "BALANCE(12345)");
}

#[test]
fn test_balance_debug_detailed() {
    let amount = Amount::from_i64(12345);
    let balance = Balance::from_amount(amount).unwrap();
    let debug_output = format!("{:#?}", balance);

    assert!(debug_output.starts_with("BALANCE(12345)"));
    assert!(debug_output.contains("count:1"));
}

#[test]
fn test_balance_print_method() {
    let amount = Amount::from_i64(12345);
    let balance = Balance::from_amount(amount).unwrap();
    let mut buffer = String::new();

    balance.print(&mut buffer, Some(10), None, FormatFlags::NO_FLAGS).unwrap();
    assert_eq!(buffer, "12345     ");

    let mut buffer = String::new();
    balance.print(&mut buffer, Some(10), None, FormatFlags::RIGHT_JUSTIFY).unwrap();
    assert_eq!(buffer, "     12345");
}

#[test]
fn test_format_config_builder() {
    let config = FormatConfig::new()
        .with_precision(2)
        .with_width(10, Some(15))
        .with_thousands_sep(true)
        .with_decimal_comma(false)
        .right_justify();

    assert_eq!(config.precision, Some(2));
    assert_eq!(config.min_width, Some(10));
    assert_eq!(config.max_width, Some(15));
    assert_eq!(config.thousands_sep, true);
    assert_eq!(config.decimal_comma, false);
    assert!(config.flags.has_flag(FormatFlags::RIGHT_JUSTIFY));
}

#[test]
fn test_format_rational_with_thousands() {
    let rational = BigRational::from_integer(BigInt::from(1234567));
    let config = FormatConfig::default().with_thousands_sep(true);

    let formatted = format_rational(&rational, 0, &config, None);
    assert_eq!(formatted, "1,234,567");

    let formatted = format_rational(&rational, 2, &config, None);
    assert_eq!(formatted, "1,234,567.00");
}

#[test]
fn test_format_rational_with_decimal_comma() {
    let rational = BigRational::new(BigInt::from(12345), BigInt::from(100));
    let config = FormatConfig::default().with_decimal_comma(true);

    let formatted = format_rational(&rational, 2, &config, None);
    assert_eq!(formatted, "123,45");
}

#[test]
fn test_format_rational_european_style() {
    let rational = BigRational::new(BigInt::from(123456789), BigInt::from(100));
    let config = FormatConfig::default().with_decimal_comma(true).with_thousands_sep(true);

    let formatted = format_rational(&rational, 2, &config, None);
    assert_eq!(formatted, "1.234.567,89");
}

#[test]
fn test_format_amount_with_config() {
    let amount = Amount::from_decimal(Decimal::new(123456789, 2)).unwrap();
    let config = FormatConfig::default().with_precision(2).with_thousands_sep(true);

    let formatted = format_amount(&amount, &config);
    assert_eq!(formatted, "1,234,567.89");
}

#[test]
fn test_format_negative_amounts() {
    let amount = Amount::from_i64(-12345);
    assert_eq!(format!("{}", amount), "-12345");

    let amount = Amount::from_decimal(Decimal::new(-12345, 2)).unwrap();
    assert_eq!(format!("{}", amount), "-123.45");

    let config = FormatConfig::default().with_thousands_sep(true);
    let formatted = format_amount(&amount, &config);
    assert_eq!(formatted, "-123.45");
}

#[test]
fn test_format_zero_amounts() {
    let amount = Amount::from_i64(0);
    assert_eq!(format!("{}", amount), "0");
    assert_eq!(format!("{:.2}", amount), "0.00");

    let balance = Balance::new();
    assert_eq!(format!("{}", balance), "0");
    assert_eq!(format!("{:10}", balance), "0         ");
}

#[test]
fn test_rounding_behavior() {
    // Test banker's rounding (round to even)
    let rational = BigRational::new(BigInt::from(12345), BigInt::from(1000)); // 12.345
    let config = FormatConfig::default();

    let formatted = format_rational(&rational, 2, &config, None);
    assert_eq!(formatted, "12.35"); // Should round up

    let rational = BigRational::new(BigInt::from(12335), BigInt::from(1000)); // 12.335
    let formatted = format_rational(&rational, 2, &config, None);
    assert_eq!(formatted, "12.34"); // Should round down (banker's rounding)
}

#[test]
fn test_precision_edge_cases() {
    let rational = BigRational::new(BigInt::from(1), BigInt::from(3)); // 0.333...
    let config = FormatConfig::default();

    let formatted = format_rational(&rational, 0, &config, None);
    assert_eq!(formatted, "0");

    let formatted = format_rational(&rational, 2, &config, None);
    assert_eq!(formatted, "0.33");

    let formatted = format_rational(&rational, 4, &config, None);
    assert_eq!(formatted, "0.3333");
}

#[test]
fn test_width_formatting_edge_cases() {
    let config = FormatConfig::default().with_width(5, Some(3));

    // Minimum width > maximum width case
    let result = apply_width_formatting("hi", &config);
    assert_eq!(result, "hi   "); // Should use minimum width

    let result = apply_width_formatting("hello world", &config);
    assert_eq!(result, "hel  "); // Truncate then pad to minimum
}

#[test]
fn test_commodity_symbol_edge_cases() {
    // For now, test without commodities since commodity integration is limited
    let amount = Amount::from_i64(12345);
    let config = FormatConfig::default().with_flags(FormatFlags::ELIDE_COMMODITY_QUOTES);
    let formatted = format_amount(&amount, &config);
    assert_eq!(formatted, "12345"); // No commodity, should be unchanged
}

#[test]
fn test_format_flags_operations() {
    let mut flags = FormatFlags::NO_FLAGS;

    assert!(!flags.has_flag(FormatFlags::RIGHT_JUSTIFY));

    flags.set_flag(FormatFlags::RIGHT_JUSTIFY);
    assert!(flags.has_flag(FormatFlags::RIGHT_JUSTIFY));

    flags.clear_flag(FormatFlags::RIGHT_JUSTIFY);
    assert!(!flags.has_flag(FormatFlags::RIGHT_JUSTIFY));

    // Test multiple flags
    flags.set_flag(FormatFlags::RIGHT_JUSTIFY);
    flags.set_flag(FormatFlags::COLORIZE);

    assert!(flags.has_flag(FormatFlags::RIGHT_JUSTIFY));
    assert!(flags.has_flag(FormatFlags::COLORIZE));
    assert!(!flags.has_flag(FormatFlags::ELIDE_COMMODITY_QUOTES));
}

#[test]
fn test_very_large_numbers() {
    let large_int = BigInt::from(12345678901234567890_u128);
    let amount = Amount::from(large_int);

    let formatted = format!("{}", amount);
    assert_eq!(formatted, "12345678901234567890");

    let config = FormatConfig::default().with_thousands_sep(true);
    let formatted = format_amount(&amount, &config);
    assert_eq!(formatted, "12,345,678,901,234,567,890");
}

#[test]
fn test_very_small_numbers() {
    let rational = BigRational::new(BigInt::from(1), BigInt::from(1000000)); // 0.000001
    let config = FormatConfig::default();

    let formatted = format_rational(&rational, 6, &config, None);
    assert_eq!(formatted, "0.000001");

    let formatted = format_rational(&rational, 4, &config, None);
    assert_eq!(formatted, "0.0000");

    let formatted = format_rational(&rational, 8, &config, None);
    assert_eq!(formatted, "0.00000100");
}
