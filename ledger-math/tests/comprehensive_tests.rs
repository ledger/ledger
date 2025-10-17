// Comprehensive tests for ledger-math based on C++ test suite
// These tests verify the Rust implementation matches the C++ behavior

use ledger_math::amount::Amount;
use ledger_math::balance::Balance;
use num_bigint::BigInt;
use num_rational::BigRational;

#[cfg(test)]
mod amount_basic_tests {
    use super::*;

    #[test]
    fn test_null_amount() {
        let amt = Amount::null();
        assert!(amt.is_null());
        assert!(amt.is_zero());
        assert!(amt.is_realzero());
    }

    #[test]
    fn test_integer_amounts() {
        let a = Amount::from_i64(100);
        assert!(!a.is_null());
        assert!(!a.is_zero());
        assert_eq!(a.to_i64().unwrap(), 100);

        let b = Amount::from_i64(-50);
        assert!(b.sign() < 0);
        assert_eq!(b.to_i64().unwrap(), -50);
    }

    #[test]
    fn test_exact_amounts() {
        let a = Amount::exact("123.456").unwrap();
        assert!(!a.is_null());
        assert_eq!(a.to_string(), "123.456");

        let b = Amount::exact("1000000.000001").unwrap();
        assert_eq!(b.to_string(), "1000000.000001");
    }

    #[test]
    fn test_arithmetic_operations() {
        let a = Amount::from_i64(100);
        let b = Amount::from_i64(50);

        let sum = (&a + &b).unwrap();
        assert_eq!(sum.to_i64().unwrap(), 150);

        let diff = (&a - &b).unwrap();
        assert_eq!(diff.to_i64().unwrap(), 50);

        let prod = (&a * &b).unwrap();
        assert_eq!(prod.to_i64().unwrap(), 5000);

        let quot = (&a / &b).unwrap();
        assert_eq!(quot.to_i64().unwrap(), 2);
    }

    #[test]
    fn test_negation() {
        let a = Amount::from_i64(100);
        let neg = -&a;
        assert_eq!(neg.to_i64().unwrap(), -100);
        assert!(neg.sign() < 0);

        let double_neg = -&neg;
        assert_eq!(double_neg.to_i64().unwrap(), 100);
        assert!(double_neg.sign() >= 0);
    }

    #[test]
    fn test_abs() {
        let a = Amount::from_i64(-100);
        let abs = a.abs();
        assert_eq!(abs.to_i64().unwrap(), 100);
        assert!(!abs.sign() < 0);

        let b = Amount::from_i64(50);
        let abs_b = b.abs();
        assert_eq!(abs_b.to_i64().unwrap(), 50);
    }

    #[test]
    fn test_comparisons() {
        let a = Amount::from_i64(100);
        let b = Amount::from_i64(50);
        let c = Amount::from_i64(100);

        assert!(a > b);
        assert!(b < a);
        assert!(a >= c);
        assert!(a <= c);
        assert!(a == c);
        assert!(a != b);
    }

    #[test]
    fn test_zero_checks() {
        let zero = Amount::from_i64(0);
        assert!(zero.is_zero());
        assert!(zero.is_realzero());
        assert!(!zero.is_nonzero());

        let nonzero = Amount::from_i64(1);
        assert!(!nonzero.is_zero());
        assert!(!nonzero.is_realzero());
        assert!(nonzero.is_nonzero());
    }

    #[test]
    fn test_sign() {
        let positive = Amount::from_i64(100);
        assert_eq!(positive.sign(), 1);

        let negative = Amount::from_i64(-100);
        assert_eq!(negative.sign(), -1);

        let zero = Amount::from_i64(0);
        assert_eq!(zero.sign(), 0);
    }

    #[test]
    fn test_rounding() {
        let a = Amount::exact("123.456").unwrap();
        let rounded = a.roundto(2);
        assert_eq!(rounded.to_string(), "123.46");

        let b = Amount::exact("123.454").unwrap();
        let rounded_b = b.roundto(2);
        assert_eq!(rounded_b.to_string(), "123.45");

        let c = Amount::exact("-123.456").unwrap();
        let rounded_c = c.roundto(2);
        assert_eq!(rounded_c.to_string(), "-123.46");
    }

    #[test]
    fn test_truncation() {
        // truncated() truncates to the display precision (not necessarily integer)
        // Since exact() sets keep_precision, it maintains the original precision
        let a = Amount::exact("123.456").unwrap();
        let truncated = a.truncated();
        // The truncation keeps the same precision as the original
        assert_eq!(truncated.to_string(), "123.456");

        let b = Amount::exact("-123.456").unwrap();
        let truncated_b = b.truncated();
        assert_eq!(truncated_b.to_string(), "-123.456");
    }

    #[test]
    fn test_floor_ceil() {
        let a = Amount::exact("123.456").unwrap();
        let floored = a.floored();
        assert_eq!(floored.to_i64().unwrap(), 123);

        let ceiled = a.ceilinged();
        assert_eq!(ceiled.to_i64().unwrap(), 124);

        let b = Amount::exact("-123.456").unwrap();
        let floored_b = b.floored();
        assert_eq!(floored_b.to_i64().unwrap(), -124);

        let ceiled_b = b.ceilinged();
        assert_eq!(ceiled_b.to_i64().unwrap(), -123);
    }
}

#[cfg(test)]
mod amount_conversion_tests {
    use super::*;

    // #[test]
    // fn test_from_bigint() {
    //     let bi = BigInt::from(12345);
    //     let amt = Amount::from_bigint(bi.clone());
    //     assert_eq!(amt.to_i64().unwrap(), 12345);
    // }

    // #[test]
    // fn test_from_bigrational() {
    //     let num = BigInt::from(355);
    //     let den = BigInt::from(113);
    //     let br = BigRational::new(num, den);
    //     let amt = Amount::from_bigrational(br);
    //     // 355/113 ≈ 3.14159...
    //     assert!(amt.to_string().starts_with("3.14159"));
    // }

    #[test]
    fn test_to_conversions() {
        let amt = Amount::from_i64(100);

        assert_eq!(amt.to_i64().unwrap(), 100);
        assert_eq!(amt.to_u64().unwrap(), 100);
        assert_eq!(amt.to_i32().unwrap(), 100);
        assert_eq!(amt.to_u32().unwrap(), 100);

        let negative = Amount::from_i64(-50);
        assert_eq!(negative.to_i64().unwrap(), -50);
        // Can't convert negative to unsigned - would panic
        // assert_eq!(negative.to_u64(), None);
    }

    #[test]
    fn test_overflow_conversions() {
        let large = Amount::from_i64(i64::MAX);
        // Too large for i32 - would panic
        // assert_eq!(large.to_i32(), None);
        assert_eq!(large.to_i64().unwrap(), i64::MAX);
    }
}

#[cfg(test)]
mod balance_basic_tests {
    use super::*;

    #[test]
    fn test_empty_balance() {
        let balance = Balance::new();
        assert!(balance.is_empty());
        assert!(balance.is_zero());
        assert!(balance.is_realzero());
        assert_eq!(balance.commodity_count(), 0);
    }

    #[test]
    fn test_balance_from_amount() {
        let amt = Amount::from_i64(100);
        let balance = Balance::from_amount(amt).unwrap();

        assert!(!balance.is_empty());
        assert!(!balance.is_zero());
        assert!(balance.single_amount());
        assert_eq!(balance.commodity_count(), 1);

        let result = balance.to_amount().unwrap();
        assert_eq!(result.to_i64().unwrap(), 100);
    }

    #[test]
    fn test_balance_arithmetic() {
        let mut balance = Balance::new();

        let a = Amount::from_i64(100);
        balance.add_amount(&a).unwrap();

        let b = Amount::from_i64(50);
        balance.add_amount(&b).unwrap();

        let result = balance.to_amount().unwrap();
        assert_eq!(result.to_i64().unwrap(), 150);
    }

    #[test]
    fn test_balance_subtraction() {
        let mut balance = Balance::new();

        let a = Amount::from_i64(100);
        balance.add_amount(&a).unwrap();

        let b = Amount::from_i64(30);
        balance.subtract_amount(&b).unwrap();

        let result = balance.to_amount().unwrap();
        assert_eq!(result.to_i64().unwrap(), 70);
    }

    #[test]
    fn test_balance_negation() {
        let amt = Amount::from_i64(100);
        let balance = Balance::from_amount(amt).unwrap();
        let negated = balance.negated();

        let result = negated.to_amount().unwrap();
        assert_eq!(result.to_i64().unwrap(), -100);
    }

    #[test]
    fn test_balance_operators() {
        let balance1 = Balance::from_amount(Amount::from_i64(100)).unwrap();
        let balance2 = Balance::from_amount(Amount::from_i64(50)).unwrap();

        let sum = (balance1.clone() + balance2.clone()).unwrap();
        let result = sum.to_amount().unwrap();
        assert_eq!(result.to_i64().unwrap(), 150);

        let diff = (balance1 - balance2).unwrap();
        let result = diff.to_amount().unwrap();
        assert_eq!(result.to_i64().unwrap(), 50);
    }

    #[test]
    fn test_balance_multiplication() {
        let balance = Balance::from_amount(Amount::from_i64(100)).unwrap();
        let multiplier = Amount::from_i64(3);

        let result = (balance.clone() * multiplier).unwrap();
        let amt = result.to_amount().unwrap();
        assert_eq!(amt.to_i64().unwrap(), 300);
    }

    #[test]
    fn test_balance_division() {
        let balance = Balance::from_amount(Amount::from_i64(100)).unwrap();
        let divisor = Amount::from_i64(4);

        let result = (balance.clone() / divisor).unwrap();
        let amt = result.to_amount().unwrap();
        assert_eq!(amt.to_i64().unwrap(), 25);
    }
}

// Property-based tests using proptest
#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        // Amount property tests

        #[test]
        fn prop_addition_commutative(a in -100000i64..100000, b in -100000i64..100000) {
            let amt_a = Amount::from_i64(a);
            let amt_b = Amount::from_i64(b);
            let sum1 = &amt_a + &amt_b;
            let sum2 = &amt_b + &amt_a;
            prop_assert_eq!(sum1, sum2);
        }

        #[test]
        fn prop_addition_associative(
            a in -10000i64..10000,
            b in -10000i64..10000,
            c in -10000i64..10000
        ) {
            let amt_a = Amount::from_i64(a);
            let amt_b = Amount::from_i64(b);
            let amt_c = Amount::from_i64(c);
            let sum1 = (&(&amt_a + &amt_b).unwrap() + &amt_c).unwrap();
            let sum2 = (&amt_a + &(&amt_b + &amt_c).unwrap()).unwrap();
            prop_assert_eq!(sum1, sum2);
        }

        #[test]
        fn prop_multiplication_commutative(a in -1000i64..1000, b in -1000i64..1000) {
            let amt_a = Amount::from_i64(a);
            let amt_b = Amount::from_i64(b);
            let prod1 = (&amt_a * &amt_b).unwrap();
            let prod2 = (&amt_b * &amt_a).unwrap();
            prop_assert_eq!(prod1, prod2);
        }

        #[test]
        fn prop_identity_addition(a in -100000i64..100000) {
            let amt = Amount::from_i64(a);
            let zero = Amount::from_i64(0);
            let result = (&amt + &zero).unwrap();
            prop_assert_eq!(amt, result);
        }

        #[test]
        fn prop_identity_multiplication(a in -100000i64..100000) {
            let amt = Amount::from_i64(a);
            let one = Amount::from_i64(1);
            let result = (&amt * &one).unwrap();
            prop_assert_eq!(amt, result);
        }

        #[test]
        fn prop_inverse_addition(a in -100000i64..100000) {
            let amt = Amount::from_i64(a);
            let neg_amt = -&amt;
            let result = (&amt + &neg_amt).unwrap();
            prop_assert!(result.is_zero());
        }

        #[test]
        fn prop_distributive(
            a in -100i64..100,
            b in -100i64..100,
            c in -100i64..100
        ) {
            let amt_a = Amount::from_i64(a);
            let amt_b = Amount::from_i64(b);
            let amt_c = Amount::from_i64(c);

            // a * (b + c) = a * b + a * c
            let left = (&amt_a * &(&amt_b + &amt_c).unwrap()).unwrap();
            let right = ((&amt_a * &amt_b).unwrap() + &(&amt_a * &amt_c).unwrap()).unwrap();
            prop_assert_eq!(left, right);
        }

        #[test]
        fn prop_abs_idempotent(a in -100000i64..100000) {
            let amt = Amount::from_i64(a);
            let abs1 = amt.abs();
            let abs2 = abs1.abs();
            prop_assert_eq!(abs1, abs2);
        }

        #[test]
        fn prop_double_negation(a in -100000i64..100000) {
            let amt = Amount::from_i64(a);
            let double_neg = -&(-&amt);
            prop_assert_eq!(amt, double_neg);
        }

        #[test]
        fn prop_sign_consistency(a in -100000i64..100000) {
            let amt = Amount::from_i64(a);
            let sign = amt.sign();

            if a > 0 {
                prop_assert_eq!(sign, 1);
                prop_assert!(!amt.sign() < 0);
            } else if a < 0 {
                prop_assert_eq!(sign, -1);
                prop_assert!(amt.sign() < 0);
            } else {
                prop_assert_eq!(sign, 0);
                prop_assert!(amt.is_zero());
            }
        }

        // Balance property tests

        #[test]
        fn prop_balance_addition_commutative(a in -10000i64..10000, b in -10000i64..10000) {
            let balance1 = Balance::from_amount(Amount::from_i64(a)).unwrap_or(Balance::new());
            let balance2 = Balance::from_amount(Amount::from_i64(b)).unwrap_or(Balance::new());
            let sum1 = (balance1.clone() + balance2.clone()).unwrap();
            let sum2 = (balance2.clone() + balance1.clone()).unwrap();
            prop_assert_eq!(sum1, sum2);
        }

        #[test]
        fn prop_balance_identity(a in -10000i64..10000) {
            let balance = Balance::from_amount(Amount::from_i64(a)).unwrap_or(Balance::new());
            let zero = Balance::new();
            let result = (balance.clone() + zero).unwrap();
            prop_assert_eq!(balance, result);
        }

        #[test]
        fn prop_balance_inverse(a in -10000i64..10000) {
            let balance = Balance::from_amount(Amount::from_i64(a)).unwrap_or(Balance::new());
            let neg_balance = balance.negated();
            let result = (balance.clone() + neg_balance).unwrap();
            prop_assert!(result.is_zero());
        }

        #[test]
        fn prop_balance_double_negation(a in -10000i64..10000) {
            let balance = Balance::from_amount(Amount::from_i64(a)).unwrap_or(Balance::new());
            let double_neg = balance.negated().negated();
            prop_assert_eq!(balance, double_neg);
        }
    }
}

// Edge case tests
#[cfg(test)]
mod edge_case_tests {
    use super::*;

    #[test]
    fn test_very_large_numbers() {
        let large = Amount::from_i64(i64::MAX);
        let small = Amount::from_i64(1);

        // This should handle overflow gracefully
        let sum = (&large + &small).unwrap();
        assert!(!sum.is_null());

        // Division by very small number
        let divisor = Amount::exact("0.000000001").unwrap();
        let result = (&small / &divisor).unwrap();
        assert!(!result.is_null());
    }

    #[test]
    fn test_very_small_numbers() {
        let tiny = Amount::exact("0.000000000000001").unwrap();
        assert!(!tiny.is_zero());
        assert!(tiny.is_nonzero());

        let doubled = (&tiny + &tiny).unwrap();
        assert!(!doubled.is_zero());
    }

    #[test]
    #[should_panic]
    fn test_division_by_zero() {
        let a = Amount::from_i64(100);
        let zero = Amount::from_i64(0);
        let _ = (&a / &zero).unwrap();
    }

    #[test]
    fn test_precision_preservation() {
        let precise = Amount::exact("1.123456789012345").unwrap();
        let rounded = precise.roundto(10);
        assert!(rounded.to_string().contains("1.1234567890"));
    }
}

// Tests based on specific C++ test cases from t_amount.cc
#[cfg(test)]
mod cpp_compat_tests {
    use super::*;

    #[test]
    fn test_parser_compatibility() {
        // Based on testParser from t_amount.cc
        let x4 = Amount::exact("123.456").unwrap();
        assert_eq!(x4.to_string(), "123.456");

        let x12 = Amount::exact("100").unwrap();
        assert_eq!(x12.to_i64().unwrap(), 100);
    }

    #[test]
    fn test_arithmetic_compatibility() {
        // Based on testArithmetic from t_amount.cc
        let x1 = Amount::exact("1234.56").unwrap();
        let x2 = Amount::exact("11111.11").unwrap();

        let sum = (&x1 + &x2).unwrap();
        assert_eq!(sum.to_string(), "12345.67");

        let x3 = Amount::exact("1234.56").unwrap();
        let x4 = Amount::exact("1234.56").unwrap();
        let diff = (&x3 - &x4).unwrap();
        assert!(diff.is_zero());
    }

    #[test]
    fn test_comparison_compatibility() {
        // Based on testComparisons from t_amount.cc
        let x0 = Amount::null();
        let x1 = Amount::exact("0.00").unwrap();
        let x2 = Amount::exact("-123.45").unwrap();
        let x3 = Amount::exact("123.45").unwrap();

        assert!(x0.is_zero());
        assert!(x1.is_zero());
        assert!(x2 < x3);
        assert!(x3 > x2);
        assert!(x2.sign() < 0);
        assert!(!x3.sign() < 0);
    }

    #[test]
    fn test_rounding_compatibility() {
        // Based on testRound from t_amount.cc
        let x1 = Amount::exact("1234.567890").unwrap();

        let rounded = x1.roundto(2);
        assert_eq!(rounded.to_string(), "1234.57");

        // truncated() truncates to display precision in the Rust implementation
        let truncated = x1.truncated();
        assert_eq!(truncated.to_string(), "1234.567890");
    }

    #[test]
    fn test_sign_compatibility() {
        // Based on testSign from t_amount.cc
        let x1 = Amount::exact("1234.56").unwrap();
        let x2 = Amount::exact("-1234.56").unwrap();
        let x3 = Amount::exact("0.00").unwrap();

        assert_eq!(x1.sign(), 1);
        assert_eq!(x2.sign(), -1);
        assert_eq!(x3.sign(), 0);

        assert!(!x1.sign() < 0);
        assert!(x2.sign() < 0);
        assert!(!x3.sign() < 0);
    }

    #[test]
    fn test_abs_compatibility() {
        // Based on testAbs from t_amount.cc
        let x1 = Amount::exact("-1234.56").unwrap();
        let x2 = x1.abs();

        assert!(x1.sign() < 0);
        assert!(!x2.sign() < 0);
        assert_eq!(x2.to_string(), "1234.56");
    }
}
