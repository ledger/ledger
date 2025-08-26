//! Expression operator evaluation
//!
//! This module implements the evaluation logic for all expression operators,
//! handling type coercion, arithmetic operations, and value conversions.

use crate::expr::{
    ExprNode, BinaryOp, UnaryOp, Value, ExprContext, ExprError, ExprResult
};
use ledger_math::{Amount, BigRational, Decimal};
use num_bigint::BigInt;
use num_traits::{Zero, One};
use std::cmp::Ordering;
use chrono::{DateTime, Local, Utc};

/// Evaluate a binary operation
pub fn evaluate_binary_op(
    op: BinaryOp,
    left: &ExprNode,
    right: &ExprNode,
    context: &ExprContext,
) -> ExprResult<Value> {
    // For short-circuiting logical operators
    if matches!(op, BinaryOp::And | BinaryOp::Or) {
        return evaluate_logical_op(op, left, right, context);
    }
    
    // Evaluate both operands
    let left_val = super::evaluate_node(left, context)?;
    let right_val = super::evaluate_node(right, context)?;
    
    match op {
        BinaryOp::Add => add_values(&left_val, &right_val),
        BinaryOp::Sub => subtract_values(&left_val, &right_val),
        BinaryOp::Mul => multiply_values(&left_val, &right_val),
        BinaryOp::Div => divide_values(&left_val, &right_val),
        BinaryOp::Mod => modulo_values(&left_val, &right_val),
        BinaryOp::Eq => Ok(Value::Bool(values_equal(&left_val, &right_val))),
        BinaryOp::Ne => Ok(Value::Bool(!values_equal(&left_val, &right_val))),
        BinaryOp::Lt => compare_values(&left_val, &right_val).map(|cmp| Value::Bool(cmp == Ordering::Less)),
        BinaryOp::Gt => compare_values(&left_val, &right_val).map(|cmp| Value::Bool(cmp == Ordering::Greater)),
        BinaryOp::Le => compare_values(&left_val, &right_val).map(|cmp| Value::Bool(cmp != Ordering::Greater)),
        BinaryOp::Ge => compare_values(&left_val, &right_val).map(|cmp| Value::Bool(cmp != Ordering::Less)),
        BinaryOp::Match => match_values(&left_val, &right_val),
        BinaryOp::Cons => cons_values(&left_val, &right_val),
        BinaryOp::Seq => Ok(right_val), // Sequence operator returns the right value
        BinaryOp::And | BinaryOp::Or => unreachable!("Handled above"),
        BinaryOp::Query | BinaryOp::Colon => {
            Err(ExprError::RuntimeError("Ternary operator should be handled specially".to_string()))
        }
    }
}

/// Evaluate logical operators with short-circuiting
fn evaluate_logical_op(
    op: BinaryOp,
    left: &ExprNode,
    right: &ExprNode,
    context: &ExprContext,
) -> ExprResult<Value> {
    let left_val = super::evaluate_node(left, context)?;
    
    match op {
        BinaryOp::And => {
            if !left_val.is_truthy() {
                Ok(Value::Bool(false))
            } else {
                let right_val = super::evaluate_node(right, context)?;
                Ok(Value::Bool(right_val.is_truthy()))
            }
        }
        BinaryOp::Or => {
            if left_val.is_truthy() {
                Ok(Value::Bool(true))
            } else {
                let right_val = super::evaluate_node(right, context)?;
                Ok(Value::Bool(right_val.is_truthy()))
            }
        }
        _ => unreachable!(),
    }
}

/// Evaluate a unary operation
pub fn evaluate_unary_op(
    op: UnaryOp,
    operand: &ExprNode,
    context: &ExprContext,
) -> ExprResult<Value> {
    let val = super::evaluate_node(operand, context)?;
    
    match op {
        UnaryOp::Neg => negate_value(&val),
        UnaryOp::Not => Ok(Value::Bool(!val.is_truthy())),
    }
}

/// Add two values
fn add_values(left: &Value, right: &Value) -> ExprResult<Value> {
    match (left, right) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
        (Value::Decimal(a), Value::Decimal(b)) => Ok(Value::Decimal(a + b)),
        (Value::Rational(a), Value::Rational(b)) => Ok(Value::Rational(a + b)),
        (Value::Amount(a), Value::Amount(b)) => {
            (a + b)
                .map(Value::Amount)
                .map_err(|e| ExprError::RuntimeError(e.to_string()))
        }
        (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
        
        // Type coercion
        (Value::Integer(a), Value::Decimal(b)) => Ok(Value::Decimal(Decimal::from(*a) + b)),
        (Value::Decimal(a), Value::Integer(b)) => Ok(Value::Decimal(a + Decimal::from(*b))),
        (Value::Integer(a), Value::Rational(b)) => {
            Ok(Value::Rational(BigRational::from(BigInt::from(*a)) + b))
        },
        (Value::Rational(a), Value::Integer(b)) => {
            Ok(Value::Rational(a + BigRational::from(BigInt::from(*b))))
        },
        
        // Date arithmetic
        (Value::Date(date), Value::Integer(days)) => {
            use chrono::TimeDelta;
            let new_date = *date + TimeDelta::days(*days);
            Ok(Value::Date(new_date))
        }
        (Value::Integer(days), Value::Date(date)) => {
            use chrono::TimeDelta;
            let new_date = *date + TimeDelta::days(*days);
            Ok(Value::Date(new_date))
        }
        
        _ => Err(ExprError::TypeMismatch {
            expected: format!("compatible types for addition"),
            found: format!("{} + {}", left.type_name(), right.type_name()),
            operation: "addition".to_string(),
        }),
    }
}

/// Subtract two values
fn subtract_values(left: &Value, right: &Value) -> ExprResult<Value> {
    match (left, right) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
        (Value::Decimal(a), Value::Decimal(b)) => Ok(Value::Decimal(a - b)),
        (Value::Rational(a), Value::Rational(b)) => Ok(Value::Rational(a - b)),
        (Value::Amount(a), Value::Amount(b)) => {
            (a - b)
                .map(Value::Amount)
                .map_err(|e| ExprError::RuntimeError(e.to_string()))
        }
        
        // Type coercion
        (Value::Integer(a), Value::Decimal(b)) => Ok(Value::Decimal(Decimal::from(*a) - b)),
        (Value::Decimal(a), Value::Integer(b)) => Ok(Value::Decimal(a - Decimal::from(*b))),
        (Value::Integer(a), Value::Rational(b)) => Ok(Value::Rational(BigRational::from(BigInt::from(*a)) - b)),
        (Value::Rational(a), Value::Integer(b)) => Ok(Value::Rational(a - BigRational::from(BigInt::from(*b)))),
        
        // Date arithmetic
        (Value::Date(a), Value::Date(b)) => {
            let diff = *a - *b;
            Ok(Value::Integer(diff.num_days()))
        }
        (Value::Date(date), Value::Integer(days)) => {
            use chrono::TimeDelta;
            let new_date = *date - TimeDelta::days(*days);
            Ok(Value::Date(new_date))
        }
        
        _ => Err(ExprError::TypeMismatch {
            expected: format!("compatible types for subtraction"),
            found: format!("{} - {}", left.type_name(), right.type_name()),
            operation: "subtraction".to_string(),
        }),
    }
}

/// Multiply two values
fn multiply_values(left: &Value, right: &Value) -> ExprResult<Value> {
    match (left, right) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
        (Value::Decimal(a), Value::Decimal(b)) => Ok(Value::Decimal(a * b)),
        (Value::Rational(a), Value::Rational(b)) => Ok(Value::Rational(a * b)),
        (Value::Amount(a), Value::Integer(b)) => {
            // Convert amount to rational, multiply, convert back
            if let Some(rational) = a.to_rational() {
                let result_rational = rational * BigRational::from(BigInt::from(*b));
                Ok(Value::Amount(Amount::from_rational(result_rational)))
            } else {
                Err(ExprError::RuntimeError("Cannot multiply amount without quantity".to_string()))
            }
        }
        (Value::Integer(a), Value::Amount(b)) => {
            // Convert amount to rational, multiply, convert back
            if let Some(rational) = b.to_rational() {
                let result_rational = rational * BigRational::from(BigInt::from(*a));
                Ok(Value::Amount(Amount::from_rational(result_rational)))
            } else {
                Err(ExprError::RuntimeError("Cannot multiply amount without quantity".to_string()))
            }
        }
        (Value::Amount(a), Value::Rational(b)) => {
            // Convert amount to rational, multiply, convert back
            if let Some(rational) = a.to_rational() {
                let result_rational = rational * b;
                Ok(Value::Amount(Amount::from_rational(result_rational)))
            } else {
                Err(ExprError::RuntimeError("Cannot multiply amount without quantity".to_string()))
            }
        }
        (Value::Rational(a), Value::Amount(b)) => {
            // Convert amount to rational, multiply, convert back
            if let Some(rational) = b.to_rational() {
                let result_rational = rational * a;
                Ok(Value::Amount(Amount::from_rational(result_rational)))
            } else {
                Err(ExprError::RuntimeError("Cannot multiply amount without quantity".to_string()))
            }
        }
        
        // String repetition
        (Value::String(s), Value::Integer(n)) => {
            if *n < 0 {
                Err(ExprError::RuntimeError("Cannot repeat string negative times".to_string()))
            } else {
                Ok(Value::String(s.repeat(*n as usize)))
            }
        }
        (Value::Integer(n), Value::String(s)) => {
            if *n < 0 {
                Err(ExprError::RuntimeError("Cannot repeat string negative times".to_string()))
            } else {
                Ok(Value::String(s.repeat(*n as usize)))
            }
        }
        
        // Type coercion
        (Value::Integer(a), Value::Decimal(b)) => Ok(Value::Decimal(Decimal::from(*a) * b)),
        (Value::Decimal(a), Value::Integer(b)) => Ok(Value::Decimal(a * Decimal::from(*b))),
        (Value::Integer(a), Value::Rational(b)) => Ok(Value::Rational(BigRational::from(BigInt::from(*a)) * b)),
        (Value::Rational(a), Value::Integer(b)) => Ok(Value::Rational(a * BigRational::from(BigInt::from(*b)))),
        
        _ => Err(ExprError::TypeMismatch {
            expected: format!("compatible types for multiplication"),
            found: format!("{} * {}", left.type_name(), right.type_name()),
            operation: "multiplication".to_string(),
        }),
    }
}

/// Divide two values
fn divide_values(left: &Value, right: &Value) -> ExprResult<Value> {
    // Check for division by zero
    match right {
        Value::Integer(n) if *n == 0 => return Err(ExprError::DivisionByZero),
        Value::Decimal(d) if d.is_zero() => return Err(ExprError::DivisionByZero),
        Value::Rational(r) if r.is_zero() => return Err(ExprError::DivisionByZero),
        Value::Amount(a) if a.is_zero() => return Err(ExprError::DivisionByZero),
        _ => {}
    }
    
    match (left, right) {
        (Value::Integer(a), Value::Integer(b)) => {
            // Integer division returns rational to preserve precision
            Ok(Value::Rational(BigRational::new((*a).into(), (*b).into())))
        }
        (Value::Decimal(a), Value::Decimal(b)) => Ok(Value::Decimal(a / b)),
        (Value::Rational(a), Value::Rational(b)) => Ok(Value::Rational(a / b)),
        (Value::Amount(a), Value::Integer(b)) => {
            // Convert amount to rational, divide, convert back
            if let Some(rational) = a.to_rational() {
                let result_rational = rational / BigRational::from(BigInt::from(*b));
                Ok(Value::Amount(Amount::from_rational(result_rational)))
            } else {
                Err(ExprError::RuntimeError("Cannot divide amount without quantity".to_string()))
            }
        }
        (Value::Amount(a), Value::Rational(b)) => {
            // Convert amount to rational, divide, convert back
            if let Some(rational) = a.to_rational() {
                let result_rational = rational / b;
                Ok(Value::Amount(Amount::from_rational(result_rational)))
            } else {
                Err(ExprError::RuntimeError("Cannot divide amount without quantity".to_string()))
            }
        }
        (Value::Amount(a), Value::Amount(b)) => {
            // Amount divided by amount returns a ratio (rational number)
            // For now, just try division and handle the error
            (a / b)
                .map(Value::Amount)
                .map_err(|e| ExprError::RuntimeError(e.to_string()))
        }
        
        // Type coercion
        (Value::Integer(a), Value::Decimal(b)) => Ok(Value::Decimal(Decimal::from(*a) / b)),
        (Value::Decimal(a), Value::Integer(b)) => Ok(Value::Decimal(a / Decimal::from(*b))),
        (Value::Integer(a), Value::Rational(b)) => Ok(Value::Rational(BigRational::from(BigInt::from(*a)) / b)),
        (Value::Rational(a), Value::Integer(b)) => Ok(Value::Rational(a / BigRational::from(BigInt::from(*b)))),
        
        _ => Err(ExprError::TypeMismatch {
            expected: format!("compatible types for division"),
            found: format!("{} / {}", left.type_name(), right.type_name()),
            operation: "division".to_string(),
        }),
    }
}

/// Modulo operation
fn modulo_values(left: &Value, right: &Value) -> ExprResult<Value> {
    match (left, right) {
        (Value::Integer(a), Value::Integer(b)) => {
            if *b == 0 {
                Err(ExprError::DivisionByZero)
            } else {
                Ok(Value::Integer(a % b))
            }
        }
        _ => Err(ExprError::TypeMismatch {
            expected: "integers".to_string(),
            found: format!("{} % {}", left.type_name(), right.type_name()),
            operation: "modulo".to_string(),
        }),
    }
}

/// Negate a value
fn negate_value(val: &Value) -> ExprResult<Value> {
    match val {
        Value::Integer(n) => Ok(Value::Integer(-n)),
        Value::Decimal(d) => Ok(Value::Decimal(-d)),
        Value::Rational(r) => Ok(Value::Rational(-r)),
        Value::Amount(a) => Ok(Value::Amount(-a)),
        _ => Err(ExprError::TypeMismatch {
            expected: "numeric type".to_string(),
            found: val.type_name().to_string(),
            operation: "negation".to_string(),
        }),
    }
}

/// Check if two values are equal
fn values_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Decimal(a), Value::Decimal(b)) => a == b,
        (Value::Rational(a), Value::Rational(b)) => a == b,
        (Value::Amount(a), Value::Amount(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Date(a), Value::Date(b)) => a == b,
        (Value::DateTime(a), Value::DateTime(b)) => a == b,
        (Value::Sequence(a), Value::Sequence(b)) => a == b,
        (Value::Regex(a), Value::Regex(b)) => a == b,
        
        // Type coercion for numeric comparisons
        (Value::Integer(a), Value::Decimal(b)) => Decimal::from(*a) == *b,
        (Value::Decimal(a), Value::Integer(b)) => *a == Decimal::from(*b),
        (Value::Integer(a), Value::Rational(b)) => BigRational::from(BigInt::from(*a)) == *b,
        (Value::Rational(a), Value::Integer(b)) => *a == BigRational::from(BigInt::from(*b)),
        (Value::Decimal(a), Value::Rational(b)) => {
            // Convert decimal to rational for comparison
            // This is approximate due to decimal representation limits
            if let Some(rational) = decimal_to_rational(*a) {
                rational == *b
            } else {
                false
            }
        }
        (Value::Rational(a), Value::Decimal(b)) => {
            if let Some(rational) = decimal_to_rational(*b) {
                *a == rational
            } else {
                false
            }
        }
        
        _ => false, // Different types are not equal
    }
}

/// Compare two values
fn compare_values(left: &Value, right: &Value) -> ExprResult<Ordering> {
    match (left, right) {
        (Value::Integer(a), Value::Integer(b)) => Ok(a.cmp(b)),
        (Value::Decimal(a), Value::Decimal(b)) => Ok(a.cmp(b)),
        (Value::Rational(a), Value::Rational(b)) => Ok(a.cmp(b)),
        (Value::Amount(a), Value::Amount(b)) => {
            a.partial_cmp(b)
                .ok_or_else(|| ExprError::RuntimeError("Cannot compare amounts with different commodities".to_string()))
        }
        (Value::String(a), Value::String(b)) => Ok(a.cmp(b)),
        (Value::Date(a), Value::Date(b)) => Ok(a.cmp(b)),
        (Value::DateTime(a), Value::DateTime(b)) => Ok(a.cmp(b)),
        
        // Type coercion for numeric comparisons
        (Value::Integer(a), Value::Decimal(b)) => Ok(Decimal::from(*a).cmp(b)),
        (Value::Decimal(a), Value::Integer(b)) => Ok(a.cmp(&Decimal::from(*b))),
        (Value::Integer(a), Value::Rational(b)) => Ok(BigRational::from(BigInt::from(*a)).cmp(b)),
        (Value::Rational(a), Value::Integer(b)) => Ok(a.cmp(&BigRational::from(BigInt::from(*b)))),
        
        // Amount comparisons with numeric values (compare numeric part only)
        (Value::Amount(a), Value::Decimal(b)) => {
            Ok(a.value().cmp(b))
        }
        (Value::Decimal(a), Value::Amount(b)) => {
            Ok(a.cmp(&b.value()))
        }
        (Value::Amount(a), Value::Integer(b)) => {
            Ok(a.value().cmp(&Decimal::from(*b)))
        }
        (Value::Integer(a), Value::Amount(b)) => {
            Ok(Decimal::from(*a).cmp(&b.value()))
        }
        
        // Date comparisons with string values (parse string as date)
        (Value::Date(a), Value::String(b)) => {
            // Try to parse the string as a date
            use chrono::NaiveDate;
            match NaiveDate::parse_from_str(b, "%Y-%m-%d") {
                Ok(date_b) => Ok(a.cmp(&date_b)),
                Err(_) => Err(ExprError::RuntimeError(format!("Cannot parse '{}' as date", b))),
            }
        }
        (Value::String(a), Value::Date(b)) => {
            // Try to parse the string as a date
            use chrono::NaiveDate;
            match NaiveDate::parse_from_str(a, "%Y-%m-%d") {
                Ok(date_a) => Ok(date_a.cmp(b)),
                Err(_) => Err(ExprError::RuntimeError(format!("Cannot parse '{}' as date", a))),
            }
        }
        
        _ => Err(ExprError::TypeMismatch {
            expected: "comparable types".to_string(),
            found: format!("{} and {}", left.type_name(), right.type_name()),
            operation: "comparison".to_string(),
        }),
    }
}

/// Pattern matching operation
fn match_values(left: &Value, right: &Value) -> ExprResult<Value> {
    match (left, right) {
        (Value::String(text), Value::Regex(pattern)) => {
            match regex::Regex::new(pattern) {
                Ok(re) => Ok(Value::Bool(re.is_match(text))),
                Err(_) => Err(ExprError::RuntimeError(format!("Invalid regex pattern: {}", pattern))),
            }
        }
        (Value::String(text), Value::String(pattern)) => {
            // Simple string contains match
            Ok(Value::Bool(text.contains(pattern)))
        }
        _ => Err(ExprError::TypeMismatch {
            expected: "string and regex/string".to_string(),
            found: format!("{} =~ {}", left.type_name(), right.type_name()),
            operation: "pattern matching".to_string(),
        }),
    }
}

/// List construction operation
fn cons_values(left: &Value, right: &Value) -> ExprResult<Value> {
    match right {
        Value::Sequence(seq) => {
            let mut new_seq = vec![left.clone()];
            new_seq.extend(seq.iter().cloned());
            Ok(Value::Sequence(new_seq))
        }
        _ => {
            // Create a new sequence with both elements
            Ok(Value::Sequence(vec![left.clone(), right.clone()]))
        }
    }
}

/// Convert a decimal to rational (approximate)
fn decimal_to_rational(decimal: Decimal) -> Option<BigRational> {
    // Convert decimal to string and parse as fraction
    let s = decimal.to_string();
    if let Some(dot_pos) = s.find('.') {
        let integer_part = &s[..dot_pos];
        let fractional_part = &s[dot_pos + 1..];
        
        let integer: i64 = integer_part.parse().ok()?;
        let fractional: i64 = fractional_part.parse().ok()?;
        let denominator = 10_i64.pow(fractional_part.len() as u32);
        
        Some(BigRational::new((integer * denominator + fractional).into(), denominator.into()))
    } else {
        let integer: i64 = s.parse().ok()?;
        Some(BigRational::from(BigInt::from(integer)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::{ExprNode, Value};
    
    #[test]
    fn test_add_integers() {
        let left = ExprNode::Value(Value::Integer(5));
        let right = ExprNode::Value(Value::Integer(3));
        let context = ExprContext::new();
        
        let result = evaluate_binary_op(BinaryOp::Add, &left, &right, &context).unwrap();
        assert_eq!(result, Value::Integer(8));
    }
    
    #[test]
    fn test_subtract_integers() {
        let left = ExprNode::Value(Value::Integer(10));
        let right = ExprNode::Value(Value::Integer(4));
        let context = ExprContext::new();
        
        let result = evaluate_binary_op(BinaryOp::Sub, &left, &right, &context).unwrap();
        assert_eq!(result, Value::Integer(6));
    }
    
    #[test]
    fn test_multiply_integers() {
        let left = ExprNode::Value(Value::Integer(6));
        let right = ExprNode::Value(Value::Integer(7));
        let context = ExprContext::new();
        
        let result = evaluate_binary_op(BinaryOp::Mul, &left, &right, &context).unwrap();
        assert_eq!(result, Value::Integer(42));
    }
    
    #[test]
    fn test_divide_integers() {
        let left = ExprNode::Value(Value::Integer(15));
        let right = ExprNode::Value(Value::Integer(3));
        let context = ExprContext::new();
        
        let result = evaluate_binary_op(BinaryOp::Div, &left, &right, &context).unwrap();
        assert_eq!(result, Value::Rational(BigRational::from(BigInt::from(5))));
    }
    
    #[test]
    fn test_division_by_zero() {
        let left = ExprNode::Value(Value::Integer(10));
        let right = ExprNode::Value(Value::Integer(0));
        let context = ExprContext::new();
        
        let result = evaluate_binary_op(BinaryOp::Div, &left, &right, &context);
        assert!(matches!(result, Err(ExprError::DivisionByZero)));
    }
    
    #[test]
    fn test_negate_integer() {
        let operand = ExprNode::Value(Value::Integer(42));
        let context = ExprContext::new();
        
        let result = evaluate_unary_op(UnaryOp::Neg, &operand, &context).unwrap();
        assert_eq!(result, Value::Integer(-42));
    }
    
    #[test]
    fn test_logical_not() {
        let operand = ExprNode::Value(Value::Bool(true));
        let context = ExprContext::new();
        
        let result = evaluate_unary_op(UnaryOp::Not, &operand, &context).unwrap();
        assert_eq!(result, Value::Bool(false));
    }
    
    #[test]
    fn test_string_concatenation() {
        let left = ExprNode::Value(Value::String("Hello, ".to_string()));
        let right = ExprNode::Value(Value::String("World!".to_string()));
        let context = ExprContext::new();
        
        let result = evaluate_binary_op(BinaryOp::Add, &left, &right, &context).unwrap();
        assert_eq!(result, Value::String("Hello, World!".to_string()));
    }
    
    #[test]
    fn test_equality_comparison() {
        let left = ExprNode::Value(Value::Integer(42));
        let right = ExprNode::Value(Value::Integer(42));
        let context = ExprContext::new();
        
        let result = evaluate_binary_op(BinaryOp::Eq, &left, &right, &context).unwrap();
        assert_eq!(result, Value::Bool(true));
    }
    
    #[test]
    fn test_less_than_comparison() {
        let left = ExprNode::Value(Value::Integer(5));
        let right = ExprNode::Value(Value::Integer(10));
        let context = ExprContext::new();
        
        let result = evaluate_binary_op(BinaryOp::Lt, &left, &right, &context).unwrap();
        assert_eq!(result, Value::Bool(true));
    }
}