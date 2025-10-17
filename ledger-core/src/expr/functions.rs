//! Built-in function implementations
//!
//! This module implements all the built-in functions available in expressions,
//! including mathematical, string, date/time, and utility functions.

use crate::expr::{BuiltinFunction, ExprContext, ExprError, ExprNode, ExprResult, Value};
use chrono::Local;
use ledger_math::{Amount, BigRational};
use num_bigint::BigInt;
use num_traits::{Signed, Zero};
use rust_decimal::{prelude::*, Decimal};

/// Evaluate a built-in function call
pub fn evaluate_builtin_function(
    function: BuiltinFunction,
    args: &[ExprNode],
    context: &ExprContext,
) -> ExprResult<Value> {
    // Evaluate all arguments first
    let mut evaluated_args = Vec::new();
    for arg in args {
        evaluated_args.push(super::evaluate_node(arg, context)?);
    }

    match function {
        // Mathematical functions
        BuiltinFunction::Abs => fn_abs(&evaluated_args),
        BuiltinFunction::Floor => fn_floor(&evaluated_args),
        BuiltinFunction::Ceiling => fn_ceiling(&evaluated_args),
        BuiltinFunction::Round => fn_round(&evaluated_args),
        BuiltinFunction::Truncate => fn_truncate(&evaluated_args),
        BuiltinFunction::Min => fn_min(&evaluated_args),
        BuiltinFunction::Max => fn_max(&evaluated_args),

        // Date/time functions
        BuiltinFunction::Now => fn_now(&evaluated_args),
        BuiltinFunction::Today => fn_today(&evaluated_args),
        BuiltinFunction::Age => fn_age(&evaluated_args),
        BuiltinFunction::FormatDate => fn_format_date(&evaluated_args),

        // String functions
        BuiltinFunction::FormatString => fn_format_string(&evaluated_args),
        BuiltinFunction::ToUpper => fn_to_upper(&evaluated_args),
        BuiltinFunction::ToLower => fn_to_lower(&evaluated_args),
        BuiltinFunction::Trim => fn_trim(&evaluated_args),

        // Type conversion functions
        BuiltinFunction::ToString => fn_to_string(&evaluated_args),
        BuiltinFunction::ToInt => fn_to_int(&evaluated_args),
        BuiltinFunction::ToDecimal => fn_to_decimal(&evaluated_args),
        BuiltinFunction::ToAmount => fn_to_amount(&evaluated_args),

        // Aggregation functions
        BuiltinFunction::Sum => fn_sum(&evaluated_args),
        BuiltinFunction::Count => fn_count(&evaluated_args),
        BuiltinFunction::Average => fn_average(&evaluated_args),

        // Utility functions
        BuiltinFunction::IsEmpty => fn_is_empty(&evaluated_args),
        BuiltinFunction::Length => fn_length(&evaluated_args),
        BuiltinFunction::Type => fn_type(&evaluated_args),
    }
}

/// Helper function to check argument count
fn check_arg_count(args: &[Value], expected: usize, function_name: &str) -> ExprResult<()> {
    if args.len() != expected {
        Err(ExprError::InvalidArgCount {
            function: function_name.to_string(),
            expected,
            found: args.len(),
        })
    } else {
        Ok(())
    }
}

/// Helper function to check minimum argument count
fn check_min_arg_count(args: &[Value], minimum: usize, function_name: &str) -> ExprResult<()> {
    if args.len() < minimum {
        Err(ExprError::InvalidArgCount {
            function: function_name.to_string(),
            expected: minimum,
            found: args.len(),
        })
    } else {
        Ok(())
    }
}

// Mathematical Functions

fn fn_abs(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "abs")?;

    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(n.abs())),
        Value::Decimal(d) => Ok(Value::Decimal(d.abs())),
        Value::Rational(r) => Ok(Value::Rational(r.abs())),
        Value::Amount(a) => Ok(Value::Amount(a.abs())),
        _ => Err(ExprError::TypeMismatch {
            expected: "numeric type".to_string(),
            found: args[0].type_name().to_string(),
            operation: "abs".to_string(),
        }),
    }
}

fn fn_floor(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "floor")?;

    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Decimal(d) => Ok(Value::Integer(d.floor().to_i64().unwrap_or(0))),
        Value::Rational(r) => Ok(Value::Integer(r.floor().to_integer().to_i64().unwrap_or(0))),
        _ => Err(ExprError::TypeMismatch {
            expected: "numeric type".to_string(),
            found: args[0].type_name().to_string(),
            operation: "floor".to_string(),
        }),
    }
}

fn fn_ceiling(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "ceiling")?;

    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Decimal(d) => Ok(Value::Integer(d.ceil().to_i64().unwrap_or(0))),
        Value::Rational(r) => Ok(Value::Integer(r.ceil().to_integer().to_i64().unwrap_or(0))),
        _ => Err(ExprError::TypeMismatch {
            expected: "numeric type".to_string(),
            found: args[0].type_name().to_string(),
            operation: "ceiling".to_string(),
        }),
    }
}

fn fn_round(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "round")?;

    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Decimal(d) => Ok(Value::Integer(d.round().to_i64().unwrap_or(0))),
        Value::Rational(r) => Ok(Value::Integer(r.round().to_integer().to_i64().unwrap_or(0))),
        _ => Err(ExprError::TypeMismatch {
            expected: "numeric type".to_string(),
            found: args[0].type_name().to_string(),
            operation: "round".to_string(),
        }),
    }
}

fn fn_truncate(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "truncate")?;

    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Decimal(d) => Ok(Value::Integer(d.trunc().to_i64().unwrap_or(0))),
        Value::Rational(r) => Ok(Value::Integer(r.trunc().to_integer().to_i64().unwrap_or(0))),
        _ => Err(ExprError::TypeMismatch {
            expected: "numeric type".to_string(),
            found: args[0].type_name().to_string(),
            operation: "truncate".to_string(),
        }),
    }
}

fn fn_min(args: &[Value]) -> ExprResult<Value> {
    check_min_arg_count(args, 1, "min")?;

    let mut result = args[0].clone();

    for arg in &args[1..] {
        match (&result, arg) {
            (Value::Integer(a), Value::Integer(b)) => {
                if b < a {
                    result = Value::Integer(*b);
                }
            }
            (Value::Decimal(a), Value::Decimal(b)) => {
                if b < a {
                    result = Value::Decimal(*b);
                }
            }
            (Value::Rational(a), Value::Rational(b)) => {
                if b < a {
                    result = Value::Rational(b.clone());
                }
            }
            _ => {
                return Err(ExprError::TypeMismatch {
                    expected: "comparable numeric types".to_string(),
                    found: format!("{} and {}", result.type_name(), arg.type_name()),
                    operation: "min".to_string(),
                })
            }
        }
    }

    Ok(result)
}

fn fn_max(args: &[Value]) -> ExprResult<Value> {
    check_min_arg_count(args, 1, "max")?;

    let mut result = args[0].clone();

    for arg in &args[1..] {
        match (&result, arg) {
            (Value::Integer(a), Value::Integer(b)) => {
                if b > a {
                    result = Value::Integer(*b);
                }
            }
            (Value::Decimal(a), Value::Decimal(b)) => {
                if b > a {
                    result = Value::Decimal(*b);
                }
            }
            (Value::Rational(a), Value::Rational(b)) => {
                if b > a {
                    result = Value::Rational(b.clone());
                }
            }
            _ => {
                return Err(ExprError::TypeMismatch {
                    expected: "comparable numeric types".to_string(),
                    found: format!("{} and {}", result.type_name(), arg.type_name()),
                    operation: "max".to_string(),
                })
            }
        }
    }

    Ok(result)
}

// Date/Time Functions

fn fn_now(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 0, "now")?;
    Ok(Value::DateTime(Local::now()))
}

fn fn_today(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 0, "today")?;
    Ok(Value::Date(Local::now().naive_local().date()))
}

fn fn_age(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "age")?;

    match &args[0] {
        Value::Date(date) => {
            let today = Local::now().naive_local().date();
            let diff = today - *date;
            Ok(Value::Integer(diff.num_days()))
        }
        Value::DateTime(datetime) => {
            let now = Local::now();
            let diff = now - *datetime;
            Ok(Value::Integer(diff.num_days()))
        }
        _ => Err(ExprError::TypeMismatch {
            expected: "date or datetime".to_string(),
            found: args[0].type_name().to_string(),
            operation: "age".to_string(),
        }),
    }
}

fn fn_format_date(args: &[Value]) -> ExprResult<Value> {
    if args.is_empty() || args.len() > 2 {
        return Err(ExprError::InvalidArgCount {
            function: "format_date".to_string(),
            expected: 2,
            found: args.len(),
        });
    }

    let format_str = if args.len() == 2 {
        match &args[1] {
            Value::String(s) => s.clone(),
            _ => {
                return Err(ExprError::TypeMismatch {
                    expected: "string".to_string(),
                    found: args[1].type_name().to_string(),
                    operation: "format_date".to_string(),
                })
            }
        }
    } else {
        "%Y-%m-%d".to_string()
    };

    match &args[0] {
        Value::Date(date) => Ok(Value::String(date.format(&format_str).to_string())),
        Value::DateTime(datetime) => Ok(Value::String(datetime.format(&format_str).to_string())),
        _ => Err(ExprError::TypeMismatch {
            expected: "date or datetime".to_string(),
            found: args[0].type_name().to_string(),
            operation: "format_date".to_string(),
        }),
    }
}

// String Functions

fn fn_format_string(args: &[Value]) -> ExprResult<Value> {
    check_min_arg_count(args, 1, "format")?;

    match &args[0] {
        Value::String(format_str) => {
            // Simple string formatting - for now just return the format string
            // In a full implementation, this would handle printf-style formatting
            let mut result = format_str.clone();
            for (i, arg) in args[1..].iter().enumerate() {
                let placeholder = format!("{{{}}}", i);
                result = result.replace(&placeholder, &arg.to_string());
            }
            Ok(Value::String(result))
        }
        _ => Err(ExprError::TypeMismatch {
            expected: "string".to_string(),
            found: args[0].type_name().to_string(),
            operation: "format".to_string(),
        }),
    }
}

fn fn_to_upper(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "to_upper")?;

    match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_uppercase())),
        _ => Err(ExprError::TypeMismatch {
            expected: "string".to_string(),
            found: args[0].type_name().to_string(),
            operation: "to_upper".to_string(),
        }),
    }
}

fn fn_to_lower(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "to_lower")?;

    match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_lowercase())),
        _ => Err(ExprError::TypeMismatch {
            expected: "string".to_string(),
            found: args[0].type_name().to_string(),
            operation: "to_lower".to_string(),
        }),
    }
}

fn fn_trim(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "trim")?;

    match &args[0] {
        Value::String(s) => Ok(Value::String(s.trim().to_string())),
        _ => Err(ExprError::TypeMismatch {
            expected: "string".to_string(),
            found: args[0].type_name().to_string(),
            operation: "trim".to_string(),
        }),
    }
}

// Type Conversion Functions

fn fn_to_string(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "to_string")?;
    Ok(Value::String(args[0].to_string()))
}

fn fn_to_int(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "to_int")?;

    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Decimal(d) => Ok(Value::Integer(d.to_i64().unwrap_or(0))),
        Value::Rational(r) => Ok(Value::Integer(r.to_integer().to_i64().unwrap_or(0))),
        Value::String(s) => s
            .parse::<i64>()
            .map(Value::Integer)
            .map_err(|_| ExprError::RuntimeError(format!("Cannot convert '{}' to integer", s))),
        _ => Err(ExprError::TypeMismatch {
            expected: "convertible to integer".to_string(),
            found: args[0].type_name().to_string(),
            operation: "to_int".to_string(),
        }),
    }
}

fn fn_to_decimal(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "to_decimal")?;

    match &args[0] {
        Value::Integer(n) => Ok(Value::Decimal(Decimal::from(*n))),
        Value::Decimal(d) => Ok(Value::Decimal(*d)),
        Value::String(s) => s
            .parse::<Decimal>()
            .map(Value::Decimal)
            .map_err(|_| ExprError::RuntimeError(format!("Cannot convert '{}' to decimal", s))),
        _ => Err(ExprError::TypeMismatch {
            expected: "convertible to decimal".to_string(),
            found: args[0].type_name().to_string(),
            operation: "to_decimal".to_string(),
        }),
    }
}

fn fn_to_amount(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "to_amount")?;

    match &args[0] {
        Value::Integer(n) => Ok(Value::Amount(Amount::from(*n))),
        Value::Decimal(d) => Amount::from_decimal(*d)
            .map(Value::Amount)
            .map_err(|e| ExprError::RuntimeError(e.to_string())),
        Value::Rational(r) => Ok(Value::Amount(Amount::from_rational(r.clone()))),
        Value::Amount(a) => Ok(Value::Amount(a.clone())),
        Value::String(s) => s
            .parse::<Amount>()
            .map(Value::Amount)
            .map_err(|_| ExprError::RuntimeError(format!("Cannot convert '{}' to amount", s))),
        _ => Err(ExprError::TypeMismatch {
            expected: "convertible to amount".to_string(),
            found: args[0].type_name().to_string(),
            operation: "to_amount".to_string(),
        }),
    }
}

// Aggregation Functions

fn fn_sum(args: &[Value]) -> ExprResult<Value> {
    if args.is_empty() {
        return Ok(Value::Integer(0));
    }

    match &args[0] {
        Value::Sequence(seq) => {
            // Sum elements of a sequence
            let mut result = Value::Integer(0);
            for item in seq {
                result = add_values(&result, item)?;
            }
            Ok(result)
        }
        _ => {
            // Sum all arguments
            let mut result = args[0].clone();
            for arg in &args[1..] {
                result = add_values(&result, arg)?;
            }
            Ok(result)
        }
    }
}

fn fn_count(args: &[Value]) -> ExprResult<Value> {
    if args.is_empty() {
        return Ok(Value::Integer(0));
    }

    match &args[0] {
        Value::Sequence(seq) => Ok(Value::Integer(seq.len() as i64)),
        _ => Ok(Value::Integer(args.len() as i64)),
    }
}

fn fn_average(args: &[Value]) -> ExprResult<Value> {
    if args.is_empty() {
        return Ok(Value::Null);
    }

    let sum = fn_sum(args)?;
    let count = fn_count(args)?;

    match (sum, count) {
        (sum_val, Value::Integer(n)) if n > 0 => {
            // Simple division for average - implement inline
            match sum_val {
                Value::Integer(s) => Ok(Value::Rational(BigRational::new(s.into(), n.into()))),
                Value::Decimal(s) => Ok(Value::Decimal(s / Decimal::from(n))),
                Value::Rational(s) => Ok(Value::Rational(s / BigRational::from(BigInt::from(n)))),
                _ => Ok(Value::Null),
            }
        }
        _ => Ok(Value::Null),
    }
}

// Utility Functions

fn fn_is_empty(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "is_empty")?;

    let is_empty = match &args[0] {
        Value::Null => true,
        Value::String(s) => s.is_empty(),
        Value::Sequence(seq) => seq.is_empty(),
        Value::Integer(n) => *n == 0,
        Value::Decimal(d) => d.is_zero(),
        Value::Rational(r) => r.is_zero(),
        Value::Amount(a) => a.is_zero(),
        _ => false,
    };

    Ok(Value::Bool(is_empty))
}

fn fn_length(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "length")?;

    match &args[0] {
        Value::String(s) => Ok(Value::Integer(s.len() as i64)),
        Value::Sequence(seq) => Ok(Value::Integer(seq.len() as i64)),
        _ => Err(ExprError::TypeMismatch {
            expected: "string or sequence".to_string(),
            found: args[0].type_name().to_string(),
            operation: "length".to_string(),
        }),
    }
}

fn fn_type(args: &[Value]) -> ExprResult<Value> {
    check_arg_count(args, 1, "type")?;
    Ok(Value::String(args[0].type_name().to_string()))
}

// Helper functions that need access to op functions
fn add_values(left: &Value, right: &Value) -> ExprResult<Value> {
    // Simple addition for aggregation functions - implement inline
    match (left, right) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
        (Value::Decimal(a), Value::Decimal(b)) => Ok(Value::Decimal(a + b)),
        (Value::Rational(a), Value::Rational(b)) => Ok(Value::Rational(a + b)),
        // Type coercion
        (Value::Integer(a), Value::Decimal(b)) => Ok(Value::Decimal(Decimal::from(*a) + b)),
        (Value::Decimal(a), Value::Integer(b)) => Ok(Value::Decimal(a + Decimal::from(*b))),
        _ => Err(ExprError::RuntimeError(format!(
            "Cannot add {} and {}",
            left.type_name(),
            right.type_name()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_abs_function() {
        let args = vec![Value::Integer(-42)];
        let result = fn_abs(&args).unwrap();
        assert_eq!(result, Value::Integer(42));
    }

    #[test]
    fn test_max_function() {
        let args = vec![Value::Integer(1), Value::Integer(5), Value::Integer(3)];
        let result = fn_max(&args).unwrap();
        assert_eq!(result, Value::Integer(5));
    }

    #[test]
    fn test_min_function() {
        let args = vec![Value::Integer(1), Value::Integer(5), Value::Integer(3)];
        let result = fn_min(&args).unwrap();
        assert_eq!(result, Value::Integer(1));
    }

    #[test]
    fn test_to_upper_function() {
        let args = vec![Value::String("hello".to_string())];
        let result = fn_to_upper(&args).unwrap();
        assert_eq!(result, Value::String("HELLO".to_string()));
    }

    #[test]
    fn test_length_function() {
        let args = vec![Value::String("hello".to_string())];
        let result = fn_length(&args).unwrap();
        assert_eq!(result, Value::Integer(5));
    }

    #[test]
    fn test_sum_function() {
        let args = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
        let result = fn_sum(&args).unwrap();
        assert_eq!(result, Value::Integer(6));
    }

    #[test]
    fn test_count_function() {
        let args = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
        let result = fn_count(&args).unwrap();
        assert_eq!(result, Value::Integer(3));
    }

    #[test]
    fn test_type_function() {
        let args = vec![Value::Integer(42)];
        let result = fn_type(&args).unwrap();
        assert_eq!(result, Value::String("integer".to_string()));
    }

    #[test]
    fn test_is_empty_function() {
        let args = vec![Value::String("".to_string())];
        let result = fn_is_empty(&args).unwrap();
        assert_eq!(result, Value::Bool(true));

        let args = vec![Value::String("hello".to_string())];
        let result = fn_is_empty(&args).unwrap();
        assert_eq!(result, Value::Bool(false));
    }
}
