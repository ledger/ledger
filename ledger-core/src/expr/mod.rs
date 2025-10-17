//! Expression evaluation engine
//!
//! This module provides an AST-based expression evaluation system that supports:
//! - Arithmetic operations (+, -, *, /, %)
//! - Comparison operations (==, !=, <, >, <=, >=)
//! - Logical operations (&&, ||, !)
//! - Function calls (built-in and user-defined)
//! - Variable references and scoping
//! - Query predicates for filtering
//! - Conditional expressions (ternary operator)
//! - Sequences and lists

use chrono::NaiveDate as Date;
use chrono::{DateTime, Local};
use ledger_math::{Amount, BigRational, Decimal};
use num_bigint::BigInt;
use num_traits::Zero;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub mod format;
pub mod functions;
pub mod op;
pub mod parser;
pub mod predicate;

/// Value type that expressions can evaluate to
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    /// Null/empty value
    Null,
    /// Boolean value
    Bool(bool),
    /// Integer value
    Integer(i64),
    /// Arbitrary precision rational number
    Rational(BigRational),
    /// Decimal value (for precise monetary calculations)
    Decimal(Decimal),
    /// Amount with optional commodity
    Amount(Amount),
    /// String value
    String(String),
    /// Date value
    Date(Date),
    /// DateTime value
    DateTime(DateTime<Local>),
    /// Sequence/list of values
    Sequence(Vec<Value>),
    /// Regular expression pattern
    Regex(String),
}

impl Value {
    /// Check if value is truthy for logical operations
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Integer(i) => *i != 0,
            Value::Rational(r) => !r.is_zero(),
            Value::Decimal(d) => !d.is_zero(),
            Value::Amount(a) => !a.is_zero(),
            Value::String(s) => !s.is_empty(),
            Value::Date(_) => true,
            Value::DateTime(_) => true,
            Value::Sequence(seq) => !seq.is_empty(),
            Value::Regex(_) => true,
        }
    }

    /// Get the type name for error messages
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Bool(_) => "bool",
            Value::Integer(_) => "integer",
            Value::Rational(_) => "rational",
            Value::Decimal(_) => "decimal",
            Value::Amount(_) => "amount",
            Value::String(_) => "string",
            Value::Date(_) => "date",
            Value::DateTime(_) => "datetime",
            Value::Sequence(_) => "sequence",
            Value::Regex(_) => "regex",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Rational(r) => write!(f, "{}", r),
            Value::Decimal(d) => write!(f, "{}", d),
            Value::Amount(a) => write!(f, "{}", a),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Date(d) => write!(f, "{}", d),
            Value::DateTime(dt) => write!(f, "{}", dt),
            Value::Sequence(seq) => {
                write!(f, "[")?;
                for (i, v) in seq.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Regex(r) => write!(f, "/{}/", r),
        }
    }
}

// Arithmetic operations for Value
impl std::ops::Add for Value {
    type Output = Result<Value, ExprError>;

    fn add(self, rhs: Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => Ok(Integer(a + b)),
            (Rational(a), Rational(b)) => Ok(Rational(a + b)),
            (Decimal(a), Decimal(b)) => Ok(Decimal(a + b)),
            (Amount(a), Amount(b)) => match a.add(&b) {
                Ok(result) => Ok(Amount(result)),
                Err(_) => Err(ExprError::RuntimeError("Amount addition failed".to_string())),
            },
            (Integer(a), Rational(b)) => {
                Ok(Rational(BigRational::new(BigInt::from(a), BigInt::from(1)) + b))
            }
            (Rational(a), Integer(b)) => {
                Ok(Rational(a + BigRational::new(BigInt::from(b), BigInt::from(1))))
            }
            (String(a), String(b)) => Ok(String(a + &b)),
            (Sequence(mut a), Sequence(b)) => {
                a.extend(b);
                Ok(Sequence(a))
            }
            (a, b) => Err(ExprError::TypeMismatch {
                expected: a.type_name().to_string(),
                found: b.type_name().to_string(),
                operation: "addition".to_string(),
            }),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Result<Value, ExprError>;

    fn sub(self, rhs: Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => Ok(Integer(a - b)),
            (Rational(a), Rational(b)) => Ok(Rational(a - b)),
            (Decimal(a), Decimal(b)) => Ok(Decimal(a - b)),
            (Amount(a), Amount(b)) => match a - &b {
                Ok(result) => Ok(Amount(result)),
                Err(_) => Err(ExprError::RuntimeError("Amount subtraction failed".to_string())),
            },
            (Integer(a), Rational(b)) => {
                Ok(Rational(BigRational::new(BigInt::from(a), BigInt::from(1)) - b))
            }
            (Rational(a), Integer(b)) => {
                Ok(Rational(a - BigRational::new(BigInt::from(b), BigInt::from(1))))
            }
            (a, b) => Err(ExprError::TypeMismatch {
                expected: a.type_name().to_string(),
                found: b.type_name().to_string(),
                operation: "subtraction".to_string(),
            }),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Result<Value, ExprError>;

    fn mul(self, rhs: Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => Ok(Integer(a * b)),
            (Rational(a), Rational(b)) => Ok(Rational(a * b)),
            (Decimal(a), Decimal(b)) => Ok(Decimal(a * b)),
            (Amount(a), Amount(b)) => match a * &b {
                Ok(result) => Ok(Amount(result)),
                Err(_) => Err(ExprError::RuntimeError("Amount multiplication failed".to_string())),
            },
            (Amount(a), Integer(b)) => {
                let amount_b = ledger_math::Amount::from_i64(b);
                match a * &amount_b {
                    Ok(result) => Ok(Amount(result)),
                    Err(_) => {
                        Err(ExprError::RuntimeError("Amount multiplication failed".to_string()))
                    }
                }
            }
            (Integer(a), Amount(b)) => {
                let amount_a = ledger_math::Amount::from_i64(a);
                match amount_a * &b {
                    Ok(result) => Ok(Amount(result)),
                    Err(_) => {
                        Err(ExprError::RuntimeError("Amount multiplication failed".to_string()))
                    }
                }
            }
            (Integer(a), Rational(b)) => {
                Ok(Rational(BigRational::new(BigInt::from(a), BigInt::from(1)) * b))
            }
            (Rational(a), Integer(b)) => {
                Ok(Rational(a * BigRational::new(BigInt::from(b), BigInt::from(1))))
            }
            (a, b) => Err(ExprError::TypeMismatch {
                expected: a.type_name().to_string(),
                found: b.type_name().to_string(),
                operation: "multiplication".to_string(),
            }),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Result<Value, ExprError>;

    fn div(self, rhs: Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => {
                if b == 0 {
                    Err(ExprError::DivisionByZero)
                } else {
                    Ok(Rational(BigRational::new(a.into(), b.into())))
                }
            }
            (Rational(a), Rational(b)) => {
                if b.is_zero() {
                    Err(ExprError::DivisionByZero)
                } else {
                    Ok(Rational(a / b))
                }
            }
            (Decimal(a), Decimal(b)) => {
                if b.is_zero() {
                    Err(ExprError::DivisionByZero)
                } else {
                    Ok(Decimal(a / b))
                }
            }
            (Amount(a), Amount(b)) => {
                if b.is_zero() {
                    Err(ExprError::DivisionByZero)
                } else {
                    match a / &b {
                        Ok(result) => Ok(Amount(result)),
                        Err(_) => {
                            Err(ExprError::RuntimeError("Amount division failed".to_string()))
                        }
                    }
                }
            }
            (Amount(a), Integer(b)) => {
                if b == 0 {
                    Err(ExprError::DivisionByZero)
                } else {
                    let amount_b = ledger_math::Amount::from_i64(b);
                    match a / &amount_b {
                        Ok(result) => Ok(Amount(result)),
                        Err(_) => {
                            Err(ExprError::RuntimeError("Amount division failed".to_string()))
                        }
                    }
                }
            }
            (Integer(a), Rational(b)) => {
                if b.is_zero() {
                    Err(ExprError::DivisionByZero)
                } else {
                    Ok(Rational(BigRational::new(BigInt::from(a), BigInt::from(1)) / b))
                }
            }
            (Rational(a), Integer(b)) => {
                if b == 0 {
                    Err(ExprError::DivisionByZero)
                } else {
                    Ok(Rational(a / BigRational::new(BigInt::from(b), BigInt::from(1))))
                }
            }
            (a, b) => Err(ExprError::TypeMismatch {
                expected: a.type_name().to_string(),
                found: b.type_name().to_string(),
                operation: "division".to_string(),
            }),
        }
    }
}

impl std::ops::Neg for Value {
    type Output = Result<Value, ExprError>;

    fn neg(self) -> Self::Output {
        use Value::*;
        match self {
            Integer(i) => Ok(Integer(-i)),
            Rational(r) => Ok(Rational(-r)),
            Decimal(d) => Ok(Decimal(-d)),
            Amount(a) => Ok(Amount(a.negated())),
            _ => Err(ExprError::TypeMismatch {
                expected: "numeric type".to_string(),
                found: self.type_name().to_string(),
                operation: "negation".to_string(),
            }),
        }
    }
}

// Additional helper methods for Value arithmetic
impl Value {
    /// Perform addition that can fail
    pub fn add(&self, other: &Value) -> Result<Value, ExprError> {
        self.clone() + other.clone()
    }

    /// Perform subtraction that can fail
    pub fn subtract(&self, other: &Value) -> Result<Value, ExprError> {
        self.clone() - other.clone()
    }

    /// Perform multiplication that can fail
    pub fn multiply(&self, other: &Value) -> Result<Value, ExprError> {
        self.clone() * other.clone()
    }

    /// Perform division that can fail
    pub fn divide(&self, other: &Value) -> Result<Value, ExprError> {
        self.clone() / other.clone()
    }

    /// Perform negation that can fail
    pub fn negate(&self) -> Result<Value, ExprError> {
        -self.clone()
    }

    /// Compare two values (for ordering operations)
    pub fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, ExprError> {
        use std::cmp::Ordering;
        use Value::*;

        match (self, other) {
            (Integer(a), Integer(b)) => Ok(a.cmp(b)),
            (Rational(a), Rational(b)) => Ok(a.cmp(b)),
            (Decimal(a), Decimal(b)) => Ok(a.cmp(b)),
            (Amount(a), Amount(b)) => {
                // Amounts need special comparison handling due to commodities
                if a == b {
                    Ok(Ordering::Equal)
                } else if a < b {
                    Ok(Ordering::Less)
                } else {
                    Ok(Ordering::Greater)
                }
            }
            (String(a), String(b)) => Ok(a.cmp(b)),
            (Date(a), Date(b)) => Ok(a.cmp(b)),
            (DateTime(a), DateTime(b)) => Ok(a.cmp(b)),
            (Bool(a), Bool(b)) => Ok(a.cmp(b)),

            // Cross-type numeric comparisons
            (Integer(a), Rational(b)) => {
                Ok(BigRational::new(BigInt::from(*a), BigInt::from(1)).cmp(b))
            }
            (Rational(a), Integer(b)) => {
                Ok(a.cmp(&BigRational::new(BigInt::from(*b), BigInt::from(1))))
            }

            // Null comparisons
            (Null, Null) => Ok(Ordering::Equal),
            (Null, _) => Ok(Ordering::Less),
            (_, Null) => Ok(Ordering::Greater),

            (a, b) => Err(ExprError::TypeMismatch {
                expected: a.type_name().to_string(),
                found: b.type_name().to_string(),
                operation: "comparison".to_string(),
            }),
        }
    }

    /// Check if two values are equal
    pub fn equals(&self, other: &Value) -> bool {
        use Value::*;
        match (self, other) {
            (Null, Null) => true,
            (Bool(a), Bool(b)) => a == b,
            (Integer(a), Integer(b)) => a == b,
            (Rational(a), Rational(b)) => a == b,
            (Decimal(a), Decimal(b)) => a == b,
            (Amount(a), Amount(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Date(a), Date(b)) => a == b,
            (DateTime(a), DateTime(b)) => a == b,
            (Sequence(a), Sequence(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.equals(y))
            }
            (Regex(a), Regex(b)) => a == b,

            // Cross-type numeric equality
            (Integer(a), Rational(b)) => &BigRational::new(BigInt::from(*a), BigInt::from(1)) == b,
            (Rational(a), Integer(b)) => a == &BigRational::new(BigInt::from(*b), BigInt::from(1)),

            _ => false,
        }
    }
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,

    // Logical
    And,
    Or,

    // Special
    Query, // ? in ternary
    Colon, // : in ternary
    Cons,  // List construction
    Seq,   // Sequence operator
    Match, // Pattern matching
}

impl BinaryOp {
    /// Get operator precedence (higher number = higher precedence)
    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOp::Or => 1,
            BinaryOp::And => 2,
            BinaryOp::Eq | BinaryOp::Ne => 3,
            BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => 4,
            BinaryOp::Add | BinaryOp::Sub => 5,
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 6,
            BinaryOp::Query => 0, // Ternary has special handling
            BinaryOp::Colon => 0, // Ternary has special handling
            BinaryOp::Cons => 7,
            BinaryOp::Seq => 0,   // Sequence has lowest precedence
            BinaryOp::Match => 4, // Same as comparison
        }
    }

    /// Check if operator is right-associative
    pub fn is_right_associative(&self) -> bool {
        matches!(self, BinaryOp::Query | BinaryOp::Colon)
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Le => "<=",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Query => "?",
            BinaryOp::Colon => ":",
            BinaryOp::Cons => "::",
            BinaryOp::Seq => ";",
            BinaryOp::Match => "=~",
        };
        write!(f, "{}", op)
    }
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    /// Negation (-)
    Neg,
    /// Logical NOT (!)
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
        };
        write!(f, "{}", op)
    }
}

/// Built-in functions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BuiltinFunction {
    // Mathematical functions
    Abs,
    Floor,
    Ceiling,
    Round,
    Truncate,
    Min,
    Max,

    // Date/time functions
    Now,
    Today,
    Age,
    FormatDate,

    // String functions
    FormatString,
    ToUpper,
    ToLower,
    Trim,

    // Type conversion
    ToString,
    ToInt,
    ToDecimal,
    ToAmount,

    // Aggregation functions
    Sum,
    Count,
    Average,

    // Utility functions
    IsEmpty,
    Length,
    Type,
}

impl fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            BuiltinFunction::Abs => "abs",
            BuiltinFunction::Floor => "floor",
            BuiltinFunction::Ceiling => "ceiling",
            BuiltinFunction::Round => "round",
            BuiltinFunction::Truncate => "truncate",
            BuiltinFunction::Min => "min",
            BuiltinFunction::Max => "max",
            BuiltinFunction::Now => "now",
            BuiltinFunction::Today => "today",
            BuiltinFunction::Age => "age",
            BuiltinFunction::FormatDate => "format_date",
            BuiltinFunction::FormatString => "format",
            BuiltinFunction::ToUpper => "to_upper",
            BuiltinFunction::ToLower => "to_lower",
            BuiltinFunction::Trim => "trim",
            BuiltinFunction::ToString => "to_string",
            BuiltinFunction::ToInt => "to_int",
            BuiltinFunction::ToDecimal => "to_decimal",
            BuiltinFunction::ToAmount => "to_amount",
            BuiltinFunction::Sum => "sum",
            BuiltinFunction::Count => "count",
            BuiltinFunction::Average => "average",
            BuiltinFunction::IsEmpty => "is_empty",
            BuiltinFunction::Length => "length",
            BuiltinFunction::Type => "type",
        };
        write!(f, "{}", name)
    }
}

/// Expression AST node
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExprNode {
    /// Literal value
    Value(Value),

    /// Variable identifier
    Identifier(String),

    /// Binary operation
    Binary { op: BinaryOp, left: Box<ExprNode>, right: Box<ExprNode> },

    /// Unary operation  
    Unary { op: UnaryOp, operand: Box<ExprNode> },

    /// Function call
    FunctionCall { function: BuiltinFunction, args: Vec<ExprNode> },

    /// User-defined function call
    UserFunction { name: String, args: Vec<ExprNode> },

    /// Conditional expression (ternary operator)
    Conditional { condition: Box<ExprNode>, if_true: Box<ExprNode>, if_false: Box<ExprNode> },

    /// Variable definition/assignment
    Define { name: String, value: Box<ExprNode> },

    /// Lambda expression
    Lambda { params: Vec<String>, body: Box<ExprNode> },

    /// Sequence of expressions
    Sequence(Vec<ExprNode>),
}

impl ExprNode {
    /// Create a literal integer value
    pub fn integer(value: i64) -> Self {
        ExprNode::Value(Value::Integer(value))
    }

    /// Create a literal decimal value
    pub fn decimal(value: Decimal) -> Self {
        ExprNode::Value(Value::Decimal(value))
    }

    /// Create a literal string value
    pub fn string(value: String) -> Self {
        ExprNode::Value(Value::String(value))
    }

    /// Create a literal boolean value
    pub fn boolean(value: bool) -> Self {
        ExprNode::Value(Value::Bool(value))
    }

    /// Create an identifier reference
    pub fn identifier(name: String) -> Self {
        ExprNode::Identifier(name)
    }

    /// Create a binary operation
    pub fn binary(op: BinaryOp, left: ExprNode, right: ExprNode) -> Self {
        ExprNode::Binary { op, left: Box::new(left), right: Box::new(right) }
    }

    /// Create a unary operation
    pub fn unary(op: UnaryOp, operand: ExprNode) -> Self {
        ExprNode::Unary { op, operand: Box::new(operand) }
    }

    /// Create a function call
    pub fn function_call(function: BuiltinFunction, args: Vec<ExprNode>) -> Self {
        ExprNode::FunctionCall { function, args }
    }

    /// Create a conditional expression
    pub fn conditional(condition: ExprNode, if_true: ExprNode, if_false: ExprNode) -> Self {
        ExprNode::Conditional {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false),
        }
    }
}

impl fmt::Display for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprNode::Value(v) => write!(f, "{}", v),
            ExprNode::Identifier(name) => write!(f, "{}", name),
            ExprNode::Binary { op, left, right } => write!(f, "({} {} {})", left, op, right),
            ExprNode::Unary { op, operand } => write!(f, "({}{})", op, operand),
            ExprNode::FunctionCall { function, args } => {
                write!(f, "{}(", function)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            ExprNode::UserFunction { name, args } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            ExprNode::Conditional { condition, if_true, if_false } => {
                write!(f, "({} ? {} : {})", condition, if_true, if_false)
            }
            ExprNode::Define { name, value } => write!(f, "({} = {})", name, value),
            ExprNode::Lambda { params, body } => {
                write!(f, "(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, " => {})", body)
            }
            ExprNode::Sequence(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            }
        }
    }
}

/// Expression evaluation context
#[derive(Debug, Clone)]
pub struct ExprContext {
    /// Variable bindings
    pub variables: HashMap<String, Value>,
    /// Parent scope (for nested contexts)
    parent: Option<Rc<ExprContext>>,
    /// User-defined functions
    functions: HashMap<String, (Vec<String>, ExprNode)>,
}

impl ExprContext {
    /// Create a new empty context
    pub fn new() -> Self {
        ExprContext { variables: HashMap::new(), parent: None, functions: HashMap::new() }
    }

    /// Create a child context with this as parent
    pub fn child(&self) -> Self {
        ExprContext {
            variables: HashMap::new(),
            parent: Some(Rc::new(self.clone())),
            functions: HashMap::new(),
        }
    }

    /// Set a variable value
    pub fn set_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    /// Get a variable value
    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        self.variables.get(name).or_else(|| self.parent.as_ref().and_then(|p| p.get_variable(name)))
    }

    /// Define a user function
    pub fn define_function(&mut self, name: String, params: Vec<String>, body: ExprNode) {
        self.functions.insert(name, (params, body));
    }

    /// Get a user function definition
    pub fn get_function(&self, name: &str) -> Option<&(Vec<String>, ExprNode)> {
        self.functions.get(name).or_else(|| self.parent.as_ref().and_then(|p| p.get_function(name)))
    }
}

impl Default for ExprContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Error types for expression evaluation
#[derive(Debug, Clone, PartialEq)]
pub enum ExprError {
    /// Unknown variable
    UnknownVariable(String),
    /// Unknown function
    UnknownFunction(String),
    /// Type mismatch in operation
    TypeMismatch { expected: String, found: String, operation: String },
    /// Invalid number of arguments
    InvalidArgCount { function: String, expected: usize, found: usize },
    /// Division by zero
    DivisionByZero,
    /// Parse error
    ParseError(String),
    /// Runtime error
    RuntimeError(String),
}

impl fmt::Display for ExprError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprError::UnknownVariable(name) => write!(f, "Unknown variable: {}", name),
            ExprError::UnknownFunction(name) => write!(f, "Unknown function: {}", name),
            ExprError::TypeMismatch { expected, found, operation } => {
                write!(f, "Type mismatch in {}: expected {}, found {}", operation, expected, found)
            }
            ExprError::InvalidArgCount { function, expected, found } => {
                write!(f, "Function {} expects {} arguments, got {}", function, expected, found)
            }
            ExprError::DivisionByZero => write!(f, "Division by zero"),
            ExprError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            ExprError::RuntimeError(msg) => write!(f, "Runtime error: {}", msg),
        }
    }
}

impl std::error::Error for ExprError {}

/// Result type for expression operations
pub type ExprResult<T> = Result<T, ExprError>;

/// Main expression type wrapping the AST
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Expression {
    /// Root AST node
    pub root: ExprNode,
    /// Original source text (for debugging)
    pub source: Option<String>,
}

impl Expression {
    /// Create a new expression from AST node
    pub fn new(root: ExprNode) -> Self {
        Expression { root, source: None }
    }

    /// Create an expression with source text
    pub fn with_source(root: ExprNode, source: String) -> Self {
        Expression { root, source: Some(source) }
    }

    /// Parse an expression from a string
    pub fn parse(input: &str) -> ExprResult<Self> {
        let mut expr = parser::parse_expression(input)?;
        expr.source = Some(input.to_string());
        Ok(expr)
    }

    /// Evaluate the expression with given context
    pub fn evaluate(&self, context: &ExprContext) -> ExprResult<Value> {
        evaluate_node(&self.root, context)
    }

    /// Check if expression is a constant value
    pub fn is_constant(&self) -> bool {
        is_constant_node(&self.root)
    }

    /// Get constant value if expression is constant
    pub fn constant_value(&self) -> Option<&Value> {
        match &self.root {
            ExprNode::Value(v) => Some(v),
            _ => None,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref source) = self.source {
            write!(f, "{}", source)
        } else {
            write!(f, "{}", self.root)
        }
    }
}

/// Check if an AST node represents a constant expression
fn is_constant_node(node: &ExprNode) -> bool {
    match node {
        ExprNode::Value(_) => true,
        ExprNode::Identifier(_) => false,
        ExprNode::Binary { left, right, .. } => is_constant_node(left) && is_constant_node(right),
        ExprNode::Unary { operand, .. } => is_constant_node(operand),
        ExprNode::FunctionCall { args, .. } => args.iter().all(is_constant_node),
        ExprNode::UserFunction { .. } => false,
        ExprNode::Conditional { condition, if_true, if_false } => {
            is_constant_node(condition) && is_constant_node(if_true) && is_constant_node(if_false)
        }
        ExprNode::Define { .. } => false,
        ExprNode::Lambda { .. } => false,
        ExprNode::Sequence(exprs) => exprs.iter().all(is_constant_node),
    }
}

/// Evaluate an AST node with given context
fn evaluate_node(node: &ExprNode, context: &ExprContext) -> ExprResult<Value> {
    match node {
        ExprNode::Value(v) => Ok(v.clone()),

        ExprNode::Identifier(name) => {
            match context.get_variable(name) {
                Some(value) => Ok(value.clone()),
                None => {
                    // Handle tag variables specially - return Null if tag doesn't exist
                    if name.starts_with("tag_") || name.starts_with("posting_tag_") {
                        Ok(Value::Null)
                    } else {
                        Err(ExprError::UnknownVariable(name.clone()))
                    }
                }
            }
        }

        ExprNode::Binary { op, left, right } => evaluate_binary_op(*op, left, right, context),

        ExprNode::Unary { op, operand } => evaluate_unary_op(*op, operand, context),

        ExprNode::FunctionCall { function, args } => {
            evaluate_builtin_function(*function, args, context)
        }

        ExprNode::UserFunction { name, args } => evaluate_user_function(name, args, context),

        ExprNode::Conditional { condition, if_true, if_false } => {
            let cond_val = evaluate_node(condition, context)?;
            if cond_val.is_truthy() {
                evaluate_node(if_true, context)
            } else {
                evaluate_node(if_false, context)
            }
        }

        ExprNode::Define { name: _, value } => {
            // This should be handled at a higher level to modify context
            // For now, just evaluate the value
            evaluate_node(value, context)
        }

        ExprNode::Lambda { .. } => {
            // Return the lambda as a value (not supported yet)
            Err(ExprError::RuntimeError("Lambda expressions not yet implemented".to_string()))
        }

        ExprNode::Sequence(exprs) => {
            let mut result = Value::Null;
            for expr in exprs {
                result = evaluate_node(expr, context)?;
            }
            Ok(result)
        }
    }
}

// Evaluation functions implemented in op.rs
fn evaluate_binary_op(
    op: BinaryOp,
    left: &ExprNode,
    right: &ExprNode,
    context: &ExprContext,
) -> ExprResult<Value> {
    op::evaluate_binary_op(op, left, right, context)
}

fn evaluate_unary_op(op: UnaryOp, operand: &ExprNode, context: &ExprContext) -> ExprResult<Value> {
    op::evaluate_unary_op(op, operand, context)
}

fn evaluate_builtin_function(
    function: BuiltinFunction,
    args: &[ExprNode],
    context: &ExprContext,
) -> ExprResult<Value> {
    functions::evaluate_builtin_function(function, args, context)
}

fn evaluate_user_function(
    _name: &str,
    _args: &[ExprNode],
    _context: &ExprContext,
) -> ExprResult<Value> {
    // Placeholder - will be implemented with user function support
    Err(ExprError::RuntimeError("User functions not yet implemented".to_string()))
}
