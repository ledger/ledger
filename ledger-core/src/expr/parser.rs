//! Expression parser implementing recursive descent with operator precedence
//!
//! This module provides a Pratt parser for converting text expressions into AST.
//! Supports all operators, function calls, and special syntax from the C++ ledger.

use crate::expr::{
    ExprNode, BinaryOp, UnaryOp, BuiltinFunction, Value, Expression, ExprError, ExprResult
};
use rust_decimal::Decimal;
use std::collections::HashMap;
use std::str::Chars;
use std::iter::Peekable;

/// Token types for lexical analysis
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Integer(i64),
    Decimal(Decimal),
    String(String),
    Boolean(bool),
    Null,
    
    // Identifiers and functions
    Identifier(String),
    Function(BuiltinFunction),
    
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Not,
    Question,
    Colon,
    Match,
    
    // Delimiters
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Semicolon,
    
    // Special
    EndOfInput,
}

impl Token {
    /// Convert token to binary operator if possible
    pub fn to_binary_op(&self) -> Option<BinaryOp> {
        match self {
            Token::Plus => Some(BinaryOp::Add),
            Token::Minus => Some(BinaryOp::Sub),
            Token::Star => Some(BinaryOp::Mul),
            Token::Slash => Some(BinaryOp::Div),
            Token::Percent => Some(BinaryOp::Mod),
            Token::Equal => Some(BinaryOp::Eq),
            Token::NotEqual => Some(BinaryOp::Ne),
            Token::Less => Some(BinaryOp::Lt),
            Token::Greater => Some(BinaryOp::Gt),
            Token::LessEqual => Some(BinaryOp::Le),
            Token::GreaterEqual => Some(BinaryOp::Ge),
            Token::And => Some(BinaryOp::And),
            Token::Or => Some(BinaryOp::Or),
            Token::Question => Some(BinaryOp::Query),
            Token::Colon => Some(BinaryOp::Colon),
            Token::Match => Some(BinaryOp::Match),
            Token::Semicolon => Some(BinaryOp::Seq),
            _ => None,
        }
    }
    
    /// Convert token to unary operator if possible
    pub fn to_unary_op(&self) -> Option<UnaryOp> {
        match self {
            Token::Minus => Some(UnaryOp::Neg),
            Token::Not => Some(UnaryOp::Not),
            _ => None,
        }
    }
}

/// Position information for error reporting
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new() -> Self {
        Position { line: 1, column: 1 }
    }
    
    pub fn advance(&mut self, ch: char) {
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}

/// Lexer for tokenizing expression strings
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    position: Position,
    functions: HashMap<String, BuiltinFunction>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut functions = HashMap::new();
        
        // Register built-in functions
        functions.insert("abs".to_string(), BuiltinFunction::Abs);
        functions.insert("floor".to_string(), BuiltinFunction::Floor);
        functions.insert("ceiling".to_string(), BuiltinFunction::Ceiling);
        functions.insert("round".to_string(), BuiltinFunction::Round);
        functions.insert("truncate".to_string(), BuiltinFunction::Truncate);
        functions.insert("min".to_string(), BuiltinFunction::Min);
        functions.insert("max".to_string(), BuiltinFunction::Max);
        functions.insert("now".to_string(), BuiltinFunction::Now);
        functions.insert("today".to_string(), BuiltinFunction::Today);
        functions.insert("age".to_string(), BuiltinFunction::Age);
        functions.insert("format_date".to_string(), BuiltinFunction::FormatDate);
        functions.insert("format".to_string(), BuiltinFunction::FormatString);
        functions.insert("to_upper".to_string(), BuiltinFunction::ToUpper);
        functions.insert("to_lower".to_string(), BuiltinFunction::ToLower);
        functions.insert("trim".to_string(), BuiltinFunction::Trim);
        functions.insert("to_string".to_string(), BuiltinFunction::ToString);
        functions.insert("to_int".to_string(), BuiltinFunction::ToInt);
        functions.insert("to_decimal".to_string(), BuiltinFunction::ToDecimal);
        functions.insert("to_amount".to_string(), BuiltinFunction::ToAmount);
        functions.insert("sum".to_string(), BuiltinFunction::Sum);
        functions.insert("count".to_string(), BuiltinFunction::Count);
        functions.insert("average".to_string(), BuiltinFunction::Average);
        functions.insert("is_empty".to_string(), BuiltinFunction::IsEmpty);
        functions.insert("length".to_string(), BuiltinFunction::Length);
        functions.insert("type".to_string(), BuiltinFunction::Type);
        
        Lexer {
            input: input.chars().peekable(),
            position: Position::new(),
            functions,
        }
    }
    
    pub fn position(&self) -> Position {
        self.position
    }
    
    /// Peek at the next character without consuming it
    fn peek(&mut self) -> Option<char> {
        self.input.peek().cloned()
    }
    
    /// Consume and return the next character
    fn next_char(&mut self) -> Option<char> {
        if let Some(ch) = self.input.next() {
            self.position.advance(ch);
            Some(ch)
        } else {
            None
        }
    }
    
    /// Skip whitespace characters
    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.input.peek() {
            if ch.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }
    
    /// Read a number (integer or decimal)
    fn read_number(&mut self, first_digit: char) -> ExprResult<Token> {
        let mut number = String::new();
        number.push(first_digit);
        
        let mut has_decimal = false;
        
        while let Some(&ch) = self.input.peek() {
            if ch.is_ascii_digit() {
                number.push(ch);
                self.next_char();
            } else if ch == '.' && !has_decimal {
                has_decimal = true;
                number.push(ch);
                self.next_char();
            } else {
                break;
            }
        }
        
        if has_decimal {
            number.parse::<Decimal>()
                .map(Token::Decimal)
                .map_err(|_| ExprError::ParseError(format!("Invalid decimal number: {}", number)))
        } else {
            number.parse::<i64>()
                .map(Token::Integer)
                .map_err(|_| ExprError::ParseError(format!("Invalid integer: {}", number)))
        }
    }
    
    /// Read an identifier or keyword
    fn read_identifier(&mut self, first_char: char) -> Token {
        let mut identifier = String::new();
        identifier.push(first_char);
        
        while let Some(&ch) = self.input.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                identifier.push(ch);
                self.next_char();
            } else {
                break;
            }
        }
        
        // Check for keywords
        match identifier.as_str() {
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "null" => Token::Null,
            _ => {
                // Check if it's a built-in function
                if let Some(&function) = self.functions.get(&identifier) {
                    Token::Function(function)
                } else {
                    Token::Identifier(identifier)
                }
            }
        }
    }
    
    /// Read a quoted string
    fn read_string(&mut self) -> ExprResult<Token> {
        let mut string = String::new();
        
        // Skip opening quote
        self.next_char();
        
        while let Some(ch) = self.next_char() {
            if ch == '"' {
                return Ok(Token::String(string));
            } else if ch == '\\' {
                // Handle escape sequences
                if let Some(escaped) = self.next_char() {
                    match escaped {
                        'n' => string.push('\n'),
                        't' => string.push('\t'),
                        'r' => string.push('\r'),
                        '\\' => string.push('\\'),
                        '"' => string.push('"'),
                        _ => {
                            string.push('\\');
                            string.push(escaped);
                        }
                    }
                }
            } else {
                string.push(ch);
            }
        }
        
        Err(ExprError::ParseError("Unterminated string".to_string()))
    }
    
    /// Get the next token
    pub fn next_token(&mut self) -> ExprResult<Token> {
        self.skip_whitespace();
        
        match self.next_char() {
            None => Ok(Token::EndOfInput),
            Some(ch) => match ch {
                '+' => Ok(Token::Plus),
                '-' => Ok(Token::Minus),
                '*' => Ok(Token::Star),
                '/' => Ok(Token::Slash),
                '%' => Ok(Token::Percent),
                '(' => Ok(Token::LeftParen),
                ')' => Ok(Token::RightParen),
                '[' => Ok(Token::LeftBracket),
                ']' => Ok(Token::RightBracket),
                ',' => Ok(Token::Comma),
                ';' => Ok(Token::Semicolon),
                '?' => Ok(Token::Question),
                ':' => Ok(Token::Colon),
                '!' => {
                    if self.peek() == Some('=') {
                        self.next_char();
                        Ok(Token::NotEqual)
                    } else {
                        Ok(Token::Not)
                    }
                }
                '=' => {
                    if self.peek() == Some('=') {
                        self.next_char();
                        Ok(Token::Equal)
                    } else if self.peek() == Some('~') {
                        self.next_char();
                        Ok(Token::Match)
                    } else {
                        Err(ExprError::ParseError("Expected '==' or '=~'".to_string()))
                    }
                }
                '<' => {
                    if self.peek() == Some('=') {
                        self.next_char();
                        Ok(Token::LessEqual)
                    } else {
                        Ok(Token::Less)
                    }
                }
                '>' => {
                    if self.peek() == Some('=') {
                        self.next_char();
                        Ok(Token::GreaterEqual)
                    } else {
                        Ok(Token::Greater)
                    }
                }
                '&' => {
                    if self.peek() == Some('&') {
                        self.next_char();
                        Ok(Token::And)
                    } else {
                        Err(ExprError::ParseError("Expected '&&'".to_string()))
                    }
                }
                '|' => {
                    if self.peek() == Some('|') {
                        self.next_char();
                        Ok(Token::Or)
                    } else {
                        Err(ExprError::ParseError("Expected '||'".to_string()))
                    }
                }
                '"' => self.read_string(),
                ch if ch.is_ascii_digit() => self.read_number(ch),
                ch if ch.is_alphabetic() || ch == '_' => Ok(self.read_identifier(ch)),
                _ => Err(ExprError::ParseError(format!("Unexpected character: '{}'", ch))),
            }
        }
    }
}

/// Expression parser using Pratt parsing algorithm
pub struct ExprParser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> ExprParser<'a> {
    pub fn new(input: &'a str) -> ExprResult<Self> {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token()?;
        
        Ok(ExprParser {
            lexer,
            current_token,
        })
    }
    
    /// Advance to the next token
    fn advance(&mut self) -> ExprResult<()> {
        self.current_token = self.lexer.next_token()?;
        Ok(())
    }
    
    /// Check if current token matches expected token
    fn expect(&mut self, expected: Token) -> ExprResult<()> {
        if std::mem::discriminant(&self.current_token) == std::mem::discriminant(&expected) {
            self.advance()
        } else {
            Err(ExprError::ParseError(format!(
                "Expected {:?}, found {:?}",
                expected, self.current_token
            )))
        }
    }
    
    /// Parse a complete expression
    pub fn parse(&mut self) -> ExprResult<Expression> {
        let root = self.parse_expression(0)?;
        
        if self.current_token != Token::EndOfInput {
            return Err(ExprError::ParseError(
                "Unexpected token after expression".to_string()
            ));
        }
        
        Ok(Expression::new(root))
    }
    
    /// Parse expression with given minimum precedence (Pratt parsing)
    fn parse_expression(&mut self, min_precedence: u8) -> ExprResult<ExprNode> {
        let mut left = self.parse_primary()?;
        
        while let Some(op) = self.current_token.to_binary_op() {
            let precedence = op.precedence();
            if precedence < min_precedence {
                break;
            }
            
            self.advance()?; // consume operator
            
            let next_min_precedence = if op.is_right_associative() {
                precedence
            } else {
                precedence + 1
            };
            
            let right = self.parse_expression(next_min_precedence)?;
            
            // Handle ternary operator specially
            if matches!(op, BinaryOp::Query) {
                self.expect(Token::Colon)?;
                let else_expr = self.parse_expression(0)?;
                left = ExprNode::Conditional {
                    condition: Box::new(left),
                    if_true: Box::new(right),
                    if_false: Box::new(else_expr),
                };
            } else {
                left = ExprNode::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
        }
        
        Ok(left)
    }
    
    /// Parse primary expressions (literals, identifiers, parenthesized expressions)
    fn parse_primary(&mut self) -> ExprResult<ExprNode> {
        match self.current_token.clone() {
            Token::Integer(n) => {
                self.advance()?;
                Ok(ExprNode::Value(Value::Integer(n)))
            }
            Token::Decimal(d) => {
                self.advance()?;
                Ok(ExprNode::Value(Value::Decimal(d)))
            }
            Token::String(s) => {
                self.advance()?;
                Ok(ExprNode::Value(Value::String(s)))
            }
            Token::Boolean(b) => {
                self.advance()?;
                Ok(ExprNode::Value(Value::Bool(b)))
            }
            Token::Null => {
                self.advance()?;
                Ok(ExprNode::Value(Value::Null))
            }
            Token::Identifier(name) => {
                self.advance()?;
                if self.current_token == Token::LeftParen {
                    // User function call
                    let args = self.parse_argument_list()?;
                    Ok(ExprNode::UserFunction { name, args })
                } else {
                    Ok(ExprNode::Identifier(name))
                }
            }
            Token::Function(function) => {
                self.advance()?;
                let args = self.parse_argument_list()?;
                Ok(ExprNode::FunctionCall { function, args })
            }
            Token::LeftParen => {
                self.advance()?;
                let expr = self.parse_expression(0)?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            }
            Token::Minus | Token::Not => {
                let op = self.current_token.to_unary_op().unwrap();
                self.advance()?;
                let operand = self.parse_primary()?;
                Ok(ExprNode::Unary {
                    op,
                    operand: Box::new(operand),
                })
            }
            _ => Err(ExprError::ParseError(format!(
                "Unexpected token in primary expression: {:?}",
                self.current_token
            ))),
        }
    }
    
    /// Parse function argument list
    fn parse_argument_list(&mut self) -> ExprResult<Vec<ExprNode>> {
        self.expect(Token::LeftParen)?;
        
        let mut args = Vec::new();
        
        if self.current_token != Token::RightParen {
            loop {
                args.push(self.parse_expression(0)?);
                
                if self.current_token == Token::Comma {
                    self.advance()?;
                } else {
                    break;
                }
            }
        }
        
        self.expect(Token::RightParen)?;
        Ok(args)
    }
}

/// Parse an expression from a string
pub fn parse_expression(input: &str) -> ExprResult<Expression> {
    let mut parser = ExprParser::new(input)?;
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_integer() {
        let expr = parse_expression("42").unwrap();
        assert_eq!(expr.root, ExprNode::Value(Value::Integer(42)));
    }
    
    #[test]
    fn test_parse_decimal() {
        use std::str::FromStr;
        let expr = parse_expression("3.14").unwrap();
        assert_eq!(expr.root, ExprNode::Value(Value::Decimal(Decimal::from_str("3.14").unwrap())));
    }
    
    #[test]
    fn test_parse_string() {
        let expr = parse_expression("\"hello\"").unwrap();
        assert_eq!(expr.root, ExprNode::Value(Value::String("hello".to_string())));
    }
    
    #[test]
    fn test_parse_boolean() {
        let expr = parse_expression("true").unwrap();
        assert_eq!(expr.root, ExprNode::Value(Value::Bool(true)));
    }
    
    #[test]
    fn test_parse_identifier() {
        let expr = parse_expression("foo").unwrap();
        assert_eq!(expr.root, ExprNode::Identifier("foo".to_string()));
    }
    
    #[test]
    fn test_parse_binary_add() {
        let expr = parse_expression("1 + 2").unwrap();
        match expr.root {
            ExprNode::Binary { op: BinaryOp::Add, left, right } => {
                assert_eq!(*left, ExprNode::Value(Value::Integer(1)));
                assert_eq!(*right, ExprNode::Value(Value::Integer(2)));
            }
            _ => panic!("Expected binary addition"),
        }
    }
    
    #[test]
    fn test_parse_function_call() {
        let expr = parse_expression("abs(-5)").unwrap();
        match expr.root {
            ExprNode::FunctionCall { function: BuiltinFunction::Abs, args } => {
                assert_eq!(args.len(), 1);
                match &args[0] {
                    ExprNode::Unary { op: UnaryOp::Neg, operand } => {
                        assert_eq!(**operand, ExprNode::Value(Value::Integer(5)));
                    }
                    _ => panic!("Expected unary negation"),
                }
            }
            _ => panic!("Expected function call"),
        }
    }
    
    #[test]
    fn test_parse_precedence() {
        let expr = parse_expression("1 + 2 * 3").unwrap();
        match expr.root {
            ExprNode::Binary { op: BinaryOp::Add, left, right } => {
                assert_eq!(*left, ExprNode::Value(Value::Integer(1)));
                match *right {
                    ExprNode::Binary { op: BinaryOp::Mul, left, right } => {
                        assert_eq!(*left, ExprNode::Value(Value::Integer(2)));
                        assert_eq!(*right, ExprNode::Value(Value::Integer(3)));
                    }
                    _ => panic!("Expected multiplication on right side"),
                }
            }
            _ => panic!("Expected binary addition"),
        }
    }
    
    #[test]
    fn test_parse_parentheses() {
        let expr = parse_expression("(1 + 2) * 3").unwrap();
        match expr.root {
            ExprNode::Binary { op: BinaryOp::Mul, left, right } => {
                match *left {
                    ExprNode::Binary { op: BinaryOp::Add, left, right } => {
                        assert_eq!(*left, ExprNode::Value(Value::Integer(1)));
                        assert_eq!(*right, ExprNode::Value(Value::Integer(2)));
                    }
                    _ => panic!("Expected addition on left side"),
                }
                assert_eq!(*right, ExprNode::Value(Value::Integer(3)));
            }
            _ => panic!("Expected binary multiplication"),
        }
    }
}