//! Query predicate support for filtering transactions and postings
//!
//! This module provides expression-based filtering capabilities for transactions and postings.
//! It supports special variables like `account`, `amount`, `date`, `payee`, `note`, `cleared`, 
//! `pending`, and regex matching with optimized evaluation strategies.

use super::{Expression, ExprContext, Value, ExprResult, ExprError};
use crate::transaction::{Transaction, TransactionStatus};
use crate::posting::Posting;
use ledger_math::Amount;
use std::collections::HashMap;
use regex::Regex;
use chrono::NaiveDate;

/// Wrapper for expressions used as query predicates
#[derive(Debug, Clone)]
pub struct QueryPredicate {
    /// The underlying expression
    expression: Expression,
    /// Cached compiled regex patterns for performance
    regex_cache: HashMap<String, Regex>,
    /// Short-circuit optimization flags
    optimization_hints: OptimizationHints,
}

/// Optimization hints for predicate evaluation
#[derive(Debug, Clone, Default)]
pub struct OptimizationHints {
    /// Whether the predicate always evaluates to true
    pub always_true: bool,
    /// Whether the predicate always evaluates to false
    pub always_false: bool,
    /// Whether the predicate is a simple constant
    pub is_constant: bool,
    /// Variables referenced by the predicate
    pub referenced_variables: Vec<String>,
}

/// Context for evaluating predicates against specific data
#[derive(Debug, Clone)]
pub struct PredicateContext {
    /// Base expression context
    base_context: ExprContext,
    /// Current transaction being evaluated (if any)
    current_transaction: Option<Transaction>,
    /// Current posting being evaluated (if any)
    current_posting: Option<Posting>,
    /// Additional user-defined variables
    user_variables: HashMap<String, Value>,
}

impl QueryPredicate {
    /// Create a new query predicate from an expression
    pub fn new(expression: Expression) -> Self {
        let optimization_hints = Self::analyze_optimization_hints(&expression);
        
        QueryPredicate {
            expression,
            regex_cache: HashMap::new(),
            optimization_hints,
        }
    }
    
    /// Parse and create a query predicate from a string
    pub fn parse(input: &str) -> ExprResult<Self> {
        let expression = Expression::parse(input)?;
        Ok(Self::new(expression))
    }
    
    /// Check if predicate matches a transaction
    pub fn matches_transaction(&mut self, transaction: &Transaction) -> ExprResult<bool> {
        // Short-circuit optimizations
        if self.optimization_hints.always_true {
            return Ok(true);
        }
        if self.optimization_hints.always_false {
            return Ok(false);
        }
        if self.optimization_hints.is_constant {
            let context = ExprContext::new();
            let result = self.expression.evaluate(&context)?;
            return Ok(result.is_truthy());
        }
        
        // Create context with transaction variables
        let context = self.create_transaction_context(transaction);
        
        // Evaluate expression
        let result = self.expression.evaluate(&context)?;
        Ok(result.is_truthy())
    }
    
    /// Check if predicate matches a posting (with optional transaction context)
    pub fn matches_posting(&mut self, posting: &Posting, transaction: Option<&Transaction>) -> ExprResult<bool> {
        // Short-circuit optimizations
        if self.optimization_hints.always_true {
            return Ok(true);
        }
        if self.optimization_hints.always_false {
            return Ok(false);
        }
        if self.optimization_hints.is_constant {
            let context = ExprContext::new();
            let result = self.expression.evaluate(&context)?;
            return Ok(result.is_truthy());
        }
        
        // Create context with posting variables
        let context = self.create_posting_context(posting, transaction);
        
        // Evaluate expression
        let result = self.expression.evaluate(&context)?;
        Ok(result.is_truthy())
    }
    
    /// Check if predicate matches both a transaction and a specific posting
    pub fn matches_transaction_posting(&mut self, transaction: &Transaction, posting: &Posting) -> ExprResult<bool> {
        // Short-circuit optimizations
        if self.optimization_hints.always_true {
            return Ok(true);
        }
        if self.optimization_hints.always_false {
            return Ok(false);
        }
        if self.optimization_hints.is_constant {
            let context = ExprContext::new();
            let result = self.expression.evaluate(&context)?;
            return Ok(result.is_truthy());
        }
        
        // Create context with both transaction and posting variables
        let context = self.create_combined_context(transaction, posting);
        
        // Evaluate expression
        let result = self.expression.evaluate(&context)?;
        Ok(result.is_truthy())
    }
    
    /// Filter a collection of transactions
    pub fn filter_transactions<'a>(&mut self, transactions: &'a [Transaction]) -> ExprResult<Vec<&'a Transaction>> {
        let mut filtered = Vec::new();
        
        for transaction in transactions {
            if self.matches_transaction(transaction)? {
                filtered.push(transaction);
            }
        }
        
        Ok(filtered)
    }
    
    /// Filter postings from a transaction
    pub fn filter_postings<'a>(&mut self, transaction: &'a Transaction) -> ExprResult<Vec<&'a Posting>> {
        let mut filtered = Vec::new();
        
        for posting in &transaction.postings {
            if self.matches_posting(posting, Some(transaction))? {
                filtered.push(posting);
            }
        }
        
        Ok(filtered)
    }
    
    /// Create expression context for a transaction
    fn create_transaction_context(&self, transaction: &Transaction) -> ExprContext {
        let mut context = ExprContext::new();
        
        // Transaction-specific variables
        context.set_variable("date".to_string(), Value::Date(transaction.date));
        
        if let Some(aux_date) = transaction.aux_date {
            context.set_variable("aux_date".to_string(), Value::Date(aux_date));
        }
        
        context.set_variable("payee".to_string(), Value::String(transaction.payee.clone()));
        
        if let Some(ref note) = transaction.note {
            context.set_variable("note".to_string(), Value::String(note.clone()));
        }
        
        if let Some(ref code) = transaction.code {
            context.set_variable("code".to_string(), Value::String(code.clone()));
        }
        
        // Status flags
        context.set_variable("cleared".to_string(), Value::Bool(matches!(transaction.status, TransactionStatus::Cleared)));
        context.set_variable("pending".to_string(), Value::Bool(matches!(transaction.status, TransactionStatus::Pending)));
        context.set_variable("uncleared".to_string(), Value::Bool(matches!(transaction.status, TransactionStatus::Uncleared)));
        
        // Transaction metadata
        for (key, tag_data) in &transaction.metadata {
            let value = if let Some(ref tag_value) = tag_data.value {
                Value::String(tag_value.clone())
            } else {
                Value::Bool(true)
            };
            context.set_variable(format!("tag_{}", key), value);
        }
        
        // Calculated values
        context.set_variable("posting_count".to_string(), Value::Integer(transaction.postings.len() as i64));
        context.set_variable("has_virtual".to_string(), Value::Bool(transaction.has_virtual_postings()));
        
        context
    }
    
    /// Create expression context for a posting
    fn create_posting_context(&self, posting: &Posting, transaction: Option<&Transaction>) -> ExprContext {
        let mut context = ExprContext::new();
        
        // Account information
        let account_name = posting.account.borrow().name().to_string();
        context.set_variable("account".to_string(), Value::String(account_name.clone()));
        context.set_variable("account_name".to_string(), Value::String(account_name));
        
        // Amount information
        if let Some(ref amount) = posting.amount {
            context.set_variable("amount".to_string(), Value::Amount(amount.clone()));
            context.set_variable("value".to_string(), Value::Decimal(amount.value()));
            
            if let Some(commodity) = amount.commodity() {
                context.set_variable("commodity".to_string(), Value::String(commodity.symbol().to_string()));
            }
        }
        
        // Cost information
        if let Some(ref cost) = posting.cost {
            context.set_variable("cost".to_string(), Value::Amount(cost.clone()));
        }
        
        // Posting-specific payee and note
        if let Some(ref payee) = posting.payee {
            context.set_variable("posting_payee".to_string(), Value::String(payee.to_string()));
        }
        
        if let Some(ref note) = posting.note {
            context.set_variable("posting_note".to_string(), Value::String(note.to_string()));
        }
        
        // Status flags
        context.set_variable("virtual".to_string(), Value::Bool(posting.is_virtual()));
        context.set_variable("calculated".to_string(), Value::Bool(posting.is_calculated()));
        context.set_variable("deferred".to_string(), Value::Bool(posting.is_deferred()));
        context.set_variable("timelog".to_string(), Value::Bool(posting.is_timelog()));
        
        // Posting metadata
        for (key, tag_data) in &posting.metadata {
            let value = if let Some(ref tag_value) = tag_data.value {
                Value::String(tag_value.clone())
            } else {
                Value::Bool(true)
            };
            context.set_variable(format!("posting_tag_{}", key), value);
        }
        
        // If transaction is provided, include transaction context
        if let Some(transaction) = transaction {
            let tx_context = self.create_transaction_context(transaction);
            // Merge transaction variables
            for (name, value) in tx_context.variables.iter() {
                if !context.variables.contains_key(name) {
                    context.set_variable(name.clone(), value.clone());
                }
            }
        }
        
        context
    }
    
    /// Create combined context for transaction and posting
    fn create_combined_context(&self, transaction: &Transaction, posting: &Posting) -> ExprContext {
        self.create_posting_context(posting, Some(transaction))
    }
    
    /// Analyze optimization hints for the expression
    fn analyze_optimization_hints(expression: &Expression) -> OptimizationHints {
        let mut hints = OptimizationHints::default();
        
        // Check if constant
        if expression.is_constant() {
            hints.is_constant = true;
            if let Some(constant_value) = expression.constant_value() {
                hints.always_true = constant_value.is_truthy();
                hints.always_false = !constant_value.is_truthy();
            }
        }
        
        // Analyze referenced variables
        hints.referenced_variables = Self::extract_variable_references(&expression.root);
        
        hints
    }
    
    /// Extract variable references from an expression AST
    fn extract_variable_references(node: &crate::expr::ExprNode) -> Vec<String> {
        let mut variables = Vec::new();
        Self::extract_variables_recursive(node, &mut variables);
        variables.sort();
        variables.dedup();
        variables
    }
    
    /// Recursively extract variables from AST nodes
    fn extract_variables_recursive(node: &crate::expr::ExprNode, variables: &mut Vec<String>) {
        use crate::expr::ExprNode;
        
        match node {
            ExprNode::Identifier(name) => {
                variables.push(name.clone());
            },
            ExprNode::Binary { left, right, .. } => {
                Self::extract_variables_recursive(left, variables);
                Self::extract_variables_recursive(right, variables);
            },
            ExprNode::Unary { operand, .. } => {
                Self::extract_variables_recursive(operand, variables);
            },
            ExprNode::FunctionCall { args, .. } => {
                for arg in args {
                    Self::extract_variables_recursive(arg, variables);
                }
            },
            ExprNode::UserFunction { args, .. } => {
                for arg in args {
                    Self::extract_variables_recursive(arg, variables);
                }
            },
            ExprNode::Conditional { condition, if_true, if_false } => {
                Self::extract_variables_recursive(condition, variables);
                Self::extract_variables_recursive(if_true, variables);
                Self::extract_variables_recursive(if_false, variables);
            },
            ExprNode::Define { value, .. } => {
                Self::extract_variables_recursive(value, variables);
            },
            ExprNode::Lambda { body, .. } => {
                Self::extract_variables_recursive(body, variables);
            },
            ExprNode::Sequence(exprs) => {
                for expr in exprs {
                    Self::extract_variables_recursive(expr, variables);
                }
            },
            _ => {} // Value nodes don't contain variables
        }
    }
    
    /// Get cached regex or compile new one
    fn get_regex(&mut self, pattern: &str) -> Result<&Regex, ExprError> {
        if !self.regex_cache.contains_key(pattern) {
            let regex = Regex::new(pattern)
                .map_err(|e| ExprError::RuntimeError(format!("Invalid regex pattern '{}': {}", pattern, e)))?;
            self.regex_cache.insert(pattern.to_string(), regex);
        }
        Ok(self.regex_cache.get(pattern).unwrap())
    }
}

/// Specialized predicates for common query patterns
pub struct PredicateBuilder;

impl PredicateBuilder {
    /// Create a predicate for account name matching
    pub fn account_matches(pattern: &str) -> ExprResult<QueryPredicate> {
        let expr_str = format!("account =~ /{}/", pattern);
        QueryPredicate::parse(&expr_str)
    }
    
    /// Create a predicate for payee matching
    pub fn payee_matches(pattern: &str) -> ExprResult<QueryPredicate> {
        let expr_str = format!("payee =~ /{}/", pattern);
        QueryPredicate::parse(&expr_str)
    }
    
    /// Create a predicate for amount comparison
    pub fn amount_greater_than(amount: &str) -> ExprResult<QueryPredicate> {
        let expr_str = format!("amount > {}", amount);
        QueryPredicate::parse(&expr_str)
    }
    
    /// Create a predicate for amount less than
    pub fn amount_less_than(amount: &str) -> ExprResult<QueryPredicate> {
        let expr_str = format!("amount < {}", amount);
        QueryPredicate::parse(&expr_str)
    }
    
    /// Create a predicate for date range
    pub fn date_between(start: NaiveDate, end: NaiveDate) -> ExprResult<QueryPredicate> {
        let expr_str = format!("date >= '{}' && date <= '{}'", start.format("%Y-%m-%d"), end.format("%Y-%m-%d"));
        QueryPredicate::parse(&expr_str)
    }
    
    /// Create a predicate for cleared transactions only
    pub fn cleared_only() -> ExprResult<QueryPredicate> {
        QueryPredicate::parse("cleared")
    }
    
    /// Create a predicate for pending transactions only
    pub fn pending_only() -> ExprResult<QueryPredicate> {
        QueryPredicate::parse("pending")
    }
    
    /// Create a predicate for transactions with specific tag
    pub fn has_tag(tag: &str) -> ExprResult<QueryPredicate> {
        let expr_str = format!("tag_{}", tag);
        QueryPredicate::parse(&expr_str)
    }
    
    /// Create a predicate for transactions with tag value
    pub fn tag_equals(tag: &str, value: &str) -> ExprResult<QueryPredicate> {
        let expr_str = format!("tag_{} == '{}'", tag, value.replace("'", "\\'"));
        QueryPredicate::parse(&expr_str)
    }
    
    /// Combine multiple predicates with AND
    pub fn and(predicates: Vec<QueryPredicate>) -> ExprResult<QueryPredicate> {
        if predicates.is_empty() {
            return QueryPredicate::parse("true");
        }
        
        if predicates.len() == 1 {
            return Ok(predicates.into_iter().next().unwrap());
        }
        
        // Combine expressions with AND
        let expr_strings: Vec<String> = predicates.iter()
            .map(|p| format!("({})", p.expression))
            .collect();
        let combined = expr_strings.join(" && ");
        QueryPredicate::parse(&combined)
    }
    
    /// Combine multiple predicates with OR
    pub fn or(predicates: Vec<QueryPredicate>) -> ExprResult<QueryPredicate> {
        if predicates.is_empty() {
            return QueryPredicate::parse("false");
        }
        
        if predicates.len() == 1 {
            return Ok(predicates.into_iter().next().unwrap());
        }
        
        // Combine expressions with OR
        let expr_strings: Vec<String> = predicates.iter()
            .map(|p| format!("({})", p.expression))
            .collect();
        let combined = expr_strings.join(" || ");
        QueryPredicate::parse(&combined)
    }
    
    /// Create a negated predicate
    pub fn not(predicate: QueryPredicate) -> ExprResult<QueryPredicate> {
        let expr_str = format!("!({})", predicate.expression);
        QueryPredicate::parse(&expr_str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::transaction::{Transaction, TransactionBuilder};
    use crate::posting::{Posting, PostingFlags};
    use crate::account::AccountTree;
    use ledger_math::{Amount, Decimal, commodity::Commodity};
    use chrono::NaiveDate;
    use std::sync::Arc;

    fn usd_commodity() -> Option<Arc<Commodity>> {
        Some(Arc::new(Commodity::new("USD")))
    }
    
    fn create_test_transaction() -> Transaction {
        let mut account_tree = AccountTree::new();
        let checking = account_tree.find_account("Assets:Checking", true).unwrap();
        let groceries = account_tree.find_account("Expenses:Groceries", true).unwrap();
        
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        
        TransactionBuilder::new(date, "Whole Foods Market".to_string())
            .code("DEBIT001")
            .tag("category", Some("groceries"))
            .tag("receipt", Some("12345"))
            .post_to(checking, Amount::with_commodity(Decimal::new(-4250, 2), usd_commodity()))
            .post_to(groceries, Amount::with_commodity(Decimal::new(4250, 2), usd_commodity()))
            .build()
            .unwrap()
    }

    #[test]
    fn test_simple_predicate() {
        let mut predicate = QueryPredicate::parse("true").unwrap();
        let transaction = create_test_transaction();
        
        assert!(predicate.matches_transaction(&transaction).unwrap());
        
        let mut false_predicate = QueryPredicate::parse("false").unwrap();
        assert!(!false_predicate.matches_transaction(&transaction).unwrap());
    }

    #[test]
    fn test_account_predicate() {
        let mut predicate = PredicateBuilder::account_matches("Checking").unwrap();
        let transaction = create_test_transaction();
        
        // Should match first posting (Assets:Checking)
        assert!(predicate.matches_posting(&transaction.postings[0], Some(&transaction)).unwrap());
        // Should not match second posting (Expenses:Groceries)
        assert!(!predicate.matches_posting(&transaction.postings[1], Some(&transaction)).unwrap());
    }

    #[test]
    fn test_payee_predicate() {
        let mut predicate = PredicateBuilder::payee_matches("Whole Foods").unwrap();
        let transaction = create_test_transaction();
        
        assert!(predicate.matches_transaction(&transaction).unwrap());
        
        let mut no_match = PredicateBuilder::payee_matches("Target").unwrap();
        assert!(!no_match.matches_transaction(&transaction).unwrap());
    }

    #[test]
    fn test_amount_predicate() {
        let mut greater_predicate = PredicateBuilder::amount_greater_than("40.00").unwrap();
        let mut less_predicate = PredicateBuilder::amount_less_than("50.00").unwrap();
        let transaction = create_test_transaction();
        
        // Second posting has amount 42.50
        assert!(greater_predicate.matches_posting(&transaction.postings[1], Some(&transaction)).unwrap());
        assert!(less_predicate.matches_posting(&transaction.postings[1], Some(&transaction)).unwrap());
    }

    #[test]
    fn test_tag_predicate() {
        let mut has_category = PredicateBuilder::has_tag("category").unwrap();
        let mut category_groceries = PredicateBuilder::tag_equals("category", "groceries").unwrap();
        let mut no_such_tag = PredicateBuilder::has_tag("nonexistent").unwrap();
        
        let transaction = create_test_transaction();
        
        assert!(has_category.matches_transaction(&transaction).unwrap());
        assert!(category_groceries.matches_transaction(&transaction).unwrap());
        assert!(!no_such_tag.matches_transaction(&transaction).unwrap());
    }

    #[test]
    fn test_date_predicate() {
        let start = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
        let end = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
        let mut date_predicate = PredicateBuilder::date_between(start, end).unwrap();
        
        let transaction = create_test_transaction();
        assert!(date_predicate.matches_transaction(&transaction).unwrap());
        
        let start_future = NaiveDate::from_ymd_opt(2024, 2, 1).unwrap();
        let end_future = NaiveDate::from_ymd_opt(2024, 2, 28).unwrap();
        let mut future_predicate = PredicateBuilder::date_between(start_future, end_future).unwrap();
        assert!(!future_predicate.matches_transaction(&transaction).unwrap());
    }

    #[test]
    fn test_compound_predicates() {
        let payee_pred = PredicateBuilder::payee_matches("Whole Foods").unwrap();
        let tag_pred = PredicateBuilder::has_tag("category").unwrap();
        let amount_pred = PredicateBuilder::amount_greater_than("40.00").unwrap();
        
        // Test AND combination
        let mut and_predicate = PredicateBuilder::and(vec![payee_pred.clone(), tag_pred.clone()]).unwrap();
        let transaction = create_test_transaction();
        assert!(and_predicate.matches_transaction(&transaction).unwrap());
        
        // Test OR combination  
        let mut or_predicate = PredicateBuilder::or(vec![
            PredicateBuilder::payee_matches("Target").unwrap(),
            tag_pred.clone()
        ]).unwrap();
        assert!(or_predicate.matches_transaction(&transaction).unwrap());
        
        // Test NOT
        let mut not_predicate = PredicateBuilder::not(
            PredicateBuilder::payee_matches("Target").unwrap()
        ).unwrap();
        assert!(not_predicate.matches_transaction(&transaction).unwrap());
    }

    #[test]
    fn test_filter_transactions() {
        let transactions = vec![
            create_test_transaction(),
            // Add more test transactions with different attributes
        ];
        
        let mut predicate = PredicateBuilder::payee_matches("Whole Foods").unwrap();
        let filtered = predicate.filter_transactions(&transactions).unwrap();
        
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].payee, "Whole Foods Market");
    }

    #[test]
    fn test_filter_postings() {
        let transaction = create_test_transaction();
        let mut predicate = PredicateBuilder::account_matches("Checking").unwrap();
        
        let filtered = predicate.filter_postings(&transaction).unwrap();
        assert_eq!(filtered.len(), 1);
        assert!(filtered[0].account.borrow().name().contains("Checking"));
    }

    #[test]
    fn test_optimization_hints() {
        let always_true = QueryPredicate::parse("true").unwrap();
        assert!(always_true.optimization_hints.always_true);
        assert!(always_true.optimization_hints.is_constant);
        
        let always_false = QueryPredicate::parse("false").unwrap();
        assert!(always_false.optimization_hints.always_false);
        assert!(always_false.optimization_hints.is_constant);
        
        let variable_ref = QueryPredicate::parse("account").unwrap();
        assert!(!variable_ref.optimization_hints.is_constant);
        assert!(variable_ref.optimization_hints.referenced_variables.contains(&"account".to_string()));
    }
}