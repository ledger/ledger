//! Hierarchical account structure with parent-child relationships
//!
//! This module provides the core Account struct that forms a tree structure
//! supporting hierarchical accounts with efficient lookups, aliasing, and metadata storage.

use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::rc::{Rc, Weak};
use compact_str::CompactString;
use serde_json::Value;
use regex::Regex;

use crate::strings::{AccountName, intern_string, AccountPathBuilder};

/// Account flags indicating various states
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AccountFlags {
    /// Normal account with no special flags
    Normal = 0x00,
    /// Account is known/recognized
    Known = 0x01,
    /// Account is a temporary object
    Temp = 0x02,
    /// Account was automatically generated
    Generated = 0x04,
    /// Virtual account (excluded from certain calculations)
    Virtual = 0x08,
}

/// Account type classification for ledger semantics
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AccountType {
    /// Asset account (normally debit balance)
    Asset,
    /// Liability account (normally credit balance)
    Liability,
    /// Income account (normally credit balance)
    Income,
    /// Expense account (normally debit balance)
    Expense,
    /// Equity account (normally credit balance)
    Equity,
    /// Unknown or unclassified account type
    Unknown,
}

/// Account directive for special behaviors
#[derive(Debug, Clone, PartialEq)]
pub enum AccountDirective {
    /// Account declaration with optional type
    Account(AccountName, Option<AccountType>),
    /// Alias declaration
    Alias(AccountName),
    /// Default payee for postings to this account
    Payee(AccountName),
    /// Default check number pattern
    Check(AccountName),
    /// Account assertion (balance assertion)
    Assert(AccountName),
    /// Account note/description
    Note(AccountName),
    /// Tag directive
    Tag(AccountName),
    /// Default commodity for this account
    Default(AccountName),
    /// Account format specification
    Format(AccountName),
    /// Evaluation/calculation directive
    Eval(AccountName),
}

/// Account reference type for shared ownership
pub type AccountRef = Rc<RefCell<Account>>;
/// Weak account reference to prevent reference cycles
pub type WeakAccountRef = Weak<RefCell<Account>>;

/// Core Account structure representing a node in the account hierarchy
#[derive(Debug)]
pub struct Account {
    /// Account name (without path) - optimized for memory efficiency
    pub name: AccountName,
    /// Parent account (weak reference to prevent cycles)
    pub parent: Option<WeakAccountRef>,
    /// Child accounts (strong references) - using AccountName keys for memory efficiency
    pub children: HashMap<AccountName, AccountRef>,
    /// Account depth in the hierarchy (0 for root accounts)
    pub depth: usize,
    /// Unique account identifier
    pub account_id: usize,
    /// Optional account note/description - optimized for memory
    pub note: Option<AccountName>,
    /// Account flags as bitfield
    pub flags: u8,
    /// Account type classification
    pub account_type: AccountType,
    /// Account directives
    pub directives: Vec<AccountDirective>,
    /// Metadata storage for flexible data association
    pub metadata: HashMap<String, Value>,
    /// Cached full name to avoid repeated string building - optimized
    cached_fullname: Option<AccountName>,
}

impl Account {
    /// Create a new account with the specified name and parent
    pub fn new(name: CompactString, parent: Option<WeakAccountRef>, account_id: usize) -> Self {
        let account_name = intern_string(&name);
        let depth = if let Some(parent_weak) = &parent {
            if let Some(parent_rc) = parent_weak.upgrade() {
                parent_rc.borrow().depth + 1
            } else {
                0 // Parent was dropped, treat as root
            }
        } else {
            0 // Root account
        };

        Self {
            name: account_name,
            parent,
            children: HashMap::new(),
            depth,
            account_id,
            note: None,
            flags: AccountFlags::Normal as u8,
            account_type: AccountType::Unknown,
            directives: Vec::new(),
            metadata: HashMap::new(),
            cached_fullname: None,
        }
    }

    /// Create a root account (no parent)
    pub fn new_root(name: CompactString, account_id: usize) -> Self {
        Self::new(name, None, account_id)
    }

    /// Create a new account with optimized string interning
    pub fn new_interned(name: AccountName, parent: Option<WeakAccountRef>, account_id: usize) -> Self {
        let depth = if let Some(parent_weak) = &parent {
            if let Some(parent_rc) = parent_weak.upgrade() {
                parent_rc.borrow().depth + 1
            } else {
                0 // Parent was dropped, treat as root
            }
        } else {
            0 // Root account
        };

        Self {
            name,
            parent,
            children: HashMap::new(),
            depth,
            account_id,
            note: None,
            flags: AccountFlags::Normal as u8,
            account_type: AccountType::Unknown,
            directives: Vec::new(),
            metadata: HashMap::new(),
            cached_fullname: None,
        }
    }

    /// Add a child account to this account
    pub fn add_child(&mut self, child: AccountRef) {
        let child_name = child.borrow().name.clone();
        self.children.insert(child_name, child);
        self.invalidate_cached_paths();
    }

    /// Remove a child account by name
    pub fn remove_child(&mut self, name: &str) -> Option<AccountRef> {
        let account_name = AccountName::new(name);
        let result = self.children.remove(&account_name);
        if result.is_some() {
            self.invalidate_cached_paths();
        }
        result
    }

    /// Get the full account name (e.g., "Assets:Bank:Checking")
    pub fn fullname(&mut self) -> AccountName {
        if let Some(ref cached) = self.cached_fullname {
            return cached.clone();
        }

        let mut path_builder = AccountPathBuilder::new();
        self.build_path_recursive(&mut path_builder);
        let fullname = path_builder.build();

        self.cached_fullname = Some(fullname.clone());
        fullname
    }

    /// Recursively build the account path
    fn build_path_recursive(&self, path_builder: &mut AccountPathBuilder) {
        if let Some(parent_weak) = &self.parent {
            if let Some(parent_rc) = parent_weak.upgrade() {
                parent_rc.borrow().build_path_recursive(path_builder);
            }
        }
        // Skip empty names (root account)
        if !self.name.is_empty() {
            path_builder.push_compact(self.name.clone());
        }
    }

    /// Get the partial name based on the specified depth
    pub fn partial_name(&mut self, flat: bool) -> AccountName {
        if flat {
            self.name.clone()
        } else {
            self.fullname()
        }
    }

    /// Alias for fullname() to match legacy API
    pub fn full_name(&mut self) -> AccountName {
        self.fullname()
    }

    /// Get the account's simple name (without path)
    pub fn name(&self) -> AccountName {
        self.name.clone()
    }

    /// Parse a colon-separated account path (e.g., "Assets:Bank:Checking") - optimized
    pub fn parse_account_path(path: &str) -> Vec<AccountName> {
        if path.is_empty() {
            return Vec::new();
        }
        
        use crate::strings::FastParser;
        FastParser::split_account_path(path)
            .into_iter()
            .map(|s| intern_string(s))
            .collect()
    }

    /// Check if this account has the specified flag
    pub fn has_flag(&self, flag: AccountFlags) -> bool {
        self.flags & (flag as u8) != 0
    }

    /// Add the specified flag to this account
    pub fn add_flag(&mut self, flag: AccountFlags) {
        self.flags |= flag as u8;
    }

    /// Remove the specified flag from this account
    pub fn remove_flag(&mut self, flag: AccountFlags) {
        self.flags &= !(flag as u8);
    }

    /// Set a metadata value for this account
    pub fn set_metadata<V: Into<Value>>(&mut self, key: String, value: V) {
        self.metadata.insert(key, value.into());
    }

    /// Get a metadata value for this account
    pub fn get_metadata(&self, key: &str) -> Option<&Value> {
        self.metadata.get(key)
    }

    /// Check if this account is a root account (has no parent)
    pub fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    /// Get the number of child accounts
    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    /// Check if this account is a leaf (has no children)
    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    /// Check if this account is virtual
    pub fn is_virtual(&self) -> bool {
        self.has_flag(AccountFlags::Virtual)
    }

    /// Set account type
    pub fn set_account_type(&mut self, account_type: AccountType) {
        self.account_type = account_type;
    }

    /// Get account type
    pub fn get_account_type(&self) -> AccountType {
        self.account_type
    }

    /// Add a directive to this account
    pub fn add_directive(&mut self, directive: AccountDirective) {
        self.directives.push(directive);
    }

    /// Get all directives for this account
    pub fn get_directives(&self) -> &[AccountDirective] {
        &self.directives
    }

    /// Get directives of a specific type
    pub fn get_directives_by_type(&self, directive_type: &str) -> Vec<&AccountDirective> {
        self.directives.iter()
            .filter(|directive| {
                match directive {
                    AccountDirective::Account(_, _) => directive_type == "account",
                    AccountDirective::Alias(_) => directive_type == "alias",
                    AccountDirective::Payee(_) => directive_type == "payee",
                    AccountDirective::Check(_) => directive_type == "check",
                    AccountDirective::Assert(_) => directive_type == "assert",
                    AccountDirective::Note(_) => directive_type == "note",
                    AccountDirective::Tag(_) => directive_type == "tag",
                    AccountDirective::Default(_) => directive_type == "default",
                    AccountDirective::Format(_) => directive_type == "format",
                    AccountDirective::Eval(_) => directive_type == "eval",
                }
            })
            .collect()
    }

    /// Depth-first traversal starting from this account
    pub fn depth_first_iter(&self) -> DepthFirstIterator {
        DepthFirstIterator::new(self)
    }

    /// Breadth-first traversal starting from this account
    pub fn breadth_first_iter(&self) -> BreadthFirstIterator {
        BreadthFirstIterator::new(self)
    }

    /// Check if account name matches regex pattern
    pub fn matches_pattern(&mut self, pattern: &Regex) -> bool {
        pattern.is_match(&self.fullname())
    }

    /// Get fullname without mutable borrow (for use in iterators)
    pub fn fullname_immutable(&self) -> String {
        if let Some(ref cached) = self.cached_fullname {
            return cached.to_string();
        }

        if let Some(parent_weak) = &self.parent {
            if let Some(parent_rc) = parent_weak.upgrade() {
                let parent_name = parent_rc.borrow().fullname_immutable();
                if parent_name.is_empty() {
                    self.name.to_string()
                } else {
                    format!("{}:{}", parent_name, self.name)
                }
            } else {
                self.name.to_string()
            }
        } else {
            self.name.to_string()
        }
    }

    /// Check if account matches predicate function
    pub fn matches_predicate<F>(&self, predicate: F) -> bool
    where
        F: Fn(&Account) -> bool,
    {
        predicate(self)
    }

    /// Apply visitor pattern to this account and optionally its children
    pub fn accept<V: AccountVisitor>(&mut self, visitor: &mut V, recursive: bool) {
        visitor.visit(self);
        if recursive {
            for child in self.children.values() {
                child.borrow_mut().accept(visitor, recursive);
            }
        }
    }

    /// Get default payee for this account (from directives)
    pub fn get_default_payee(&self) -> Option<String> {
        for directive in &self.directives {
            if let AccountDirective::Payee(payee) = directive {
                return Some(payee.to_string());
            }
        }
        None
    }

    /// Get default commodity for this account (from directives)
    pub fn get_default_commodity(&self) -> Option<String> {
        for directive in &self.directives {
            if let AccountDirective::Default(commodity) = directive {
                return Some(commodity.to_string());
            }
        }
        None
    }

    /// Get account assertions (from directives)
    pub fn get_assertions(&self) -> Vec<String> {
        self.directives.iter()
            .filter_map(|directive| {
                if let AccountDirective::Assert(assertion) = directive {
                    Some(assertion.to_string())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get account tags (from directives)
    pub fn get_tags(&self) -> Vec<String> {
        self.directives.iter()
            .filter_map(|directive| {
                if let AccountDirective::Tag(tag) = directive {
                    Some(tag.to_string())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Check if account has a specific tag
    pub fn has_tag(&self, tag: &str) -> bool {
        self.directives.iter().any(|directive| {
            if let AccountDirective::Tag(account_tag) = directive {
                account_tag.as_str() == tag
            } else {
                false
            }
        })
    }

    /// Get account format specification
    pub fn get_format(&self) -> Option<CompactString> {
        for directive in &self.directives {
            if let AccountDirective::Format(format) = directive {
                return Some(format.clone());
            }
        }
        None
    }

    /// Check if this is a temporary account (created for calculations)
    pub fn is_temp(&self) -> bool {
        self.has_flag(AccountFlags::Temp)
    }

    /// Check if this account was auto-generated
    pub fn is_generated(&self) -> bool {
        self.has_flag(AccountFlags::Generated)
    }

    /// Mark account as known (explicitly declared)
    pub fn mark_as_known(&mut self) {
        self.add_flag(AccountFlags::Known);
    }

    /// Check if account is explicitly known/declared
    pub fn is_known(&self) -> bool {
        self.has_flag(AccountFlags::Known)
    }

    /// Invalidate cached paths (called when hierarchy changes)
    fn invalidate_cached_paths(&mut self) {
        self.cached_fullname = None;
        // Also invalidate children's cached paths
        for child in self.children.values() {
            child.borrow_mut().invalidate_cached_paths();
        }
    }
}

impl AccountFlags {
    /// Convert from bits to AccountFlags
    pub fn from_bits(bits: u8) -> Self {
        // Return the highest priority flag found
        if bits & (AccountFlags::Virtual as u8) != 0 {
            AccountFlags::Virtual
        } else if bits & (AccountFlags::Generated as u8) != 0 {
            AccountFlags::Generated
        } else if bits & (AccountFlags::Temp as u8) != 0 {
            AccountFlags::Temp
        } else if bits & (AccountFlags::Known as u8) != 0 {
            AccountFlags::Known
        } else {
            AccountFlags::Normal
        }
    }
}

impl AccountType {
    /// Determine account type from account path/name
    pub fn from_path(path: &str) -> Self {
        let path_lower = path.to_lowercase();
        if path_lower.starts_with("asset") {
            AccountType::Asset
        } else if path_lower.starts_with("liab") {
            AccountType::Liability
        } else if path_lower.starts_with("income") || path_lower.starts_with("revenue") {
            AccountType::Income
        } else if path_lower.starts_with("expense") {
            AccountType::Expense
        } else if path_lower.starts_with("equity") {
            AccountType::Equity
        } else {
            AccountType::Unknown
        }
    }

    /// Get the normal balance side for this account type
    pub fn normal_balance_side(&self) -> &'static str {
        match self {
            AccountType::Asset | AccountType::Expense => "debit",
            AccountType::Liability | AccountType::Income | AccountType::Equity => "credit",
            AccountType::Unknown => "unknown",
        }
    }
}

/// Visitor pattern trait for account traversal and operations
pub trait AccountVisitor {
    /// Visit an account node
    fn visit(&mut self, account: &mut Account);
}

/// Depth-first iterator for account traversal
pub struct DepthFirstIterator {
    stack: Vec<(CompactString, AccountRef)>,
    visited: std::collections::HashSet<usize>,
}

impl DepthFirstIterator {
    fn new(root_account: &Account) -> Self {
        let mut stack = Vec::new();
        
        // Add children to stack in reverse order for correct traversal order
        let mut children: Vec<_> = root_account.children.iter()
            .map(|(name, account_ref)| (name.clone(), account_ref.clone()))
            .collect();
        children.reverse();
        stack.extend(children);
        
        Self {
            stack,
            visited: std::collections::HashSet::new(),
        }
    }
}

impl Iterator for DepthFirstIterator {
    type Item = AccountRef;
    
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((_, account_ref)) = self.stack.pop() {
            let account_id = account_ref.borrow().account_id;
            
            // Skip if already visited (prevent cycles)
            if self.visited.contains(&account_id) {
                continue;
            }
            
            self.visited.insert(account_id);
            
            // Add children to stack in reverse order
            let mut children: Vec<_> = account_ref.borrow().children.iter()
                .map(|(name, child_ref)| (name.clone(), child_ref.clone()))
                .collect();
            children.reverse();
            self.stack.extend(children);
            
            return Some(account_ref);
        }
        None
    }
}

/// Breadth-first iterator for account traversal
pub struct BreadthFirstIterator {
    queue: VecDeque<AccountRef>,
    visited: std::collections::HashSet<usize>,
}

impl BreadthFirstIterator {
    fn new(root_account: &Account) -> Self {
        let mut queue = VecDeque::new();
        
        // Add children to queue
        for child_ref in root_account.children.values() {
            queue.push_back(child_ref.clone());
        }
        
        Self {
            queue,
            visited: std::collections::HashSet::new(),
        }
    }
}

impl Iterator for BreadthFirstIterator {
    type Item = AccountRef;
    
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(account_ref) = self.queue.pop_front() {
            let account_id = account_ref.borrow().account_id;
            
            // Skip if already visited (prevent cycles)
            if self.visited.contains(&account_id) {
                continue;
            }
            
            self.visited.insert(account_id);
            
            // Add children to queue
            for child_ref in account_ref.borrow().children.values() {
                self.queue.push_back(child_ref.clone());
            }
            
            return Some(account_ref);
        }
        None
    }
}

impl fmt::Display for Account {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Note: We can't get the fullname here due to immutable borrow
        write!(f, "Account({})", self.name)
    }
}

impl PartialEq for Account {
    fn eq(&self, other: &Self) -> bool {
        self.account_id == other.account_id
    }
}

impl Eq for Account {}

/// AccountTree manages the hierarchical structure of all accounts
/// with efficient path-based indexing and lookup mechanisms
#[derive(Debug)]
pub struct AccountTree {
    /// Root account (typically unnamed or named "")
    root: AccountRef,
    /// Path-based index for fast lookups
    path_index: HashMap<CompactString, AccountRef>,
    /// ID-based index for lookups by account ID
    id_index: HashMap<usize, AccountRef>,
    /// Account ID counter for generating unique IDs
    next_account_id: usize,
    /// Alias registry mapping alias names to canonical account paths
    alias_registry: HashMap<CompactString, CompactString>,
}

impl AccountTree {
    /// Create a new AccountTree with an empty root account
    pub fn new() -> Self {
        let root = Rc::new(RefCell::new(Account::new_root(CompactString::from(""), 0)));
        let mut path_index = HashMap::new();
        let mut id_index = HashMap::new();
        
        // Index the root account
        path_index.insert(CompactString::from(""), root.clone());
        id_index.insert(0, root.clone());
        
        Self {
            root,
            path_index,
            id_index,
            next_account_id: 1,
            alias_registry: HashMap::new(),
        }
    }

    /// Find an account by path, optionally creating it if it doesn't exist
    pub fn find_account(&mut self, path: &str, auto_create: bool) -> Option<AccountRef> {
        // Check if we already have this account in our index
        let path_key = CompactString::from(path);
        if let Some(account_ref) = self.path_index.get(&path_key) {
            return Some(account_ref.clone());
        }

        if path.is_empty() {
            return Some(self.root.clone());
        }

        // Parse the path into components
        let components = Account::parse_account_path(path);
        if components.is_empty() {
            return Some(self.root.clone());
        }

        if !auto_create {
            return None;
        }

        // Build the account path step by step, creating accounts as needed
        let mut current_parent = self.root.clone();
        let mut built_path = CompactString::new("");

        for (i, component) in components.iter().enumerate() {
            if i > 0 {
                built_path.push(':');
            }
            built_path.push_str(component);

            // Check if this intermediate account already exists
            if let Some(existing) = self.path_index.get(&built_path) {
                current_parent = existing.clone();
                continue;
            }

            // Create new account
            let account_id = self.next_account_id;
            self.next_account_id += 1;

            let new_account = Rc::new(RefCell::new(Account::new(
                component.clone(),
                Some(Rc::downgrade(&current_parent)),
                account_id,
            )));

            // Add to parent's children
            current_parent.borrow_mut().add_child(new_account.clone());

            // Index the new account
            self.path_index.insert(built_path.clone(), new_account.clone());
            self.id_index.insert(account_id, new_account.clone());

            current_parent = new_account;
        }

        Some(current_parent)
    }

    /// Find an account by ID
    pub fn find_account_by_id(&self, id: usize) -> Option<AccountRef> {
        self.id_index.get(&id).cloned()
    }

    /// Get the root account
    pub fn root(&self) -> AccountRef {
        self.root.clone()
    }

    /// Get all accounts as a vector
    pub fn all_accounts(&self) -> Vec<AccountRef> {
        self.path_index.values().cloned().collect()
    }

    /// Get all account paths
    pub fn all_paths(&self) -> Vec<CompactString> {
        self.path_index.keys().cloned().collect()
    }

    /// Get the number of accounts in the tree
    pub fn account_count(&self) -> usize {
        self.path_index.len()
    }

    /// Remove an account from the tree (if it has no children)
    pub fn remove_account(&mut self, path: &str) -> Result<AccountRef, String> {
        // Don't allow removal of root account
        if path.is_empty() {
            return Err("Cannot remove root account".into());
        }

        let path_key = CompactString::from(path);
        let account_ref = self.path_index.get(&path_key)
            .ok_or_else(|| format!("Account '{}' not found", path))?
            .clone();

        // Check if account has children
        if !account_ref.borrow().is_leaf() {
            return Err(format!("Cannot remove account '{}' - it has children", path));
        }

        let account_id = account_ref.borrow().account_id;

        // Remove from parent's children
        if let Some(parent_weak) = &account_ref.borrow().parent {
            if let Some(parent_rc) = parent_weak.upgrade() {
                parent_rc.borrow_mut().remove_child(&account_ref.borrow().name);
            }
        }

        // Remove from indices
        self.path_index.remove(&path_key);
        self.id_index.remove(&account_id);

        Ok(account_ref)
    }

    /// Rebuild the path index (useful after manual account tree modifications)
    pub fn rebuild_index(&mut self) {
        self.path_index.clear();
        self.id_index.clear();
        self._index_account_recursive(&self.root.clone(), CompactString::new(""));
    }

    /// Recursively index an account and its children
    fn _index_account_recursive(&mut self, account_ref: &AccountRef, path: CompactString) {
        let account_id = account_ref.borrow().account_id;
        
        // Index this account
        self.path_index.insert(path.clone(), account_ref.clone());
        self.id_index.insert(account_id, account_ref.clone());

        // Collect children first to avoid borrowing issues
        let children: Vec<(CompactString, AccountRef)> = account_ref
            .borrow()
            .children
            .iter()
            .map(|(name, child_ref)| {
                let child_path = if path.is_empty() {
                    name.clone()
                } else {
                    CompactString::from(format!("{}:{}", path, name))
                };
                (child_path, child_ref.clone())
            })
            .collect();

        // Recursively index children
        for (child_path, child_ref) in children {
            self._index_account_recursive(&child_ref, child_path);
        }
    }

    /// Register an alias for an account path
    pub fn register_alias(&mut self, alias: CompactString, canonical_path: CompactString) -> Result<(), String> {
        // Validate that the canonical path exists or can be created
        if self.find_account(&canonical_path, false).is_none() {
            return Err(format!("Canonical path '{}' does not exist", canonical_path));
        }

        // Check for circular aliases
        if self.resolve_alias_chain(&alias).contains(&canonical_path) {
            return Err(format!("Circular alias detected: '{}' -> '{}'", alias, canonical_path));
        }

        // Check if alias already exists
        if self.alias_registry.contains_key(&alias) {
            return Err(format!("Alias '{}' already exists", alias));
        }

        // Register the alias
        self.alias_registry.insert(alias, canonical_path);
        Ok(())
    }

    /// Remove an alias
    pub fn remove_alias(&mut self, alias: &str) -> Result<CompactString, String> {
        self.alias_registry.remove(alias)
            .ok_or_else(|| format!("Alias '{}' not found", alias))
    }

    /// Resolve an alias to its canonical path
    pub fn resolve_alias(&self, alias: &str) -> Option<&CompactString> {
        self.alias_registry.get(alias)
    }

    /// Resolve an alias chain to prevent circular references
    fn resolve_alias_chain(&self, alias: &str) -> Vec<CompactString> {
        let mut chain = Vec::new();
        let mut current = CompactString::from(alias);
        
        while let Some(resolved) = self.alias_registry.get(&current) {
            if chain.contains(resolved) {
                break; // Circular reference detected
            }
            chain.push(current);
            current = resolved.clone();
        }
        chain.push(current);
        chain
    }

    /// Find an account by path or alias
    pub fn find_account_or_alias(&mut self, path_or_alias: &str, auto_create: bool) -> Option<AccountRef> {
        // First try direct path lookup
        if let Some(account) = self.find_account(path_or_alias, false) {
            return Some(account);
        }

        // Try alias resolution
        if let Some(canonical_path) = self.resolve_alias(path_or_alias).cloned() {
            return self.find_account(&canonical_path, auto_create);
        }

        // If auto_create is true and no alias exists, create account with the given path
        if auto_create {
            return self.find_account(path_or_alias, true);
        }

        None
    }

    /// Get all aliases
    pub fn all_aliases(&self) -> Vec<(CompactString, CompactString)> {
        self.alias_registry.iter()
            .map(|(alias, path)| (alias.clone(), path.clone()))
            .collect()
    }

    /// Check if a string is an alias
    pub fn is_alias(&self, name: &str) -> bool {
        self.alias_registry.contains_key(name)
    }

    /// Get the number of registered aliases
    pub fn alias_count(&self) -> usize {
        self.alias_registry.len()
    }

    /// Find accounts matching a regex pattern
    pub fn find_accounts_by_pattern(&mut self, pattern: &Regex) -> Vec<AccountRef> {
        self.all_accounts()
            .into_iter()
            .filter(|account_ref| {
                account_ref.borrow_mut().matches_pattern(pattern)
            })
            .collect()
    }

    /// Find accounts matching a predicate function
    pub fn find_accounts_by_predicate<F>(&self, predicate: F) -> Vec<AccountRef>
    where
        F: Fn(&Account) -> bool,
    {
        self.all_accounts()
            .into_iter()
            .filter(|account_ref| {
                account_ref.borrow().matches_predicate(&predicate)
            })
            .collect()
    }

    /// Filter accounts by type
    pub fn find_accounts_by_type(&self, account_type: AccountType) -> Vec<AccountRef> {
        self.find_accounts_by_predicate(|account| account.account_type == account_type)
    }

    /// Filter accounts by flags
    pub fn find_accounts_by_flag(&self, flag: AccountFlags) -> Vec<AccountRef> {
        self.find_accounts_by_predicate(|account| account.has_flag(flag))
    }

    /// Filter accounts by depth range
    pub fn find_accounts_by_depth(&self, min_depth: usize, max_depth: usize) -> Vec<AccountRef> {
        self.find_accounts_by_predicate(|account| {
            account.depth >= min_depth && account.depth <= max_depth
        })
    }

    /// Filter accounts by metadata key existence
    pub fn find_accounts_by_metadata_key(&self, key: &str) -> Vec<AccountRef> {
        let key = key.to_string();
        self.find_accounts_by_predicate(move |account| {
            account.metadata.contains_key(&key)
        })
    }

    /// Find virtual accounts
    pub fn find_virtual_accounts(&self) -> Vec<AccountRef> {
        self.find_accounts_by_flag(AccountFlags::Virtual)
    }

    /// Find leaf accounts (accounts with no children)
    pub fn find_leaf_accounts(&self) -> Vec<AccountRef> {
        self.find_accounts_by_predicate(|account| account.is_leaf())
    }

    /// Find root accounts (accounts with no parent, excluding the tree root)
    pub fn find_root_accounts(&self) -> Vec<AccountRef> {
        self.find_accounts_by_predicate(|account| {
            account.depth == 1 && !account.name.is_empty()
        })
    }

    /// Create a temporary/virtual account
    pub fn create_virtual_account(&mut self, path: &str, account_type: Option<AccountType>) -> AccountRef {
        let account_ref = self.find_account(path, true).unwrap();
        {
            let mut account = account_ref.borrow_mut();
            account.add_flag(AccountFlags::Virtual);
            account.add_flag(AccountFlags::Temp);
            if let Some(account_type) = account_type {
                account.set_account_type(account_type);
            } else {
                account.set_account_type(AccountType::from_path(path));
            }
        }
        account_ref
    }

    /// Apply account directives from parsing
    pub fn apply_directive(&mut self, path: &str, directive: AccountDirective) -> Result<(), String> {
        match directive {
            AccountDirective::Account(account_path, account_type) => {
                let account_ref = self.find_account(&account_path, true)
                    .ok_or_else(|| format!("Failed to create account: {}", account_path))?;
                
                let mut account = account_ref.borrow_mut();
                if let Some(account_type) = account_type {
                    account.set_account_type(account_type);
                } else {
                    account.set_account_type(AccountType::from_path(&account_path));
                }
                account.add_directive(AccountDirective::Account(account_path, account_type));
                account.mark_as_known();
                Ok(())
            },
            AccountDirective::Alias(alias_name) => {
                self.register_alias(alias_name.clone(), CompactString::from(path))
                    .map_err(|e| format!("Failed to register alias '{}': {}", alias_name, e))?;
                
                if let Some(account_ref) = self.find_account(path, false) {
                    account_ref.borrow_mut().add_directive(AccountDirective::Alias(alias_name));
                }
                Ok(())
            },
            AccountDirective::Note(note_text) => {
                let account_ref = self.find_account(path, false)
                    .ok_or_else(|| format!("Account '{}' not found for note directive", path))?;
                
                let mut account = account_ref.borrow_mut();
                account.note = Some(note_text.clone());
                account.add_directive(AccountDirective::Note(note_text));
                Ok(())
            },
            _ => {
                // For other directives, ensure account exists and add the directive
                let account_ref = self.find_account(path, false)
                    .ok_or_else(|| format!("Account '{}' not found for directive", path))?;
                
                account_ref.borrow_mut().add_directive(directive);
                Ok(())
            }
        }
    }

    /// Parse and apply account directive from a directive line
    pub fn parse_and_apply_directive(&mut self, directive_line: &str) -> Result<(), String> {
        let directive_line = directive_line.trim();
        if directive_line.is_empty() || directive_line.starts_with(';') || directive_line.starts_with('#') {
            return Ok(()); // Skip empty lines and comments
        }

        let parts: Vec<&str> = directive_line.splitn(3, char::is_whitespace).collect();
        if parts.is_empty() {
            return Ok(());
        }

        let directive_type = parts[0].to_lowercase();
        match directive_type.as_str() {
            "account" => {
                if parts.len() < 2 {
                    return Err("Account directive requires account path".into());
                }
                let account_path = parts[1];
                let account_type = if parts.len() > 2 {
                    match parts[2].to_lowercase().as_str() {
                        "asset" | "assets" => Some(AccountType::Asset),
                        "liability" | "liabilities" => Some(AccountType::Liability),
                        "income" | "revenue" => Some(AccountType::Income),
                        "expense" | "expenses" => Some(AccountType::Expense),
                        "equity" => Some(AccountType::Equity),
                        _ => None,
                    }
                } else {
                    None
                };
                let directive = AccountDirective::Account(CompactString::from(account_path), account_type);
                self.apply_directive(account_path, directive)
            },
            "alias" => {
                if parts.len() < 3 {
                    return Err("Alias directive requires alias name and account path".into());
                }
                let alias_name = parts[1];
                let account_path = parts[2];
                let directive = AccountDirective::Alias(CompactString::from(alias_name));
                self.apply_directive(account_path, directive)
            },
            "payee" => {
                if parts.len() < 3 {
                    return Err("Payee directive requires account path and payee name".into());
                }
                let account_path = parts[1];
                let payee_name = parts[2..].join(" ");
                let directive = AccountDirective::Payee(CompactString::from(payee_name));
                self.apply_directive(account_path, directive)
            },
            "note" => {
                if parts.len() < 3 {
                    return Err("Note directive requires account path and note text".into());
                }
                let account_path = parts[1];
                let note_text = parts[2..].join(" ");
                let directive = AccountDirective::Note(CompactString::from(note_text));
                self.apply_directive(account_path, directive)
            },
            "tag" => {
                if parts.len() < 3 {
                    return Err("Tag directive requires account path and tag name".into());
                }
                let account_path = parts[1];
                let tag_name = parts[2];
                let directive = AccountDirective::Tag(CompactString::from(tag_name));
                self.apply_directive(account_path, directive)
            },
            "assert" => {
                if parts.len() < 3 {
                    return Err("Assert directive requires account path and assertion".into());
                }
                let account_path = parts[1];
                let assertion = parts[2..].join(" ");
                let directive = AccountDirective::Assert(CompactString::from(assertion));
                self.apply_directive(account_path, directive)
            },
            "default" => {
                if parts.len() < 3 {
                    return Err("Default directive requires account path and commodity".into());
                }
                let account_path = parts[1];
                let commodity = parts[2];
                let directive = AccountDirective::Default(CompactString::from(commodity));
                self.apply_directive(account_path, directive)
            },
            "format" => {
                if parts.len() < 3 {
                    return Err("Format directive requires account path and format spec".into());
                }
                let account_path = parts[1];
                let format_spec = parts[2..].join(" ");
                let directive = AccountDirective::Format(CompactString::from(format_spec));
                self.apply_directive(account_path, directive)
            },
            _ => {
                Err(format!("Unknown directive type: {}", directive_type))
            }
        }
    }

    /// Create temporary account for calculations
    pub fn create_temp_account(&mut self, path: &str, account_type: Option<AccountType>) -> AccountRef {
        let account_ref = self.find_account(path, true).unwrap();
        {
            let mut account = account_ref.borrow_mut();
            account.add_flag(AccountFlags::Temp);
            account.add_flag(AccountFlags::Generated);
            if let Some(account_type) = account_type {
                account.set_account_type(account_type);
            } else {
                account.set_account_type(AccountType::from_path(path));
            }
        }
        account_ref
    }

    /// Find accounts that match all provided tags
    pub fn find_accounts_by_tags(&self, tags: &[String]) -> Vec<AccountRef> {
        self.find_accounts_by_predicate(|account| {
            let account_tags = account.get_tags();
            tags.iter().all(|tag| account_tags.contains(tag))
        })
    }

    /// Find accounts with assertions
    pub fn find_accounts_with_assertions(&self) -> Vec<AccountRef> {
        self.find_accounts_by_predicate(|account| {
            !account.get_assertions().is_empty()
        })
    }

    /// Get all account directives grouped by type
    pub fn get_all_directives(&self) -> std::collections::HashMap<String, Vec<(String, AccountDirective)>> {
        let mut directives_map = std::collections::HashMap::new();
        
        for account_ref in self.all_accounts() {
            let account = account_ref.borrow();
            let account_path = account.fullname_immutable();
            
            for directive in account.get_directives() {
                let directive_type = match directive {
                    AccountDirective::Account(_, _) => "account",
                    AccountDirective::Alias(_) => "alias",
                    AccountDirective::Payee(_) => "payee",
                    AccountDirective::Check(_) => "check",
                    AccountDirective::Assert(_) => "assert",
                    AccountDirective::Note(_) => "note",
                    AccountDirective::Tag(_) => "tag",
                    AccountDirective::Default(_) => "default",
                    AccountDirective::Format(_) => "format",
                    AccountDirective::Eval(_) => "eval",
                };
                
                directives_map.entry(directive_type.into())
                    .or_insert_with(Vec::new)
                    .push((account_path.clone(), directive.clone()));
            }
        }
        
        directives_map
    }

    /// Depth-first traversal of the entire tree
    pub fn depth_first_iter(&self) -> DepthFirstIterator {
        self.root.borrow().depth_first_iter()
    }

    /// Breadth-first traversal of the entire tree
    pub fn breadth_first_iter(&self) -> BreadthFirstIterator {
        self.root.borrow().breadth_first_iter()
    }

    /// Apply visitor to all accounts in the tree
    pub fn accept_all<V: AccountVisitor>(&mut self, visitor: &mut V) {
        self.root.borrow_mut().accept(visitor, true);
    }
}

impl Default for AccountTree {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_account_creation() {
        let account = Account::new_root("Assets".into(), 1);
        assert_eq!(account.name, "Assets");
        assert_eq!(account.depth, 0);
        assert_eq!(account.account_id, 1);
        assert!(account.is_root());
        assert!(account.is_leaf());
    }

    #[test]
    fn test_account_path_parsing() {
        let path = "Assets:Bank:Checking";
        let components = Account::parse_account_path(path);
        assert_eq!(components, vec!["Assets", "Bank", "Checking"]);
    }

    #[test]
    fn test_empty_path_parsing() {
        let components = Account::parse_account_path("");
        assert!(components.is_empty());
    }

    #[test]
    fn test_account_flags() {
        let mut account = Account::new_root("Test".into(), 1);
        assert!(!account.has_flag(AccountFlags::Known));
        
        account.add_flag(AccountFlags::Known);
        assert!(account.has_flag(AccountFlags::Known));
        
        account.remove_flag(AccountFlags::Known);
        assert!(!account.has_flag(AccountFlags::Known));
    }

    #[test]
    fn test_account_metadata() {
        let mut account = Account::new_root("Test".into(), 1);
        
        account.set_metadata("description".to_string(), "Test account");
        assert_eq!(
            account.get_metadata("description"),
            Some(&Value::String("Test account".into()))
        );
    }

    #[test]
    fn test_parent_child_relationships() {
        let assets_rc = Rc::new(RefCell::new(Account::new_root("Assets".into(), 1)));
        let bank_rc = Rc::new(RefCell::new(Account::new(
            "Bank".into(),
            Some(Rc::downgrade(&assets_rc)),
            2
        )));
        let checking_rc = Rc::new(RefCell::new(Account::new(
            "Checking".into(),
            Some(Rc::downgrade(&bank_rc)),
            3
        )));

        // Test hierarchy depths
        assert_eq!(assets_rc.borrow().depth, 0);
        assert_eq!(bank_rc.borrow().depth, 1);
        assert_eq!(checking_rc.borrow().depth, 2);

        // Add children to parents
        assets_rc.borrow_mut().add_child(bank_rc.clone());
        bank_rc.borrow_mut().add_child(checking_rc.clone());

        // Test relationship methods
        assert!(assets_rc.borrow().is_root());
        assert!(!bank_rc.borrow().is_root());
        assert!(!checking_rc.borrow().is_root());

        assert!(!assets_rc.borrow().is_leaf());
        assert!(!bank_rc.borrow().is_leaf());
        assert!(checking_rc.borrow().is_leaf());

        // Test child count
        assert_eq!(assets_rc.borrow().child_count(), 1);
        assert_eq!(bank_rc.borrow().child_count(), 1);
        assert_eq!(checking_rc.borrow().child_count(), 0);

        // Test fullname
        assert_eq!(assets_rc.borrow_mut().fullname(), "Assets");
        assert_eq!(bank_rc.borrow_mut().fullname(), "Assets:Bank");
        assert_eq!(checking_rc.borrow_mut().fullname(), "Assets:Bank:Checking");
    }

    #[test]
    fn test_account_tree_creation() {
        let tree = AccountTree::new();
        assert_eq!(tree.account_count(), 1); // Root account
        assert_eq!(tree.root().borrow().name, "");
        assert_eq!(tree.root().borrow().depth, 0);
    }

    #[test]
    fn test_account_tree_find_and_create() {
        let mut tree = AccountTree::new();

        // Create a hierarchical account path
        let checking = tree.find_account("Assets:Bank:Checking", true).unwrap();
        assert_eq!(checking.borrow_mut().fullname(), "Assets:Bank:Checking");
        assert_eq!(checking.borrow().depth, 3);

        // Verify intermediate accounts were created
        let assets = tree.find_account("Assets", false).unwrap();
        assert_eq!(assets.borrow_mut().fullname(), "Assets");
        assert_eq!(assets.borrow().depth, 1);

        let bank = tree.find_account("Assets:Bank", false).unwrap();
        assert_eq!(bank.borrow_mut().fullname(), "Assets:Bank");
        assert_eq!(bank.borrow().depth, 2);

        // Should have root + 3 accounts
        assert_eq!(tree.account_count(), 4);
    }

    #[test]
    fn test_account_tree_no_autocreate() {
        let mut tree = AccountTree::new();

        // Try to find non-existent account without auto-create
        let result = tree.find_account("NonExistent", false);
        assert!(result.is_none());
    }

    #[test]
    fn test_account_tree_id_lookup() {
        let mut tree = AccountTree::new();

        let checking = tree.find_account("Assets:Bank:Checking", true).unwrap();
        let checking_id = checking.borrow().account_id;

        let found_by_id = tree.find_account_by_id(checking_id).unwrap();
        assert_eq!(found_by_id.borrow().account_id, checking_id);
        assert_eq!(found_by_id.borrow_mut().fullname(), "Assets:Bank:Checking");
    }

    #[test]
    fn test_account_tree_removal() {
        let mut tree = AccountTree::new();

        // Create accounts
        tree.find_account("Assets:Bank:Checking", true);
        tree.find_account("Assets:Bank:Savings", true);
        let initial_count = tree.account_count();

        // Remove leaf account
        let removed = tree.remove_account("Assets:Bank:Checking").unwrap();
        assert_eq!(removed.borrow_mut().fullname(), "Assets:Bank:Checking");
        assert_eq!(tree.account_count(), initial_count - 1);

        // Verify it's no longer findable
        assert!(tree.find_account("Assets:Bank:Checking", false).is_none());

        // Try to remove account with children - should fail
        let result = tree.remove_account("Assets:Bank");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("has children"));

        // Try to remove root - should fail
        let result = tree.remove_account("");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Cannot remove root"));
    }

    #[test]
    fn test_account_tree_all_accounts_and_paths() {
        let mut tree = AccountTree::new();

        tree.find_account("Assets:Bank:Checking", true);
        tree.find_account("Expenses:Food", true);
        tree.find_account("Income:Salary", true);

        let all_accounts = tree.all_accounts();
        assert_eq!(all_accounts.len(), 8); // root + 7 accounts created (Assets, Assets:Bank, Assets:Bank:Checking, Expenses, Expenses:Food, Income, Income:Salary)

        let all_paths = tree.all_paths();
        assert!(all_paths.contains(&"Assets".into()));
        assert!(all_paths.contains(&"Assets:Bank".into()));
        assert!(all_paths.contains(&"Assets:Bank:Checking".into()));
        assert!(all_paths.contains(&"Expenses".into()));
        assert!(all_paths.contains(&"Expenses:Food".into()));
        assert!(all_paths.contains(&"Income".into()));
        assert!(all_paths.contains(&"Income:Salary".into()));
    }

    #[test]
    fn test_account_tree_rebuild_index() {
        let mut tree = AccountTree::new();

        // Create some accounts normally
        tree.find_account("Assets:Bank:Checking", true);
        let initial_count = tree.account_count();

        // Simulate manual modification (this is more for testing the rebuild functionality)
        tree.rebuild_index();
        assert_eq!(tree.account_count(), initial_count);

        // Verify accounts are still findable after rebuild
        let checking = tree.find_account("Assets:Bank:Checking", false);
        assert!(checking.is_some());
        assert_eq!(checking.unwrap().borrow_mut().fullname(), "Assets:Bank:Checking");
    }

    #[test]
    fn test_account_alias_registration() {
        let mut tree = AccountTree::new();

        // Create accounts first
        tree.find_account("Assets:Bank:Checking", true);
        tree.find_account("Expenses:Food", true);

        // Register aliases
        assert!(tree.register_alias("Checking".into(), "Assets:Bank:Checking".into()).is_ok());
        assert!(tree.register_alias("Food".into(), "Expenses:Food".into()).is_ok());

        // Verify aliases exist
        assert!(tree.is_alias("Checking"));
        assert!(tree.is_alias("Food"));
        assert!(!tree.is_alias("NonExistent"));

        // Test alias resolution
        assert_eq!(tree.resolve_alias("Checking"), Some(&CompactString::from("Assets:Bank:Checking")));
        assert_eq!(tree.resolve_alias("Food"), Some(&CompactString::from("Expenses:Food")));
        assert_eq!(tree.resolve_alias("NonExistent"), None);

        // Test alias count
        assert_eq!(tree.alias_count(), 2);
    }

    #[test]
    fn test_account_alias_errors() {
        let mut tree = AccountTree::new();

        // Try to create alias for non-existent account
        let result = tree.register_alias("Bad".into(), "NonExistent".into());
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("does not exist"));

        // Create account and alias
        tree.find_account("Assets:Bank:Checking", true);
        tree.find_account("Assets:Bank:Savings", true);
        assert!(tree.register_alias("Checking".into(), "Assets:Bank:Checking".into()).is_ok());

        // Try to register same alias again
        let result = tree.register_alias("Checking".into(), "Assets:Bank:Savings".into());
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("already exists"));
    }

    #[test]
    fn test_account_alias_removal() {
        let mut tree = AccountTree::new();

        // Create account and alias
        tree.find_account("Assets:Bank:Checking", true);
        tree.register_alias("Checking".into(), "Assets:Bank:Checking".into()).unwrap();

        assert_eq!(tree.alias_count(), 1);

        // Remove alias
        let removed = tree.remove_alias("Checking").unwrap();
        assert_eq!(removed, "Assets:Bank:Checking");
        assert_eq!(tree.alias_count(), 0);

        // Try to remove non-existent alias
        let result = tree.remove_alias("NonExistent");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not found"));
    }

    #[test]
    fn test_account_find_by_alias() {
        let mut tree = AccountTree::new();

        // Create accounts and aliases
        let checking = tree.find_account("Assets:Bank:Checking", true).unwrap();
        tree.register_alias("Checking".into(), "Assets:Bank:Checking".into()).unwrap();

        // Find by alias
        let found = tree.find_account_or_alias("Checking", false).unwrap();
        assert_eq!(found.borrow().account_id, checking.borrow().account_id);
        assert_eq!(found.borrow_mut().fullname(), "Assets:Bank:Checking");

        // Find by actual path should still work
        let found_direct = tree.find_account_or_alias("Assets:Bank:Checking", false).unwrap();
        assert_eq!(found_direct.borrow().account_id, checking.borrow().account_id);
    }

    #[test]
    fn test_account_all_aliases() {
        let mut tree = AccountTree::new();

        // Create accounts and aliases
        tree.find_account("Assets:Bank:Checking", true);
        tree.find_account("Expenses:Food", true);
        
        tree.register_alias("Checking".into(), "Assets:Bank:Checking".into()).unwrap();
        tree.register_alias("Food".into(), "Expenses:Food".into()).unwrap();

        let aliases = tree.all_aliases();
        assert_eq!(aliases.len(), 2);
        
        let alias_map: HashMap<String, String> = aliases.into_iter()
            .map(|(k, v)| (k.to_string(), v.into()))
            .collect();
        assert_eq!(alias_map.get("Checking"), Some(&"Assets:Bank:Checking".into()));
        assert_eq!(alias_map.get("Food"), Some(&"Expenses:Food".into()));
    }

    #[test] 
    fn test_account_metadata_comprehensive() {
        let mut account = Account::new_root("Test".into(), 1);
        
        // Test different metadata types
        account.set_metadata("description".to_string(), "Test account description");
        account.set_metadata("code".to_string(), "001");
        account.set_metadata("priority".to_string(), 5);
        account.set_metadata("active".to_string(), true);
        
        // Test retrieval
        assert_eq!(
            account.get_metadata("description"),
            Some(&Value::String("Test account description".into()))
        );
        assert_eq!(
            account.get_metadata("code"),
            Some(&Value::String("001".into()))
        );
        assert_eq!(
            account.get_metadata("priority"),
            Some(&Value::Number(5.into()))
        );
        assert_eq!(
            account.get_metadata("active"),
            Some(&Value::Bool(true))
        );
        
        // Test non-existent metadata
        assert_eq!(account.get_metadata("nonexistent"), None);
        
        // Test metadata count
        assert_eq!(account.metadata.len(), 4);
    }

    #[test]
    fn test_account_type_classification() {
        // Test from_path classification
        assert_eq!(AccountType::from_path("Assets:Bank"), AccountType::Asset);
        assert_eq!(AccountType::from_path("Liabilities:CreditCard"), AccountType::Liability);
        assert_eq!(AccountType::from_path("Income:Salary"), AccountType::Income);
        assert_eq!(AccountType::from_path("Revenue:Sales"), AccountType::Income);
        assert_eq!(AccountType::from_path("Expenses:Food"), AccountType::Expense);
        assert_eq!(AccountType::from_path("Equity:OpeningBalance"), AccountType::Equity);
        assert_eq!(AccountType::from_path("Unknown:Account"), AccountType::Unknown);
        
        // Test normal balance sides
        assert_eq!(AccountType::Asset.normal_balance_side(), "debit");
        assert_eq!(AccountType::Expense.normal_balance_side(), "debit");
        assert_eq!(AccountType::Liability.normal_balance_side(), "credit");
        assert_eq!(AccountType::Income.normal_balance_side(), "credit");
        assert_eq!(AccountType::Equity.normal_balance_side(), "credit");
        assert_eq!(AccountType::Unknown.normal_balance_side(), "unknown");
    }

    #[test]
    fn test_account_virtual_flag() {
        let mut account = Account::new_root("Test".into(), 1);
        
        assert!(!account.is_virtual());
        
        account.add_flag(AccountFlags::Virtual);
        assert!(account.is_virtual());
        assert!(account.has_flag(AccountFlags::Virtual));
        
        account.remove_flag(AccountFlags::Virtual);
        assert!(!account.is_virtual());
    }

    #[test]
    fn test_account_directives() {
        let mut account = Account::new_root("Test".into(), 1);
        
        // Add various directives
        account.add_directive(AccountDirective::Payee("Default Payee".into()));
        account.add_directive(AccountDirective::Note("Account notes".into()));
        account.add_directive(AccountDirective::Tag("important".into()));
        
        let directives = account.get_directives();
        assert_eq!(directives.len(), 3);
        
        // Test filtering by type
        let payee_directives = account.get_directives_by_type("payee");
        assert_eq!(payee_directives.len(), 1);
        match payee_directives[0] {
            AccountDirective::Payee(ref payee) => assert_eq!(payee, &CompactString::from("Default Payee")),
            _ => panic!("Expected Payee directive"),
        }
        
        let note_directives = account.get_directives_by_type("note");
        assert_eq!(note_directives.len(), 1);
        
        let tag_directives = account.get_directives_by_type("tag");
        assert_eq!(tag_directives.len(), 1);
        
        // Test non-existent directive type
        let check_directives = account.get_directives_by_type("check");
        assert_eq!(check_directives.len(), 0);
    }

    #[test]
    fn test_account_type_setting() {
        let mut account = Account::new_root("Assets".into(), 1);
        
        assert_eq!(account.get_account_type(), AccountType::Unknown);
        
        account.set_account_type(AccountType::Asset);
        assert_eq!(account.get_account_type(), AccountType::Asset);
    }

    #[test]
    fn test_depth_first_traversal() {
        let mut tree = AccountTree::new();
        
        // Create a tree structure
        tree.find_account("A", true);
        tree.find_account("A:B", true);
        tree.find_account("A:C", true);
        tree.find_account("A:B:D", true);
        tree.find_account("A:B:E", true);
        tree.find_account("A:C:F", true);
        
        let root_a = tree.find_account("A", false).unwrap();
        let mut visited_paths = Vec::new();
        
        for account_ref in root_a.borrow().depth_first_iter() {
            visited_paths.push(account_ref.borrow().fullname_immutable());
        }
        
        // Should visit in depth-first order
        // Note: exact order depends on HashMap iteration, but should visit children before siblings
        assert!(visited_paths.contains(&"A:B".into()));
        assert!(visited_paths.contains(&"A:C".into()));
        assert!(visited_paths.contains(&"A:B:D".into()));
        assert!(visited_paths.contains(&"A:B:E".into()));
        assert!(visited_paths.contains(&"A:C:F".into()));
        assert_eq!(visited_paths.len(), 5);
    }

    #[test]
    fn test_breadth_first_traversal() {
        let mut tree = AccountTree::new();
        
        // Create a tree structure
        tree.find_account("A", true);
        tree.find_account("A:B", true);
        tree.find_account("A:C", true);
        tree.find_account("A:B:D", true);
        tree.find_account("A:B:E", true);
        tree.find_account("A:C:F", true);
        
        let root_a = tree.find_account("A", false).unwrap();
        let mut visited_paths = Vec::new();
        
        for account_ref in root_a.borrow().breadth_first_iter() {
            visited_paths.push(account_ref.borrow().fullname_immutable());
        }
        
        // Should visit in breadth-first order
        assert!(visited_paths.contains(&"A:B".into()));
        assert!(visited_paths.contains(&"A:C".into()));
        assert!(visited_paths.contains(&"A:B:D".into()));
        assert!(visited_paths.contains(&"A:B:E".into()));
        assert!(visited_paths.contains(&"A:C:F".into()));
        assert_eq!(visited_paths.len(), 5);
    }

    #[test]
    fn test_tree_traversal_iterators() {
        let mut tree = AccountTree::new();
        
        // Create accounts
        tree.find_account("Assets:Bank:Checking", true);
        tree.find_account("Assets:Bank:Savings", true);
        tree.find_account("Expenses:Food", true);
        tree.find_account("Income:Salary", true);
        
        // Test tree-level depth-first traversal
        let mut df_visited = Vec::new();
        for account_ref in tree.depth_first_iter() {
            df_visited.push(account_ref.borrow().fullname_immutable());
        }
        
        assert!(df_visited.len() > 0);
        assert!(df_visited.iter().any(|path| path.contains("Assets:Bank:Checking")));
        
        // Test tree-level breadth-first traversal
        let mut bf_visited = Vec::new();
        for account_ref in tree.breadth_first_iter() {
            bf_visited.push(account_ref.borrow().fullname_immutable());
        }
        
        assert!(bf_visited.len() > 0);
        assert!(bf_visited.iter().any(|path| path.contains("Income:Salary")));
    }

    #[test]
    fn test_regex_pattern_matching() {
        let mut tree = AccountTree::new();
        
        // Create various accounts
        tree.find_account("Assets:Bank:Checking", true);
        tree.find_account("Assets:Bank:Savings", true);
        tree.find_account("Assets:Investments", true);
        tree.find_account("Expenses:Food:Restaurant", true);
        tree.find_account("Expenses:Transportation", true);
        tree.find_account("Income:Salary", true);
        
        // Test pattern matching
        let bank_pattern = Regex::new(r".*Bank.*").unwrap();
        let bank_accounts = tree.find_accounts_by_pattern(&bank_pattern);
        assert_eq!(bank_accounts.len(), 3); // Assets:Bank, Assets:Bank:Checking, Assets:Bank:Savings
        
        let expense_pattern = Regex::new(r"^Expenses:.*").unwrap();
        let expense_accounts = tree.find_accounts_by_pattern(&expense_pattern);
        assert_eq!(expense_accounts.len(), 3); // Expenses, Expenses:Food:Restaurant, Expenses:Transportation
    }

    #[test]
    fn test_account_filtering_by_type() {
        let mut tree = AccountTree::new();
        
        // Create accounts with different types
        let assets = tree.find_account("Assets:Bank", true).unwrap();
        assets.borrow_mut().set_account_type(AccountType::Asset);
        
        let expenses = tree.find_account("Expenses:Food", true).unwrap();
        expenses.borrow_mut().set_account_type(AccountType::Expense);
        
        let income = tree.find_account("Income:Salary", true).unwrap();
        income.borrow_mut().set_account_type(AccountType::Income);
        
        // Test filtering by type
        let asset_accounts = tree.find_accounts_by_type(AccountType::Asset);
        assert_eq!(asset_accounts.len(), 1);
        
        let expense_accounts = tree.find_accounts_by_type(AccountType::Expense);
        assert_eq!(expense_accounts.len(), 1);
        
        let income_accounts = tree.find_accounts_by_type(AccountType::Income);
        assert_eq!(income_accounts.len(), 1);
        
        let unknown_accounts = tree.find_accounts_by_type(AccountType::Unknown);
        assert!(unknown_accounts.len() > 0); // Should include intermediate accounts and root
    }

    #[test]
    fn test_account_filtering_by_flags() {
        let mut tree = AccountTree::new();
        
        // Create accounts with different flags
        let known_account = tree.find_account("Assets:Bank", true).unwrap();
        known_account.borrow_mut().add_flag(AccountFlags::Known);
        
        let virtual_account = tree.find_account("Virtual:Account", true).unwrap();
        virtual_account.borrow_mut().add_flag(AccountFlags::Virtual);
        
        let temp_account = tree.find_account("Temp:Account", true).unwrap();
        temp_account.borrow_mut().add_flag(AccountFlags::Temp);
        
        // Test filtering by flags
        let known_accounts = tree.find_accounts_by_flag(AccountFlags::Known);
        assert_eq!(known_accounts.len(), 1);
        
        let virtual_accounts = tree.find_accounts_by_flag(AccountFlags::Virtual);
        assert_eq!(virtual_accounts.len(), 1);
        
        let temp_accounts = tree.find_accounts_by_flag(AccountFlags::Temp);
        assert_eq!(temp_accounts.len(), 1);
    }

    #[test]
    fn test_account_filtering_by_depth() {
        let mut tree = AccountTree::new();
        
        // Create accounts at different depths
        tree.find_account("A", true);           // depth 1
        tree.find_account("A:B", true);         // depth 2
        tree.find_account("A:B:C", true);       // depth 3
        tree.find_account("A:B:C:D", true);     // depth 4
        
        // Test depth filtering
        let depth_1_accounts = tree.find_accounts_by_depth(1, 1);
        assert!(depth_1_accounts.iter().any(|acc| acc.borrow().fullname_immutable() == "A"));
        
        let depth_2_accounts = tree.find_accounts_by_depth(2, 2);
        assert!(depth_2_accounts.iter().any(|acc| acc.borrow().fullname_immutable() == "A:B"));
        
        let depth_range_accounts = tree.find_accounts_by_depth(2, 3);
        let names: Vec<_> = depth_range_accounts.iter()
            .map(|acc| acc.borrow().fullname_immutable())
            .collect();
        assert!(names.contains(&"A:B".into()));
        assert!(names.contains(&"A:B:C".into()));
    }

    #[test]
    fn test_account_filtering_by_metadata() {
        let mut tree = AccountTree::new();
        
        // Create accounts with metadata
        let account1 = tree.find_account("Account1", true).unwrap();
        account1.borrow_mut().set_metadata("category".to_string(), "important");
        
        let account2 = tree.find_account("Account2", true).unwrap();
        account2.borrow_mut().set_metadata("priority".to_string(), 5);
        
        let account3 = tree.find_account("Account3", true).unwrap();
        account3.borrow_mut().set_metadata("category".to_string(), "normal");
        
        // Test metadata key filtering
        let category_accounts = tree.find_accounts_by_metadata_key("category");
        assert_eq!(category_accounts.len(), 2);
        
        let priority_accounts = tree.find_accounts_by_metadata_key("priority");
        assert_eq!(priority_accounts.len(), 1);
        
        let nonexistent_accounts = tree.find_accounts_by_metadata_key("nonexistent");
        assert_eq!(nonexistent_accounts.len(), 0);
    }

    #[test]
    fn test_virtual_account_creation() {
        let mut tree = AccountTree::new();
        
        // Create virtual account
        let virtual_account = tree.create_virtual_account("Virtual:Test", Some(AccountType::Asset));
        
        assert!(virtual_account.borrow().is_virtual());
        assert!(virtual_account.borrow().has_flag(AccountFlags::Virtual));
        assert!(virtual_account.borrow().has_flag(AccountFlags::Temp));
        assert_eq!(virtual_account.borrow().get_account_type(), AccountType::Asset);
        
        // Test auto-type detection
        let auto_virtual = tree.create_virtual_account("Expenses:Virtual", None);
        assert_eq!(auto_virtual.borrow().get_account_type(), AccountType::Expense);
    }

    #[test]
    fn test_account_directive_application() {
        let mut tree = AccountTree::new();
        
        // Test account directive
        let account_directive = AccountDirective::Account(
            "Assets:Bank".into(), 
            Some(AccountType::Asset)
        );
        tree.apply_directive("Assets:Bank", account_directive).unwrap();
        
        let account = tree.find_account("Assets:Bank", false).unwrap();
        assert!(account.borrow().is_known());
        assert_eq!(account.borrow().get_account_type(), AccountType::Asset);
        
        // Test alias directive
        let alias_directive = AccountDirective::Alias("Bank".into());
        tree.apply_directive("Assets:Bank", alias_directive).unwrap();
        
        assert!(tree.is_alias("Bank"));
        assert_eq!(tree.resolve_alias("Bank"), Some(&CompactString::from("Assets:Bank")));
        
        // Test payee directive
        let payee_directive = AccountDirective::Payee("Default Bank".into());
        tree.apply_directive("Assets:Bank", payee_directive).unwrap();
        
        {
            let account_borrowed = account.borrow();
            let payee_directives = account_borrowed.get_directives_by_type("payee");
            assert_eq!(payee_directives.len(), 1);
            assert_eq!(account_borrowed.get_default_payee(), Some("Default Bank".into()));
        }
    }

    #[test]
    fn test_leaf_and_root_account_finding() {
        let mut tree = AccountTree::new();
        
        // Create account structure
        tree.find_account("Assets", true);
        tree.find_account("Assets:Bank", true);
        tree.find_account("Assets:Bank:Checking", true);
        tree.find_account("Expenses", true);
        tree.find_account("Expenses:Food", true);
        
        // Test leaf account finding
        let leaf_accounts = tree.find_leaf_accounts();
        let leaf_names: Vec<_> = leaf_accounts.iter()
            .map(|acc| acc.borrow().fullname_immutable())
            .collect();
        
        assert!(leaf_names.contains(&"Assets:Bank:Checking".into()));
        assert!(leaf_names.contains(&"Expenses:Food".into()));
        // Should not contain intermediate accounts
        assert!(!leaf_names.contains(&"Assets".into()));
        assert!(!leaf_names.contains(&"Assets:Bank".into()));
        
        // Test root account finding (excludes tree root "")
        let root_accounts = tree.find_root_accounts();
        let root_names: Vec<_> = root_accounts.iter()
            .map(|acc| acc.borrow().fullname_immutable())
            .collect();
        
        assert!(root_names.contains(&"Assets".into()));
        assert!(root_names.contains(&"Expenses".into()));
        assert!(!root_names.contains(&"".into())); // Tree root should be excluded
    }

    // Test visitor pattern implementation
    struct CountingVisitor {
        count: usize,
        asset_count: usize,
    }

    impl AccountVisitor for CountingVisitor {
        fn visit(&mut self, account: &mut Account) {
            self.count += 1;
            if account.get_account_type() == AccountType::Asset {
                self.asset_count += 1;
            }
        }
    }

    #[test]
    fn test_visitor_pattern() {
        let mut tree = AccountTree::new();
        
        // Create accounts with different types
        let assets = tree.find_account("Assets", true).unwrap();
        assets.borrow_mut().set_account_type(AccountType::Asset);
        
        let checking = tree.find_account("Assets:Checking", true).unwrap();
        checking.borrow_mut().set_account_type(AccountType::Asset);
        
        let expenses = tree.find_account("Expenses", true).unwrap();
        expenses.borrow_mut().set_account_type(AccountType::Expense);
        
        // Test visitor on single account (non-recursive)
        let mut visitor = CountingVisitor { count: 0, asset_count: 0 };
        assets.borrow_mut().accept(&mut visitor, false);
        assert_eq!(visitor.count, 1);
        assert_eq!(visitor.asset_count, 1);
        
        // Test visitor on tree (recursive)
        let mut visitor_recursive = CountingVisitor { count: 0, asset_count: 0 };
        tree.accept_all(&mut visitor_recursive);
        assert!(visitor_recursive.count >= 4); // At least root, Assets, Assets:Checking, Expenses
        assert_eq!(visitor_recursive.asset_count, 2); // Assets and Assets:Checking
    }

    #[test]
    fn test_comprehensive_directive_support() {
        let mut tree = AccountTree::new();

        // Create the account first
        tree.find_account("Assets:Test", true).unwrap();

        // Test note directive with automatic note setting
        let note_directive = AccountDirective::Note("This is a test account".into());
        tree.apply_directive("Assets:Test", note_directive).unwrap();
        
        let account = tree.find_account("Assets:Test", false).unwrap();
        {
            let account_borrowed = account.borrow();
            assert_eq!(account_borrowed.note, Some("This is a test account".into()));
            let note_directives = account_borrowed.get_directives_by_type("note");
            assert_eq!(note_directives.len(), 1);
        }

        // Test default commodity directive
        let default_directive = AccountDirective::Default("USD".into());
        tree.apply_directive("Assets:Test", default_directive).unwrap();
        
        {
            let account_borrowed = account.borrow();
            assert_eq!(account_borrowed.get_default_commodity(), Some("USD".into()));
        }

        // Test format directive
        let format_directive = AccountDirective::Format("$%.2f".into());
        tree.apply_directive("Assets:Test", format_directive).unwrap();
        
        {
            let account_borrowed = account.borrow();
            assert_eq!(account_borrowed.get_format(), Some("$%.2f".into()));
        }

        // Test assertion directive
        let assert_directive = AccountDirective::Assert("balance >= $1000".into());
        tree.apply_directive("Assets:Test", assert_directive).unwrap();
        
        {
            let account_borrowed = account.borrow();
            let assertions = account_borrowed.get_assertions();
            assert_eq!(assertions.len(), 1);
            assert_eq!(assertions[0], "balance >= $1000");
        }

        // Test tag directive
        let tag_directive = AccountDirective::Tag("important".into());
        tree.apply_directive("Assets:Test", tag_directive).unwrap();
        
        {
            let account_borrowed = account.borrow();
            assert!(account_borrowed.has_tag("important"));
            let tags = account_borrowed.get_tags();
            assert_eq!(tags.len(), 1);
            assert_eq!(tags[0], "important");
        }
    }

    #[test]
    fn test_directive_line_parsing() {
        let mut tree = AccountTree::new();

        // Test account directive parsing
        tree.parse_and_apply_directive("account Assets:Bank asset").unwrap();
        let account = tree.find_account("Assets:Bank", false).unwrap();
        assert!(account.borrow().is_known());
        assert_eq!(account.borrow().get_account_type(), AccountType::Asset);

        // Test payee directive parsing
        tree.parse_and_apply_directive("payee Assets:Bank First National Bank").unwrap();
        {
            let account_borrowed = account.borrow();
            assert_eq!(account_borrowed.get_default_payee(), Some("First National Bank".into()));
        }

        // Test note directive parsing
        tree.parse_and_apply_directive("note Assets:Bank Primary checking account").unwrap();
        {
            let account_borrowed = account.borrow();
            assert_eq!(account_borrowed.note, Some("Primary checking account".into()));
        }

        // Test tag directive parsing
        tree.parse_and_apply_directive("tag Assets:Bank primary").unwrap();
        assert!(account.borrow().has_tag("primary"));

        // Test default commodity parsing
        tree.parse_and_apply_directive("default Assets:Bank USD").unwrap();
        {
            let account_borrowed = account.borrow();
            assert_eq!(account_borrowed.get_default_commodity(), Some("USD".into()));
        }

        // Test alias parsing
        tree.parse_and_apply_directive("alias Bank Assets:Bank").unwrap();
        assert!(tree.is_alias("Bank"));
        assert_eq!(tree.resolve_alias("Bank"), Some(&CompactString::from("Assets:Bank")));

        // Test comment and empty line handling
        assert!(tree.parse_and_apply_directive("# This is a comment").is_ok());
        assert!(tree.parse_and_apply_directive("; This is also a comment").is_ok());
        assert!(tree.parse_and_apply_directive("").is_ok());
        assert!(tree.parse_and_apply_directive("   ").is_ok());

        // Test error cases
        assert!(tree.parse_and_apply_directive("account").is_err());
        assert!(tree.parse_and_apply_directive("alias OnlyOneArg").is_err());
        assert!(tree.parse_and_apply_directive("payee Assets:Bank").is_err());
        assert!(tree.parse_and_apply_directive("unknown_directive test").is_err());
    }

    #[test]
    fn test_temp_account_creation() {
        let mut tree = AccountTree::new();

        // Test temp account creation with explicit type
        let temp_account = tree.create_temp_account("Temp:Calculation", Some(AccountType::Asset));
        
        assert!(temp_account.borrow().is_temp());
        assert!(temp_account.borrow().is_generated());
        assert!(!temp_account.borrow().is_virtual());
        assert_eq!(temp_account.borrow().get_account_type(), AccountType::Asset);

        // Test temp account creation with auto type detection
        let auto_temp = tree.create_temp_account("Expenses:Temp", None);
        assert_eq!(auto_temp.borrow().get_account_type(), AccountType::Expense);
    }

    #[test]
    fn test_account_tag_operations() {
        let mut tree = AccountTree::new();

        // Create accounts with different tags
        let account1 = tree.find_account("Account1", true).unwrap();
        account1.borrow_mut().add_directive(AccountDirective::Tag("important".into()));
        account1.borrow_mut().add_directive(AccountDirective::Tag("primary".into()));

        let account2 = tree.find_account("Account2", true).unwrap();
        account2.borrow_mut().add_directive(AccountDirective::Tag("important".into()));

        let account3 = tree.find_account("Account3", true).unwrap();
        account3.borrow_mut().add_directive(AccountDirective::Tag("secondary".into()));

        // Test tag queries
        assert!(account1.borrow().has_tag("important"));
        assert!(account1.borrow().has_tag("primary"));
        assert!(!account1.borrow().has_tag("secondary"));

        let tags1 = account1.borrow().get_tags();
        assert_eq!(tags1.len(), 2);
        assert!(tags1.contains(&"important".into()));
        assert!(tags1.contains(&"primary".into()));

        // Test finding accounts by tags
        let important_accounts = tree.find_accounts_by_tags(&[String::from("important")]);
        assert_eq!(important_accounts.len(), 2); // account1 and account2

        let primary_accounts = tree.find_accounts_by_tags(&[String::from("primary")]);
        assert_eq!(primary_accounts.len(), 1); // only account1

        let both_tags = tree.find_accounts_by_tags(&[String::from("important"), String::from("primary")]);
        assert_eq!(both_tags.len(), 1); // only account1 has both

        let nonexistent_tags = tree.find_accounts_by_tags(&[String::from("nonexistent")]);
        assert_eq!(nonexistent_tags.len(), 0);
    }

    #[test]
    fn test_account_assertions() {
        let mut tree = AccountTree::new();

        // Create accounts with assertions
        let account1 = tree.find_account("Assets:Bank", true).unwrap();
        account1.borrow_mut().add_directive(AccountDirective::Assert("balance >= $1000".into()));
        account1.borrow_mut().add_directive(AccountDirective::Assert("balance <= $50000".into()));

        let account2 = tree.find_account("Expenses:Food", true).unwrap();
        account2.borrow_mut().add_directive(AccountDirective::Assert("monthly_total <= $500".into()));

        // Test assertion retrieval
        let assertions1 = account1.borrow().get_assertions();
        assert_eq!(assertions1.len(), 2);
        assert!(assertions1.contains(&"balance >= $1000".into()));
        assert!(assertions1.contains(&"balance <= $50000".into()));

        let assertions2 = account2.borrow().get_assertions();
        assert_eq!(assertions2.len(), 1);
        assert_eq!(assertions2[0], "monthly_total <= $500");

        // Test finding accounts with assertions
        let accounts_with_assertions = tree.find_accounts_with_assertions();
        assert_eq!(accounts_with_assertions.len(), 2);
    }

    #[test]
    fn test_all_directives_retrieval() {
        let mut tree = AccountTree::new();

        // Create accounts with various directives
        tree.parse_and_apply_directive("account Assets:Bank asset").unwrap();
        tree.parse_and_apply_directive("payee Assets:Bank First National").unwrap();
        tree.parse_and_apply_directive("tag Assets:Bank primary").unwrap();
        tree.parse_and_apply_directive("account Expenses:Food expense").unwrap();
        tree.parse_and_apply_directive("tag Expenses:Food frequent").unwrap();

        let all_directives = tree.get_all_directives();

        // Check account directives
        let account_directives = all_directives.get("account").unwrap();
        assert_eq!(account_directives.len(), 2);

        // Check payee directives
        let payee_directives = all_directives.get("payee").unwrap();
        assert_eq!(payee_directives.len(), 1);

        // Check tag directives
        let tag_directives = all_directives.get("tag").unwrap();
        assert_eq!(tag_directives.len(), 2);

        // Check non-existent directive type
        assert!(all_directives.get("nonexistent").is_none());
    }

    #[test]
    fn test_account_status_methods() {
        let mut tree = AccountTree::new();

        // Test regular account
        let regular_account = tree.find_account("Assets:Regular", true).unwrap();
        {
            let account_borrowed = regular_account.borrow();
            assert!(!account_borrowed.is_virtual());
            assert!(!account_borrowed.is_temp());
            assert!(!account_borrowed.is_generated());
            assert!(!account_borrowed.is_known());
        }

        // Test virtual account
        let virtual_account = tree.create_virtual_account("Virtual:Test", None);
        {
            let account_borrowed = virtual_account.borrow();
            assert!(account_borrowed.is_virtual());
            assert!(account_borrowed.is_temp());
            assert!(!account_borrowed.is_generated());
            assert!(!account_borrowed.is_known());
        }

        // Test temp account
        let temp_account = tree.create_temp_account("Temp:Test", None);
        {
            let account_borrowed = temp_account.borrow();
            assert!(!account_borrowed.is_virtual());
            assert!(account_borrowed.is_temp());
            assert!(account_borrowed.is_generated());
            assert!(!account_borrowed.is_known());
        }

        // Test known account
        tree.parse_and_apply_directive("account Assets:Known asset").unwrap();
        let known_account = tree.find_account("Assets:Known", false).unwrap();
        {
            let account_borrowed = known_account.borrow();
            assert!(account_borrowed.is_known());
        }
    }
}
