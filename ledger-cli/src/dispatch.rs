//! Command dispatch and handler framework
//! 
//! This module provides the framework for dispatching commands to their appropriate
//! handlers and managing the execution flow.

use anyhow::{Result, Context};
use ledger_core::parser::JournalParser;
use std::io::{self, Write};
use crate::cli::{Cli, Commands};
use crate::session::Session;
use crate::completion;
use crate::help::{HelpTopic, show_man_page};
use clap::{Command, CommandFactory};
use std::ops::Add;

/// Main command dispatcher
pub struct Dispatcher {
    session: Session,
}

impl Dispatcher {
    /// Create a new dispatcher with the given session
    pub fn new(session: Session) -> Self {
        Self { session }
    }
    
    /// Execute the command specified in the CLI arguments
    pub fn execute(&mut self, cli: &Cli) -> Result<i32> {
        // Handle global options first
        if cli.options {
            self.show_options()?;
            return Ok(0);
        }
        
        if cli.verify || cli.verify_memory {
            eprintln!("Memory verification is not implemented in the Rust version");
        }
        
        // Handle pre-commands that don't require journal files
        if cli.is_precommand() {
            return self.execute_precommand(cli);
        }
        
        // Load journal files for regular commands
        self.load_journal_files()?;
        
        // Execute the main command
        match &cli.command {
            Some(command) => self.execute_command(command),
            None => {
                // No command specified - show version and enter REPL mode
                println!("{}", self.session.show_version_info());
                if self.session.journal_files.is_empty() {
                    eprintln!("Warning: No journal files found");
                }
                self.enter_repl_mode()
            }
        }
    }
    
    /// Execute a pre-command that doesn't require journal files
    fn execute_precommand(&mut self, cli: &Cli) -> Result<i32> {
        match &cli.command {
            Some(Commands::Parse(args)) => {
                println!("Parsing expression: {}", args.expression);
                // TODO: Implement expression parsing
                Ok(0)
            }
            Some(Commands::Eval(args)) => {
                println!("Evaluating expression: {}", args.expression);
                // TODO: Implement expression evaluation
                Ok(0)
            }
            Some(Commands::Format(args)) => {
                println!("Parsing format string: {}", args.format_string);
                // TODO: Implement format parsing
                Ok(0)
            }
            Some(Commands::Period(args)) => {
                println!("Parsing period: {}", args.period);
                // TODO: Implement period parsing
                Ok(0)
            }
            Some(Commands::Query(args)) => {
                println!("Parsing query: {}", args.query);
                // TODO: Implement query parsing
                Ok(0)
            }
            Some(Commands::Source(args)) => {
                println!("Running script: {}", args.script_file);
                // TODO: Implement script execution
                Ok(0)
            }
            Some(Commands::Generate(args)) => {
                println!("Generating {} sample transactions", args.count);
                if let Some(seed) = args.seed {
                    println!("Using seed: {}", seed);
                }
                // TODO: Implement transaction generation
                Ok(0)
            }
            Some(Commands::Template(args)) => {
                println!("Processing template: {}", args.template_file);
                // TODO: Implement template processing
                Ok(0)
            }
            Some(Commands::Completion(args)) => {
                self.execute_completion_command(args)
            }
            Some(Commands::HelpTopic(args)) => {
                self.execute_help_topic_command(args)
            }
            _ => {
                unreachable!("Non-precommand passed to execute_precommand")
            }
        }
    }
    
    /// Execute a main command that requires journal files
    fn execute_command(&mut self, command: &Commands) -> Result<i32> {
        match command {
            Commands::Balance(args) => {
                self.execute_balance_command(args)
            }
            Commands::Register(args) => {
                self.execute_register_command(args)
            }
            Commands::Print(args) => {
                self.execute_print_command(args)
            }
            Commands::Accounts(args) => {
                self.execute_accounts_command(args)
            }
            Commands::Commodities(args) => {
                self.execute_commodities_command(args)
            }
            Commands::Payees(args) => {
                self.execute_payees_command(args)
            }
            Commands::Tags(args) => {
                self.execute_tags_command(args)
            }
            Commands::Csv(args) => {
                self.execute_csv_command(args)
            }
            Commands::Cleared(args) => {
                self.execute_cleared_command(args)
            }
            Commands::Budget(args) => {
                self.execute_budget_command(args)
            }
            Commands::Equity(args) => {
                self.execute_equity_command(args)
            }
            Commands::Prices(args) => {
                self.execute_prices_command(args)
            }
            Commands::Pricedb(args) => {
                self.execute_pricedb_command(args)
            }
            Commands::Pricemap(args) => {
                self.execute_pricemap_command(args)
            }
            Commands::Stats(args) => {
                self.execute_stats_command(args)
            }
            Commands::Xact(args) => {
                self.execute_xact_command(args)
            }
            Commands::Select(args) => {
                self.execute_select_command(args)
            }
            Commands::Convert(args) => {
                self.execute_convert_command(args)
            }
            Commands::Emacs(args) => {
                self.execute_emacs_command(args)
            }
            Commands::Xml(args) => {
                self.execute_xml_command(args)
            }
            _ => {
                // This should not happen as pre-commands are handled separately
                Err(anyhow::anyhow!("Unexpected command type"))
            }
        }
    }
    
    /// Load journal files specified in the session and store the parsed journal
    fn load_journal_files(&mut self) -> Result<()> {
        if self.session.journal_files.is_empty() {
            return Err(anyhow::anyhow!("No journal files specified"));
        }
        
        for file in &self.session.journal_files {
            if self.session.verbose_enabled {
                eprintln!("Loading journal file: {}", file.display());
            }
            
            if !file.exists() {
                return Err(anyhow::anyhow!("Journal file not found: {}", file.display()));
            }
            
            let contents = std::fs::read_to_string(file)?;
            let mut parser = JournalParser::new();
            let journal = parser.parse_journal(&contents)?;
            
            // FIXME: this only supports 1 journal file
            // TODO: combine all input journal files into one journal
            self.session.parsed_journal = Some(journal);
        }
        
        Ok(())
    }
    
    /// Enter REPL (Read-Eval-Print-Loop) mode
    fn enter_repl_mode(&mut self) -> Result<i32> {
        println!();
        println!("Entering REPL mode. Type 'quit' to exit.");
        
        let mut line = String::new();
        loop {
            print!("{}", self.session.prompt_string());
            io::stdout().flush()?;
            
            line.clear();
            match io::stdin().read_line(&mut line) {
                Ok(0) => break, // EOF
                Ok(_) => {
                    let line = line.trim();
                    if line.is_empty() || line.starts_with('#') {
                        continue;
                    }
                    
                    if line == "quit" || line == "exit" {
                        break;
                    }
                    
                    // TODO: Parse and execute the command
                    println!("Command: {}", line);
                }
                Err(e) => {
                    eprintln!("Error reading input: {}", e);
                    break;
                }
            }
        }
        
        Ok(0)
    }
    
    /// Show all configuration options and their values
    fn show_options(&self) -> Result<()> {
        println!("===============================================================================");
        println!("[Global scope options]");
        println!();
        
        // Show global options from CLI
        println!("debug: {}", self.session.debug_enabled);
        println!("verbose: {}", self.session.verbose_enabled);
        
        if !self.session.journal_files.is_empty() {
            println!("files:");
            for file in &self.session.journal_files {
                println!("  {}", file.display());
            }
        }
        
        if let Some(ref price_db) = self.session.config.price_db_path {
            println!("price-db: {}", price_db.display());
        }
        
        println!();
        println!("[Session scope options]");
        println!();
        
        // Show session options
        for (key, value) in &self.session.config.options {
            println!("{}: {}", key, value);
        }
        
        println!();
        println!("[Report scope options]");
        println!();
        
        // Show runtime options
        if let Some(ref begin) = self.session.options.begin_date {
            println!("begin: {}", begin);
        }
        if let Some(ref end) = self.session.options.end_date {
            println!("end: {}", end);
        }
        if let Some(ref sort) = self.session.options.sort_expr {
            println!("sort: {}", sort);
        }
        if let Some(limit) = self.session.options.limit {
            println!("limit: {}", limit);
        }
        
        println!("color: {}", self.session.options.use_color);
        println!("pager: {}", self.session.options.use_pager);
        
        if let Some(columns) = self.session.options.columns {
            println!("columns: {}", columns);
        }
        
        println!("===============================================================================");
        Ok(())
    }
}

// Command implementations - these will be expanded with actual functionality

impl Dispatcher {
    fn execute_balance_command(&mut self, args: &crate::cli::BalanceArgs) -> Result<i32> {
        // Get the parsed journal
        let journal = match &self.session.parsed_journal {
            Some(journal) => journal,
            None => {
                return Err(anyhow::anyhow!("No journal loaded"));
            }
        };
        
        println!("CLI Demo - Balance Report");
        println!("========================");
        
        // Show basic journal info
        println!("Transactions loaded: {}", journal.transactions.len());
        println!("Accounts created: {}", journal.accounts.len());
        
        // Show accounts if --empty specified
        if args.empty {
            println!("\nAccounts:");
            for (name, _) in &journal.accounts {
                println!("  {}", name);
            }
        }
        
        if !args.pattern.is_empty() {
            println!("Account patterns (not yet implemented): {:?}", args.pattern);
        }
        
        if args.flat {
            println!("Flat account listing enabled");
        }
        
        if let Some(depth) = args.depth {
            println!("Max depth: {}", depth);
        }
        
        Ok(0)
    }
    
    fn execute_register_command(&mut self, args: &crate::cli::RegisterArgs) -> Result<i32> {
        // Get the parsed journal
        let journal = match &self.session.parsed_journal {
            Some(journal) => journal,
            None => {
                return Err(anyhow::anyhow!("No journal loaded"));
            }
        };
        
        println!("Demo Register Report");
        println!("====================");
        
        let transactions = &journal.transactions;
        let mut running_total = ledger_math::amount::Amount::null();
        
        for transaction in transactions {
            let postings = &transaction.postings;
            for posting in postings {
                if let Some(amount) = &posting.amount {
                    let account_name = "Demo Account"; // placeholder
                    let desc = &transaction.payee;
                    
                    // Update running total
                    running_total = running_total.add(amount).unwrap_or_else(|_| amount.clone());
                    
                    if args.wide {
                        println!("{} {:30} {:>12} {:>15}", 
                                transaction.date.format("%Y/%m/%d"),
                                desc, 
                                account_name, 
                                amount);
                    } else {
                        println!("{} {:20} {:>12} {:>12}", 
                                transaction.date.format("%Y/%m/%d"),
                                desc,
                                account_name, 
                                amount);
                    }
                }
            }
        }
        
        if !args.pattern.is_empty() && self.session.verbose_enabled {
            eprintln!("Account patterns: {:?}", args.pattern);
        }
        
        if args.subtotal && self.session.verbose_enabled {
            eprintln!("Subtotal option not yet implemented");
        }
        
        Ok(0)
    }
    
    fn execute_print_command(&mut self, args: &crate::cli::PrintArgs) -> Result<i32> {
        // Get the parsed journal
        let journal = match &self.session.parsed_journal {
            Some(journal) => journal,
            None => {
                return Err(anyhow::anyhow!("No journal loaded"));
            }
        };
        
        let transactions = &journal.transactions;
        
        for (i, transaction) in transactions.iter().enumerate() {
            if i != 0 {
                println!(); // Empty line between transactions
            }
            
            println!("{} {}", 
                    transaction.date.format("%Y/%m/%d"),
                    &transaction.payee);
            
            let postings = &transaction.postings;
            let width = postings.iter()
                .map(|p| p.account.borrow_mut().fullname())
                .max_by(|a, b| a.len().cmp(&b.len()))
                .map(|name| name.len().add(4).max(40))
                .unwrap_or(40);
            for posting in postings {
                let account_name = posting.account.borrow().fullname_immutable();
                
                if let Some(amount) = &posting.amount {
                    println!("    {:width$} {}", account_name, amount, width = width);
                } else {
                    println!("    {:width$}", account_name, width = width);
                }
            }
        }
        
        if !args.pattern.is_empty() && self.session.verbose_enabled {
            eprintln!("Account patterns: {:?}", args.pattern);
        }
        
        if args.raw && self.session.verbose_enabled {
            eprintln!("Raw format option not yet implemented");
        }
        
        Ok(0)
    }
    
    fn execute_accounts_command(&mut self, args: &crate::cli::AccountsArgs) -> Result<i32> {
        println!("Accounts command");
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual accounts listing
        Ok(0)
    }
    
    fn execute_commodities_command(&mut self, args: &crate::cli::CommoditiesArgs) -> Result<i32> {
        println!("Commodities command");
        if !args.pattern.is_empty() {
            println!("Commodity patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual commodities listing
        Ok(0)
    }
    
    fn execute_payees_command(&mut self, args: &crate::cli::PayeesArgs) -> Result<i32> {
        println!("Payees command");
        if !args.pattern.is_empty() {
            println!("Payee patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual payees listing
        Ok(0)
    }
    
    fn execute_tags_command(&mut self, args: &crate::cli::TagsArgs) -> Result<i32> {
        println!("Tags command");
        if !args.pattern.is_empty() {
            println!("Tag patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual tags listing
        Ok(0)
    }
    
    fn execute_csv_command(&mut self, args: &crate::cli::CsvArgs) -> Result<i32> {
        println!("CSV command");
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual CSV output
        Ok(0)
    }
    
    fn execute_cleared_command(&mut self, args: &crate::cli::ClearedArgs) -> Result<i32> {
        println!("Cleared command");
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual cleared report
        Ok(0)
    }
    
    fn execute_budget_command(&mut self, args: &crate::cli::BudgetArgs) -> Result<i32> {
        println!("Budget command");
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        if args.unbudgeted {
            println!("Show unbudgeted accounts");
        }
        // TODO: Implement actual budget report
        Ok(0)
    }
    
    fn execute_equity_command(&mut self, args: &crate::cli::EquityArgs) -> Result<i32> {
        println!("Equity command");
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual equity report
        Ok(0)
    }
    
    fn execute_prices_command(&mut self, args: &crate::cli::PricesArgs) -> Result<i32> {
        println!("Prices command");
        if !args.pattern.is_empty() {
            println!("Commodity patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual prices report
        Ok(0)
    }
    
    fn execute_pricedb_command(&mut self, args: &crate::cli::PricedbArgs) -> Result<i32> {
        println!("Pricedb command");
        if !args.pattern.is_empty() {
            println!("Commodity patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual pricedb report
        Ok(0)
    }
    
    fn execute_pricemap_command(&mut self, args: &crate::cli::PricemapArgs) -> Result<i32> {
        println!("Pricemap command");
        if !args.pattern.is_empty() {
            println!("Commodity patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual pricemap report
        Ok(0)
    }
    
    fn execute_stats_command(&mut self, args: &crate::cli::StatsArgs) -> Result<i32> {
        println!("Stats command");
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual stats report
        Ok(0)
    }
    
    fn execute_xact_command(&mut self, args: &crate::cli::XactArgs) -> Result<i32> {
        println!("Xact command");
        if let Some(ref account) = args.account {
            println!("Account: {}", account);
        }
        if let Some(ref payee) = args.payee {
            println!("Payee: {}", payee);
        }
        // TODO: Implement actual transaction generation
        Ok(0)
    }
    
    fn execute_select_command(&mut self, args: &crate::cli::SelectArgs) -> Result<i32> {
        println!("Select command");
        println!("Expression: {}", args.expression);
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual select command
        Ok(0)
    }
    
    fn execute_convert_command(&mut self, args: &crate::cli::ConvertArgs) -> Result<i32> {
        println!("Convert command");
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual convert command
        Ok(0)
    }
    
    fn execute_emacs_command(&mut self, args: &crate::cli::EmacsArgs) -> Result<i32> {
        println!("Emacs command");
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual Emacs output
        Ok(0)
    }
    
    fn execute_xml_command(&mut self, args: &crate::cli::XmlArgs) -> Result<i32> {
        println!("XML command");
        if !args.pattern.is_empty() {
            println!("Account patterns: {:?}", args.pattern);
        }
        // TODO: Implement actual XML output
        Ok(0)
    }
    
    /// Execute completion command to generate shell completions
    fn execute_completion_command(&mut self, args: &crate::cli::CompletionArgs) -> Result<i32> {
        let mut cmd = crate::cli::Cli::command();
        
        if let Some(ref output_dir) = args.output_dir {
            let output_path = std::path::Path::new(output_dir);
            if !output_path.exists() {
                std::fs::create_dir_all(output_path)
                    .with_context(|| format!("Failed to create output directory: {}", output_dir))?;
            }
            
            completion::save_completions(args.shell, &mut cmd, output_path)
                .map_err(|e| anyhow::anyhow!("Failed to save completion file: {}", e))?;
        } else {
            completion::generate_completion(args.shell, &mut cmd);
        }
        
        Ok(0)
    }
    
    /// Execute help topic command to show detailed help
    fn execute_help_topic_command(&mut self, args: &crate::cli::HelpTopicArgs) -> Result<i32> {
        let topic = match args.topic.to_lowercase().as_str() {
            "options" => HelpTopic::Options,
            "expressions" | "expr" => HelpTopic::Expressions,
            "periods" | "period" => HelpTopic::Periods,
            "formats" | "format" => HelpTopic::Formats,
            "queries" | "query" => HelpTopic::Queries,
            "man" | "manual" => {
                show_man_page();
                return Ok(0);
            }
            cmd => HelpTopic::Command(cmd.to_string()),
        };
        
        println!("{}", topic);
        Ok(0)
    }
}