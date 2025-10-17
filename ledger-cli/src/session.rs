//! Session state and configuration management
//!
//! This module manages global state for the ledger CLI, including configuration
//! file parsing, environment variable handling, and session state management.

use crate::cli::Cli;
use anyhow::{Context, Result};
use chrono::{Local, NaiveDate};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

/// Global session state for the ledger application
pub struct Session {
    /// Configuration loaded from files and environment
    pub config: Config,

    /// Runtime options from command line
    pub options: RuntimeOptions,

    /// Journal files to process
    pub journal_files: Vec<PathBuf>,

    /// Whether debug mode is enabled
    pub debug_enabled: bool,

    /// Whether verbose mode is enabled
    pub verbose_enabled: bool,

    /// Current working directory when started
    pub initial_cwd: PathBuf,

    /// Parsed journal (loaded on demand)
    pub parsed_journal: Option<ledger_core::journal::Journal>,
}

/// Configuration loaded from init files and environment variables
#[derive(Default)]
pub struct Config {
    /// Key-value configuration options
    pub options: HashMap<String, String>,

    /// File paths discovered during configuration
    pub file_paths: Vec<PathBuf>,

    /// Price database path
    pub price_db_path: Option<PathBuf>,

    /// Default commodity for display
    pub default_commodity: Option<String>,

    /// Default date format
    pub date_format: Option<String>,
}

/// Runtime options derived from command-line arguments
pub struct RuntimeOptions {
    /// Begin date filter
    pub begin_date: Option<NaiveDate>,

    /// End date filter
    pub end_date: Option<NaiveDate>,

    /// Sort expression
    pub sort_expr: Option<String>,

    /// Limit on number of results
    pub limit: Option<usize>,

    /// Display format string
    pub format: Option<String>,

    /// Output file path
    pub output_file: Option<PathBuf>,

    /// Whether to use color output
    pub use_color: bool,

    /// Whether to use pager
    pub use_pager: bool,

    /// Pager command
    pub pager_command: Option<String>,

    /// Column width
    pub columns: Option<usize>,

    /// Account query patterns
    pub account_patterns: Vec<String>,
}

impl Session {
    /// Create a new session from command-line arguments
    pub fn new(cli: &Cli) -> Result<Self> {
        let initial_cwd = env::current_dir().context("Failed to get current working directory")?;

        let mut session = Session {
            config: Config::default(),
            options: RuntimeOptions::from_cli(cli)?,
            journal_files: Vec::new(),
            debug_enabled: cli.debug.is_some() || cli.verbose,
            verbose_enabled: cli.verbose,
            initial_cwd,
            parsed_journal: None,
        };

        // Load configuration if not in args-only mode
        if !cli.args_only {
            session.load_environment_config()?;
            session.load_init_file(cli)?;
        }

        // Determine journal files to process
        session.determine_journal_files(cli)?;

        Ok(session)
    }

    /// Load configuration from environment variables
    fn load_environment_config(&mut self) -> Result<()> {
        // Standard Ledger environment variables
        if let Ok(file) = env::var("LEDGER_FILE") {
            self.config.file_paths.push(PathBuf::from(file));
        }

        if let Ok(init_file) = env::var("LEDGER_INIT") {
            self.config.options.insert("init-file".to_string(), init_file);
        }

        if let Ok(price_db) = env::var("LEDGER_PRICE_DB") {
            self.config.price_db_path = Some(PathBuf::from(price_db));
        }

        if let Ok(price_exp) = env::var("LEDGER_PRICE_EXP") {
            self.config.options.insert("price-exp".to_string(), price_exp);
        }

        // Legacy environment variables for compatibility
        if let Ok(file) = env::var("LEDGER") {
            if self.config.file_paths.is_empty() {
                self.config.file_paths.push(PathBuf::from(file));
            }
        }

        if let Ok(price_db) = env::var("PRICE_HIST") {
            if self.config.price_db_path.is_none() {
                self.config.price_db_path = Some(PathBuf::from(price_db));
            }
        }

        if let Ok(price_exp) = env::var("PRICE_EXP") {
            if !self.config.options.contains_key("price-exp") {
                self.config.options.insert("price-exp".to_string(), price_exp);
            }
        }

        // Load all LEDGER_* environment variables
        for (key, value) in env::vars() {
            if key.starts_with("LEDGER_") && key != "LEDGER_FILE" && key != "LEDGER_INIT" {
                let option_name =
                    key.strip_prefix("LEDGER_").unwrap().to_lowercase().replace('_', "-");
                self.config.options.insert(option_name, value);
            }
        }

        Ok(())
    }

    /// Load configuration from init file
    fn load_init_file(&mut self, cli: &Cli) -> Result<()> {
        let init_file_path = if let Some(ref path) = cli.init_file {
            PathBuf::from(path)
        } else if let Some(ref path) = self.config.options.get("init-file") {
            PathBuf::from(path)
        } else {
            // Default init file locations
            let home_dir = dirs::home_dir().context("Could not determine home directory")?;

            let mut candidates = vec![
                home_dir.join(".ledgerrc"),
                home_dir.join(".config").join("ledger").join("ledgerrc"),
            ];

            // Check for XDG config directory
            if let Ok(xdg_config) = env::var("XDG_CONFIG_HOME") {
                candidates.insert(0, PathBuf::from(xdg_config).join("ledger").join("ledgerrc"));
            }

            candidates
                .into_iter()
                .find(|p| p.exists())
                .unwrap_or_else(|| home_dir.join(".ledgerrc"))
        };

        if init_file_path.exists() {
            self.parse_init_file(&init_file_path)?;
        }

        Ok(())
    }

    /// Parse an init file and load its configuration
    fn parse_init_file(&mut self, path: &Path) -> Result<()> {
        let content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read init file: {}", path.display()))?;

        for (line_num, line) in content.lines().enumerate() {
            let line = line.trim();

            // Skip comments and empty lines
            if line.is_empty() || line.starts_with('#') || line.starts_with(';') {
                continue;
            }

            // Parse option lines
            if line.starts_with("--") {
                self.parse_init_option(line, line_num + 1).with_context(|| {
                    format!("Error parsing init file {} at line {}", path.display(), line_num + 1)
                })?;
            } else {
                // Handle other directives (account, commodity, etc.)
                // For now, just store them as-is
                self.config.options.insert(format!("directive_{}", line_num), line.to_string());
            }
        }

        Ok(())
    }

    /// Parse a single option line from init file
    fn parse_init_option(&mut self, line: &str, _line_num: usize) -> Result<()> {
        let parts: Vec<&str> = line.splitn(2, ' ').collect();
        if parts.is_empty() {
            return Ok(());
        }

        let option = parts[0].strip_prefix("--").unwrap_or(parts[0]);
        let value = if parts.len() > 1 { parts[1].to_string() } else { "true".to_string() };

        // Handle special options
        match option {
            "file" => {
                self.config.file_paths.push(PathBuf::from(&value));
            }
            "price-db" => {
                self.config.price_db_path = Some(PathBuf::from(&value));
            }
            _ => {
                self.config.options.insert(option.to_string(), value);
            }
        }

        Ok(())
    }

    /// Determine which journal files to process
    fn determine_journal_files(&mut self, cli: &Cli) -> Result<()> {
        // Command line files take priority
        if !cli.file.is_empty() {
            self.journal_files = cli.file.iter().map(PathBuf::from).collect();
            return Ok(());
        }

        // Use files from config
        if !self.config.file_paths.is_empty() {
            self.journal_files = self.config.file_paths.clone();
            return Ok(());
        }

        // Default file search
        let mut candidates = vec![
            PathBuf::from(".ledger"),
            PathBuf::from("ledger.dat"),
            PathBuf::from("main.ledger"),
        ];

        // Check home directory
        if let Some(home_dir) = dirs::home_dir() {
            candidates.extend(vec![
                home_dir.join(".ledger"),
                home_dir.join("ledger.dat"),
                home_dir.join("Documents").join("ledger.dat"),
            ]);
        }

        // Find the first existing file
        if let Some(file) = candidates.into_iter().find(|p| p.exists()) {
            self.journal_files.push(file);
        }

        Ok(())
    }

    /// Get the prompt string for REPL mode
    pub fn prompt_string(&self) -> String {
        if self.verbose_enabled {
            format!("[{}]> ", chrono::Local::now().format("%H:%M:%S"))
        } else {
            "> ".to_string()
        }
    }

    /// Show version information
    pub fn show_version_info(&self) -> String {
        format!(
            "Ledger {} (Rust implementation)\nCopyright (c) 2003-2024, John Wiegley. All rights reserved.",
            env!("CARGO_PKG_VERSION")
        )
    }

    /// Get configuration value by key
    pub fn get_config_value(&self, key: &str) -> Option<&String> {
        self.config.options.get(key)
    }

    /// Set configuration value
    pub fn set_config_value(&mut self, key: String, value: String) {
        self.config.options.insert(key, value);
    }

    /// Check if color output should be used
    pub fn should_use_color(&self) -> bool {
        if self.options.use_color {
            true
        } else {
            // Check if output is to a terminal
            atty::is(atty::Stream::Stdout)
        }
    }
}

impl RuntimeOptions {
    /// Create runtime options from CLI arguments
    fn from_cli(cli: &Cli) -> Result<Self> {
        let begin_date = if let Some(ref date_str) = cli.begin {
            Some(parse_date(date_str).context("Failed to parse begin date")?)
        } else {
            None
        };

        let end_date = if let Some(ref date_str) = cli.end {
            Some(parse_date(date_str).context("Failed to parse end date")?)
        } else {
            None
        };

        let use_color = if cli.no_color {
            false
        } else if cli.force_color || cli.color {
            true
        } else {
            atty::is(atty::Stream::Stdout)
        };

        let use_pager = if cli.force_pager {
            true
        } else if cli.pager.is_some() {
            atty::is(atty::Stream::Stdout)
        } else {
            false
        };

        Ok(RuntimeOptions {
            begin_date,
            end_date,
            sort_expr: cli.sort.clone(),
            limit: cli.limit,
            format: cli.format.clone(),
            output_file: cli.output.as_ref().map(PathBuf::from),
            use_color,
            use_pager,
            pager_command: cli
                .pager
                .clone()
                .or_else(|| env::var("PAGER").ok().or_else(|| Some("less".to_string()))),
            columns: cli.columns.or_else(|| env::var("COLUMNS").ok().and_then(|c| c.parse().ok())),
            account_patterns: Vec::new(), // Will be populated by commands
        })
    }
}

/// Parse a date string in various formats
fn parse_date(date_str: &str) -> Result<NaiveDate> {
    // Try full date formats first
    let full_formats = ["%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m/%d/%y"];

    for format in &full_formats {
        if let Ok(date) = NaiveDate::parse_from_str(date_str, format) {
            return Ok(date);
        }
    }

    // Handle partial dates (year-month, year only)
    if let Some(captures) =
        regex::Regex::new(r"^(\d{4})-(\d{1,2})$").ok().and_then(|re| re.captures(date_str))
    {
        let year: i32 = captures[1].parse().map_err(|_| anyhow::anyhow!("Invalid year"))?;
        let month: u32 = captures[2].parse().map_err(|_| anyhow::anyhow!("Invalid month"))?;
        return NaiveDate::from_ymd_opt(year, month, 1)
            .ok_or_else(|| anyhow::anyhow!("Invalid date: {}", date_str));
    }

    if let Some(captures) =
        regex::Regex::new(r"^(\d{4})/(\d{1,2})$").ok().and_then(|re| re.captures(date_str))
    {
        let year: i32 = captures[1].parse().map_err(|_| anyhow::anyhow!("Invalid year"))?;
        let month: u32 = captures[2].parse().map_err(|_| anyhow::anyhow!("Invalid month"))?;
        return NaiveDate::from_ymd_opt(year, month, 1)
            .ok_or_else(|| anyhow::anyhow!("Invalid date: {}", date_str));
    }

    // Try relative dates
    match date_str.to_lowercase().as_str() {
        "today" => Ok(Local::now().date_naive()),
        "yesterday" => Ok(Local::now().date_naive() - chrono::Duration::days(1)),
        "tomorrow" => Ok(Local::now().date_naive() + chrono::Duration::days(1)),
        _ => Err(anyhow::anyhow!("Could not parse date: {}", date_str)),
    }
}
