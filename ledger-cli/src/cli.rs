//! Command-line interface definitions using Clap
//!
//! This module defines the complete command structure for the Rust ledger implementation,
//! matching the C++ version's interface as closely as possible.

use clap::{Args, Parser, Subcommand};
use clap_complete::Shell;

/// Ledger - a powerful command-line accounting system
#[derive(Parser)]
#[command(name = "ledger")]
#[command(version, about = "A powerful command-line double-entry accounting system", long_about = None)]
pub struct Cli {
    // Global options that apply to all commands
    /// Input file(s) to process
    #[arg(short = 'f', long = "file", global = true)]
    pub file: Vec<String>,

    /// Initialization file to use
    #[arg(short = 'i', long = "init-file", global = true)]
    pub init_file: Option<String>,

    /// Sort postings by this value expression
    #[arg(long = "sort", short = 'S', global = true)]
    pub sort: Option<String>,

    /// Limit the number of postings shown
    #[arg(long = "limit", short = 'l', global = true)]
    pub limit: Option<usize>,

    /// Begin date for the report
    #[arg(long = "begin", short = 'b', global = true)]
    pub begin: Option<String>,

    /// End date for the report
    #[arg(long = "end", short = 'e', global = true)]
    pub end: Option<String>,

    /// Report for the current period
    #[arg(long = "current", short = 'c', global = true)]
    pub current: bool,

    /// Clear all previous account and payee filters
    #[arg(long = "cleared", short = 'C', global = true)]
    pub cleared: bool,

    /// Show only transactions with cleared postings
    #[arg(long = "uncleared", short = 'U', global = true)]
    pub uncleared: bool,

    /// Show only real (non-virtual) postings
    #[arg(long = "real", short = 'R', global = true)]
    pub real: bool,

    /// Show only actual (non-forecasted) postings
    #[arg(long = "actual", short = 'L', global = true)]
    pub actual: bool,

    /// Show transactions in daily periods
    #[arg(long = "daily", short = 'D', global = true)]
    pub daily: bool,

    /// Show transactions in weekly periods
    #[arg(long = "weekly", short = 'W', global = true)]
    pub weekly: bool,

    /// Show transactions in monthly periods
    #[arg(long = "monthly", short = 'M', global = true)]
    pub monthly: bool,

    /// Show transactions in quarterly periods
    #[arg(long = "quarterly", global = true)]
    pub quarterly: bool,

    /// Show transactions in yearly periods
    #[arg(long = "yearly", short = 'Y', global = true)]
    pub yearly: bool,

    /// Show running total
    #[arg(long = "total", short = 't', global = true)]
    pub total: Option<String>,

    /// Show running average
    #[arg(long = "average", short = 'A', global = true)]
    pub average: bool,

    /// Show cost basis
    #[arg(long = "basis", short = 'B', global = true)]
    pub basis: bool,

    /// Use market prices
    #[arg(long = "market", global = true)]
    pub market: bool,

    /// Use prices from this date
    #[arg(long = "exchange", short = 'X', global = true)]
    pub exchange: Option<String>,

    /// Enable color output
    #[arg(long = "color", global = true)]
    pub color: bool,

    /// Force color output even to non-terminals
    #[arg(long = "force-color", global = true)]
    pub force_color: bool,

    /// Disable color output
    #[arg(long = "no-color", global = true)]
    pub no_color: bool,

    /// Output width in columns
    #[arg(long = "columns", global = true)]
    pub columns: Option<usize>,

    /// Enable verbose output
    #[arg(long = "verbose", short = 'v', global = true)]
    pub verbose: bool,

    /// Enable debug output for specified category
    #[arg(long = "debug", global = true)]
    pub debug: Option<String>,

    /// Enable trace output at specified level
    #[arg(long = "trace", global = true)]
    pub trace: Option<u8>,

    /// Run script file
    #[arg(long = "script", global = true)]
    pub script: Option<String>,

    /// Show all options and their values
    #[arg(long = "options", global = true)]
    pub options: bool,

    /// Process only command-line arguments
    #[arg(long = "args-only", global = true)]
    pub args_only: bool,

    /// Verify memory allocations
    #[arg(long = "verify", global = true)]
    pub verify: bool,

    /// Verify memory allocations extensively
    #[arg(long = "verify-memory", global = true)]
    pub verify_memory: bool,

    /// Output to specified file
    #[arg(long = "output", short = 'o', global = true)]
    pub output: Option<String>,

    /// Use pager for output
    #[arg(long = "pager", global = true)]
    pub pager: Option<String>,

    /// Force pager even for non-terminals
    #[arg(long = "force-pager", global = true)]
    pub force_pager: bool,

    /// Format string for output
    #[arg(long = "format", short = 'F', global = true)]
    pub format: Option<String>,

    /// Display expression for amounts
    #[arg(long = "display", short = 'd', global = true)]
    pub display: Option<String>,

    /// The command to execute (or REPL if none)
    #[command(subcommand)]
    pub command: Option<Command>,
}

/// Available commands
#[derive(Subcommand)]
pub enum Command {
    /// Show account balances (aliases: bal, b)
    #[command(name = "balance", aliases = &["bal", "b"])]
    Balance(BalanceArgs),

    /// Show transaction register (aliases: reg, r)
    #[command(name = "register", aliases = &["reg", "r"])]
    Register(RegisterArgs),

    /// Print transactions (aliases: p)
    #[command(name = "print", aliases = &["p"])]
    Print(PrintArgs),

    /// Show account names
    #[command(name = "accounts")]
    Accounts(AccountsArgs),

    /// Show commodity names
    #[command(name = "commodities")]
    Commodities(CommoditiesArgs),

    /// Show payee names
    #[command(name = "payees")]
    Payees(PayeesArgs),

    /// Show tag names and values
    #[command(name = "tags")]
    Tags(TagsArgs),

    /// Generate CSV output
    #[command(name = "csv")]
    Csv(CsvArgs),

    /// Show cleared account balances
    #[command(name = "cleared")]
    Cleared(ClearedArgs),

    /// Show budget report
    #[command(name = "budget")]
    Budget(BudgetArgs),

    /// Generate equity postings
    #[command(name = "equity")]
    Equity(EquityArgs),

    /// Show price history
    #[command(name = "prices")]
    Prices(PricesArgs),

    /// Show price database
    #[command(name = "pricedb", aliases = &["pricesdb"])]
    Pricedb(PricedbArgs),

    /// Show price mapping
    #[command(name = "pricemap")]
    Pricemap(PricemapArgs),

    /// Show statistics about the ledger
    #[command(name = "stats", aliases = &["stat"])]
    Stats(StatsArgs),

    /// Generate a transaction template
    #[command(name = "xact", aliases = &["entry", "draft"])]
    Xact(XactArgs),

    /// Select specific postings
    #[command(name = "select")]
    Select(SelectArgs),

    /// Convert entries to different formats
    #[command(name = "convert")]
    Convert(ConvertArgs),

    /// Output in Emacs Lisp format
    #[command(name = "emacs", aliases = &["lisp"])]
    Emacs(EmacsArgs),

    /// Output in XML format
    #[command(name = "xml")]
    Xml(XmlArgs),

    // Pre-commands (don't read journal files)
    /// Parse and display a value expression
    #[command(name = "parse")]
    Parse(ParseArgs),

    /// Evaluate a value expression
    #[command(name = "eval")]
    Eval(EvalArgs),

    /// Parse and display a format string
    #[command(name = "format")]
    Format(FormatArgs),

    /// Parse and display a period expression
    #[command(name = "period")]
    Period(PeriodArgs),

    /// Parse and display a query expression
    #[command(name = "query")]
    Query(QueryArgs),

    /// Run a Ledger script
    #[command(name = "source", aliases = &["script"])]
    Source(SourceArgs),

    /// Generate sample transactions
    #[command(name = "generate")]
    Generate(GenerateArgs),

    /// Process a template
    #[command(name = "template")]
    Template(TemplateArgs),

    // Utility commands
    /// Generate shell completion scripts
    #[command(name = "completion")]
    Completion(CompletionArgs),

    /// Show help for various topics
    #[command(name = "help-topic")]
    HelpTopic(HelpTopicArgs),
}

impl Command {
    /// Check if the command is a pre-command that doesn't require journal files
    pub fn is_precommand(&self) -> bool {
        matches!(
            &self,
            Command::Parse(_)
                | Command::Eval(_)
                | Command::Format(_)
                | Command::Period(_)
                | Command::Query(_)
                | Command::Source(_)
                | Command::Generate(_)
                | Command::Template(_)
                | Command::Completion(_)
                | Command::HelpTopic(_)
        )
    }

    /// Get the command name as a string
    #[allow(dead_code)]
    pub fn command_name(&self) -> &'static str {
        match &self {
            Command::Balance(_) => "balance",
            Command::Register(_) => "register",
            Command::Print(_) => "print",
            Command::Accounts(_) => "accounts",
            Command::Commodities(_) => "commodities",
            Command::Payees(_) => "payees",
            Command::Tags(_) => "tags",
            Command::Csv(_) => "csv",
            Command::Cleared(_) => "cleared",
            Command::Budget(_) => "budget",
            Command::Equity(_) => "equity",
            Command::Prices(_) => "prices",
            Command::Pricedb(_) => "pricedb",
            Command::Pricemap(_) => "pricemap",
            Command::Stats(_) => "stats",
            Command::Xact(_) => "xact",
            Command::Select(_) => "select",
            Command::Convert(_) => "convert",
            Command::Emacs(_) => "emacs",
            Command::Xml(_) => "xml",
            Command::Parse(_) => "parse",
            Command::Eval(_) => "eval",
            Command::Format(_) => "format",
            Command::Period(_) => "period",
            Command::Query(_) => "query",
            Command::Source(_) => "source",
            Command::Generate(_) => "generate",
            Command::Template(_) => "template",
            Command::Completion(_) => "completion",
            Command::HelpTopic(_) => "help-topic",
        }
    }
}

// Command-specific argument structures

#[derive(Args)]
pub struct BalanceArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,

    /// Show empty accounts
    #[arg(long = "empty")]
    pub empty: bool,

    /// Show flat account listing
    #[arg(long = "flat")]
    pub flat: bool,

    /// Collapse accounts to specified depth
    #[arg(long = "depth")]
    pub depth: Option<usize>,

    /// Don't show accounts with zero balance
    #[arg(long = "no-total")]
    pub no_total: bool,

    /// Show accounts by percent of total
    #[arg(long = "percent")]
    pub percent: bool,

    /// Show accounts with inverted sign
    #[arg(long = "invert")]
    pub invert: bool,
}

#[derive(Args)]
pub struct RegisterArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,

    /// Show account totals
    #[arg(long = "subtotal")]
    pub subtotal: bool,

    /// Show only first N entries
    #[arg(long = "head")]
    pub head: Option<usize>,

    /// Show only last N entries
    #[arg(long = "tail")]
    pub tail: Option<usize>,

    /// Related postings
    #[arg(long = "related")]
    pub related: bool,

    /// Show wide format
    #[arg(long = "wide")]
    pub wide: bool,
}

#[derive(Args)]
pub struct PrintArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,

    /// Raw format (no pretty printing)
    #[arg(long = "raw")]
    pub raw: bool,

    /// Show first N entries
    #[arg(long = "head")]
    pub head: Option<usize>,

    /// Show last N entries
    #[arg(long = "tail")]
    pub tail: Option<usize>,
}

#[derive(Args)]
pub struct AccountsArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct CommoditiesArgs {
    /// Commodity pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct PayeesArgs {
    /// Payee pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct TagsArgs {
    /// Tag pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct CsvArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct ClearedArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct BudgetArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,

    /// Show unbudgeted accounts
    #[arg(long = "unbudgeted")]
    pub unbudgeted: bool,
}

#[derive(Args)]
pub struct EquityArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct PricesArgs {
    /// Commodity pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct PricedbArgs {
    /// Commodity pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct PricemapArgs {
    /// Commodity pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct StatsArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct XactArgs {
    /// Account name to use
    pub account: Option<String>,

    /// Payee name
    pub payee: Option<String>,
}

#[derive(Args)]
pub struct SelectArgs {
    /// Selection expression
    pub expression: String,

    /// Account pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct ConvertArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct EmacsArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,
}

#[derive(Args)]
pub struct XmlArgs {
    /// Account pattern to match
    pub pattern: Vec<String>,
}

// Pre-command arguments

#[derive(Args)]
pub struct ParseArgs {
    /// Expression to parse
    pub expression: String,
}

#[derive(Args)]
pub struct EvalArgs {
    /// Expression to evaluate
    pub expression: String,
}

#[derive(Args)]
pub struct FormatArgs {
    /// Format string to parse
    pub format_string: String,
}

#[derive(Args)]
pub struct PeriodArgs {
    /// Period expression to parse
    pub period: String,
}

#[derive(Args)]
pub struct QueryArgs {
    /// Query expression to parse
    pub query: String,
}

#[derive(Args)]
pub struct SourceArgs {
    /// Script file to run
    pub script_file: String,
}

#[derive(Args)]
pub struct GenerateArgs {
    /// Number of transactions to generate
    #[arg(short = 'n', long = "count", default_value = "10")]
    pub count: usize,

    /// Seed for random generation
    #[arg(long = "seed")]
    pub seed: Option<u64>,
}

#[derive(Args)]
pub struct TemplateArgs {
    /// Template file to process
    pub template_file: String,
}

// Utility command arguments

#[derive(Args)]
pub struct CompletionArgs {
    /// Shell to generate completions for
    #[arg(value_enum)]
    pub shell: Shell,

    /// Output directory for completion files
    #[arg(long = "output-dir")]
    pub output_dir: Option<String>,
}

#[derive(Args)]
pub struct HelpTopicArgs {
    /// Help topic to display
    #[arg(default_value = "general")]
    pub topic: String,
}
