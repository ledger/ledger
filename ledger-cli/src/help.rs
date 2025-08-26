//! Help system for ledger CLI
//! 
//! This module provides comprehensive help documentation matching the C++ version

use std::fmt;

/// Help topics available in the ledger CLI
#[derive(Debug, Clone)]
pub enum HelpTopic {
    /// General usage help
    General,
    /// Command-specific help
    Command(String),
    /// Option help
    Options,
    /// Expression syntax help  
    Expressions,
    /// Period syntax help
    Periods,
    /// Format strings help
    Formats,
    /// Query syntax help
    Queries,
}

impl HelpTopic {
    /// Get help text for the topic
    pub fn get_help_text(&self) -> String {
        match self {
            HelpTopic::General => GENERAL_HELP.to_string(),
            HelpTopic::Command(cmd) => get_command_help(cmd),
            HelpTopic::Options => OPTIONS_HELP.to_string(),
            HelpTopic::Expressions => EXPRESSIONS_HELP.to_string(),
            HelpTopic::Periods => PERIODS_HELP.to_string(),
            HelpTopic::Formats => FORMATS_HELP.to_string(),
            HelpTopic::Queries => QUERIES_HELP.to_string(),
        }
    }
}

impl fmt::Display for HelpTopic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get_help_text())
    }
}

const GENERAL_HELP: &str = r#"
OVERVIEW:
Ledger is a powerful, command-line accounting tool that provides a double-entry
accounting system for UNIX systems.

USAGE:
    ledger [global-options] COMMAND [command-options] [ARGS]
    ledger [global-options]                # Enter REPL mode

EXAMPLES:
    ledger balance                         # Show account balances
    ledger register expenses               # Show expense register
    ledger -f data.ledger bal assets       # Use specific file
    ledger -b 2023-01-01 -e 2023-12-31 reg # Date range

GLOBAL OPTIONS:
    -f, --file FILE                        Input ledger file
    -b, --begin DATE                       Begin date
    -e, --end DATE                         End date  
    -v, --verbose                          Verbose output
    --help                                 Show help
    --version                              Show version

MAIN COMMANDS:
    balance, bal, b                        Show account balances
    register, reg, r                       Show transaction register
    print, p                               Print transactions
    accounts                               List account names
    payees                                 List payee names
    commodities                            List commodities
    stats                                  Show ledger statistics

PRE-COMMANDS (don't read journal):
    parse EXPR                             Parse value expression
    eval EXPR                              Evaluate expression
    format FORMAT                          Parse format string
    
For detailed help on any command, use:
    ledger COMMAND --help
    
For more information, see the manual page: man ledger
"#;

const OPTIONS_HELP: &str = r#"
GLOBAL OPTIONS:
These options can be used with any command:

FILE AND DATA OPTIONS:
    -f, --file FILE               Input ledger file(s)
    -i, --init-file FILE          Use alternate ledger init file
    --args-only                   Don't process environment or init files

DATE AND PERIOD OPTIONS:
    -b, --begin DATE              Begin date (YYYY-MM-DD, etc.)
    -e, --end DATE                End date
    -c, --current                 Show only current period
    -D, --daily                   Group by day
    -W, --weekly                  Group by week  
    -M, --monthly                 Group by month
    -Y, --yearly                  Group by year
    --quarterly                   Group by quarter

FILTERING OPTIONS:
    -C, --cleared                 Show only cleared transactions
    -U, --uncleared              Show only uncleared transactions
    -R, --real                   Show only real (non-virtual) postings
    -L, --actual                 Show only actual (non-forecasted) postings

CALCULATION OPTIONS:
    -t, --total EXPR             Show running total
    -A, --average                Show running average
    -B, --basis                  Show cost basis
    --market                     Use current market prices
    -X, --exchange COMMODITY     Convert to commodity

FORMATTING OPTIONS:
    --color                      Force color output
    --no-color                   Disable color output
    --columns N                  Set output width
    -F, --format FORMAT          Use custom format string
    -d, --display EXPR           Display expression
    -o, --output FILE            Write output to file
    --pager PAGER               Use pager for output

SORTING AND LIMITING:
    -S, --sort EXPR              Sort by value expression
    -l, --limit N                Limit to N entries
    --head N                     Show first N entries
    --tail N                     Show last N entries

DEBUG OPTIONS:
    -v, --verbose                Enable verbose output
    --debug CATEGORY             Enable debug output
    --trace LEVEL                Enable trace output
    --verify                     Verify calculations
    --options                    Show all options
"#;

const EXPRESSIONS_HELP: &str = r#"
VALUE EXPRESSIONS:
Ledger supports powerful value expressions for filtering, sorting, and display.

BASIC OPERATORS:
    +, -, *, /                   Arithmetic operators
    <, <=, >, >=                 Comparison operators  
    ==, !=                       Equality operators
    =~, !~                       Regular expression matching
    &, |                         Logical AND, OR
    not, !                       Logical NOT

ACCOUNT MATCHING:
    account =~ /regex/           Match account by regex
    account == "Assets"          Exact account match
    depth(account) <= 2          Account depth

PAYEE AND DESCRIPTION:
    payee =~ /grocery/           Match payee by regex
    note =~ /pattern/            Match transaction note

DATE EXPRESSIONS:
    date >= [2023-01-01]         Date comparison
    date =~ /2023/               Date regex match
    
AMOUNT EXPRESSIONS:
    amount > $100                Amount comparison
    abs(amount) > $50            Absolute value
    
POSTING ATTRIBUTES:
    cleared                      Posting is cleared
    pending                      Posting is pending
    real                         Posting is real (not virtual)
    
FUNCTIONS:
    abs(EXPR)                    Absolute value
    round(EXPR)                  Round to nearest integer
    floor(EXPR)                  Round down
    ceil(EXPR)                   Round up
    depth(account)               Account depth
    parent(account)              Parent account
    
EXAMPLES:
    account =~ /^Expenses/ and amount > $50
    payee =~ /grocery/i and date >= [2023-01-01]
    cleared and abs(amount) > $100
"#;

const PERIODS_HELP: &str = r#"
PERIOD EXPRESSIONS:
Ledger supports flexible period specifications for date ranges.

BASIC FORMATS:
    2023                         Entire year 2023
    2023-03                      March 2023
    2023-03-15                   Specific date
    march                        Current March (or last/next)
    
RANGES:
    2023-01-01 to 2023-12-31     Explicit range
    from 2023-01-01              From date onwards
    to 2023-12-31                Up to date
    
RELATIVE PERIODS:
    today                        Today only
    yesterday                    Yesterday only
    this week                    Current week
    last week                    Previous week
    next month                   Following month
    this year                    Current year
    
RECURRING PERIODS:
    daily                        Each day
    weekly                       Each week
    monthly                      Each month
    quarterly                    Each quarter
    yearly                       Each year
    
PERIOD ARITHMETIC:
    2023 + 1 month               January 2023 plus one month
    this month - 1 year          Same month last year
    
EXAMPLES:
    --begin 2023-01-01 --end 2023-12-31
    --period "from 2023"
    --period "this year"
    --period "monthly from 2023"
    --weekly --begin "3 months ago"
"#;

const FORMATS_HELP: &str = r#"
FORMAT STRINGS:
Ledger uses format strings to control output appearance.

BASIC SYNTAX:
    %[-][width][.precision]specifier
    
    -                            Left justify
    width                        Minimum field width  
    .precision                   Decimal precision
    
ACCOUNT FORMATS:
    %a                           Full account name
    %A                           Partial account name
    %t                           Account total
    %T                           Account and children total
    
POSTING FORMATS:
    %d                           Date (posting date)
    %D                           Date (aux date if present)
    %S                           Date (effective date)
    
    %p                           Payee
    %P                           Payee with account
    %n                           Note/description
    
    %l                           Amount (posting amount)  
    %L                           Amount (total)
    %t                           Amount (running total)
    %T                           Amount (display total)
    
SPACING AND ALIGNMENT:
    %/                           Show only if not zero
    %(format)                    Conditional format
    %$                           Right justify to terminal width
    
COLORS:
    %[red]...%[normal]          Color text red
    %[bold]...%[normal]         Bold text
    
EXAMPLES:
    "%d %p %l\n"                 Simple register format
    "%-20a %12t\n"              Balance format
    "%d %-30p %12l %12t\n"      Wide register format
    "%[bold]%a%[normal] %t\n"    Bold account names
"#;

const QUERIES_HELP: &str = r#"
QUERY EXPRESSIONS:
Queries filter transactions and postings using logical expressions.

BASIC SYNTAX:
    Query expressions are boolean expressions that return true/false
    Multiple expressions can be combined with 'and', 'or', 'not'
    
ACCOUNT QUERIES:
    expenses                     Accounts starting with 'expenses'
    ^assets                      Accounts starting with 'assets'
    checking$                    Accounts ending with 'checking'
    /grocery/                    Accounts matching regex
    
PAYEE QUERIES:  
    @safeway                     Payee containing 'safeway'
    @/grocery store/             Payee matching regex
    
DATE QUERIES:
    2023                         Year 2023
    2023/03                      March 2023  
    [2023-01-01]                 Specific date
    [this month]                 This month
    
AMOUNT QUERIES:
    =100                         Exact amount $100
    >50                          Greater than $50
    <=-25.50                     Less than or equal to -$25.50
    
METADATA QUERIES:
    %pending                     Pending transactions
    %cleared                     Cleared transactions
    %uncleared                   Uncleared transactions
    
COMPLEX EXAMPLES:
    expenses and >50             Expenses over $50
    @grocery and 2023/03         Grocery shopping in March 2023
    ^assets and not checking     Asset accounts except checking
    %cleared and expenses        Cleared expense transactions
    
COMBINING QUERIES:
    Multiple query terms are combined with AND by default:
        expenses food            = expenses AND food
        
    Use explicit operators for other logic:
        expenses or assets       = expenses OR assets
        not expenses            = NOT expenses
        expenses and not food   = expenses AND NOT food
"#;

/// Get help text for a specific command
pub fn get_command_help(command: &str) -> String {
    match command.to_lowercase().as_str() {
        "balance" | "bal" | "b" => BALANCE_HELP.to_string(),
        "register" | "reg" | "r" => REGISTER_HELP.to_string(), 
        "print" | "p" => PRINT_HELP.to_string(),
        "accounts" => ACCOUNTS_HELP.to_string(),
        "payees" => PAYEES_HELP.to_string(),
        "commodities" => COMMODITIES_HELP.to_string(),
        "stats" => STATS_HELP.to_string(),
        _ => format!("No specific help available for command: {}", command),
    }
}

const BALANCE_HELP: &str = r#"
BALANCE COMMAND:
Show account balances in a hierarchical tree format.

USAGE:
    ledger balance [options] [account-patterns]

ALIASES: bal, b

DESCRIPTION:
The balance command shows the current balance for all accounts, or for those 
accounts matching the given patterns. Accounts are displayed in a tree format
showing the hierarchical relationship.

OPTIONS:
    --flat                       Show accounts in flat list
    --depth N                    Limit depth of account tree
    --empty                      Show accounts with zero balance
    --no-total                   Don't show total line
    --percent                    Show percentages of total
    --invert                     Invert the balances shown

EXAMPLES:
    ledger balance               Show all account balances
    ledger bal assets            Show asset account balances  
    ledger balance expenses --monthly
                                 Show monthly expense balances
    ledger bal --depth 2         Show balances to depth 2
"#;

const REGISTER_HELP: &str = r#"
REGISTER COMMAND:
Show a register of all transactions affecting the selected accounts.

USAGE:
    ledger register [options] [account-patterns]
    
ALIASES: reg, r

DESCRIPTION:
The register command displays all postings that match the given account patterns,
showing date, payee, account, amount, and running total.

OPTIONS:
    --subtotal                   Show subtotal for each account
    --related                    Show related postings
    --wide                       Use wide format
    --head N                     Show first N postings
    --tail N                     Show last N postings

EXAMPLES:
    ledger register              Show all postings
    ledger reg expenses          Show all expense postings
    ledger reg checking --monthly
                                 Show checking account by month
    ledger reg --related assets  Show assets and related postings
"#;

const PRINT_HELP: &str = r#"
PRINT COMMAND:
Print transactions in ledger format.

USAGE:
    ledger print [options] [patterns]
    
ALIASES: p

DESCRIPTION:
The print command outputs transactions in standard ledger format, suitable for 
creating new ledger files or reviewing transaction details.

OPTIONS:
    --raw                        Print in raw format
    --head N                     Print first N transactions
    --tail N                     Print last N transactions

EXAMPLES:
    ledger print                 Print all transactions
    ledger print expenses        Print expense transactions
    ledger print --raw           Print without formatting
"#;

const ACCOUNTS_HELP: &str = r#"
ACCOUNTS COMMAND:
List all account names found in the journal.

USAGE:
    ledger accounts [account-patterns]
    
DESCRIPTION:
Shows a list of all account names, one per line. Useful for shell scripting
and getting a quick overview of your chart of accounts.

EXAMPLES:
    ledger accounts              List all accounts
    ledger accounts assets       List asset accounts
"#;

const PAYEES_HELP: &str = r#"
PAYEES COMMAND:
List all payee names found in the journal.

USAGE:
    ledger payees [payee-patterns]
    
DESCRIPTION:
Shows a list of all payee names, one per line. Useful for reviewing and 
standardizing payee names.

EXAMPLES:
    ledger payees                List all payees
    ledger payees grocery        List payees matching 'grocery'
"#;

const COMMODITIES_HELP: &str = r#"
COMMODITIES COMMAND:
List all commodities (currencies) found in the journal.

USAGE:
    ledger commodities [commodity-patterns]
    
DESCRIPTION:
Shows all commodity symbols used in the journal, such as currency codes
and stock symbols.

EXAMPLES:
    ledger commodities           List all commodities
    ledger commodities '^[A-Z]{3}$'
                                 List 3-letter currency codes
"#;

const STATS_HELP: &str = r#"
STATS COMMAND:
Show statistics about the journal file.

USAGE:
    ledger stats
    
ALIASES: stat

DESCRIPTION:
Displays various statistics about the journal, including number of transactions,
postings, accounts, and file information.

EXAMPLE:
    ledger stats                 Show journal statistics
"#;

/// Show manual page equivalent help
pub fn show_man_page() {
    println!("{}", MAN_PAGE_CONTENT);
}

const MAN_PAGE_CONTENT: &str = r#"
LEDGER(1)                        User Commands                       LEDGER(1)

NAME
       ledger - a powerful double-entry accounting system

SYNOPSIS
       ledger [global-options] command [command-options] [arguments]

DESCRIPTION
       Ledger  is  a  powerful,  double-entry accounting system that is accessed
       from the UNIX command-line.  This may put off some users, since there is
       no  flashy  UI,  but  for those who want unparalleled reporting access to
       their data there are few alternatives.

GLOBAL OPTIONS
       -f, --file FILE
              Use FILE as the input ledger file.

       -b, --begin DATE
              Begin the report from this date.

       -e, --end DATE
              End the report on this date.

       --verbose
              Show verbose output during processing.

COMMANDS
       balance [account-patterns]
              Show account balances.

       register [account-patterns]  
              Show transaction register.

       print [patterns]
              Print transactions in ledger format.

       accounts [patterns]
              List account names.

EXAMPLES
       Show all account balances:
              ledger balance

       Show expense register for 2023:
              ledger -b 2023-01-01 -e 2023-12-31 register expenses

AUTHOR
       Written by John Wiegley.

REPORTING BUGS
       Report bugs to <johnw@newartisans.com>

COPYRIGHT
       Copyright © 2003-2024 John Wiegley.  All rights reserved.
"#;