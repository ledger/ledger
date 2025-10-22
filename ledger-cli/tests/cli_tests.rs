//! CLI integration tests
//!
//! Comprehensive tests for the command-line interface to ensure compatibility
//! with the C++ version and proper functionality of all options and commands.

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Test basic command help
#[test]
fn test_help_output() {
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("A powerful command-line double-entry accounting system"))
        .stdout(predicate::str::contains("balance"))
        .stdout(predicate::str::contains("register"))
        .stdout(predicate::str::contains("print"));
}

/// Test version output
#[test]
fn test_version_output() {
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.arg("--version");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("ledger"))
        .stdout(predicate::str::contains("3.5.0"));
}

/// Test all main commands are recognized
#[test]
fn test_main_commands() {
    let commands = [
        "balance",
        "bal",
        "b",
        "register",
        "reg",
        "r",
        "print",
        "p",
        "accounts",
        "commodities",
        "payees",
        "tags",
        "csv",
        "cleared",
        "budget",
        "equity",
        "prices",
        "pricedb",
        "pricemap",
        "stats",
        "xact",
        "select",
        "convert",
        "emacs",
        "xml",
    ];

    for command in &commands {
        let mut cmd = Command::cargo_bin("ledger").unwrap();
        cmd.arg(command).arg("--help");
        cmd.assert().success().stdout(predicate::str::contains("Usage:"));
    }
}

/// Test pre-commands (don't require journal files)
#[test]
fn test_precommands() {
    let precommands = [
        ("parse", "amount"),
        ("eval", "1+1"),
        ("format", "%d %p"),
        ("period", "this month"),
        ("query", "expenses"),
        ("generate", ""),
    ];

    for (command, arg) in &precommands {
        let mut cmd = Command::cargo_bin("ledger").unwrap();
        if arg.is_empty() {
            cmd.arg(command);
        } else {
            cmd.arg(command).arg(arg);
        }
        cmd.assert().success();
    }
}

/// Test global options are recognized
#[test]
fn test_global_options() {
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["--verbose", "--debug", "memory", "parse", "amount"]);
    cmd.assert().success();

    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["--begin", "2023-01-01", "--end", "2023-12-31", "parse", "amount"]);
    cmd.assert().success();

    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["--monthly", "--yearly", "parse", "amount"]);
    cmd.assert().success();
}

/// Test option parsing with various formats
#[test]
fn test_option_formats() {
    // Short options
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["-b", "2023-01-01", "-e", "2023-12-31", "parse", "amount"]);
    cmd.assert().success();

    // Long options with =
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["--begin=2023-01-01", "--end=2023-12-31", "parse", "amount"]);
    cmd.assert().success();

    // Mixed formats
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["-v", "--debug=memory", "--limit", "10", "parse", "amount"]);
    cmd.assert().success();
}

/// Test command-specific options
#[test]
fn test_command_specific_options() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let ledger_file = temp_dir.path().join("test.ledger");

    fs::write(&ledger_file,
        "2023-01-01 Opening Balance\n  Assets:Bank  $1000\n  Equity:Opening Balance\n\n2023-01-15 Grocery Store\n  Expenses:Food  $45.67\n  Assets:Bank\n"
    )?;

    // Balance command options
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&[
        "-f",
        ledger_file.to_str().unwrap(),
        "balance",
        "--empty",
        "--flat",
        "--depth",
        "2",
        "assets",
    ]);
    cmd.assert().success();

    // Register command options
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&[
        "-f",
        ledger_file.to_str().unwrap(),
        "register",
        "--subtotal",
        "--wide",
        "expenses",
    ]);
    cmd.assert().success();

    // Print command options
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["-f", ledger_file.to_str().unwrap(), "print", "--raw", "--head", "5"]);
    cmd.assert().success();

    Ok(())
}

/// Test error handling for invalid options
#[test]
fn test_invalid_options() {
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.arg("--invalid-option");
    cmd.assert().failure().stderr(predicate::str::contains("error:"));

    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.arg("invalid-command");
    cmd.assert().failure().stderr(predicate::str::contains("error:"));
}

/// Test shell completion generation
#[test]
fn test_completion_generation() {
    let shells = ["bash", "zsh", "fish", "powershell"];

    for shell in &shells {
        let mut cmd = Command::cargo_bin("ledger").unwrap();
        cmd.args(&["completion", shell]);
        cmd.assert().success().stdout(predicate::str::contains("ledger"));
    }
}

/// Test help topics
#[test]
fn test_help_topics() {
    let topics = ["options", "expressions", "periods", "formats", "queries"];

    for topic in &topics {
        let mut cmd = Command::cargo_bin("ledger").unwrap();
        cmd.args(&["help-topic", topic]);
        cmd.assert().success().stdout(predicate::str::contains(topic.to_uppercase()));
    }
}

/// Test configuration file handling
#[test]
fn test_config_file_handling() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let config_file = temp_dir.path().join("test.ledgerrc");

    fs::write(&config_file, "--monthly\n--depth 3\n")?;

    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["--init-file", config_file.to_str().unwrap(), "parse", "amount"]);
    cmd.assert().success();

    Ok(())
}

/// Test environment variable handling
#[test]
fn test_environment_variables() {
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.env("LEDGER_FILE", "/tmp/test.ledger");
    cmd.env("LEDGER_INIT", "/tmp/test.ledgerrc");
    cmd.arg("parse").arg("amount");
    cmd.assert().success();
}

/// Test file arguments
#[test]
fn test_file_arguments() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let ledger_file = temp_dir.path().join("test.ledger");

    fs::write(&ledger_file, "2023-01-01 Test\n  Assets:Bank  $100\n  Expenses:Test\n")?;

    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["-f", ledger_file.to_str().unwrap(), "parse", "amount"]);
    cmd.assert().success();

    Ok(())
}

/// Test date parsing
#[test]
fn test_date_parsing() {
    let date_formats = ["2023-01-01", "2023/01/01", "01/01/2023", "2023-01", "today", "yesterday"];

    for date in &date_formats {
        let mut cmd = Command::cargo_bin("ledger").unwrap();
        cmd.args(&["--begin", date, "parse", "amount"]);
        cmd.assert().success();
    }
}

/// Test option precedence (command line over config file over environment)
#[test]
fn test_option_precedence() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let config_file = temp_dir.path().join("test.ledgerrc");

    fs::write(&config_file, "--monthly\n")?;

    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.env("LEDGER_MONTHLY", "1"); // Environment variable
    cmd.args(&[
        "--init-file",
        config_file.to_str().unwrap(), // Config file
        "--yearly",                    // Command line (should override)
        "parse",
        "amount",
    ]);
    cmd.assert().success();

    Ok(())
}

/// Test REPL mode entry when no command is specified
#[test]
fn test_repl_mode_entry() -> Result<(), Box<dyn std::error::Error>> {
    let temp_dir = TempDir::new()?;
    let ledger_file = temp_dir.path().join("test.ledger");

    fs::write(
        &ledger_file,
        "2023-01-01 Opening Balance\n  Assets:Bank  $1000\n  Equity:Opening Balance\n",
    )?;

    // This test is tricky as REPL mode waits for input
    // For now, just test that version info is shown
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&["-f", ledger_file.to_str().unwrap()]);
    cmd.write_stdin("quit\n");
    cmd.assert().success().stdout(predicate::str::contains("Ledger"));

    Ok(())
}

/// Test that all documented aliases work
#[test]
fn test_command_aliases() {
    let aliases = [
        ("bal", "balance"),
        ("b", "balance"),
        ("reg", "register"),
        ("r", "register"),
        ("p", "print"),
        ("stat", "stats"),
    ];

    for (alias, _full_name) in &aliases {
        let mut cmd = Command::cargo_bin("ledger").unwrap();
        cmd.arg(alias).arg("--help");
        cmd.assert().success().stdout(predicate::str::contains("Usage:"));
    }
}

/// Test combination of multiple global options
#[test]
fn test_complex_option_combinations() {
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.args(&[
        "-f",
        "/tmp/nonexistent.ledger",
        "--begin",
        "2023-01-01",
        "--end",
        "2023-12-31",
        "--monthly",
        "--cleared",
        "--real",
        "--sort",
        "date",
        "--limit",
        "10",
        "--format",
        "%d %p %l",
        "--verbose",
        "parse",
        "amount",
    ]);
    cmd.assert().success();
}

/// Benchmark CLI parsing performance
#[test]
fn test_cli_parsing_performance() {
    use std::time::Instant;

    let start = Instant::now();
    for _ in 0..100 {
        let mut cmd = Command::cargo_bin("ledger").unwrap();
        cmd.args(&["--help"]);
        cmd.assert().success();
    }
    let elapsed = start.elapsed();

    // Should be fast - less than 5 seconds for 100 iterations
    assert!(elapsed.as_secs() < 5, "CLI parsing too slow: {:?}", elapsed);
}

/// Test that options show proper help text
#[test]
fn test_options_help_completeness() {
    let mut cmd = Command::cargo_bin("ledger").unwrap();
    cmd.arg("--options");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Global scope options"))
        .stdout(predicate::str::contains("Session scope options"))
        .stdout(predicate::str::contains("Report scope options"));
}
