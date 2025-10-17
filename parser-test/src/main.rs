//! Test program to parse real ledger files and report results

use std::env;
use std::fs;
use std::path::Path;

use ledger_core::parser::JournalParser;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <ledger_file>", args[0]);
        eprintln!("       cargo run --bin test_parser <ledger_file>");
        std::process::exit(1);
    }

    let file_path = &args[1];

    if !Path::new(file_path).exists() {
        eprintln!("Error: File '{}' not found", file_path);
        std::process::exit(1);
    }

    println!("Testing Rust Ledger parser with file: {}", file_path);
    println!("{}", "=".repeat(60));

    // Create parser and attempt to parse
    let mut parser = JournalParser::with_file(file_path);

    match parser.parse_file(file_path) {
        Ok(journal) => {
            println!("✓ Successfully parsed ledger file!");
            println!();

            // Report statistics
            println!("Statistics:");
            println!("-----------");
            println!("Transactions: {}", journal.transactions.len());
            println!("Accounts: {}", journal.accounts.len());
            println!("Commodities: {}", journal.commodities.len());

            // Show sample transactions
            if !journal.transactions.is_empty() {
                println!();
                println!("Sample transactions (first 5):");
                println!("{}", "-".repeat(30));

                for (i, transaction) in journal.transactions.iter().take(5).enumerate() {
                    println!(
                        "{}: {} - {} (postings: {})",
                        i + 1,
                        transaction.date.format("%Y-%m-%d"),
                        transaction.payee,
                        transaction.postings.len()
                    );

                    // Show postings for first transaction
                    if i == 0 {
                        for posting in &transaction.postings {
                            let account_ref = posting.account.borrow();
                            let account_name = account_ref.name.as_str();
                            let amount_str = if let Some(ref amount) = posting.amount {
                                format!(" {}", amount)
                            } else {
                                " [auto-balanced]".to_string()
                            };
                            println!("  {}{}", account_name, amount_str);
                        }
                    }
                }
            }

            // Show account names
            if !journal.accounts.is_empty() {
                println!();
                println!("Sample accounts (first 10):");
                println!("{}", "-".repeat(28));

                for (i, account) in journal.accounts.values().take(10).enumerate() {
                    let acc = account.borrow();
                    println!("{}: {}", i + 1, acc.name.as_str());
                }
            }

            // Show commodities
            if !journal.commodities.is_empty() {
                println!();
                println!("Commodities found:");
                println!("------------------");

                for commodity in journal.commodities.values() {
                    println!("- {}", commodity.symbol());
                }
            }
        }
        Err(e) => {
            println!("✗ Failed to parse ledger file");
            println!();
            println!("Error Details:");
            println!("{}", "-".repeat(14));
            println!("{}", e);

            // Try to provide helpful information
            if let Ok(contents) = fs::read_to_string(file_path) {
                let lines: Vec<&str> = contents.lines().collect();
                println!();
                println!("File has {} lines", lines.len());

                // Show first few lines for context
                println!();
                println!("First 10 lines of file:");
                println!("{}", "-".repeat(23));
                for (i, line) in lines.iter().take(10).enumerate() {
                    println!("{:3}: {}", i + 1, line);
                }
            }

            std::process::exit(1);
        }
    }

    Ok(())
}
