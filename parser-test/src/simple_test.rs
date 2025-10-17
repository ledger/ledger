//! Simple test to understand parser behavior and check account/commodity registration

use ledger_core::parser::JournalParser;

fn main() {
    let test_ledger = r#"
account Assets:Checking

2024/01/01 * Test transaction
    Assets:Checking     $100.00
    Expenses:Food

2024/01/02 Another transaction  
    Assets:Checking     €200.00
    Expenses:Travel     -€200.00

commodity USD
    format $1,000.00

commodity EUR
    format 1.000,00 €
"#;

    println!("Testing ledger with accounts and commodities:");
    println!("{}", test_ledger);
    println!("{}", "=".repeat(60));

    let mut parser = JournalParser::new();

    match parser.parse_journal(test_ledger) {
        Ok(journal) => {
            println!("✓ Parsed successfully!");
            println!("Transactions: {}", journal.transactions.len());
            println!("Accounts found: {}", journal.accounts.len());
            println!("Commodities found: {}", journal.commodities.len());

            // List accounts
            println!("\nAccounts:");
            for (name, _) in &journal.accounts {
                println!("  {}", name);
            }

            // List commodities
            println!("\nCommodities:");
            for (symbol, _) in &journal.commodities {
                println!("  {}", symbol);
            }

            // Show transaction details
            println!("\nTransaction details:");
            for (i, transaction) in journal.transactions.iter().enumerate() {
                println!(
                    "Transaction {}: {} - {}",
                    i + 1,
                    transaction.date.format("%Y-%m-%d"),
                    transaction.payee
                );

                for posting in &transaction.postings {
                    let account = posting.account.borrow();
                    println!("  Account: {}", account.name);
                    match &posting.amount {
                        Some(amount) => println!("    Amount: {}", amount),
                        None => println!("    Amount: [auto-balanced]"),
                    }
                }
            }
        }
        Err(e) => {
            println!("✗ Parse failed: {}", e);
        }
    }
}
