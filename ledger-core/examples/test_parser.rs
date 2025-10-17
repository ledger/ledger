use ledger_core::parser::JournalParser;
use std::path::Path;

fn main() {
    // Test with a simple inline ledger
    let test_ledger = r#"2024/01/01 Opening Balance
    Assets:Checking     $1000.00
    Equity:Opening Balance

2024/01/05 Grocery Store
    Expenses:Food       $50.00
    Assets:Checking

2024/01/10 Paycheck
    Assets:Checking     $2500.00
    Income:Salary
"#;

    println!("Testing parser with sample ledger data...\n");

    let mut parser = JournalParser::new();

    match parser.parse_journal(test_ledger) {
        Ok(journal) => {
            println!("✓ Successfully parsed journal!");
            println!("  - Transactions: {}", journal.transactions.len());
            println!("  - Accounts: {}", journal.accounts.len());

            for (i, tx) in journal.transactions.iter().enumerate() {
                println!("\nTransaction {}:", i + 1);
                println!("  Date: {}", tx.date);
                println!("  Description: {}", tx.payee);
                println!("  Postings: {}", tx.postings.len());

                for (j, posting) in tx.postings.iter().enumerate() {
                    println!("    Posting {}: {}", j + 1, posting.account.borrow().name);
                    if let Some(ref amount) = posting.amount {
                        println!("      Amount: {:?}", amount);
                    } else {
                        println!("      Amount: (none - will be calculated)");
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("✗ Failed to parse journal: {}", e);
        }
    }

    // Test with actual sample file if it exists
    let sample_path = Path::new("test/input/sample.dat");
    if sample_path.exists() {
        println!("\n\nTesting with actual sample file: {:?}", sample_path);

        match parser.parse_file(sample_path) {
            Ok(journal) => {
                println!("✓ Successfully parsed sample.dat!");
                println!("  - Transactions: {}", journal.transactions.len());
                println!("  - Accounts: {}", journal.accounts.len());

                // Print first few transactions as examples
                for (i, tx) in journal.transactions.iter().take(3).enumerate() {
                    println!("\n  Transaction {}:", i + 1);
                    println!("    Date: {}", tx.date);
                    println!("    Description: {}", tx.payee);
                    println!("    Postings: {}", tx.postings.len());

                    for (j, posting) in tx.postings.iter().enumerate() {
                        println!("      Posting {}: {}", j + 1, posting.account.borrow().name);
                        if let Some(ref amount) = posting.amount {
                            println!("        Amount: {:?}", amount);
                        } else {
                            println!("        Amount: (none - will be calculated)");
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!("✗ Failed to parse sample.dat: {}", e);
            }
        }
    } else {
        println!("\nSample file not found at: {:?}", sample_path);
    }
}
