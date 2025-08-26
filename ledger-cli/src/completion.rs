//! Shell completion support for ledger CLI
//! 
//! This module provides shell completion generation for bash, zsh, fish, and PowerShell

use clap::Command;
use clap_complete::{generate, Generator, Shell};
use std::io;

/// Generate shell completion script for the given shell
pub fn generate_completion<G: Generator>(gen: G, cmd: &mut Command) {
    generate(gen, cmd, cmd.get_name().to_string(), &mut io::stdout());
}

/// Print completion scripts for all supported shells
pub fn print_all_completions(cmd: &mut Command) {
    println!("# Bash completion");
    generate_completion(Shell::Bash, cmd);
    
    println!("\n\n# Zsh completion");
    generate_completion(Shell::Zsh, cmd);
    
    println!("\n\n# Fish completion");
    generate_completion(Shell::Fish, cmd);
    
    println!("\n\n# PowerShell completion");
    generate_completion(Shell::PowerShell, cmd);
}

/// Generate and save completion scripts to files
pub fn save_completions(shell: Shell, cmd: &mut Command, output_dir: &std::path::Path) -> Result<(), Box<dyn std::error::Error>> {
    let file_name = match shell {
        Shell::Bash => "ledger.bash",
        Shell::Zsh => "_ledger",
        Shell::Fish => "ledger.fish", 
        Shell::PowerShell => "_ledger.ps1",
        _ => return Err("Unsupported shell".into()),
    };
    
    let output_path = output_dir.join(file_name);
    let mut file = std::fs::File::create(&output_path)?;
    
    generate(shell, cmd, "ledger".to_string(), &mut file);
    
    println!("Saved {} completion to {}", 
             shell.to_string().to_lowercase(), 
             output_path.display());
    
    Ok(())
}

/// Custom completion logic for account names and payees
/// This would integrate with the journal parser to provide dynamic completions
pub fn get_dynamic_completions(_current_input: &str) -> Vec<String> {
    // TODO: Implement dynamic completion by parsing journal files
    // This would provide completions for:
    // - Account names
    // - Payee names  
    // - Commodity codes
    // - Tag names
    
    // For now, return some example completions
    vec![
        "Assets".to_string(),
        "Assets:Bank".to_string(),
        "Assets:Bank:Checking".to_string(),
        "Assets:Bank:Savings".to_string(),
        "Expenses".to_string(),
        "Expenses:Food".to_string(),
        "Expenses:Rent".to_string(),
        "Income".to_string(),
        "Income:Salary".to_string(),
        "Liabilities".to_string(),
        "Liabilities:Credit".to_string(),
        "Equity".to_string(),
    ]
}