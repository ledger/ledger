//! Command-line interface for Ledger accounting system
//!
//! This is the main entry point for the Rust implementation of Ledger,
//! a powerful command-line double-entry accounting system.

use anyhow::Result;
use clap::Parser;
use std::env;
use std::process;

mod cli;
mod completion;
mod dispatch;
mod help;
mod session;
#[allow(dead_code)]
mod test_framework;

use cli::Cli;
use dispatch::Dispatcher;
use session::Session;

fn main() -> Result<()> {
    // Initialize logging
    env_logger::init();

    // Handle debug options early (similar to C++ version)
    handle_debug_options();

    // Parse command-line arguments
    let cli = Cli::parse();

    // Create session with configuration
    let session = match Session::new(&cli) {
        Ok(session) => session,
        Err(e) => {
            eprintln!("Error initializing session: {}", e);
            process::exit(1);
        }
    };

    // Create dispatcher and execute command
    let mut dispatcher = Dispatcher::new(session);

    match dispatcher.execute(&cli) {
        Ok(exit_code) => process::exit(exit_code),
        Err(e) => {
            eprintln!("Error: {}", e);

            // Show error chain if in verbose mode
            if cli.verbose {
                let mut cause = e.source();
                while let Some(err) = cause {
                    eprintln!("Caused by: {}", err);
                    cause = err.source();
                }
            }

            process::exit(1);
        }
    }
}

/// Handle early debug options that affect environment setup
/// This mirrors the handle_debug_options function in the C++ version
fn handle_debug_options() {
    let args: Vec<String> = env::args().collect();

    for (i, arg) in args.iter().enumerate() {
        match arg.as_str() {
            "--verify-memory" => {
                eprintln!("Note: Memory verification is not implemented in Rust version");
            }
            "--verify" => {
                // Verification is automatic in Rust due to memory safety
            }
            "--verbose" | "-v" => {
                // Verbose logging will be handled by env_logger
            }
            "--debug" => {
                if i + 1 < args.len() {
                    env::set_var("RUST_LOG", format!("debug,ledger_cli={}", &args[i + 1]));
                } else {
                    env::set_var("RUST_LOG", "debug");
                }
            }
            "--trace" => {
                if i + 1 < args.len() {
                    env::set_var("RUST_LOG", "trace");
                } else {
                    env::set_var("RUST_LOG", "trace");
                }
            }
            _ => {}
        }
    }
}
