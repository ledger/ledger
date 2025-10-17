//! Command-line interface library for Ledger
//!
//! This library provides all the command-line interface functionality for the Rust
//! implementation of Ledger, including argument parsing, session management, and
//! command dispatch.

pub mod cli;
pub mod completion;
pub mod dispatch;
pub mod help;
pub mod session;
#[allow(dead_code)]
pub mod test_framework;

pub use cli::Cli;
pub use dispatch::Dispatcher;
pub use session::Session;
