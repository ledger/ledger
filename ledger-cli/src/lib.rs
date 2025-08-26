//! Command-line interface library for Ledger
//! 
//! This library provides all the command-line interface functionality for the Rust
//! implementation of Ledger, including argument parsing, session management, and
//! command dispatch.

pub mod cli;
pub mod session;
pub mod dispatch;
pub mod completion;
pub mod help;
pub mod test_framework;

pub use cli::Cli;
pub use session::Session;
pub use dispatch::Dispatcher;