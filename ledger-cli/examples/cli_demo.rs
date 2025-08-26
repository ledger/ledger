//! CLI demo example

use clap::Parser;

/// Simple CLI demo
#[derive(Parser)]
struct Args {
    /// Input file
    #[arg(short, long)]
    file: String,
}

fn main() {
    let args = Args::parse();
    println!("Processing file: {}", args.file);
    // TODO: Add actual file processing
}