// Build script for ledger-core
// Generates C header files using cbindgen

use std::env;
use std::path::PathBuf;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let output_dir = env::var("OUT_DIR").unwrap();
    
    // Generate the header file using cbindgen with configuration from cbindgen.toml
    let output_path = PathBuf::from(&output_dir).join("ledger_ffi.h");
    
    let config_path = PathBuf::from(&crate_dir).join("cbindgen.toml");
    cbindgen::generate_with_config(&crate_dir, cbindgen::Config::from_file(&config_path).unwrap())
        .expect("Unable to generate bindings")
        .write_to_file(&output_path);
    
    println!("cargo:rerun-if-changed=src/ffi.rs");
    println!("cargo:rerun-if-changed=cbindgen.toml");
    println!("cargo:rustc-env=LEDGER_FFI_HEADER={}", output_path.display());
    
    // Also generate to a fixed location for easier access
    let project_header = PathBuf::from(&crate_dir).join("include").join("ledger_ffi.h");
    if let Some(parent) = project_header.parent() {
        std::fs::create_dir_all(parent).ok();
    }
    
    // Copy generated header to include directory
    if let Ok(content) = std::fs::read_to_string(&output_path) {
        std::fs::write(&project_header, content).ok();
        println!("Generated header at: {}", project_header.display());
    }
}