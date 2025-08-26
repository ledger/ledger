//! SIMD-accelerated arithmetic operations for Ledger
//!
//! This module provides SIMD optimizations for bulk arithmetic operations
//! on amounts and balances, with fallback implementations for compatibility.

use std::arch::x86_64::*;
use rust_decimal::Decimal;
use once_cell::sync::Lazy;
use crate::amount::Amount;
use crate::balance::Balance;

/// CPU feature detection for optimal SIMD instruction set selection
#[derive(Debug, Clone, Copy)]
pub struct CpuFeatures {
    pub has_sse2: bool,
    pub has_avx2: bool,
    pub has_avx512f: bool,
}

impl CpuFeatures {
    /// Detect available CPU features at runtime
    pub fn detect() -> Self {
        Self {
            has_sse2: is_x86_feature_detected!("sse2"),
            has_avx2: is_x86_feature_detected!("avx2"),
            has_avx512f: is_x86_feature_detected!("avx512f"),
        }
    }

    /// Get the best SIMD variant for this CPU
    pub fn best_simd_variant(&self) -> SimdVariant {
        if self.has_avx512f {
            SimdVariant::Avx512
        } else if self.has_avx2 {
            SimdVariant::Avx2
        } else if self.has_sse2 {
            SimdVariant::Sse2
        } else {
            SimdVariant::Scalar
        }
    }
}

/// Available SIMD instruction set variants
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimdVariant {
    Scalar,
    Sse2,
    Avx2,
    Avx512,
}

/// SIMD-accelerated arithmetic operations
pub struct SimdArithmetic {
    variant: SimdVariant,
    features: CpuFeatures,
}

impl SimdArithmetic {
    /// Create new SIMD arithmetic processor with auto-detection
    pub fn new() -> Self {
        let features = CpuFeatures::detect();
        let variant = features.best_simd_variant();
        
        Self { variant, features }
    }

    /// Create SIMD arithmetic processor with specific variant
    pub fn with_variant(variant: SimdVariant) -> Self {
        let features = CpuFeatures::detect();
        Self { variant, features }
    }

    /// Get the SIMD variant in use
    pub fn variant(&self) -> SimdVariant {
        self.variant
    }

    /// Add multiple decimal arrays using SIMD
    pub fn add_decimal_arrays(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        assert_eq!(a.len(), b.len());
        assert_eq!(a.len(), result.len());

        match self.variant {
            SimdVariant::Avx2 => unsafe { self.add_decimals_avx2(a, b, result) },
            SimdVariant::Sse2 => unsafe { self.add_decimals_sse2(a, b, result) },
            _ => self.add_decimals_scalar(a, b, result),
        }
    }

    /// Multiply multiple decimal arrays using SIMD
    pub fn mul_decimal_arrays(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        assert_eq!(a.len(), b.len());
        assert_eq!(a.len(), result.len());

        match self.variant {
            SimdVariant::Avx2 => unsafe { self.mul_decimals_avx2(a, b, result) },
            SimdVariant::Sse2 => unsafe { self.mul_decimals_sse2(a, b, result) },
            _ => self.mul_decimals_scalar(a, b, result),
        }
    }

    /// Sum an array of decimals using SIMD
    pub fn sum_decimals(&self, values: &[Decimal]) -> Decimal {
        match self.variant {
            SimdVariant::Avx2 => unsafe { self.sum_decimals_avx2(values) },
            SimdVariant::Sse2 => unsafe { self.sum_decimals_sse2(values) },
            _ => self.sum_decimals_scalar(values),
        }
    }

    /// Aggregate balances using SIMD acceleration
    pub fn aggregate_balances(&self, balances: &[Balance]) -> Balance {
        if balances.is_empty() {
            return Balance::new();
        }

        // For now, fall back to scalar implementation
        // TODO: Implement SIMD balance aggregation
        self.aggregate_balances_scalar(balances)
    }

    /// Convert between number formats using SIMD
    pub fn convert_number_format(&self, values: &[f64], result: &mut [Decimal]) {
        assert_eq!(values.len(), result.len());

        match self.variant {
            SimdVariant::Avx2 => unsafe { self.convert_f64_to_decimal_avx2(values, result) },
            SimdVariant::Sse2 => unsafe { self.convert_f64_to_decimal_sse2(values, result) },
            _ => self.convert_f64_to_decimal_scalar(values, result),
        }
    }

    /// Scalar fallback for decimal addition
    fn add_decimals_scalar(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        for i in 0..a.len() {
            result[i] = a[i] + b[i];
        }
    }

    /// Scalar fallback for decimal multiplication
    fn mul_decimals_scalar(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        for i in 0..a.len() {
            result[i] = a[i] * b[i];
        }
    }

    /// Scalar fallback for decimal summation
    fn sum_decimals_scalar(&self, values: &[Decimal]) -> Decimal {
        values.iter().sum()
    }

    /// Scalar fallback for balance aggregation
    fn aggregate_balances_scalar(&self, balances: &[Balance]) -> Balance {
        let mut result = Balance::new();
        for balance in balances {
            // TODO: Implement proper balance addition
            // result += balance;
        }
        result
    }

    /// Scalar fallback for f64 to Decimal conversion
    fn convert_f64_to_decimal_scalar(&self, values: &[f64], result: &mut [Decimal]) {
        for i in 0..values.len() {
            result[i] = Decimal::try_from(values[i]).unwrap_or_default();
        }
    }

    /// SSE2 optimized decimal addition
    #[target_feature(enable = "sse2")]
    unsafe fn add_decimals_sse2(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        // Convert decimals to f64 for SIMD processing
        let len = a.len();
        let simd_chunks = len / 2; // Process 2 doubles at a time with SSE2
        let remainder = len % 2;

        for i in 0..simd_chunks {
            let base_idx = i * 2;
            
            // Load two decimals as f64
            let a_vals = [
                a[base_idx].to_f64().unwrap_or(0.0),
                a[base_idx + 1].to_f64().unwrap_or(0.0),
            ];
            let b_vals = [
                b[base_idx].to_f64().unwrap_or(0.0),
                b[base_idx + 1].to_f64().unwrap_or(0.0),
            ];

            // Load into SSE2 registers
            let va = _mm_loadu_pd(a_vals.as_ptr());
            let vb = _mm_loadu_pd(b_vals.as_ptr());
            
            // Add
            let vresult = _mm_add_pd(va, vb);
            
            // Store back
            let mut temp = [0.0f64; 2];
            _mm_storeu_pd(temp.as_mut_ptr(), vresult);
            
            result[base_idx] = Decimal::try_from(temp[0]).unwrap_or_default();
            result[base_idx + 1] = Decimal::try_from(temp[1]).unwrap_or_default();
        }

        // Handle remainder
        for i in (simd_chunks * 2)..len {
            result[i] = a[i] + b[i];
        }
    }

    /// SSE2 optimized decimal multiplication
    #[target_feature(enable = "sse2")]
    unsafe fn mul_decimals_sse2(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        let len = a.len();
        let simd_chunks = len / 2;
        let remainder = len % 2;

        for i in 0..simd_chunks {
            let base_idx = i * 2;
            
            let a_vals = [
                a[base_idx].to_f64().unwrap_or(0.0),
                a[base_idx + 1].to_f64().unwrap_or(0.0),
            ];
            let b_vals = [
                b[base_idx].to_f64().unwrap_or(0.0),
                b[base_idx + 1].to_f64().unwrap_or(0.0),
            ];

            let va = _mm_loadu_pd(a_vals.as_ptr());
            let vb = _mm_loadu_pd(b_vals.as_ptr());
            let vresult = _mm_mul_pd(va, vb);
            
            let mut temp = [0.0f64; 2];
            _mm_storeu_pd(temp.as_mut_ptr(), vresult);
            
            result[base_idx] = Decimal::try_from(temp[0]).unwrap_or_default();
            result[base_idx + 1] = Decimal::try_from(temp[1]).unwrap_or_default();
        }

        for i in (simd_chunks * 2)..len {
            result[i] = a[i] * b[i];
        }
    }

    /// SSE2 optimized decimal summation
    #[target_feature(enable = "sse2")]
    unsafe fn sum_decimals_sse2(&self, values: &[Decimal]) -> Decimal {
        if values.is_empty() {
            return Decimal::ZERO;
        }

        let len = values.len();
        let simd_chunks = len / 2;
        let remainder = len % 2;

        let mut sum_vec = _mm_setzero_pd();

        for i in 0..simd_chunks {
            let base_idx = i * 2;
            let vals = [
                values[base_idx].to_f64().unwrap_or(0.0),
                values[base_idx + 1].to_f64().unwrap_or(0.0),
            ];
            
            let v = _mm_loadu_pd(vals.as_ptr());
            sum_vec = _mm_add_pd(sum_vec, v);
        }

        // Extract sum from SIMD register
        let mut temp = [0.0f64; 2];
        _mm_storeu_pd(temp.as_mut_ptr(), sum_vec);
        let mut total = temp[0] + temp[1];

        // Add remainder
        for i in (simd_chunks * 2)..len {
            total += values[i].to_f64().unwrap_or(0.0);
        }

        Decimal::try_from(total).unwrap_or_default()
    }

    /// SSE2 optimized f64 to decimal conversion
    #[target_feature(enable = "sse2")]
    unsafe fn convert_f64_to_decimal_sse2(&self, values: &[f64], result: &mut [Decimal]) {
        for i in 0..values.len() {
            result[i] = Decimal::try_from(values[i]).unwrap_or_default();
        }
    }

    /// AVX2 optimized decimal addition
    #[target_feature(enable = "avx2")]
    unsafe fn add_decimals_avx2(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        let len = a.len();
        let simd_chunks = len / 4; // Process 4 doubles at a time with AVX2
        let remainder = len % 4;

        for i in 0..simd_chunks {
            let base_idx = i * 4;
            
            let a_vals = [
                a[base_idx].to_f64().unwrap_or(0.0),
                a[base_idx + 1].to_f64().unwrap_or(0.0),
                a[base_idx + 2].to_f64().unwrap_or(0.0),
                a[base_idx + 3].to_f64().unwrap_or(0.0),
            ];
            let b_vals = [
                b[base_idx].to_f64().unwrap_or(0.0),
                b[base_idx + 1].to_f64().unwrap_or(0.0),
                b[base_idx + 2].to_f64().unwrap_or(0.0),
                b[base_idx + 3].to_f64().unwrap_or(0.0),
            ];

            let va = _mm256_loadu_pd(a_vals.as_ptr());
            let vb = _mm256_loadu_pd(b_vals.as_ptr());
            let vresult = _mm256_add_pd(va, vb);
            
            let mut temp = [0.0f64; 4];
            _mm256_storeu_pd(temp.as_mut_ptr(), vresult);
            
            for j in 0..4 {
                result[base_idx + j] = Decimal::try_from(temp[j]).unwrap_or_default();
            }
        }

        // Handle remainder with scalar operations
        for i in (simd_chunks * 4)..len {
            result[i] = a[i] + b[i];
        }
    }

    /// AVX2 optimized decimal multiplication
    #[target_feature(enable = "avx2")]
    unsafe fn mul_decimals_avx2(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        let len = a.len();
        let simd_chunks = len / 4;

        for i in 0..simd_chunks {
            let base_idx = i * 4;
            
            let a_vals = [
                a[base_idx].to_f64().unwrap_or(0.0),
                a[base_idx + 1].to_f64().unwrap_or(0.0),
                a[base_idx + 2].to_f64().unwrap_or(0.0),
                a[base_idx + 3].to_f64().unwrap_or(0.0),
            ];
            let b_vals = [
                b[base_idx].to_f64().unwrap_or(0.0),
                b[base_idx + 1].to_f64().unwrap_or(0.0),
                b[base_idx + 2].to_f64().unwrap_or(0.0),
                b[base_idx + 3].to_f64().unwrap_or(0.0),
            ];

            let va = _mm256_loadu_pd(a_vals.as_ptr());
            let vb = _mm256_loadu_pd(b_vals.as_ptr());
            let vresult = _mm256_mul_pd(va, vb);
            
            let mut temp = [0.0f64; 4];
            _mm256_storeu_pd(temp.as_mut_ptr(), vresult);
            
            for j in 0..4 {
                result[base_idx + j] = Decimal::try_from(temp[j]).unwrap_or_default();
            }
        }

        for i in (simd_chunks * 4)..len {
            result[i] = a[i] * b[i];
        }
    }

    /// AVX2 optimized decimal summation
    #[target_feature(enable = "avx2")]
    unsafe fn sum_decimals_avx2(&self, values: &[Decimal]) -> Decimal {
        if values.is_empty() {
            return Decimal::ZERO;
        }

        let len = values.len();
        let simd_chunks = len / 4;

        let mut sum_vec = _mm256_setzero_pd();

        for i in 0..simd_chunks {
            let base_idx = i * 4;
            let vals = [
                values[base_idx].to_f64().unwrap_or(0.0),
                values[base_idx + 1].to_f64().unwrap_or(0.0),
                values[base_idx + 2].to_f64().unwrap_or(0.0),
                values[base_idx + 3].to_f64().unwrap_or(0.0),
            ];
            
            let v = _mm256_loadu_pd(vals.as_ptr());
            sum_vec = _mm256_add_pd(sum_vec, v);
        }

        // Extract sum from AVX2 register
        let mut temp = [0.0f64; 4];
        _mm256_storeu_pd(temp.as_mut_ptr(), sum_vec);
        let mut total = temp[0] + temp[1] + temp[2] + temp[3];

        // Add remainder
        for i in (simd_chunks * 4)..len {
            total += values[i].to_f64().unwrap_or(0.0);
        }

        Decimal::try_from(total).unwrap_or_default()
    }

    /// AVX2 optimized f64 to decimal conversion
    #[target_feature(enable = "avx2")]
    unsafe fn convert_f64_to_decimal_avx2(&self, values: &[f64], result: &mut [Decimal]) {
        // For now, use scalar conversion as Decimal doesn't have direct SIMD support
        for i in 0..values.len() {
            result[i] = Decimal::try_from(values[i]).unwrap_or_default();
        }
    }
}

impl Default for SimdArithmetic {
    fn default() -> Self {
        Self::new()
    }
}

/// SIMD performance benchmarking utilities
pub struct SimdBenchmark;

impl SimdBenchmark {
    /// Benchmark SIMD vs scalar addition performance
    pub fn benchmark_addition(size: usize) -> BenchmarkResult {
        let a: Vec<Decimal> = (0..size).map(|i| Decimal::from(i)).collect();
        let b: Vec<Decimal> = (0..size).map(|i| Decimal::from(i * 2)).collect();
        let mut result_simd = vec![Decimal::ZERO; size];
        let mut result_scalar = vec![Decimal::ZERO; size];

        let simd = SimdArithmetic::new();
        let start = std::time::Instant::now();
        simd.add_decimal_arrays(&a, &b, &mut result_simd);
        let simd_time = start.elapsed();

        let scalar_simd = SimdArithmetic::with_variant(SimdVariant::Scalar);
        let start = std::time::Instant::now();
        scalar_simd.add_decimal_arrays(&a, &b, &mut result_scalar);
        let scalar_time = start.elapsed();

        BenchmarkResult {
            simd_variant: simd.variant(),
            simd_time,
            scalar_time,
            speedup: scalar_time.as_nanos() as f64 / simd_time.as_nanos() as f64,
            accuracy_matches: result_simd == result_scalar,
        }
    }

    /// Benchmark SIMD vs scalar summation performance
    pub fn benchmark_summation(size: usize) -> BenchmarkResult {
        let values: Vec<Decimal> = (0..size).map(|i| Decimal::from(i)).collect();

        let simd = SimdArithmetic::new();
        let start = std::time::Instant::now();
        let simd_result = simd.sum_decimals(&values);
        let simd_time = start.elapsed();

        let scalar_simd = SimdArithmetic::with_variant(SimdVariant::Scalar);
        let start = std::time::Instant::now();
        let scalar_result = scalar_simd.sum_decimals(&values);
        let scalar_time = start.elapsed();

        BenchmarkResult {
            simd_variant: simd.variant(),
            simd_time,
            scalar_time,
            speedup: scalar_time.as_nanos() as f64 / simd_time.as_nanos() as f64,
            accuracy_matches: simd_result == scalar_result,
        }
    }
}

/// Benchmark results for SIMD performance comparison
#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    pub simd_variant: SimdVariant,
    pub simd_time: std::time::Duration,
    pub scalar_time: std::time::Duration,
    pub speedup: f64,
    pub accuracy_matches: bool,
}

impl std::fmt::Display for BenchmarkResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "SIMD Benchmark ({:?}): {:.2}x speedup, SIMD: {:?}, Scalar: {:?}, Accuracy: {}",
            self.simd_variant,
            self.speedup,
            self.simd_time,
            self.scalar_time,
            if self.accuracy_matches { "PASS" } else { "FAIL" }
        )
    }
}

/// Global SIMD arithmetic instance for efficient reuse
pub static SIMD_ARITHMETIC: once_cell::sync::Lazy<SimdArithmetic> = 
    once_cell::sync::Lazy::new(|| SimdArithmetic::new());

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cpu_feature_detection() {
        let features = CpuFeatures::detect();
        let variant = features.best_simd_variant();
        
        // Should at least detect something
        assert!(matches!(variant, SimdVariant::Scalar | SimdVariant::Sse2 | SimdVariant::Avx2 | SimdVariant::Avx512));
    }

    #[test]
    fn test_scalar_addition() {
        let simd = SimdArithmetic::with_variant(SimdVariant::Scalar);
        
        let a = vec![Decimal::from(1), Decimal::from(2), Decimal::from(3)];
        let b = vec![Decimal::from(10), Decimal::from(20), Decimal::from(30)];
        let mut result = vec![Decimal::ZERO; 3];
        
        simd.add_decimal_arrays(&a, &b, &mut result);
        
        assert_eq!(result[0], Decimal::from(11));
        assert_eq!(result[1], Decimal::from(22));
        assert_eq!(result[2], Decimal::from(33));
    }

    #[test]
    fn test_scalar_multiplication() {
        let simd = SimdArithmetic::with_variant(SimdVariant::Scalar);
        
        let a = vec![Decimal::from(2), Decimal::from(3), Decimal::from(4)];
        let b = vec![Decimal::from(5), Decimal::from(6), Decimal::from(7)];
        let mut result = vec![Decimal::ZERO; 3];
        
        simd.mul_decimal_arrays(&a, &b, &mut result);
        
        assert_eq!(result[0], Decimal::from(10));
        assert_eq!(result[1], Decimal::from(18));
        assert_eq!(result[2], Decimal::from(28));
    }

    #[test]
    fn test_scalar_summation() {
        let simd = SimdArithmetic::with_variant(SimdVariant::Scalar);
        
        let values = vec![Decimal::from(1), Decimal::from(2), Decimal::from(3), Decimal::from(4)];
        let sum = simd.sum_decimals(&values);
        
        assert_eq!(sum, Decimal::from(10));
    }

    #[test]
    fn test_simd_vs_scalar_consistency() {
        let size = 100;
        let a: Vec<Decimal> = (0..size).map(|i| Decimal::from(i)).collect();
        let b: Vec<Decimal> = (0..size).map(|i| Decimal::from(i * 2)).collect();
        
        let simd = SimdArithmetic::new();
        let scalar = SimdArithmetic::with_variant(SimdVariant::Scalar);
        
        let mut simd_result = vec![Decimal::ZERO; size];
        let mut scalar_result = vec![Decimal::ZERO; size];
        
        simd.add_decimal_arrays(&a, &b, &mut simd_result);
        scalar.add_decimal_arrays(&a, &b, &mut scalar_result);
        
        // Results should be identical
        assert_eq!(simd_result, scalar_result);
        
        // Test summation consistency
        let simd_sum = simd.sum_decimals(&a);
        let scalar_sum = scalar.sum_decimals(&a);
        assert_eq!(simd_sum, scalar_sum);
    }

    #[test]
    fn test_number_format_conversion() {
        let simd = SimdArithmetic::new();
        
        let values = vec![1.23, 4.56, 7.89, 10.11];
        let mut result = vec![Decimal::ZERO; 4];
        
        simd.convert_number_format(&values, &mut result);
        
        assert_eq!(result[0], Decimal::try_from(1.23).unwrap());
        assert_eq!(result[1], Decimal::try_from(4.56).unwrap());
        assert_eq!(result[2], Decimal::try_from(7.89).unwrap());
        assert_eq!(result[3], Decimal::try_from(10.11).unwrap());
    }

    #[test]
    fn test_benchmark_runs() {
        let result = SimdBenchmark::benchmark_addition(1000);
        
        // Should complete without panicking
        assert!(result.speedup >= 0.0);
        assert!(result.accuracy_matches);
        
        let sum_result = SimdBenchmark::benchmark_summation(1000);
        assert!(sum_result.speedup >= 0.0);
        assert!(sum_result.accuracy_matches);
    }

    #[test]
    fn test_global_simd_arithmetic() {
        // Test that the global instance works
        let values = vec![Decimal::from(1), Decimal::from(2), Decimal::from(3)];
        let sum = SIMD_ARITHMETIC.sum_decimals(&values);
        assert_eq!(sum, Decimal::from(6));
    }
}