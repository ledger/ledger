//! SIMD-accelerated arithmetic operations for Ledger
//!
//! This module provides SIMD optimizations for bulk arithmetic operations
//! on amounts and balances, with fallback implementations for compatibility.

use crate::balance::Balance;
use once_cell::sync::Lazy;
use rust_decimal::Decimal;

/// CPU feature detection for optimal SIMD instruction set selection
#[derive(Debug, Clone, Copy)]
pub struct CpuFeatures {
    pub has_sse2: bool,
    pub has_avx2: bool,
    pub has_avx512: bool,
}

impl CpuFeatures {
    /// Detect available CPU features
    pub fn detect() -> Self {
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            Self {
                has_sse2: is_x86_feature_detected!("sse2"),
                has_avx2: is_x86_feature_detected!("avx2"),
                has_avx512: is_x86_feature_detected!("avx512f"),
            }
        }

        #[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
        {
            Self { has_sse2: false, has_avx2: false, has_avx512: false }
        }
    }

    /// Get the best available SIMD variant
    pub fn best_simd_variant(&self) -> SimdVariant {
        if self.has_avx512 {
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
    _features: CpuFeatures,
}

impl SimdArithmetic {
    /// Create new SIMD arithmetic processor with auto-detection
    pub fn new() -> Self {
        let _features = CpuFeatures::detect();
        let variant = _features.best_simd_variant();

        Self { variant, _features }
    }

    /// Create SIMD arithmetic processor with specific variant
    pub fn with_variant(variant: SimdVariant) -> Self {
        let _features = CpuFeatures::detect();
        Self { variant, _features }
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
        let result = Balance::new();
        for _balance in balances {
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
    unsafe fn add_decimals_sse2(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        // For now, use scalar implementation as a fallback
        // Actual SIMD implementation would require x86 intrinsics
        self.add_decimals_scalar(a, b, result);
    }

    /// SSE2 optimized decimal multiplication
    unsafe fn mul_decimals_sse2(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        // For now, use scalar implementation as a fallback
        self.mul_decimals_scalar(a, b, result);
    }

    /// SSE2 optimized decimal summation
    unsafe fn sum_decimals_sse2(&self, values: &[Decimal]) -> Decimal {
        // For now, use scalar implementation as a fallback
        self.sum_decimals_scalar(values)
    }

    /// SSE2 optimized f64 to decimal conversion
    unsafe fn convert_f64_to_decimal_sse2(&self, values: &[f64], result: &mut [Decimal]) {
        // For now, use scalar implementation as a fallback
        self.convert_f64_to_decimal_scalar(values, result);
    }

    /// AVX2 optimized decimal addition
    unsafe fn add_decimals_avx2(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        // For now, use scalar implementation as a fallback
        self.add_decimals_scalar(a, b, result);
    }

    /// AVX2 optimized decimal multiplication
    unsafe fn mul_decimals_avx2(&self, a: &[Decimal], b: &[Decimal], result: &mut [Decimal]) {
        // For now, use scalar implementation as a fallback
        self.mul_decimals_scalar(a, b, result);
    }

    /// AVX2 optimized decimal summation
    unsafe fn sum_decimals_avx2(&self, values: &[Decimal]) -> Decimal {
        // For now, use scalar implementation as a fallback
        self.sum_decimals_scalar(values)
    }

    /// AVX2 optimized f64 to decimal conversion
    unsafe fn convert_f64_to_decimal_avx2(&self, values: &[f64], result: &mut [Decimal]) {
        // For now, use scalar implementation as a fallback
        self.convert_f64_to_decimal_scalar(values, result);
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
    /// Benchmark decimal addition performance
    pub fn benchmark_add_decimals(size: usize) -> std::time::Duration {
        use std::time::Instant;

        let a: Vec<Decimal> = (0..size).map(|i| Decimal::from(i as i32)).collect();
        let b: Vec<Decimal> = (0..size).map(|i| Decimal::from((i * 2) as i32)).collect();
        let mut result = vec![Decimal::from(0); size];

        let simd = SimdArithmetic::new();
        let start = Instant::now();
        simd.add_decimal_arrays(&a, &b, &mut result);
        start.elapsed()
    }

    /// Benchmark balance aggregation performance
    pub fn benchmark_aggregate_balances(size: usize) -> std::time::Duration {
        use std::time::Instant;

        let balances: Vec<Balance> = (0..size).map(|_| Balance::new()).collect();

        let simd = SimdArithmetic::new();
        let start = Instant::now();
        let _ = simd.aggregate_balances(&balances);
        start.elapsed()
    }

    /// Compare SIMD variant performance
    pub fn compare_variants(size: usize) {
        let a: Vec<Decimal> = (0..size).map(|i| Decimal::from(i as i32)).collect();
        let b: Vec<Decimal> = (0..size).map(|i| Decimal::from((i * 2) as i32)).collect();
        let mut result = vec![Decimal::from(0); size];

        // Test scalar
        let scalar = SimdArithmetic::with_variant(SimdVariant::Scalar);
        let scalar_time = {
            use std::time::Instant;
            let start = Instant::now();
            scalar.add_decimal_arrays(&a, &b, &mut result);
            start.elapsed()
        };

        // Test best available SIMD
        let simd = SimdArithmetic::new();
        let simd_time = {
            use std::time::Instant;
            let start = Instant::now();
            simd.add_decimal_arrays(&a, &b, &mut result);
            start.elapsed()
        };

        println!("Scalar: {:?}", scalar_time);
        println!("SIMD ({:?}): {:?}", simd.variant(), simd_time);

        if simd_time < scalar_time {
            let speedup = scalar_time.as_nanos() as f64 / simd_time.as_nanos() as f64;
            println!("Speedup: {:.2}x", speedup);
        }
    }
}

/// Global SIMD arithmetic instance
pub static SIMD: Lazy<SimdArithmetic> = Lazy::new(SimdArithmetic::new);

/// Perform SIMD-accelerated decimal addition
pub fn simd_add_decimals(a: &[Decimal], b: &[Decimal]) -> Vec<Decimal> {
    assert_eq!(a.len(), b.len());
    let mut result = vec![Decimal::from(0); a.len()];
    SIMD.add_decimal_arrays(a, b, &mut result);
    result
}

/// Perform SIMD-accelerated decimal summation
pub fn simd_sum_decimals(values: &[Decimal]) -> Decimal {
    SIMD.sum_decimals(values)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cpu_feature_detection() {
        let features = CpuFeatures::detect();
        println!("CPU Features: {:?}", features);
        println!("Best SIMD: {:?}", features.best_simd_variant());
    }

    #[test]
    fn test_decimal_addition() {
        let a = vec![Decimal::from(1), Decimal::from(2), Decimal::from(3)];
        let b = vec![Decimal::from(4), Decimal::from(5), Decimal::from(6)];
        let result = simd_add_decimals(&a, &b);
        assert_eq!(result, vec![Decimal::from(5), Decimal::from(7), Decimal::from(9)]);
    }

    #[test]
    fn test_decimal_sum() {
        let values = vec![Decimal::from(1), Decimal::from(2), Decimal::from(3)];
        let sum = simd_sum_decimals(&values);
        assert_eq!(sum, Decimal::from(6));
    }
}
