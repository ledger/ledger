//! Date/time parsing and handling module for Ledger
//!
//! This module provides comprehensive date and datetime parsing compatible
//! with the C++ Ledger implementation. It supports multiple date formats,
//! period expressions, and timezone handling.

use chrono::{Datelike, Local, NaiveDate, NaiveDateTime};
use once_cell::sync::Lazy;
use regex::Regex;
use thiserror::Error;

/// Errors that can occur during date/time parsing
#[derive(Error, Debug, PartialEq)]
pub enum DateParseError {
    #[error("Invalid date format: {0}")]
    InvalidFormat(String),
    #[error("Invalid date value: {0}")]
    InvalidDate(String),
    #[error("Ambiguous date format: {0} (consider specifying year)")]
    AmbiguousFormat(String),
    #[error("Unsupported date format: {0}")]
    UnsupportedFormat(String),
    #[error("Date out of range: {0}")]
    OutOfRange(String),
}

/// Traits detected during date parsing
#[derive(Debug, Clone, PartialEq)]
pub struct DateTraits {
    pub has_year: bool,
    pub has_month: bool,
    pub has_day: bool,
}

impl Default for DateTraits {
    fn default() -> Self {
        Self { has_year: false, has_month: false, has_day: false }
    }
}

/// Result of parsing a date string
#[derive(Debug, Clone)]
pub struct ParsedDate {
    pub date: NaiveDate,
    pub traits: DateTraits,
    pub format_hint: DateFormat,
}

/// Recognized date formats for optimization
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DateFormat {
    /// ISO 8601: YYYY-MM-DD
    IsoYmd,
    /// ISO 8601: YYYY/MM/DD
    IsoYmdSlash,
    /// US format: MM/DD/YYYY
    UsMdy,
    /// US format: MM/DD/YY
    UsMdyShort,
    /// US format: MM/DD (current year)
    UsMd,
    /// European: DD.MM.YYYY
    EuDmy,
    /// European: DD.MM.YY
    EuDmyShort,
    /// Compact: YYYYMMDD
    Compact,
    /// Compact with time: YYYYMMDDTHHMMSS
    CompactDateTime,
    /// Named month: Feb 02, 2002
    NamedMonth,
    /// Named month short: 02-Feb-2002
    NamedMonthShort,
    /// Year only: 2002
    YearOnly,
    /// Month/day only: 02/03
    MonthDayOnly,
    /// Relative: today, yesterday, tomorrow
    Relative,
}

/// Month name mapping for parsing
static MONTH_NAMES: Lazy<Vec<(&str, u32)>> = Lazy::new(|| {
    vec![
        ("january", 1),
        ("jan", 1),
        ("february", 2),
        ("feb", 2),
        ("march", 3),
        ("mar", 3),
        ("april", 4),
        ("apr", 4),
        ("may", 5),
        ("may", 5),
        ("june", 6),
        ("jun", 6),
        ("july", 7),
        ("jul", 7),
        ("august", 8),
        ("aug", 8),
        ("september", 9),
        ("sep", 9),
        ("sept", 9),
        ("october", 10),
        ("oct", 10),
        ("november", 11),
        ("nov", 11),
        ("december", 12),
        ("dec", 12),
    ]
});

/// Weekday name mapping
static WEEKDAY_NAMES: Lazy<Vec<(&str, u32)>> = Lazy::new(|| {
    vec![
        ("sunday", 0),
        ("sun", 0),
        ("monday", 1),
        ("mon", 1),
        ("tuesday", 2),
        ("tue", 2),
        ("tues", 2),
        ("wednesday", 3),
        ("wed", 3),
        ("thursday", 4),
        ("thu", 4),
        ("thur", 4),
        ("thurs", 4),
        ("friday", 5),
        ("fri", 5),
        ("saturday", 6),
        ("sat", 6),
    ]
});

/// Compiled regex patterns for different date formats
static DATE_PATTERNS: Lazy<Vec<(DateFormat, Regex)>> = Lazy::new(|| {
    vec![
        // ISO 8601 formats
        (DateFormat::IsoYmd, Regex::new(r"^(\d{4})-(\d{1,2})-(\d{1,2})$").unwrap()),
        (DateFormat::IsoYmdSlash, Regex::new(r"^(\d{4})/(\d{1,2})/(\d{1,2})$").unwrap()),
        // US formats
        (DateFormat::UsMdy, Regex::new(r"^(\d{1,2})/(\d{1,2})/(\d{4})$").unwrap()),
        (DateFormat::UsMdyShort, Regex::new(r"^(\d{1,2})/(\d{1,2})/(\d{2})$").unwrap()),
        (DateFormat::UsMd, Regex::new(r"^(\d{1,2})/(\d{1,2})$").unwrap()),
        // European formats
        (DateFormat::EuDmy, Regex::new(r"^(\d{1,2})\.(\d{1,2})\.(\d{4})$").unwrap()),
        (DateFormat::EuDmyShort, Regex::new(r"^(\d{1,2})\.(\d{1,2})\.(\d{2})$").unwrap()),
        // Compact formats
        (DateFormat::Compact, Regex::new(r"^(\d{8})$").unwrap()),
        (DateFormat::CompactDateTime, Regex::new(r"^(\d{8})T(\d{6})(?:[-+]\d{4})?$").unwrap()),
        // Year only
        (DateFormat::YearOnly, Regex::new(r"^(\d{4})$").unwrap()),
        // Month/day only
        (DateFormat::MonthDayOnly, Regex::new(r"^(\d{1,2})/(\d{1,2})$").unwrap()),
        // Named month formats - more complex patterns
        (DateFormat::NamedMonth, Regex::new(r"^([a-zA-Z]+)\s+(\d{1,2}),?\s+(\d{4})$").unwrap()),
        (DateFormat::NamedMonthShort, Regex::new(r"^(\d{1,2})-([a-zA-Z]+)-(\d{4})$").unwrap()),
    ]
});

/// Parse a month name to number (1-12)
fn parse_month_name(name: &str) -> Option<u32> {
    let name_lower = name.to_lowercase();
    MONTH_NAMES
        .iter()
        .find(|(month_name, _)| *month_name == name_lower)
        .map(|(_, month_num)| *month_num)
}

/// Convert two-digit year to four-digit year using pivot logic
fn expand_two_digit_year(year_2d: u32) -> i32 {
    let current_year = Local::now().year();
    let current_century = (current_year / 100) * 100;
    let pivot_year = current_year % 100 + 20; // 20-year forward window

    if year_2d <= pivot_year as u32 {
        current_century + year_2d as i32
    } else {
        current_century - 100 + year_2d as i32
    }
}

/// Normalize date string by converting separators
fn normalize_separators(input: &str) -> String {
    input.replace('-', "/").replace('.', "/")
}

/// Parse relative date keywords
fn parse_relative_date(input: &str) -> Option<ParsedDate> {
    let today = Local::now().date_naive();

    match input.to_lowercase().as_str() {
        "today" => Some(ParsedDate {
            date: today,
            traits: DateTraits { has_year: true, has_month: true, has_day: true },
            format_hint: DateFormat::Relative,
        }),
        "yesterday" | "yday" => Some(ParsedDate {
            date: today - chrono::Duration::days(1),
            traits: DateTraits { has_year: true, has_month: true, has_day: true },
            format_hint: DateFormat::Relative,
        }),
        "tomorrow" => Some(ParsedDate {
            date: today + chrono::Duration::days(1),
            traits: DateTraits { has_year: true, has_month: true, has_day: true },
            format_hint: DateFormat::Relative,
        }),
        _ => None,
    }
}

/// Parse a date string using multiple format attempts
pub fn parse_date(input: &str) -> Result<ParsedDate, DateParseError> {
    let input = input.trim();

    if input.is_empty() {
        return Err(DateParseError::InvalidFormat("empty string".to_string()));
    }

    // Try relative dates first
    if let Some(parsed) = parse_relative_date(input) {
        return Ok(parsed);
    }

    // Try patterns first on the original input to preserve format detection
    for (format, pattern) in DATE_PATTERNS.iter() {
        if let Some(captures) = pattern.captures(input) {
            match parse_with_format(&captures, *format, input)? {
                Some(parsed) => return Ok(parsed),
                None => continue,
            }
        }
    }

    // If no exact match, try with normalized separators for flexibility
    let normalized = normalize_separators(input);
    if normalized != input {
        for (format, pattern) in DATE_PATTERNS.iter() {
            // Skip European formats when normalizing to avoid conflicts
            if matches!(format, DateFormat::EuDmy | DateFormat::EuDmyShort) {
                continue;
            }

            if let Some(captures) = pattern.captures(&normalized) {
                match parse_with_format(&captures, *format, &normalized)? {
                    Some(mut parsed) => {
                        // Adjust format hint if we normalized
                        if input.contains('-') && format == &DateFormat::IsoYmdSlash {
                            parsed.format_hint = DateFormat::IsoYmd;
                        }
                        return Ok(parsed);
                    }
                    None => continue,
                }
            }
        }
    }

    // Try named month formats with more flexible parsing
    if let Some(parsed) = parse_named_month_flexible(input)? {
        return Ok(parsed);
    }

    Err(DateParseError::UnsupportedFormat(input.to_string()))
}

/// Parse captures for a specific format
fn parse_with_format(
    captures: &regex::Captures,
    format: DateFormat,
    original: &str,
) -> Result<Option<ParsedDate>, DateParseError> {
    let current_date = Local::now().date_naive();
    let current_year = current_date.year();

    match format {
        DateFormat::IsoYmd | DateFormat::IsoYmdSlash => {
            let year: i32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let day: u32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::UsMdy => {
            let month: u32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let day: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let year: i32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::UsMdyShort => {
            let month: u32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let day: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let year_2d: u32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let year = expand_two_digit_year(year_2d);

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::UsMd | DateFormat::MonthDayOnly => {
            let month: u32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let day: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            // Use current year, but if the date is in the future by more than a month,
            // assume it's for last year
            let mut year = current_year;
            if let Some(date) = NaiveDate::from_ymd_opt(year, month, day) {
                if date > current_date && (date - current_date).num_days() > 31 {
                    year -= 1;
                }
            }

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: false, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::EuDmy => {
            let day: u32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let year: i32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::EuDmyShort => {
            let day: u32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let year_2d: u32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let year = expand_two_digit_year(year_2d);

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::Compact => {
            let date_str = &captures[1];
            if date_str.len() != 8 {
                return Ok(None);
            }

            let year: i32 = date_str[0..4]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month: u32 = date_str[4..6]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let day: u32 = date_str[6..8]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::CompactDateTime => {
            let date_str = &captures[1];
            if date_str.len() != 8 {
                return Ok(None);
            }

            let year: i32 = date_str[0..4]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month: u32 = date_str[4..6]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let day: u32 = date_str[6..8]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::YearOnly => {
            let year: i32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            // Year only means January 1st of that year
            let date = NaiveDate::from_ymd_opt(year, 1, 1)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: false, has_day: false },
                format_hint: format,
            }))
        }

        DateFormat::NamedMonth => {
            let month_name = &captures[1];
            let day: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let year: i32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let month = parse_month_name(month_name).ok_or_else(|| {
                DateParseError::InvalidDate(format!("Unknown month: {}", month_name))
            })?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::NamedMonthShort => {
            let day: u32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month_name = &captures[2];
            let year: i32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let month = parse_month_name(month_name).ok_or_else(|| {
                DateParseError::InvalidDate(format!("Unknown month: {}", month_name))
            })?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: format,
            }))
        }

        DateFormat::Relative => {
            // This should have been handled earlier
            Ok(None)
        }
    }
}

/// Parse named month formats with more flexibility
fn parse_named_month_flexible(input: &str) -> Result<Option<ParsedDate>, DateParseError> {
    // More flexible patterns for named months
    let patterns = [
        // "February 02, 2002", "Feb 02, 2002"
        r"^([a-zA-Z]+)\s+(\d{1,2}),?\s+(\d{4})$",
        // "02 February 2002", "02 Feb 2002"
        r"^(\d{1,2})\s+([a-zA-Z]+)\s+(\d{4})$",
        // "2002 February 02", "2002 Feb 02"
        r"^(\d{4})\s+([a-zA-Z]+)\s+(\d{1,2})$",
        // "02-Feb-2002", "02-February-2002"
        r"^(\d{1,2})-([a-zA-Z]+)-(\d{4})$",
        // "Feb-02-2002", "February-02-2002"
        r"^([a-zA-Z]+)-(\d{1,2})-(\d{4})$",
        // "Feb 02", "February 02"
        r"^([a-zA-Z]+)\s+(\d{1,2})$",
        // "02 Feb", "02 February"
        r"^(\d{1,2})\s+([a-zA-Z]+)$",
    ];

    for pattern_str in &patterns {
        let pattern = Regex::new(pattern_str).unwrap();
        if let Some(captures) = pattern.captures(input) {
            if let Some(parsed) = parse_named_month_captures(&captures, pattern_str, input)? {
                return Ok(Some(parsed));
            }
        }
    }

    Ok(None)
}

/// Parse named month regex captures
fn parse_named_month_captures(
    captures: &regex::Captures,
    pattern: &str,
    original: &str,
) -> Result<Option<ParsedDate>, DateParseError> {
    let current_year = Local::now().date_naive().year();

    match pattern {
        // "February 02, 2002", "Feb 02, 2002"
        r"^([a-zA-Z]+)\s+(\d{1,2}),?\s+(\d{4})$" => {
            let month_name = &captures[1];
            let day: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let year: i32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let month = parse_month_name(month_name).ok_or_else(|| {
                DateParseError::InvalidDate(format!("Unknown month: {}", month_name))
            })?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: DateFormat::NamedMonth,
            }))
        }

        // "02 February 2002", "02 Feb 2002"
        r"^(\d{1,2})\s+([a-zA-Z]+)\s+(\d{4})$" => {
            let day: u32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month_name = &captures[2];
            let year: i32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let month = parse_month_name(month_name).ok_or_else(|| {
                DateParseError::InvalidDate(format!("Unknown month: {}", month_name))
            })?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: DateFormat::NamedMonth,
            }))
        }

        // "2002 February 02", "2002 Feb 02"
        r"^(\d{4})\s+([a-zA-Z]+)\s+(\d{1,2})$" => {
            let year: i32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month_name = &captures[2];
            let day: u32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let month = parse_month_name(month_name).ok_or_else(|| {
                DateParseError::InvalidDate(format!("Unknown month: {}", month_name))
            })?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: DateFormat::NamedMonth,
            }))
        }

        // "02-Feb-2002", "02-February-2002"
        r"^(\d{1,2})-([a-zA-Z]+)-(\d{4})$" => {
            let day: u32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month_name = &captures[2];
            let year: i32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let month = parse_month_name(month_name).ok_or_else(|| {
                DateParseError::InvalidDate(format!("Unknown month: {}", month_name))
            })?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: DateFormat::NamedMonthShort,
            }))
        }

        // "Feb-02-2002", "February-02-2002"
        r"^([a-zA-Z]+)-(\d{1,2})-(\d{4})$" => {
            let month_name = &captures[1];
            let day: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let year: i32 = captures[3]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let month = parse_month_name(month_name).ok_or_else(|| {
                DateParseError::InvalidDate(format!("Unknown month: {}", month_name))
            })?;

            let date = NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: true, has_month: true, has_day: true },
                format_hint: DateFormat::NamedMonth,
            }))
        }

        // "Feb 02", "February 02" - current year assumed
        r"^([a-zA-Z]+)\s+(\d{1,2})$" => {
            let month_name = &captures[1];
            let day: u32 = captures[2]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;

            let month = parse_month_name(month_name).ok_or_else(|| {
                DateParseError::InvalidDate(format!("Unknown month: {}", month_name))
            })?;

            let date = NaiveDate::from_ymd_opt(current_year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: false, has_month: true, has_day: true },
                format_hint: DateFormat::NamedMonth,
            }))
        }

        // "02 Feb", "02 February" - current year assumed
        r"^(\d{1,2})\s+([a-zA-Z]+)$" => {
            let day: u32 = captures[1]
                .parse()
                .map_err(|_| DateParseError::InvalidDate(original.to_string()))?;
            let month_name = &captures[2];

            let month = parse_month_name(month_name).ok_or_else(|| {
                DateParseError::InvalidDate(format!("Unknown month: {}", month_name))
            })?;

            let date = NaiveDate::from_ymd_opt(current_year, month, day)
                .ok_or_else(|| DateParseError::InvalidDate(original.to_string()))?;

            Ok(Some(ParsedDate {
                date,
                traits: DateTraits { has_year: false, has_month: true, has_day: true },
                format_hint: DateFormat::NamedMonth,
            }))
        }

        _ => Ok(None),
    }
}

/// Parse a datetime string (date + time components)
pub fn parse_datetime(input: &str) -> Result<NaiveDateTime, DateParseError> {
    let input = input.trim();

    // Try common datetime formats
    let datetime_patterns = [
        // ISO 8601-like formats
        "%Y-%m-%d %H:%M:%S",
        "%Y/%m/%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M",
        // With AM/PM
        "%Y-%m-%d %I:%M:%S %p",
        "%Y/%m/%d %I:%M:%S %p",
        "%Y-%m-%d %I:%M %p",
        "%Y/%m/%d %I:%M %p",
        // US format with time
        "%m/%d/%Y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m/%d/%Y %I:%M:%S %p",
        "%m/%d/%Y %I:%M %p",
        // Compact format
        "%Y%m%dT%H%M%S",
    ];

    for pattern in &datetime_patterns {
        if let Ok(dt) = NaiveDateTime::parse_from_str(input, pattern) {
            return Ok(dt);
        }
    }

    // If no time component, parse as date and add 00:00:00
    if let Ok(parsed_date) = parse_date(input) {
        let datetime = parsed_date
            .date
            .and_hms_opt(0, 0, 0)
            .ok_or_else(|| DateParseError::InvalidDate(input.to_string()))?;
        return Ok(datetime);
    }

    Err(DateParseError::UnsupportedFormat(input.to_string()))
}

/// Format a date using the specified format type
pub fn format_date(date: &NaiveDate, format_type: DateFormatType) -> String {
    match format_type {
        DateFormatType::Written => date.format("%Y/%m/%d").to_string(),
        DateFormatType::Printed => date.format("%y-%b-%d").to_string(),
        DateFormatType::Custom(ref pattern) => date.format(pattern).to_string(),
    }
}

/// Format a datetime using the specified format type
pub fn format_datetime(datetime: &NaiveDateTime, format_type: DateFormatType) -> String {
    match format_type {
        DateFormatType::Written => datetime.format("%Y/%m/%d %H:%M:%S").to_string(),
        DateFormatType::Printed => datetime.format("%y-%b-%d %H:%M:%S").to_string(),
        DateFormatType::Custom(ref pattern) => datetime.format(pattern).to_string(),
    }
}

/// Date formatting types
#[derive(Debug, Clone)]
pub enum DateFormatType {
    /// Format for writing to files
    Written,
    /// Format for printing to console
    Printed,
    /// Custom format string
    Custom(String),
}

/// Period types for recurring intervals
#[derive(Debug, Clone, PartialEq)]
pub enum Period {
    /// Daily intervals
    Daily(u32),
    /// Weekly intervals  
    Weekly(u32),
    /// Bi-weekly (every 2 weeks)
    Biweekly,
    /// Monthly intervals
    Monthly(u32),
    /// Bi-monthly (every 2 months)
    Bimonthly,
    /// Quarterly intervals (every 3 months)
    Quarterly(u32),
    /// Yearly intervals
    Yearly(u32),
}

impl Period {
    /// Get the number of days in the period (approximate for non-fixed periods)
    pub fn approximate_days(&self) -> u32 {
        match self {
            Period::Daily(n) => *n,
            Period::Weekly(n) => n * 7,
            Period::Biweekly => 14,
            Period::Monthly(n) => n * 30,   // Approximate
            Period::Bimonthly => 60,        // Approximate
            Period::Quarterly(n) => n * 90, // Approximate
            Period::Yearly(n) => n * 365,   // Approximate
        }
    }

    /// Get the period name as a string
    pub fn name(&self) -> String {
        match self {
            Period::Daily(1) => "daily".to_string(),
            Period::Daily(n) => format!("every {} days", n),
            Period::Weekly(1) => "weekly".to_string(),
            Period::Weekly(n) => format!("every {} weeks", n),
            Period::Biweekly => "biweekly".to_string(),
            Period::Monthly(1) => "monthly".to_string(),
            Period::Monthly(n) => format!("every {} months", n),
            Period::Bimonthly => "bimonthly".to_string(),
            Period::Quarterly(1) => "quarterly".to_string(),
            Period::Quarterly(n) => format!("every {} quarters", n),
            Period::Yearly(1) => "yearly".to_string(),
            Period::Yearly(n) => format!("every {} years", n),
        }
    }

    /// Add this period to a date
    pub fn add_to_date(&self, date: NaiveDate) -> NaiveDate {
        match self {
            Period::Daily(n) => date + chrono::Duration::days(*n as i64),
            Period::Weekly(n) => date + chrono::Duration::weeks(*n as i64),
            Period::Biweekly => date + chrono::Duration::weeks(2),
            Period::Monthly(n) => {
                // Handle month arithmetic properly
                let months_to_add = *n as i64;
                let mut new_year = date.year() as i64;
                let mut new_month = date.month() as i64 + months_to_add;

                while new_month > 12 {
                    new_year += 1;
                    new_month -= 12;
                }

                let new_day =
                    std::cmp::min(date.day(), days_in_month(new_year as i32, new_month as u32));

                NaiveDate::from_ymd_opt(new_year as i32, new_month as u32, new_day).unwrap_or(date)
            }
            Period::Bimonthly => Period::Monthly(2).add_to_date(date),
            Period::Quarterly(n) => Period::Monthly(n * 3).add_to_date(date),
            Period::Yearly(n) => {
                let new_year = date.year() + (*n as i32);
                let new_day = if date.month() == 2 && date.day() == 29 && !is_leap_year(new_year) {
                    28 // Adjust Feb 29 to Feb 28 in non-leap years
                } else {
                    date.day()
                };

                NaiveDate::from_ymd_opt(new_year, date.month(), new_day).unwrap_or(date)
            }
        }
    }

    /// Subtract this period from a date  
    pub fn subtract_from_date(&self, date: NaiveDate) -> NaiveDate {
        match self {
            Period::Daily(n) => date - chrono::Duration::days(*n as i64),
            Period::Weekly(n) => date - chrono::Duration::weeks(*n as i64),
            Period::Biweekly => date - chrono::Duration::weeks(2),
            Period::Monthly(n) => {
                let months_to_subtract = *n as i64;
                let mut new_year = date.year() as i64;
                let mut new_month = date.month() as i64 - months_to_subtract;

                while new_month <= 0 {
                    new_year -= 1;
                    new_month += 12;
                }

                let new_day =
                    std::cmp::min(date.day(), days_in_month(new_year as i32, new_month as u32));

                NaiveDate::from_ymd_opt(new_year as i32, new_month as u32, new_day).unwrap_or(date)
            }
            Period::Bimonthly => Period::Monthly(2).subtract_from_date(date),
            Period::Quarterly(n) => Period::Monthly(n * 3).subtract_from_date(date),
            Period::Yearly(n) => {
                let new_year = date.year() - (*n as i32);
                let new_day = if date.month() == 2 && date.day() == 29 && !is_leap_year(new_year) {
                    28 // Adjust Feb 29 to Feb 28 in non-leap years
                } else {
                    date.day()
                };

                NaiveDate::from_ymd_opt(new_year, date.month(), new_day).unwrap_or(date)
            }
        }
    }
}

/// A date interval with start, end, and period
#[derive(Debug, Clone)]
pub struct DateInterval {
    /// Start date of the interval
    pub start: Option<NaiveDate>,
    /// End date of the interval (exclusive)
    pub end: Option<NaiveDate>,
    /// Period for recurring intervals
    pub period: Option<Period>,
    /// Whether the end date is inclusive
    pub end_inclusive: bool,
}

impl DateInterval {
    /// Create a new date interval
    pub fn new(start: Option<NaiveDate>, end: Option<NaiveDate>, period: Option<Period>) -> Self {
        Self { start, end, period, end_inclusive: false }
    }

    /// Create an interval from a period expression  
    pub fn from_period(period: Period) -> Self {
        Self { start: None, end: None, period: Some(period), end_inclusive: false }
    }

    /// Create an interval from a date range
    pub fn from_range(start: NaiveDate, end: NaiveDate, inclusive: bool) -> Self {
        Self { start: Some(start), end: Some(end), period: None, end_inclusive: inclusive }
    }

    /// Check if a date falls within this interval
    pub fn contains(&self, date: NaiveDate) -> bool {
        let after_start = self.start.map_or(true, |start| date >= start);
        let before_end = if let Some(end) = self.end {
            if self.end_inclusive {
                date <= end
            } else {
                date < end
            }
        } else {
            true
        };

        after_start && before_end
    }

    /// Check if this interval overlaps with another interval
    pub fn overlaps(&self, other: &DateInterval) -> bool {
        // If either interval is unbounded, they could overlap
        let self_start = self.start.unwrap_or(NaiveDate::MIN);
        let self_end = self.end.unwrap_or(NaiveDate::MAX);
        let other_start = other.start.unwrap_or(NaiveDate::MIN);
        let other_end = other.end.unwrap_or(NaiveDate::MAX);

        // Adjust for inclusivity
        let self_actual_end = if self.end_inclusive && self.end.is_some() {
            self_end + chrono::Duration::days(1)
        } else {
            self_end
        };

        let other_actual_end = if other.end_inclusive && other.end.is_some() {
            other_end + chrono::Duration::days(1)
        } else {
            other_end
        };

        // Check for overlap: intervals overlap if start1 < end2 && start2 < end1
        self_start < other_actual_end && other_start < self_actual_end
    }

    /// Add days to all dates in this interval
    pub fn add_days(&self, days: i64) -> Self {
        Self {
            start: self.start.map(|d| d + chrono::Duration::days(days)),
            end: self.end.map(|d| d + chrono::Duration::days(days)),
            period: self.period.clone(),
            end_inclusive: self.end_inclusive,
        }
    }

    /// Add months to all dates in this interval
    pub fn add_months(&self, months: i32) -> Self {
        Self {
            start: self.start.map(|d| add_months_to_date(d, months)),
            end: self.end.map(|d| add_months_to_date(d, months)),
            period: self.period.clone(),
            end_inclusive: self.end_inclusive,
        }
    }

    /// Add years to all dates in this interval
    pub fn add_years(&self, years: i32) -> Self {
        Self {
            start: self.start.map(|d| add_years_to_date(d, years)),
            end: self.end.map(|d| add_years_to_date(d, years)),
            period: self.period.clone(),
            end_inclusive: self.end_inclusive,
        }
    }

    /// Get the duration of this interval in days (if bounded)
    pub fn duration_days(&self) -> Option<i64> {
        match (self.start, self.end) {
            (Some(start), Some(end)) => {
                let actual_end =
                    if self.end_inclusive { end + chrono::Duration::days(1) } else { end };
                Some((actual_end - start).num_days())
            }
            _ => None, // Unbounded interval
        }
    }

    /// Check if this interval represents a fiscal year
    pub fn is_fiscal_year(&self, fiscal_year_start_month: u32) -> bool {
        match (self.start, self.end) {
            (Some(start), Some(end)) => {
                // Check if start is the first day of the fiscal year
                if start.month() != fiscal_year_start_month || start.day() != 1 {
                    return false;
                }

                // Calculate expected end month (previous month of fiscal year start)
                let expected_end_month = if fiscal_year_start_month == 1 {
                    12 // If fiscal year starts in Jan, it ends in Dec
                } else {
                    fiscal_year_start_month - 1
                };

                // Calculate expected end year
                let expected_end_year = if fiscal_year_start_month == 1 {
                    start.year() // Calendar year
                } else {
                    start.year() + 1 // Crosses year boundary
                };

                // Check if end matches fiscal year end
                if self.end_inclusive {
                    // Inclusive end should be the last day of the fiscal year
                    end.year() == expected_end_year
                        && end.month() == expected_end_month
                        && end.day() == days_in_month(expected_end_year, expected_end_month)
                } else {
                    // Exclusive end should be the first day of the next fiscal year
                    let next_fy_year =
                        expected_end_year + if expected_end_month == 12 { 1 } else { 0 };
                    let next_fy_month =
                        if expected_end_month == 12 { 1 } else { expected_end_month + 1 };

                    end.year() == next_fy_year && end.month() == next_fy_month && end.day() == 1
                }
            }
            _ => false,
        }
    }

    /// Create a fiscal year interval
    pub fn fiscal_year(year: i32, start_month: u32) -> Self {
        let start = NaiveDate::from_ymd_opt(year, start_month, 1)
            .unwrap_or_else(|| NaiveDate::from_ymd_opt(year, 1, 1).unwrap());

        let end_year = if start_month == 1 { year } else { year + 1 };
        let end_month = if start_month == 1 { 12 } else { start_month - 1 };
        let end_day = days_in_month(end_year, end_month);

        let end = NaiveDate::from_ymd_opt(end_year, end_month, end_day)
            .unwrap_or_else(|| NaiveDate::from_ymd_opt(end_year, 12, 31).unwrap());

        Self::from_range(start, end, true) // Fiscal years are typically inclusive
    }

    /// Get the next date in the period after the given date
    pub fn next_date_after(&self, date: NaiveDate) -> Option<NaiveDate> {
        let period = self.period.as_ref()?;
        let next = period.add_to_date(date);

        if self.contains(next) {
            Some(next)
        } else {
            None
        }
    }

    /// Create an iterator over dates in this interval
    pub fn iter_dates(&self) -> DateIterator {
        DateIterator::new(self.clone())
    }
}

/// Iterator over dates in a DateInterval
pub struct DateIterator {
    interval: DateInterval,
    current: Option<NaiveDate>,
    done: bool,
}

impl DateIterator {
    fn new(interval: DateInterval) -> Self {
        Self { current: interval.start, interval, done: false }
    }
}

impl Iterator for DateIterator {
    type Item = NaiveDate;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let current = self.current?;

        // Check if we're still within bounds
        if !self.interval.contains(current) {
            self.done = true;
            return None;
        }

        let result = current;

        // Advance to next date
        if let Some(period) = &self.interval.period {
            self.current = Some(period.add_to_date(current));
        } else {
            // No period means single date
            self.done = true;
        }

        Some(result)
    }
}

/// Errors that can occur during period parsing
#[derive(Error, Debug, PartialEq)]
pub enum PeriodParseError {
    #[error("Invalid period format: {0}")]
    InvalidFormat(String),
    #[error("Unknown period keyword: {0}")]
    UnknownKeyword(String),
    #[error("Invalid number in period: {0}")]
    InvalidNumber(String),
    #[error("Missing period specification")]
    MissingPeriod,
}

/// Parse a period expression string
pub fn parse_period(input: &str) -> Result<DateInterval, PeriodParseError> {
    let input = input.trim().to_lowercase();

    if input.is_empty() {
        return Err(PeriodParseError::MissingPeriod);
    }

    // Handle simple period keywords first
    match input.as_str() {
        "daily" => return Ok(DateInterval::from_period(Period::Daily(1))),
        "weekly" => return Ok(DateInterval::from_period(Period::Weekly(1))),
        "biweekly" => return Ok(DateInterval::from_period(Period::Biweekly)),
        "monthly" => return Ok(DateInterval::from_period(Period::Monthly(1))),
        "bimonthly" => return Ok(DateInterval::from_period(Period::Bimonthly)),
        "quarterly" => return Ok(DateInterval::from_period(Period::Quarterly(1))),
        "yearly" => return Ok(DateInterval::from_period(Period::Yearly(1))),
        _ => {}
    }

    // Handle "every N period" format
    if input.starts_with("every ") {
        return parse_every_period(&input[6..]);
    }

    // Handle date ranges like "from 2024/01 to 2024/12"
    if input.contains(" to ") || input.contains(" - ") {
        return parse_period_range(&input);
    }

    // Handle relative periods like "last month", "next week", "this year"
    if input.starts_with("last ") || input.starts_with("next ") || input.starts_with("this ") {
        return parse_relative_period(&input);
    }

    Err(PeriodParseError::InvalidFormat(input.to_string()))
}

/// Parse "every N period" expressions
fn parse_every_period(input: &str) -> Result<DateInterval, PeriodParseError> {
    let parts: Vec<&str> = input.split_whitespace().collect();

    if parts.len() == 1 {
        // "every day", "every week", etc.
        match parts[0] {
            "day" => return Ok(DateInterval::from_period(Period::Daily(1))),
            "week" => return Ok(DateInterval::from_period(Period::Weekly(1))),
            "month" => return Ok(DateInterval::from_period(Period::Monthly(1))),
            "quarter" => return Ok(DateInterval::from_period(Period::Quarterly(1))),
            "year" => return Ok(DateInterval::from_period(Period::Yearly(1))),
            _ => return Err(PeriodParseError::UnknownKeyword(parts[0].to_string())),
        }
    }

    if parts.len() == 2 {
        // "every 3 days", "every 2 weeks", etc.
        let number: u32 =
            parts[0].parse().map_err(|_| PeriodParseError::InvalidNumber(parts[0].to_string()))?;

        match parts[1] {
            "day" | "days" => return Ok(DateInterval::from_period(Period::Daily(number))),
            "week" | "weeks" => return Ok(DateInterval::from_period(Period::Weekly(number))),
            "month" | "months" => return Ok(DateInterval::from_period(Period::Monthly(number))),
            "quarter" | "quarters" => {
                return Ok(DateInterval::from_period(Period::Quarterly(number)))
            }
            "year" | "years" => return Ok(DateInterval::from_period(Period::Yearly(number))),
            _ => return Err(PeriodParseError::UnknownKeyword(parts[1].to_string())),
        }
    }

    Err(PeriodParseError::InvalidFormat(input.to_string()))
}

/// Parse date range expressions like "from 2024/01 to 2024/12"
fn parse_period_range(input: &str) -> Result<DateInterval, PeriodParseError> {
    let separator = if input.contains(" to ") { " to " } else { " - " };
    let parts: Vec<&str> = input.split(separator).collect();

    if parts.len() != 2 {
        return Err(PeriodParseError::InvalidFormat(input.to_string()));
    }

    let start_str = parts[0].trim();
    let end_str = parts[1].trim();

    // Remove "from" prefix if present
    let start_str = if start_str.starts_with("from ") { &start_str[5..] } else { start_str };

    let start_date = parse_date(start_str)
        .map_err(|_| PeriodParseError::InvalidFormat(start_str.to_string()))?
        .date;

    let end_date =
        parse_date(end_str).map_err(|_| PeriodParseError::InvalidFormat(end_str.to_string()))?.date;

    let inclusive = separator == " - ";
    Ok(DateInterval::from_range(start_date, end_date, inclusive))
}

/// Parse relative period expressions like "last month", "next week"
fn parse_relative_period(input: &str) -> Result<DateInterval, PeriodParseError> {
    let parts: Vec<&str> = input.split_whitespace().collect();

    if parts.len() != 2 {
        return Err(PeriodParseError::InvalidFormat(input.to_string()));
    }

    let modifier = parts[0]; // "last", "next", "this"
    let period_name = parts[1]; // "day", "week", "month", etc.

    let today = Local::now().date_naive();

    let (start, end) = match period_name {
        "day" => {
            let date = match modifier {
                "last" => today - chrono::Duration::days(1),
                "next" => today + chrono::Duration::days(1),
                "this" => today,
                _ => return Err(PeriodParseError::UnknownKeyword(modifier.to_string())),
            };
            (date, date + chrono::Duration::days(1))
        }
        "week" => {
            let start_of_week = find_start_of_week(today);
            let (start, end) = match modifier {
                "last" => {
                    let start = start_of_week - chrono::Duration::days(7);
                    (start, start + chrono::Duration::days(7))
                }
                "next" => {
                    let start = start_of_week + chrono::Duration::days(7);
                    (start, start + chrono::Duration::days(7))
                }
                "this" => (start_of_week, start_of_week + chrono::Duration::days(7)),
                _ => return Err(PeriodParseError::UnknownKeyword(modifier.to_string())),
            };
            (start, end)
        }
        "month" => {
            let start_of_month = NaiveDate::from_ymd_opt(today.year(), today.month(), 1)
                .ok_or_else(|| PeriodParseError::InvalidFormat("Invalid month".to_string()))?;

            let (start, end) = match modifier {
                "last" => {
                    let prev_month_start = if today.month() == 1 {
                        NaiveDate::from_ymd_opt(today.year() - 1, 12, 1)
                    } else {
                        NaiveDate::from_ymd_opt(today.year(), today.month() - 1, 1)
                    }
                    .ok_or_else(|| PeriodParseError::InvalidFormat("Invalid month".to_string()))?;

                    (prev_month_start, start_of_month)
                }
                "next" => {
                    let next_month_start = if today.month() == 12 {
                        NaiveDate::from_ymd_opt(today.year() + 1, 1, 1)
                    } else {
                        NaiveDate::from_ymd_opt(today.year(), today.month() + 1, 1)
                    }
                    .ok_or_else(|| PeriodParseError::InvalidFormat("Invalid month".to_string()))?;

                    let next_next_month_start = if next_month_start.month() == 12 {
                        NaiveDate::from_ymd_opt(next_month_start.year() + 1, 1, 1)
                    } else {
                        NaiveDate::from_ymd_opt(
                            next_month_start.year(),
                            next_month_start.month() + 1,
                            1,
                        )
                    }
                    .ok_or_else(|| PeriodParseError::InvalidFormat("Invalid month".to_string()))?;

                    (next_month_start, next_next_month_start)
                }
                "this" => {
                    let next_month_start = if today.month() == 12 {
                        NaiveDate::from_ymd_opt(today.year() + 1, 1, 1)
                    } else {
                        NaiveDate::from_ymd_opt(today.year(), today.month() + 1, 1)
                    }
                    .ok_or_else(|| PeriodParseError::InvalidFormat("Invalid month".to_string()))?;

                    (start_of_month, next_month_start)
                }
                _ => return Err(PeriodParseError::UnknownKeyword(modifier.to_string())),
            };
            (start, end)
        }
        "year" => {
            let start_of_year = NaiveDate::from_ymd_opt(today.year(), 1, 1)
                .ok_or_else(|| PeriodParseError::InvalidFormat("Invalid year".to_string()))?;

            let (start, end) = match modifier {
                "last" => {
                    let prev_year_start = NaiveDate::from_ymd_opt(today.year() - 1, 1, 1)
                        .ok_or_else(|| {
                            PeriodParseError::InvalidFormat("Invalid year".to_string())
                        })?;
                    (prev_year_start, start_of_year)
                }
                "next" => {
                    let next_year_start = NaiveDate::from_ymd_opt(today.year() + 1, 1, 1)
                        .ok_or_else(|| {
                            PeriodParseError::InvalidFormat("Invalid year".to_string())
                        })?;
                    let next_next_year_start = NaiveDate::from_ymd_opt(today.year() + 2, 1, 1)
                        .ok_or_else(|| {
                            PeriodParseError::InvalidFormat("Invalid year".to_string())
                        })?;
                    (next_year_start, next_next_year_start)
                }
                "this" => {
                    let next_year_start = NaiveDate::from_ymd_opt(today.year() + 1, 1, 1)
                        .ok_or_else(|| {
                            PeriodParseError::InvalidFormat("Invalid year".to_string())
                        })?;
                    (start_of_year, next_year_start)
                }
                _ => return Err(PeriodParseError::UnknownKeyword(modifier.to_string())),
            };
            (start, end)
        }
        _ => return Err(PeriodParseError::UnknownKeyword(period_name.to_string())),
    };

    Ok(DateInterval::from_range(start, end, false))
}

/// Helper function to find the start of the week (Sunday)
fn find_start_of_week(date: NaiveDate) -> NaiveDate {
    let days_since_sunday = date.weekday().num_days_from_sunday();
    date - chrono::Duration::days(days_since_sunday as i64)
}

/// Helper function to get days in a month
fn days_in_month(year: i32, month: u32) -> u32 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 => {
            if is_leap_year(year) {
                29
            } else {
                28
            }
        }
        _ => 30, // fallback
    }
}

/// Helper function to check if a year is a leap year
fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

/// Helper function to add months to a date with proper month-end handling
fn add_months_to_date(date: NaiveDate, months: i32) -> NaiveDate {
    let mut new_year = date.year();
    let mut new_month = date.month() as i32 + months;

    // Handle year overflow/underflow
    while new_month > 12 {
        new_year += 1;
        new_month -= 12;
    }
    while new_month < 1 {
        new_year -= 1;
        new_month += 12;
    }

    // Handle day overflow (e.g., Jan 31 + 1 month should be Feb 28/29)
    let new_day = std::cmp::min(date.day(), days_in_month(new_year, new_month as u32));

    NaiveDate::from_ymd_opt(new_year, new_month as u32, new_day).unwrap_or(date)
}

/// Helper function to add years to a date with leap year handling
fn add_years_to_date(date: NaiveDate, years: i32) -> NaiveDate {
    let new_year = date.year() + years;

    // Handle Feb 29 in leap year -> non-leap year
    let new_day = if date.month() == 2 && date.day() == 29 && !is_leap_year(new_year) {
        28
    } else {
        date.day()
    };

    NaiveDate::from_ymd_opt(new_year, date.month(), new_day).unwrap_or(date)
}

/// Duration type for relative date calculations
#[derive(Debug, Clone, PartialEq)]
pub struct DateDuration {
    /// Number of years
    pub years: i32,
    /// Number of months
    pub months: i32,
    /// Number of days
    pub days: i64,
}

impl DateDuration {
    /// Create a new DateDuration
    pub fn new(years: i32, months: i32, days: i64) -> Self {
        Self { years, months, days }
    }

    /// Create a duration from days only
    pub fn from_days(days: i64) -> Self {
        Self { years: 0, months: 0, days }
    }

    /// Create a duration from months only
    pub fn from_months(months: i32) -> Self {
        Self { years: 0, months, days: 0 }
    }

    /// Create a duration from years only
    pub fn from_years(years: i32) -> Self {
        Self { years, months: 0, days: 0 }
    }

    /// Add this duration to a date
    pub fn add_to_date(&self, date: NaiveDate) -> NaiveDate {
        let mut result = date;

        // Add years first
        if self.years != 0 {
            result = add_years_to_date(result, self.years);
        }

        // Then months
        if self.months != 0 {
            result = add_months_to_date(result, self.months);
        }

        // Finally days
        if self.days != 0 {
            result = result + chrono::Duration::days(self.days);
        }

        result
    }

    /// Subtract this duration from a date
    pub fn subtract_from_date(&self, date: NaiveDate) -> NaiveDate {
        let mut result = date;

        // Subtract in reverse order
        if self.days != 0 {
            result = result - chrono::Duration::days(self.days);
        }

        if self.months != 0 {
            result = add_months_to_date(result, -self.months);
        }

        if self.years != 0 {
            result = add_years_to_date(result, -self.years);
        }

        result
    }

    /// Get an approximate number of days for this duration
    pub fn approximate_days(&self) -> i64 {
        (self.years as i64 * 365) + (self.months as i64 * 30) + self.days
    }

    /// Check if this is a zero duration
    pub fn is_zero(&self) -> bool {
        self.years == 0 && self.months == 0 && self.days == 0
    }
}

impl std::ops::Add for DateDuration {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            years: self.years + other.years,
            months: self.months + other.months,
            days: self.days + other.days,
        }
    }
}

impl std::ops::Sub for DateDuration {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            years: self.years - other.years,
            months: self.months - other.months,
            days: self.days - other.days,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Timelike;

    #[test]
    fn test_iso_date_parsing() {
        let parsed = parse_date("2023-12-25").unwrap();
        assert_eq!(parsed.date, NaiveDate::from_ymd_opt(2023, 12, 25).unwrap());
        assert_eq!(parsed.format_hint, DateFormat::IsoYmd);
        assert!(parsed.traits.has_year);
        assert!(parsed.traits.has_month);
        assert!(parsed.traits.has_day);
    }

    #[test]
    fn test_us_date_parsing() {
        let parsed = parse_date("12/25/2023").unwrap();
        assert_eq!(parsed.date, NaiveDate::from_ymd_opt(2023, 12, 25).unwrap());
        assert_eq!(parsed.format_hint, DateFormat::UsMdy);
    }

    #[test]
    fn test_short_year_parsing() {
        let parsed = parse_date("12/25/23").unwrap();
        let expected_year = expand_two_digit_year(23);
        assert_eq!(parsed.date, NaiveDate::from_ymd_opt(expected_year, 12, 25).unwrap());
    }

    #[test]
    fn test_month_day_only() {
        let parsed = parse_date("12/25").unwrap();
        assert_eq!(parsed.date.month(), 12);
        assert_eq!(parsed.date.day(), 25);
        assert!(!parsed.traits.has_year);
    }

    #[test]
    fn test_relative_dates() {
        let today = Local::now().date_naive();

        let parsed_today = parse_date("today").unwrap();
        assert_eq!(parsed_today.date, today);

        let parsed_yesterday = parse_date("yesterday").unwrap();
        assert_eq!(parsed_yesterday.date, today - chrono::Duration::days(1));

        let parsed_tomorrow = parse_date("tomorrow").unwrap();
        assert_eq!(parsed_tomorrow.date, today + chrono::Duration::days(1));
    }

    #[test]
    fn test_named_months() {
        let parsed = parse_date("February 15, 2023").unwrap();
        assert_eq!(parsed.date, NaiveDate::from_ymd_opt(2023, 2, 15).unwrap());

        let parsed2 = parse_date("15-Feb-2023").unwrap();
        assert_eq!(parsed2.date, NaiveDate::from_ymd_opt(2023, 2, 15).unwrap());

        let parsed3 = parse_date("Feb 15").unwrap();
        let current_year = Local::now().date_naive().year();
        assert_eq!(parsed3.date, NaiveDate::from_ymd_opt(current_year, 2, 15).unwrap());
        assert!(!parsed3.traits.has_year);
    }

    #[test]
    fn test_compact_dates() {
        let parsed = parse_date("20231225").unwrap();
        assert_eq!(parsed.date, NaiveDate::from_ymd_opt(2023, 12, 25).unwrap());
        assert_eq!(parsed.format_hint, DateFormat::Compact);
    }

    #[test]
    fn test_european_dates() {
        let parsed = parse_date("25.12.2023").unwrap();
        assert_eq!(parsed.date, NaiveDate::from_ymd_opt(2023, 12, 25).unwrap());
        assert_eq!(parsed.format_hint, DateFormat::EuDmy);
    }

    #[test]
    fn test_year_only() {
        let parsed = parse_date("2023").unwrap();
        assert_eq!(parsed.date, NaiveDate::from_ymd_opt(2023, 1, 1).unwrap());
        assert!(parsed.traits.has_year);
        assert!(!parsed.traits.has_month);
        assert!(!parsed.traits.has_day);
    }

    #[test]
    fn test_datetime_parsing() {
        let datetime = parse_datetime("2023-12-25 14:30:00").unwrap();
        assert_eq!(datetime.date(), NaiveDate::from_ymd_opt(2023, 12, 25).unwrap());
        assert_eq!(datetime.hour(), 14);
        assert_eq!(datetime.minute(), 30);
        assert_eq!(datetime.second(), 0);
    }

    #[test]
    fn test_separator_normalization() {
        let parsed1 = parse_date("2023-12-25").unwrap();
        let parsed2 = parse_date("2023.12.25").unwrap();
        let parsed3 = parse_date("2023/12/25").unwrap();

        assert_eq!(parsed1.date, parsed2.date);
        assert_eq!(parsed2.date, parsed3.date);
    }

    #[test]
    fn test_invalid_dates() {
        assert!(parse_date("2023-13-25").is_err()); // Invalid month
        assert!(parse_date("2023-12-32").is_err()); // Invalid day
        assert!(parse_date("not-a-date").is_err());
        assert!(parse_date("").is_err());
    }

    #[test]
    fn test_two_digit_year_pivot() {
        let current_year = Local::now().year() % 100;
        let pivot = current_year + 20;

        // Years within 20 of current should go to current century
        if current_year < 80 {
            // Avoid overflow
            let test_year = current_year + 10;
            let expanded = expand_two_digit_year(test_year as u32);
            assert!(expanded >= 2000 && expanded < 2100);
        }

        // Years more than 20 ahead should go to previous century
        if pivot < 100 {
            let test_year = pivot + 10;
            let expanded = expand_two_digit_year(test_year as u32);
            assert!(expanded >= 1900 && expanded < 2000);
        }
    }

    #[test]
    fn test_period_parsing() {
        // Simple periods
        let daily = parse_period("daily").unwrap();
        assert_eq!(daily.period, Some(Period::Daily(1)));

        let weekly = parse_period("weekly").unwrap();
        assert_eq!(weekly.period, Some(Period::Weekly(1)));

        let monthly = parse_period("monthly").unwrap();
        assert_eq!(monthly.period, Some(Period::Monthly(1)));

        let quarterly = parse_period("quarterly").unwrap();
        assert_eq!(quarterly.period, Some(Period::Quarterly(1)));

        let yearly = parse_period("yearly").unwrap();
        assert_eq!(yearly.period, Some(Period::Yearly(1)));

        let biweekly = parse_period("biweekly").unwrap();
        assert_eq!(biweekly.period, Some(Period::Biweekly));

        let bimonthly = parse_period("bimonthly").unwrap();
        assert_eq!(bimonthly.period, Some(Period::Bimonthly));
    }

    #[test]
    fn test_every_period_parsing() {
        // "every N period" format
        let every_3_days = parse_period("every 3 days").unwrap();
        assert_eq!(every_3_days.period, Some(Period::Daily(3)));

        let every_2_weeks = parse_period("every 2 weeks").unwrap();
        assert_eq!(every_2_weeks.period, Some(Period::Weekly(2)));

        let every_6_months = parse_period("every 6 months").unwrap();
        assert_eq!(every_6_months.period, Some(Period::Monthly(6)));

        // Singular forms
        let every_day = parse_period("every day").unwrap();
        assert_eq!(every_day.period, Some(Period::Daily(1)));

        let every_week = parse_period("every week").unwrap();
        assert_eq!(every_week.period, Some(Period::Weekly(1)));
    }

    #[test]
    fn test_period_range_parsing() {
        // Date ranges
        let range = parse_period("from 2023/01/01 to 2023/12/31").unwrap();
        assert_eq!(range.start, Some(NaiveDate::from_ymd_opt(2023, 1, 1).unwrap()));
        assert_eq!(range.end, Some(NaiveDate::from_ymd_opt(2023, 12, 31).unwrap()));
        assert!(!range.end_inclusive);

        let range_dash = parse_period("2023/01/01 - 2023/12/31").unwrap();
        assert_eq!(range_dash.start, Some(NaiveDate::from_ymd_opt(2023, 1, 1).unwrap()));
        assert_eq!(range_dash.end, Some(NaiveDate::from_ymd_opt(2023, 12, 31).unwrap()));
        assert!(range_dash.end_inclusive);
    }

    #[test]
    fn test_relative_period_parsing() {
        // Note: These tests are time-dependent, so we just verify they parse without error
        // and have reasonable start/end dates
        let last_week = parse_period("last week").unwrap();
        assert!(last_week.start.is_some());
        assert!(last_week.end.is_some());

        let this_month = parse_period("this month").unwrap();
        assert!(this_month.start.is_some());
        assert!(this_month.end.is_some());

        let next_year = parse_period("next year").unwrap();
        assert!(next_year.start.is_some());
        assert!(next_year.end.is_some());
    }

    #[test]
    fn test_period_arithmetic() {
        let start_date = NaiveDate::from_ymd_opt(2023, 1, 15).unwrap();

        // Daily period
        let daily = Period::Daily(7);
        let next_date = daily.add_to_date(start_date);
        assert_eq!(next_date, NaiveDate::from_ymd_opt(2023, 1, 22).unwrap());

        // Weekly period
        let weekly = Period::Weekly(2);
        let next_date = weekly.add_to_date(start_date);
        assert_eq!(next_date, NaiveDate::from_ymd_opt(2023, 1, 29).unwrap());

        // Monthly period (with month-end handling)
        let monthly = Period::Monthly(1);
        let jan_31 = NaiveDate::from_ymd_opt(2023, 1, 31).unwrap();
        let next_month = monthly.add_to_date(jan_31);
        // Should be Feb 28, not Feb 31 (which doesn't exist)
        assert_eq!(next_month, NaiveDate::from_ymd_opt(2023, 2, 28).unwrap());

        // Yearly period with leap year handling
        let yearly = Period::Yearly(1);
        let leap_day = NaiveDate::from_ymd_opt(2020, 2, 29).unwrap(); // 2020 is leap year
        let next_year = yearly.add_to_date(leap_day);
        // Should be Feb 28, 2021 (2021 is not leap year)
        assert_eq!(next_year, NaiveDate::from_ymd_opt(2021, 2, 28).unwrap());
    }

    #[test]
    fn test_date_interval_contains() {
        let start = NaiveDate::from_ymd_opt(2023, 1, 1).unwrap();
        let end = NaiveDate::from_ymd_opt(2023, 12, 31).unwrap();

        let interval = DateInterval::from_range(start, end, false); // Exclusive end

        assert!(interval.contains(NaiveDate::from_ymd_opt(2023, 6, 15).unwrap()));
        assert!(interval.contains(start)); // Start is inclusive
        assert!(!interval.contains(end)); // End is exclusive
        assert!(!interval.contains(NaiveDate::from_ymd_opt(2022, 12, 31).unwrap()));
        assert!(!interval.contains(NaiveDate::from_ymd_opt(2024, 1, 1).unwrap()));

        let inclusive_interval = DateInterval::from_range(start, end, true); // Inclusive end
        assert!(inclusive_interval.contains(end)); // End is inclusive
    }

    #[test]
    fn test_date_interval_iterator() {
        let start = NaiveDate::from_ymd_opt(2023, 1, 1).unwrap();
        let end = NaiveDate::from_ymd_opt(2023, 1, 8).unwrap();
        let period = Period::Daily(2); // Every 2 days

        let mut interval = DateInterval::new(Some(start), Some(end), Some(period));
        interval.end_inclusive = false;

        let dates: Vec<_> = interval.iter_dates().take(5).collect(); // Limit to avoid infinite iteration

        assert_eq!(dates[0], NaiveDate::from_ymd_opt(2023, 1, 1).unwrap());
        assert_eq!(dates[1], NaiveDate::from_ymd_opt(2023, 1, 3).unwrap());
        assert_eq!(dates[2], NaiveDate::from_ymd_opt(2023, 1, 5).unwrap());
        assert_eq!(dates[3], NaiveDate::from_ymd_opt(2023, 1, 7).unwrap());
        // Should have 4 items: 1/1, 1/3, 1/5, 1/7 (1/9 is beyond end date)
        assert_eq!(dates.len(), 4);
    }

    #[test]
    fn test_period_names() {
        assert_eq!(Period::Daily(1).name(), "daily");
        assert_eq!(Period::Daily(3).name(), "every 3 days");
        assert_eq!(Period::Weekly(1).name(), "weekly");
        assert_eq!(Period::Weekly(2).name(), "every 2 weeks");
        assert_eq!(Period::Monthly(1).name(), "monthly");
        assert_eq!(Period::Biweekly.name(), "biweekly");
        assert_eq!(Period::Quarterly(1).name(), "quarterly");
        assert_eq!(Period::Yearly(1).name(), "yearly");
    }

    #[test]
    fn test_invalid_period_parsing() {
        assert!(parse_period("").is_err());
        assert!(parse_period("invalid").is_err());
        assert!(parse_period("every").is_err());
        assert!(parse_period("every abc days").is_err());
        assert!(parse_period("every 3 xyz").is_err());
        assert!(parse_period("from 2023/01/01").is_err());
        assert!(parse_period("random keyword").is_err());
    }

    #[test]
    fn test_leap_year_helper() {
        assert!(is_leap_year(2000)); // Divisible by 400
        assert!(is_leap_year(2004)); // Divisible by 4, not by 100
        assert!(!is_leap_year(1900)); // Divisible by 100, not by 400
        assert!(!is_leap_year(2001)); // Not divisible by 4
        assert!(is_leap_year(2020));
        assert!(!is_leap_year(2021));
    }

    #[test]
    fn test_days_in_month_helper() {
        assert_eq!(days_in_month(2023, 1), 31); // January
        assert_eq!(days_in_month(2023, 2), 28); // February (non-leap)
        assert_eq!(days_in_month(2020, 2), 29); // February (leap year)
        assert_eq!(days_in_month(2023, 4), 30); // April
        assert_eq!(days_in_month(2023, 12), 31); // December
    }

    #[test]
    fn test_date_interval_overlaps() {
        let interval1 = DateInterval::from_range(
            NaiveDate::from_ymd_opt(2023, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2023, 6, 30).unwrap(),
            true,
        );

        let interval2 = DateInterval::from_range(
            NaiveDate::from_ymd_opt(2023, 6, 15).unwrap(),
            NaiveDate::from_ymd_opt(2023, 12, 31).unwrap(),
            false,
        );

        // These should overlap (June 15-30)
        assert!(interval1.overlaps(&interval2));
        assert!(interval2.overlaps(&interval1));

        let interval3 = DateInterval::from_range(
            NaiveDate::from_ymd_opt(2023, 7, 1).unwrap(),
            NaiveDate::from_ymd_opt(2023, 12, 31).unwrap(),
            false,
        );

        // These should not overlap
        assert!(!interval1.overlaps(&interval3));
        assert!(!interval3.overlaps(&interval1));
    }

    #[test]
    fn test_date_interval_arithmetic() {
        let interval = DateInterval::from_range(
            NaiveDate::from_ymd_opt(2023, 1, 15).unwrap(),
            NaiveDate::from_ymd_opt(2023, 1, 31).unwrap(),
            true,
        );

        // Add days
        let shifted = interval.add_days(10);
        assert_eq!(shifted.start, Some(NaiveDate::from_ymd_opt(2023, 1, 25).unwrap()));
        assert_eq!(shifted.end, Some(NaiveDate::from_ymd_opt(2023, 2, 10).unwrap()));

        // Add months
        let monthly_shift = interval.add_months(2);
        assert_eq!(monthly_shift.start, Some(NaiveDate::from_ymd_opt(2023, 3, 15).unwrap()));
        assert_eq!(monthly_shift.end, Some(NaiveDate::from_ymd_opt(2023, 3, 31).unwrap()));

        // Add years
        let yearly_shift = interval.add_years(1);
        assert_eq!(yearly_shift.start, Some(NaiveDate::from_ymd_opt(2024, 1, 15).unwrap()));
        assert_eq!(yearly_shift.end, Some(NaiveDate::from_ymd_opt(2024, 1, 31).unwrap()));
    }

    #[test]
    fn test_date_interval_duration() {
        let interval = DateInterval::from_range(
            NaiveDate::from_ymd_opt(2023, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2023, 1, 15).unwrap(),
            false, // Exclusive end
        );

        assert_eq!(interval.duration_days(), Some(14)); // 1st to 15th exclusive = 14 days

        let inclusive_interval = DateInterval::from_range(
            NaiveDate::from_ymd_opt(2023, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2023, 1, 15).unwrap(),
            true, // Inclusive end
        );

        assert_eq!(inclusive_interval.duration_days(), Some(15)); // 1st to 15th inclusive = 15 days

        // Unbounded interval
        let unbounded = DateInterval::from_period(Period::Daily(1));
        assert_eq!(unbounded.duration_days(), None);
    }

    #[test]
    fn test_fiscal_year() {
        // US fiscal year (Oct 1 - Sep 30)
        let fy_2023 = DateInterval::fiscal_year(2022, 10); // FY 2023 starts Oct 1, 2022
        assert_eq!(fy_2023.start, Some(NaiveDate::from_ymd_opt(2022, 10, 1).unwrap()));
        assert_eq!(fy_2023.end, Some(NaiveDate::from_ymd_opt(2023, 9, 30).unwrap()));
        assert!(fy_2023.end_inclusive);

        // Calendar year fiscal year (Jan 1 - Dec 31)
        let calendar_fy = DateInterval::fiscal_year(2023, 1);
        assert_eq!(calendar_fy.start, Some(NaiveDate::from_ymd_opt(2023, 1, 1).unwrap()));
        assert_eq!(calendar_fy.end, Some(NaiveDate::from_ymd_opt(2023, 12, 31).unwrap()));

        // Test fiscal year detection
        assert!(fy_2023.is_fiscal_year(10));
        assert!(calendar_fy.is_fiscal_year(1));
        assert!(!fy_2023.is_fiscal_year(1));
    }

    #[test]
    fn test_date_duration() {
        let duration = DateDuration::new(1, 6, 15); // 1 year, 6 months, 15 days
        let start_date = NaiveDate::from_ymd_opt(2023, 1, 1).unwrap();

        let result = duration.add_to_date(start_date);
        // Should be July 16, 2024 (1 year + 6 months + 15 days)
        assert_eq!(result, NaiveDate::from_ymd_opt(2024, 7, 16).unwrap());

        // Test subtraction
        let back = duration.subtract_from_date(result);
        assert_eq!(back, start_date);

        // Test edge cases with month-end dates
        let jan_31 = NaiveDate::from_ymd_opt(2023, 1, 31).unwrap();
        let one_month = DateDuration::from_months(1);
        let result = one_month.add_to_date(jan_31);
        // Should be Feb 28 (not Feb 31 which doesn't exist)
        assert_eq!(result, NaiveDate::from_ymd_opt(2023, 2, 28).unwrap());

        // Test leap year handling
        let feb_29_2020 = NaiveDate::from_ymd_opt(2020, 2, 29).unwrap();
        let one_year = DateDuration::from_years(1);
        let result = one_year.add_to_date(feb_29_2020);
        // Should be Feb 28, 2021 (2021 is not a leap year)
        assert_eq!(result, NaiveDate::from_ymd_opt(2021, 2, 28).unwrap());
    }

    #[test]
    fn test_date_duration_arithmetic() {
        let duration1 = DateDuration::new(1, 3, 10);
        let duration2 = DateDuration::new(0, 6, 5);

        let sum = duration1.clone() + duration2.clone();
        assert_eq!(sum, DateDuration::new(1, 9, 15));

        let diff = duration1 - duration2;
        assert_eq!(diff, DateDuration::new(1, -3, 5));

        // Test zero duration
        let zero = DateDuration::new(0, 0, 0);
        assert!(zero.is_zero());

        let non_zero = DateDuration::from_days(1);
        assert!(!non_zero.is_zero());
    }

    #[test]
    fn test_add_months_helper() {
        let jan_31 = NaiveDate::from_ymd_opt(2023, 1, 31).unwrap();

        // Adding 1 month to Jan 31 should give Feb 28
        let feb_result = add_months_to_date(jan_31, 1);
        assert_eq!(feb_result, NaiveDate::from_ymd_opt(2023, 2, 28).unwrap());

        // Adding 12 months should give next year same date
        let next_year = add_months_to_date(jan_31, 12);
        assert_eq!(next_year, NaiveDate::from_ymd_opt(2024, 1, 31).unwrap());

        // Subtracting months
        let dec_result = add_months_to_date(jan_31, -1);
        assert_eq!(dec_result, NaiveDate::from_ymd_opt(2022, 12, 31).unwrap());
    }

    #[test]
    fn test_add_years_helper() {
        let normal_date = NaiveDate::from_ymd_opt(2023, 6, 15).unwrap();
        let one_year_later = add_years_to_date(normal_date, 1);
        assert_eq!(one_year_later, NaiveDate::from_ymd_opt(2024, 6, 15).unwrap());

        // Test leap year edge case
        let leap_day = NaiveDate::from_ymd_opt(2020, 2, 29).unwrap();
        let non_leap_year = add_years_to_date(leap_day, 1);
        assert_eq!(non_leap_year, NaiveDate::from_ymd_opt(2021, 2, 28).unwrap());

        let back_to_leap = add_years_to_date(non_leap_year, 3); // 2024 is leap year
        assert_eq!(back_to_leap, NaiveDate::from_ymd_opt(2024, 2, 28).unwrap());
    }
}
