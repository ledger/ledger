//! Date and time handling for Ledger
//!
//! This module provides date and time functionality compatible with the C++ Ledger
//! implementation, including timezone support and various date formats.

use chrono::{DateTime, NaiveDate, NaiveDateTime, TimeZone, Utc, Datelike, Timelike, Days, Months, TimeDelta};
use chrono_tz::{Tz, America};
use std::fmt;
use thiserror::Error;
use serde::{Serialize, Deserialize};

/// Errors that can occur during date/time operations
#[derive(Error, Debug, Clone, PartialEq)]
pub enum DateTimeError {
    #[error("Invalid date format: {0}")]
    InvalidFormat(String),
    #[error("Date out of range: {0}")]
    OutOfRange(String),
    #[error("Timezone error: {0}")]
    TimezoneError(String),
    #[error("Ambiguous time during DST transition")]
    AmbiguousTime,
}

pub type DateTimeResult<T> = Result<T, DateTimeError>;

/// Default timezone for Ledger (America/Chicago for test compatibility)
pub static DEFAULT_TIMEZONE: Tz = America::Chicago;

/// Date type compatible with C++ date_t
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Date(pub NaiveDate);

impl Date {
    /// Create a new Date
    pub fn new(year: i32, month: u32, day: u32) -> DateTimeResult<Self> {
        NaiveDate::from_ymd_opt(year, month, day)
            .map(Date)
            .ok_or_else(|| DateTimeError::OutOfRange(format!("{}-{:02}-{:02}", year, month, day)))
    }

    /// Create a Date from a NaiveDate
    pub fn from_naive_date(date: NaiveDate) -> Self {
        Date(date)
    }

    /// Get the current date in the default timezone
    pub fn current() -> Self {
        let now = Utc::now().with_timezone(&DEFAULT_TIMEZONE);
        Date(now.date_naive())
    }

    /// Check if this is a valid date (always true for constructed dates)
    pub fn is_valid(&self) -> bool {
        true
    }

    /// Check if this is not a valid date (always false for constructed dates)
    pub fn is_not_a_date(&self) -> bool {
        false
    }

    /// Get the underlying NaiveDate
    pub fn naive_date(&self) -> NaiveDate {
        self.0
    }

    /// Get year
    pub fn year(&self) -> i32 {
        self.0.year()
    }

    /// Get month (1-12)
    pub fn month(&self) -> u32 {
        self.0.month()
    }

    /// Get day of month (1-31)
    pub fn day(&self) -> u32 {
        self.0.day()
    }
    
    /// Format the date using a format string
    pub fn format<'a>(&self, fmt: &'a str) -> impl fmt::Display + 'a {
        self.0.format(fmt)
    }
}

impl fmt::Display for Date {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.format("%Y-%m-%d"))
    }
}

// Arithmetic operations for Date
impl std::ops::Add<TimeDelta> for Date {
    type Output = Date;
    
    fn add(self, rhs: TimeDelta) -> Self::Output {
        Date(self.0 + rhs)
    }
}

impl std::ops::Sub<TimeDelta> for Date {
    type Output = Date;
    
    fn sub(self, rhs: TimeDelta) -> Self::Output {
        Date(self.0 - rhs)
    }
}

impl std::ops::Sub<Date> for Date {
    type Output = TimeDelta;
    
    fn sub(self, rhs: Date) -> Self::Output {
        self.0 - rhs.0
    }
}

impl std::ops::Sub<Date> for NaiveDate {
    type Output = TimeDelta;
    
    fn sub(self, rhs: Date) -> Self::Output {
        self - rhs.0
    }
}

/// DateTime type compatible with C++ datetime_t
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalDateTime {
    datetime: DateTime<Tz>,
}

impl LocalDateTime {
    /// Create a new LocalDateTime in the default timezone
    pub fn new(year: i32, month: u32, day: u32, hour: u32, min: u32, sec: u32) -> DateTimeResult<Self> {
        let naive_dt = NaiveDate::from_ymd_opt(year, month, day)
            .and_then(|d| d.and_hms_opt(hour, min, sec))
            .ok_or_else(|| DateTimeError::OutOfRange(
                format!("{}-{:02}-{:02} {:02}:{:02}:{:02}", year, month, day, hour, min, sec)
            ))?;

        let datetime = DEFAULT_TIMEZONE
            .from_local_datetime(&naive_dt)
            .single()
            .ok_or(DateTimeError::AmbiguousTime)?;

        Ok(LocalDateTime { datetime })
    }

    /// Create from a DateTime with timezone
    pub fn from_datetime(datetime: DateTime<Tz>) -> Self {
        LocalDateTime { datetime }
    }

    /// Get the current time in the default timezone
    pub fn current() -> Self {
        let now = Utc::now().with_timezone(&DEFAULT_TIMEZONE);
        LocalDateTime { datetime: now }
    }

    /// Convert to UTC
    pub fn to_utc(&self) -> DateTime<Utc> {
        self.datetime.with_timezone(&Utc)
    }

    /// Convert to a different timezone
    pub fn with_timezone(&self, tz: Tz) -> LocalDateTime {
        LocalDateTime {
            datetime: self.datetime.with_timezone(&tz),
        }
    }

    /// Get the date part
    pub fn date(&self) -> Date {
        Date(self.datetime.date_naive())
    }

    /// Get the timezone
    pub fn timezone(&self) -> Tz {
        self.datetime.timezone()
    }

    /// Check if this is a valid datetime
    pub fn is_valid(&self) -> bool {
        true
    }

    /// Check if this is not a valid datetime
    pub fn is_not_a_date_time(&self) -> bool {
        false
    }

    /// Get the hour component (0-23)
    pub fn hour(&self) -> u32 {
        self.datetime.hour()
    }

    /// Get the minute component (0-59)
    pub fn minute(&self) -> u32 {
        self.datetime.minute()
    }

    /// Get the second component (0-59)
    pub fn second(&self) -> u32 {
        self.datetime.second()
    }
}

impl fmt::Display for LocalDateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.datetime.format("%Y-%m-%d %H:%M:%S %Z"))
    }
}

/// Format type for date/datetime formatting
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FormatType {
    Written,
    Printed,
    Custom,
}

/// Date parsing function compatible with C++ parse_date
pub fn parse_date(date_str: &str) -> DateTimeResult<Date> {
    let trimmed = date_str.trim();
    
    // Try various date formats in order of preference
    let formats = [
        "%Y/%m/%d",     // 1990/01/01
        "%Y-%m-%d",     // 2006-12-25
        "%Y.%m.%d",     // 2006.12.25
        "%m/%d/%Y",     // 02/02/2002 (US format)
        "%m-%d-%Y",     // 02-02-2002 (US format)
        "%m.%d.%Y",     // 02.02.2002 (US format)
        "%y/%m/%d",     // 02/02/02 (two digit year)
        "%m/%d/%y",     // 02/02/02 (US format, two digit year)
        "%m-%d-%y",     // 02-02-02 (US format, two digit year)
        "%m.%d.%y",     // 02.02.02 (US format, two digit year)
        "%Y%m%d",       // 20020202 (compact format)
        "%m/%d",        // 12/25 (current year assumed)
        "%m-%d",        // 12-25 (current year assumed)
        "%m.%d",        // 12.25 (current year assumed)
        "%Y/%m",        // 1990/01 (day 1 assumed)
        "%Y-%m",        // 1990-01 (day 1 assumed)
        "%Y.%m",        // 1990.01 (day 1 assumed)
        "%Y",           // 1990 (Jan 1 assumed)
    ];

    // Get current year for formats that don't specify year
    let current_year = Date::current().year();

    for format in &formats {
        if let Ok(date) = parse_date_with_format(trimmed, format, current_year) {
            return Ok(date);
        }
    }

    // Try parsing month and day names (simplified for now)
    if let Ok(date) = parse_relative_date(trimmed) {
        return Ok(date);
    }

    Err(DateTimeError::InvalidFormat(date_str.to_string()))
}

fn parse_date_with_format(date_str: &str, format: &str, current_year: i32) -> DateTimeResult<Date> {
    // Handle formats without year by adding current year
    let (actual_str, actual_format) = match format {
        "%m/%d" => (format!("{}/{}", current_year, date_str), "%Y/%m/%d".to_string()),
        "%m-%d" => (format!("{}-{}", current_year, date_str), "%Y-%m-%d".to_string()),
        "%m.%d" => (format!("{}.{}", current_year, date_str), "%Y.%m.%d".to_string()),
        "%Y/%m" => (format!("{}/01", date_str), "%Y/%m/%d".to_string()),
        "%Y-%m" => (format!("{}-01", date_str), "%Y-%m-%d".to_string()),
        "%Y.%m" => (format!("{}.01", date_str), "%Y.%m.%d".to_string()),
        "%Y" => (format!("{}/01/01", date_str), "%Y/%m/%d".to_string()),
        _ => (date_str.to_string(), format.to_string()),
    };

    NaiveDate::parse_from_str(&actual_str, &actual_format)
        .map(Date)
        .map_err(|_| DateTimeError::InvalidFormat(date_str.to_string()))
}

fn parse_relative_date(date_str: &str) -> DateTimeResult<Date> {
    let lower = date_str.to_lowercase();
    let current = Date::current();
    
    // Handle simple cases for now
    match lower.as_str() {
        "today" => Ok(current),
        "yesterday" => Ok(Date::from_naive_date(
            current.naive_date() - chrono::Duration::days(1)
        )),
        "tomorrow" => Ok(Date::from_naive_date(
            current.naive_date() + chrono::Duration::days(1)
        )),
        _ => Err(DateTimeError::InvalidFormat(date_str.to_string()))
    }
}

/// DateTime parsing function compatible with C++ parse_datetime
pub fn parse_datetime(datetime_str: &str) -> DateTimeResult<LocalDateTime> {
    let trimmed = datetime_str.trim();
    
    // Try various datetime formats
    let formats = [
        "%Y-%m-%d %H:%M:%S",
        "%Y/%m/%d %H:%M:%S",
        "%Y.%m.%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M",
        "%Y.%m.%d %H:%M",
    ];

    for format in &formats {
        if let Ok(naive_dt) = NaiveDateTime::parse_from_str(trimmed, format) {
            let datetime = DEFAULT_TIMEZONE
                .from_local_datetime(&naive_dt)
                .single()
                .ok_or(DateTimeError::AmbiguousTime)?;
            return Ok(LocalDateTime::from_datetime(datetime));
        }
    }

    Err(DateTimeError::InvalidFormat(datetime_str.to_string()))
}

/// Format a date according to the specified format type
pub fn format_date(date: &Date, format_type: FormatType, custom_format: Option<&str>) -> String {
    match format_type {
        FormatType::Written => date.naive_date().format("%Y-%m-%d").to_string(),
        FormatType::Printed => date.naive_date().format("%Y/%m/%d").to_string(),
        FormatType::Custom => {
            if let Some(fmt) = custom_format {
                date.naive_date().format(fmt).to_string()
            } else {
                date.naive_date().format("%Y-%m-%d").to_string()
            }
        }
    }
}

/// Format a datetime according to the specified format type
pub fn format_datetime(datetime: &LocalDateTime, format_type: FormatType, custom_format: Option<&str>) -> String {
    match format_type {
        FormatType::Written => datetime.datetime.format("%Y-%m-%d %H:%M:%S %Z").to_string(),
        FormatType::Printed => datetime.datetime.format("%Y/%m/%d %H:%M:%S").to_string(),
        FormatType::Custom => {
            if let Some(fmt) = custom_format {
                datetime.datetime.format(fmt).to_string()
            } else {
                datetime.datetime.format("%Y-%m-%d %H:%M:%S %Z").to_string()
            }
        }
    }
}

/// Duration for date arithmetic
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DateDuration {
    Days(i32),
    Weeks(i32),
    Months(i32),
    Quarters(i32),
    Years(i32),
}

impl DateDuration {
    /// Add this duration to a date
    pub fn add_to_date(&self, date: Date) -> Date {
        match *self {
            DateDuration::Days(n) if n >= 0 => {
                Date(date.naive_date() + Days::new(n as u64))
            },
            DateDuration::Days(n) => {
                Date(date.naive_date() - Days::new((-n) as u64))
            },
            DateDuration::Weeks(n) if n >= 0 => {
                Date(date.naive_date() + Days::new((n * 7) as u64))
            },
            DateDuration::Weeks(n) => {
                Date(date.naive_date() - Days::new(((-n) * 7) as u64))
            },
            DateDuration::Months(n) if n >= 0 => {
                Date(date.naive_date() + Months::new(n as u32))
            },
            DateDuration::Months(n) => {
                Date(date.naive_date() - Months::new((-n) as u32))
            },
            DateDuration::Quarters(n) if n >= 0 => {
                Date(date.naive_date() + Months::new((n * 3) as u32))
            },
            DateDuration::Quarters(n) => {
                Date(date.naive_date() - Months::new(((-n) * 3) as u32))
            },
            DateDuration::Years(n) if n >= 0 => {
                Date(date.naive_date() + Months::new((n * 12) as u32))
            },
            DateDuration::Years(n) => {
                Date(date.naive_date() - Months::new(((-n) * 12) as u32))
            },
        }
    }
}

/// Timezone utilities
pub mod timezone {
    use super::*;
    use std::str::FromStr;

    /// Parse a timezone from a string
    pub fn parse_timezone(tz_str: &str) -> DateTimeResult<Tz> {
        // Handle common timezone abbreviations
        let normalized = match tz_str.to_uppercase().as_str() {
            "EST" => "America/New_York",
            "CST" => "America/Chicago", 
            "MST" => "America/Denver",
            "PST" => "America/Los_Angeles",
            "UTC" | "GMT" => "UTC",
            _ => tz_str,
        };

        Tz::from_str(normalized)
            .map_err(|_| DateTimeError::TimezoneError(tz_str.to_string()))
    }

    /// Convert a datetime to a different timezone
    pub fn convert_timezone(dt: &LocalDateTime, target_tz: Tz) -> LocalDateTime {
        dt.with_timezone(target_tz)
    }

    /// Get the default timezone
    pub fn default_timezone() -> Tz {
        DEFAULT_TIMEZONE
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_date_creation() {
        let date = Date::new(2023, 12, 25).unwrap();
        assert_eq!(date.year(), 2023);
        assert_eq!(date.month(), 12);
        assert_eq!(date.day(), 25);
        assert!(date.is_valid());
        assert!(!date.is_not_a_date());
    }

    #[test]
    fn test_parse_date_basic() {
        let date1 = parse_date("1990/01/01").unwrap();
        assert_eq!(date1.year(), 1990);
        assert_eq!(date1.month(), 1);
        assert_eq!(date1.day(), 1);

        let date2 = parse_date("2006-12-25").unwrap();
        assert_eq!(date2.year(), 2006);
        assert_eq!(date2.month(), 12);
        assert_eq!(date2.day(), 25);

        let date3 = parse_date("2006.12.25").unwrap();
        assert_eq!(date3, date2);
    }

    #[test]
    fn test_format_date() {
        let date = Date::new(2023, 12, 25).unwrap();
        assert_eq!(format_date(&date, FormatType::Written, None), "2023-12-25");
        assert_eq!(format_date(&date, FormatType::Printed, None), "2023/12/25");
    }

    #[test]
    fn test_current_date() {
        let current = Date::current();
        assert!(current.is_valid());
    }

    #[test]
    fn test_timezone_parsing() {
        assert!(timezone::parse_timezone("America/Chicago").is_ok());
        assert!(timezone::parse_timezone("EST").is_ok());
        assert!(timezone::parse_timezone("UTC").is_ok());
        assert!(timezone::parse_timezone("Invalid/Timezone").is_err());
    }
}