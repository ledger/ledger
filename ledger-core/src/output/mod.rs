//! Output formatting and rendering modules
//!
//! This module provides various output formatters for different report types
//! including text, CSV, JSON, and XML output formats.

use serde_json;
use std::error::Error;
use std::fmt;
use std::io::Write;

/// Result type for output operations
pub type OutputResult<T> = Result<T, OutputError>;

/// Errors that can occur during output formatting
#[derive(Debug, Clone)]
pub enum OutputError {
    /// IO error during writing
    IoError(String),
    /// Formatting error
    FormatError(String),
    /// Invalid output configuration
    InvalidConfig(String),
}

impl fmt::Display for OutputError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OutputError::IoError(msg) => write!(f, "IO error: {}", msg),
            OutputError::FormatError(msg) => write!(f, "Format error: {}", msg),
            OutputError::InvalidConfig(msg) => write!(f, "Invalid configuration: {}", msg),
        }
    }
}

impl Error for OutputError {}

impl From<std::fmt::Error> for OutputError {
    fn from(err: std::fmt::Error) -> Self {
        OutputError::FormatError(err.to_string())
    }
}

impl From<std::io::Error> for OutputError {
    fn from(err: std::io::Error) -> Self {
        OutputError::IoError(err.to_string())
    }
}

/// Output format types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Plain text format
    Text,
    /// Comma-separated values
    Csv,
    /// JSON format
    Json,
    /// XML format
    Xml,
    /// HTML format
    Html,
}

impl OutputFormat {
    /// Get the file extension for this format
    pub fn extension(&self) -> &'static str {
        match self {
            OutputFormat::Text => "txt",
            OutputFormat::Csv => "csv",
            OutputFormat::Json => "json",
            OutputFormat::Xml => "xml",
            OutputFormat::Html => "html",
            OutputFormat::Xml => "xml",
        }
    }

    /// Get the MIME type for this format
    pub fn mime_type(&self) -> &'static str {
        match self {
            OutputFormat::Text => "text/plain",
            OutputFormat::Csv => "text/csv",
            OutputFormat::Json => "application/json",
            OutputFormat::Xml => "application/xml",
            OutputFormat::Html => "text/html",
        }
    }
}

/// Trait for output formatters
pub trait Formatter {
    /// Write formatted output to the writer
    fn format(&self, writer: &mut dyn Write) -> OutputResult<()>;

    /// Get the preferred output format
    fn output_format(&self) -> OutputFormat;

    /// Get a description of this formatter
    fn description(&self) -> &str;
}

/// Text formatter for plain text output
pub struct TextFormatter {
    /// The content to format
    pub content: Vec<String>,
    /// Column alignment settings
    pub alignments: Vec<TextAlignment>,
    /// Column widths (None for auto-width)
    pub widths: Vec<Option<usize>>,
}

/// Text alignment options
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextAlignment {
    /// Left-aligned text
    Left,
    /// Right-aligned text
    Right,
    /// Center-aligned text
    Center,
}

impl Default for TextFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl TextFormatter {
    /// Create a new text formatter
    pub fn new() -> Self {
        Self { content: Vec::new(), alignments: Vec::new(), widths: Vec::new() }
    }

    /// Add a line of content
    pub fn add_line(&mut self, line: String) {
        self.content.push(line);
    }

    /// Set column alignments
    pub fn set_alignments(&mut self, alignments: Vec<TextAlignment>) {
        self.alignments = alignments;
    }

    /// Set column widths
    pub fn set_widths(&mut self, widths: Vec<Option<usize>>) {
        self.widths = widths;
    }

    /// Format a single line with column alignment
    pub fn format_columns(&self, columns: &[&str]) -> String {
        let mut result = String::new();

        for (i, &column) in columns.iter().enumerate() {
            if i > 0 {
                result.push(' ');
            }

            let width = self.widths.get(i).and_then(|&w| w);
            let alignment = self.alignments.get(i).copied().unwrap_or(TextAlignment::Left);

            let formatted = match (width, alignment) {
                (Some(w), TextAlignment::Left) => format!("{:<width$}", column, width = w),
                (Some(w), TextAlignment::Right) => format!("{:>width$}", column, width = w),
                (Some(w), TextAlignment::Center) => {
                    let padding = w.saturating_sub(column.len());
                    let left_pad = padding / 2;
                    let right_pad = padding - left_pad;
                    format!("{}{}{}", " ".repeat(left_pad), column, " ".repeat(right_pad))
                }
                (None, _) => column.to_string(),
            };

            result.push_str(&formatted);
        }

        result
    }
}

impl Formatter for TextFormatter {
    fn format(&self, writer: &mut dyn Write) -> OutputResult<()> {
        for line in &self.content {
            writeln!(writer, "{}", line).map_err(|e| OutputError::IoError(e.to_string()))?;
        }
        Ok(())
    }

    fn output_format(&self) -> OutputFormat {
        OutputFormat::Text
    }

    fn description(&self) -> &str {
        "Plain text formatter with column alignment"
    }
}

/// CSV formatter for comma-separated output
pub struct CsvFormatter {
    /// Rows of data
    pub rows: Vec<Vec<String>>,
    /// Whether to include headers
    pub include_headers: bool,
    /// Custom delimiter (default is comma)
    pub delimiter: char,
}

impl Default for CsvFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl CsvFormatter {
    /// Create a new CSV formatter
    pub fn new() -> Self {
        Self { rows: Vec::new(), include_headers: true, delimiter: ',' }
    }

    /// Add a row of data
    pub fn add_row(&mut self, row: Vec<String>) {
        self.rows.push(row);
    }

    /// Set custom delimiter
    pub fn with_delimiter(mut self, delimiter: char) -> Self {
        self.delimiter = delimiter;
        self
    }

    /// Escape a CSV field if necessary
    fn escape_field(&self, field: &str) -> String {
        if field.contains(self.delimiter) || field.contains('"') || field.contains('\n') {
            format!("\"{}\"", field.replace('"', "\"\""))
        } else {
            field.to_string()
        }
    }
}

impl Formatter for CsvFormatter {
    fn format(&self, writer: &mut dyn Write) -> OutputResult<()> {
        for row in &self.rows {
            let escaped_row: Vec<String> =
                row.iter().map(|field| self.escape_field(field)).collect();
            writeln!(writer, "{}", escaped_row.join(&self.delimiter.to_string()))
                .map_err(|e| OutputError::IoError(e.to_string()))?;
        }
        Ok(())
    }

    fn output_format(&self) -> OutputFormat {
        OutputFormat::Csv
    }

    fn description(&self) -> &str {
        "CSV formatter with configurable delimiter"
    }
}

/// JSON formatter for structured output
pub struct JsonFormatter {
    /// Data to format as JSON
    pub data: serde_json::Value,
    /// Pretty print flag
    pub pretty: bool,
}

impl JsonFormatter {
    /// Create a new JSON formatter with raw data
    pub fn new(data: serde_json::Value) -> Self {
        Self { data, pretty: false }
    }

    /// Create a pretty-printed JSON formatter
    pub fn pretty(mut self) -> Self {
        self.pretty = true;
        self
    }

    /// Create from report data
    pub fn from_rows(headers: Vec<String>, rows: Vec<Vec<String>>) -> Self {
        let mut json_rows = Vec::new();

        for row in rows {
            let mut json_obj = serde_json::Map::new();
            for (i, value) in row.into_iter().enumerate() {
                if let Some(header) = headers.get(i) {
                    json_obj.insert(header.clone(), serde_json::Value::String(value));
                }
            }
            json_rows.push(serde_json::Value::Object(json_obj));
        }

        Self { data: serde_json::Value::Array(json_rows), pretty: false }
    }
}

impl Formatter for JsonFormatter {
    fn format(&self, writer: &mut dyn Write) -> OutputResult<()> {
        let json_string = if self.pretty {
            serde_json::to_string_pretty(&self.data)
        } else {
            serde_json::to_string(&self.data)
        }
        .map_err(|e| OutputError::FormatError(e.to_string()))?;

        write!(writer, "{}", json_string).map_err(|e| OutputError::IoError(e.to_string()))?;
        Ok(())
    }

    fn output_format(&self) -> OutputFormat {
        OutputFormat::Json
    }

    fn description(&self) -> &str {
        "JSON formatter for structured data"
    }
}

/// XML formatter for XML output
pub struct XmlFormatter {
    /// Root element name
    pub root_element: String,
    /// Row element name
    pub row_element: String,
    /// Headers for column names
    pub headers: Vec<String>,
    /// Data rows
    pub rows: Vec<Vec<String>>,
    /// Pretty print flag
    pub pretty: bool,
}

impl Default for XmlFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl XmlFormatter {
    /// Create a new XML formatter
    pub fn new() -> Self {
        Self {
            root_element: "report".to_string(),
            row_element: "entry".to_string(),
            headers: Vec::new(),
            rows: Vec::new(),
            pretty: false,
        }
    }

    /// Set root element name
    pub fn with_root(mut self, root: String) -> Self {
        self.root_element = root;
        self
    }

    /// Set row element name
    pub fn with_row_element(mut self, element: String) -> Self {
        self.row_element = element;
        self
    }

    /// Set headers
    pub fn with_headers(mut self, headers: Vec<String>) -> Self {
        self.headers = headers;
        self
    }

    /// Add a row of data
    pub fn add_row(&mut self, row: Vec<String>) {
        self.rows.push(row);
    }

    /// Enable pretty printing
    pub fn pretty(mut self) -> Self {
        self.pretty = true;
        self
    }

    /// Escape XML special characters
    fn escape_xml(text: &str) -> String {
        text.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
            .replace('"', "&quot;")
            .replace('\'', "&apos;")
    }

    /// Sanitize element name
    fn sanitize_element_name(name: &str) -> String {
        name.chars()
            .map(|c| if c.is_ascii_alphanumeric() || c == '_' || c == '-' { c } else { '_' })
            .collect()
    }
}

impl Formatter for XmlFormatter {
    fn format(&self, writer: &mut dyn Write) -> OutputResult<()> {
        let indent = if self.pretty { "  " } else { "" };
        let newline = if self.pretty { "\n" } else { "" };

        // XML declaration
        writeln!(writer, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
            .map_err(|e| OutputError::IoError(e.to_string()))?;

        // Root element
        write!(writer, "<{}>", self.root_element)?;
        if self.pretty {
            writeln!(writer)?;
        }

        // Data rows
        for row in &self.rows {
            write!(writer, "{}<{}>", indent, self.row_element)?;
            if self.pretty {
                writeln!(writer)?;
            }

            for (i, value) in row.iter().enumerate() {
                let field_name = if let Some(header) = self.headers.get(i) {
                    Self::sanitize_element_name(header)
                } else {
                    format!("field{}", i)
                };

                let escaped_value = Self::escape_xml(value);
                if self.pretty {
                    writeln!(
                        writer,
                        "{}{}  <{}>{}</{}>",
                        indent, indent, field_name, escaped_value, field_name
                    )
                    .map_err(|e| OutputError::IoError(e.to_string()))?;
                } else {
                    write!(writer, "<{}>{}</{}>", field_name, escaped_value, field_name)
                        .map_err(|e| OutputError::IoError(e.to_string()))?;
                }
            }

            write!(writer, "{}</{}>", indent, self.row_element)?;
            if self.pretty {
                writeln!(writer)?;
            }
        }

        // Close root element
        write!(writer, "</{}>", self.root_element)?;
        if self.pretty {
            writeln!(writer)?;
        }

        Ok(())
    }

    fn output_format(&self) -> OutputFormat {
        OutputFormat::Xml
    }

    fn description(&self) -> &str {
        "XML formatter with configurable element names"
    }
}

/// HTML formatter for web-friendly output
pub struct HtmlFormatter {
    /// Table headers
    pub headers: Vec<String>,
    /// Data rows
    pub rows: Vec<Vec<String>>,
    /// CSS classes for styling
    pub table_class: Option<String>,
    /// Include HTML document wrapper
    pub full_document: bool,
    /// Page title (if full document)
    pub title: String,
}

impl Default for HtmlFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl HtmlFormatter {
    /// Create a new HTML formatter
    pub fn new() -> Self {
        Self {
            headers: Vec::new(),
            rows: Vec::new(),
            table_class: None,
            full_document: false,
            title: "Ledger Report".to_string(),
        }
    }

    /// Set table headers
    pub fn with_headers(mut self, headers: Vec<String>) -> Self {
        self.headers = headers;
        self
    }

    /// Add a row of data
    pub fn add_row(&mut self, row: Vec<String>) {
        self.rows.push(row);
    }

    /// Set CSS class for the table
    pub fn with_table_class(mut self, class: String) -> Self {
        self.table_class = Some(class);
        self
    }

    /// Generate full HTML document
    pub fn full_document(mut self, title: String) -> Self {
        self.full_document = true;
        self.title = title;
        self
    }

    /// Escape HTML special characters
    fn escape_html(text: &str) -> String {
        text.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
            .replace('"', "&quot;")
            .replace('\'', "&#39;")
    }
}

impl Formatter for HtmlFormatter {
    fn format(&self, writer: &mut dyn Write) -> OutputResult<()> {
        if self.full_document {
            writeln!(writer, "<!DOCTYPE html>")?;
            writeln!(writer, "<html>")?;
            writeln!(writer, "<head>")?;
            writeln!(writer, "  <meta charset=\"UTF-8\">")?;
            writeln!(writer, "  <title>{}</title>", Self::escape_html(&self.title))?;
            writeln!(writer, "  <style>")?;
            writeln!(writer, "    table {{ border-collapse: collapse; width: 100%; }}")?;
            writeln!(
                writer,
                "    th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}"
            )?;
            writeln!(writer, "    th {{ background-color: #f2f2f2; font-weight: bold; }}")?;
            writeln!(writer, "  </style>")?;
            writeln!(writer, "</head>")?;
            writeln!(writer, "<body>")?;
        }

        // Table opening
        if let Some(ref class) = self.table_class {
            writeln!(writer, "<table class=\"{}\">", Self::escape_html(class))?;
        } else {
            writeln!(writer, "<table>")?;
        }

        // Table headers
        if !self.headers.is_empty() {
            writeln!(writer, "  <thead>")?;
            writeln!(writer, "    <tr>")?;
            for header in &self.headers {
                writeln!(writer, "      <th>{}</th>", Self::escape_html(header))?;
            }
            writeln!(writer, "    </tr>")?;
            writeln!(writer, "  </thead>")?;
        }

        // Table body
        writeln!(writer, "  <tbody>")?;
        for row in &self.rows {
            writeln!(writer, "    <tr>")?;
            for cell in row {
                writeln!(writer, "      <td>{}</td>", Self::escape_html(cell))?;
            }
            writeln!(writer, "    </tr>")?;
        }
        writeln!(writer, "  </tbody>")?;

        // Table closing
        writeln!(writer, "</table>")?;

        if self.full_document {
            writeln!(writer, "</body>")?;
            writeln!(writer, "</html>")?;
        }

        Ok(())
    }

    fn output_format(&self) -> OutputFormat {
        OutputFormat::Html
    }

    fn description(&self) -> &str {
        "HTML formatter with table generation and styling"
    }
}

/// Multi-format output manager
pub struct OutputManager;

impl OutputManager {
    /// Create appropriate formatter for the given format
    pub fn create_formatter(format: OutputFormat) -> Box<dyn Formatter> {
        match format {
            OutputFormat::Text => Box::new(TextFormatter::new()),
            OutputFormat::Csv => Box::new(CsvFormatter::new()),
            OutputFormat::Json => Box::new(JsonFormatter::new(serde_json::Value::Null)),
            OutputFormat::Xml => Box::new(XmlFormatter::new()),
            OutputFormat::Html => Box::new(HtmlFormatter::new()),
        }
    }

    /// Format data using the specified format
    pub fn format_data<W: Write>(
        format: OutputFormat,
        headers: Vec<String>,
        rows: Vec<Vec<String>>,
        writer: &mut W,
    ) -> OutputResult<()> {
        match format {
            OutputFormat::Text => {
                let mut formatter = TextFormatter::new();
                // Calculate column widths
                let mut max_widths = vec![0; headers.len()];
                for (i, header) in headers.iter().enumerate() {
                    max_widths[i] = header.len();
                }
                for row in &rows {
                    for (i, cell) in row.iter().enumerate() {
                        if let Some(width) = max_widths.get_mut(i) {
                            *width = (*width).max(cell.len());
                        }
                    }
                }

                // Add header row
                let header_columns: Vec<&str> = headers.iter().map(|s| s.as_str()).collect();
                formatter.add_line(formatter.format_columns(&header_columns));

                // Add separator
                let separator: Vec<String> = max_widths.iter().map(|&w| "-".repeat(w)).collect();
                let separator_columns: Vec<&str> = separator.iter().map(|s| s.as_str()).collect();
                formatter.add_line(formatter.format_columns(&separator_columns));

                // Add data rows
                for row in rows {
                    let row_columns: Vec<&str> = row.iter().map(|s| s.as_str()).collect();
                    formatter.add_line(formatter.format_columns(&row_columns));
                }

                formatter.format(writer)
            }
            OutputFormat::Csv => {
                let mut formatter = CsvFormatter::new();
                formatter.add_row(headers);
                for row in rows {
                    formatter.add_row(row);
                }
                formatter.format(writer)
            }
            OutputFormat::Json => {
                let formatter = JsonFormatter::from_rows(headers, rows);
                formatter.format(writer)
            }
            OutputFormat::Xml => {
                let mut formatter = XmlFormatter::new().with_headers(headers).pretty();
                for row in rows {
                    formatter.add_row(row);
                }
                formatter.format(writer)
            }
            OutputFormat::Html => {
                let mut formatter = HtmlFormatter::new().with_headers(headers);
                for row in rows {
                    formatter.add_row(row);
                }
                formatter.format(writer)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_output_format_properties() {
        assert_eq!(OutputFormat::Text.extension(), "txt");
        assert_eq!(OutputFormat::Csv.extension(), "csv");
        assert_eq!(OutputFormat::Json.mime_type(), "application/json");
    }

    #[test]
    fn test_text_formatter() {
        let mut formatter = TextFormatter::new();
        formatter.add_line("Hello World".to_string());
        formatter.add_line("Second line".to_string());

        let mut output = Cursor::new(Vec::new());
        formatter.format(&mut output).unwrap();

        let result = String::from_utf8(output.into_inner()).unwrap();
        assert!(result.contains("Hello World"));
        assert!(result.contains("Second line"));
    }

    #[test]
    fn test_csv_formatter() {
        let mut formatter = CsvFormatter::new();
        formatter.add_row(vec!["Name".to_string(), "Amount".to_string()]);
        formatter.add_row(vec!["Test Account".to_string(), "100.00".to_string()]);

        let mut output = Cursor::new(Vec::new());
        formatter.format(&mut output).unwrap();

        let result = String::from_utf8(output.into_inner()).unwrap();
        assert!(result.contains("Name,Amount"));
        assert!(result.contains("Test Account,100.00"));
    }

    #[test]
    fn test_csv_escaping() {
        let formatter = CsvFormatter::new();
        assert_eq!(formatter.escape_field("simple"), "simple");
        assert_eq!(formatter.escape_field("has,comma"), "\"has,comma\"");
        assert_eq!(formatter.escape_field("has\"quote"), "\"has\"\"quote\"");
    }

    #[test]
    fn test_column_formatting() {
        let mut formatter = TextFormatter::new();
        formatter.set_widths(vec![Some(10), Some(8)]);
        formatter.set_alignments(vec![TextAlignment::Left, TextAlignment::Right]);

        let result = formatter.format_columns(&["Account", "100.00"]);
        assert_eq!(result, "Account    100.00");
    }

    #[test]
    fn test_json_formatter() {
        let headers = vec!["Name".to_string(), "Amount".to_string()];
        let rows = vec![
            vec!["Assets:Cash".to_string(), "1000.00".to_string()],
            vec!["Expenses:Food".to_string(), "50.00".to_string()],
        ];

        let formatter = JsonFormatter::from_rows(headers, rows);
        let mut output = Cursor::new(Vec::new());
        formatter.format(&mut output).unwrap();

        let result = String::from_utf8(output.into_inner()).unwrap();
        assert!(result.contains("Assets:Cash"));
        assert!(result.contains("1000.00"));
    }

    #[test]
    fn test_xml_formatter() {
        let mut formatter =
            XmlFormatter::new().with_headers(vec!["account".to_string(), "balance".to_string()]);
        formatter.add_row(vec!["Assets:Cash".to_string(), "1000.00".to_string()]);

        let mut output = Cursor::new(Vec::new());
        formatter.format(&mut output).unwrap();

        let result = String::from_utf8(output.into_inner()).unwrap();
        assert!(result.contains("<?xml"));
        assert!(result.contains("<report>"));
        assert!(result.contains("<account>Assets:Cash</account>"));
    }

    #[test]
    fn test_html_formatter() {
        let mut formatter =
            HtmlFormatter::new().with_headers(vec!["Account".to_string(), "Balance".to_string()]);
        formatter.add_row(vec!["Assets:Cash".to_string(), "1000.00".to_string()]);

        let mut output = Cursor::new(Vec::new());
        formatter.format(&mut output).unwrap();

        let result = String::from_utf8(output.into_inner()).unwrap();
        assert!(result.contains("<table>"));
        assert!(result.contains("<th>Account</th>"));
        assert!(result.contains("<td>Assets:Cash</td>"));
    }

    #[test]
    fn test_output_manager() {
        let headers = vec!["Date".to_string(), "Account".to_string(), "Amount".to_string()];
        let rows =
            vec![vec!["2024-01-15".to_string(), "Assets:Cash".to_string(), "1000.00".to_string()]];

        // Test CSV format
        let mut csv_output = Cursor::new(Vec::new());
        OutputManager::format_data(
            OutputFormat::Csv,
            headers.clone(),
            rows.clone(),
            &mut csv_output,
        )
        .unwrap();
        let csv_result = String::from_utf8(csv_output.into_inner()).unwrap();
        assert!(csv_result.contains("Date,Account,Amount"));

        // Test JSON format
        let mut json_output = Cursor::new(Vec::new());
        OutputManager::format_data(
            OutputFormat::Json,
            headers.clone(),
            rows.clone(),
            &mut json_output,
        )
        .unwrap();
        let json_result = String::from_utf8(json_output.into_inner()).unwrap();
        assert!(json_result.contains("Assets:Cash"));
    }

    #[test]
    fn test_xml_escaping() {
        let formatter = XmlFormatter::new();
        assert_eq!(XmlFormatter::escape_xml("normal text"), "normal text");
        assert_eq!(XmlFormatter::escape_xml("text & more"), "text &amp; more");
        assert_eq!(XmlFormatter::escape_xml("<tag>"), "&lt;tag&gt;");
        assert_eq!(XmlFormatter::escape_xml("\"quoted\""), "&quot;quoted&quot;");
    }

    #[test]
    fn test_html_escaping() {
        let formatter = HtmlFormatter::new();
        assert_eq!(HtmlFormatter::escape_html("normal text"), "normal text");
        assert_eq!(HtmlFormatter::escape_html("text & more"), "text &amp; more");
        assert_eq!(HtmlFormatter::escape_html("<script>"), "&lt;script&gt;");
        assert_eq!(HtmlFormatter::escape_html("\"quoted\""), "&quot;quoted&quot;");
    }
}
