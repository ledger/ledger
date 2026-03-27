/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * @addtogroup data
 */

/**
 * @file   csv.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief CSV file parser that imports bank/financial CSV data as transactions.
 *
 * Many banks and financial institutions export account history as CSV files.
 * The csv_reader class reads such files and converts each data row into a
 * Ledger xact_t (transaction) with two postings: one for the account
 * described by the CSV data and a balancing posting to the master account
 * specified in the parse context.
 *
 * Column mapping is automatic: the first row of the CSV file is treated as
 * a header row, and each column name is matched against a set of regex
 * patterns (e.g., "date", "payee|desc(ription)?|title", "credit|amount")
 * to determine which transaction field it populates.  Columns whose names
 * do not match any known pattern are stored as transaction tags using the
 * original column name.
 *
 * The parser handles:
 *   - Quoted fields (double-quote and pipe delimiters, with escape sequences)
 *   - Separate credit/debit columns or a single amount column
 *   - Optional auxiliary date, code, cost, total (for balance assertions),
 *     and note columns
 *   - Payee alias mappings from the journal's `payee` directives
 *   - Account mappings from the journal's `account` directives for unknown
 *     payees
 *   - Rich metadata tagging (import date and original CSV line)
 *
 * @see textual.cc  The main journal parser that delegates to csv_reader
 *                  for `.csv` file includes.
 * @see xact_t     The transaction type produced by read_xact().
 */
#pragma once

#include "value.h"
#include "context.h"

namespace ledger {

class xact_t;
class journal_t;
class account_t;

/// @brief Exception type for CSV parsing errors.
class csv_error : public std::runtime_error {
public:
  explicit csv_error(const string& why) noexcept : std::runtime_error(why) {}
  ~csv_error() noexcept override {}
};

/**
 * @brief Reads CSV files and produces Ledger transactions.
 *
 * csv_reader is constructed with a parse_context_t that provides the input
 * stream, file path, journal, and master account.  Construction immediately
 * reads the header row to build the column-to-field index.  Subsequent
 * calls to read_xact() consume one data row at a time and return a
 * fully-formed transaction.
 *
 * Each transaction is created with two postings:
 *   1. A posting whose amount comes from the CSV credit/debit column(s),
 *      with the account determined by payee-to-account mappings.
 *   2. A balancing posting to the context's master account with the
 *      negated amount (and optional balance assertion from the total column).
 */
class csv_reader {
  parse_context_t context; ///< Parsing context: input stream, journal, master account
  char separator;          ///< Field separator character (default: ',')

  /**
   * @brief Field types that CSV columns can map to.
   *
   * Each header column name is matched against regex patterns to determine
   * which of these field types it represents.  FIELD_UNKNOWN causes the
   * column's data to be stored as a transaction tag.
   */
  enum headers_t : uint8_t {
    FIELD_DATE = 0, ///< Transaction date (required)
    FIELD_DATE_AUX, ///< Auxiliary/posted date
    FIELD_CODE,     ///< Transaction code (check number, reference)
    FIELD_PAYEE,    ///< Payee/description/title
    FIELD_CREDIT,   ///< Credit amount (positive inflow) or combined amount
    FIELD_DEBIT,    ///< Debit amount (negated to represent outflow)
    FIELD_COST,     ///< Cost basis for the posting
    FIELD_TOTAL,    ///< Running total (used for balance assertions)
    FIELD_NOTE,     ///< Transaction note

    FIELD_UNKNOWN ///< Unrecognized column; stored as a transaction tag
  };

  /// @brief Regex patterns matched against header column names, in priority order.
  std::array<std::pair<mask_t, headers_t>, 10> masks;

  std::vector<headers_t> index; ///< Column-to-field mapping, one entry per CSV column
  std::vector<string> names;    ///< Original header names, used as tag keys for FIELD_UNKNOWN

public:
  /**
   * @brief Construct a CSV reader and immediately parse the header row.
   *
   * The masks array defines the regex-to-field-type mapping.  Each header
   * column name is tested against these patterns in order; the first match
   * determines the field type.  The empty-string pattern for FIELD_UNKNOWN
   * acts as a catch-all.
   *
   * @param _context  Parse context providing the input stream, journal
   *                  (for payee/account mappings), and master account.
   */
  csv_reader(parse_context_t& _context, char _separator = ',')
      : context(_context),
        separator(_separator),
        masks{std::make_pair(mask_t("date"), FIELD_DATE),
              std::make_pair(mask_t("posted( ?date)?"), FIELD_DATE_AUX),
              std::make_pair(mask_t("code"), FIELD_CODE),
              std::make_pair(mask_t("(payee|desc(ription)?|title)"), FIELD_PAYEE),
              std::make_pair(mask_t("credit|amount"), FIELD_CREDIT),
              std::make_pair(mask_t("debit"), FIELD_DEBIT),
              std::make_pair(mask_t("cost"), FIELD_COST),
              std::make_pair(mask_t("total"), FIELD_TOTAL),
              std::make_pair(mask_t("note"), FIELD_NOTE),
              std::make_pair(mask_t(""), FIELD_UNKNOWN)} {
    read_index(*context.stream.get());
    TRACE_CTOR(csv_reader, "parse_context_t&");
  }
  ~csv_reader() { TRACE_DTOR(csv_reader); }

  /**
   * @brief Parse the header row and build the column-to-field index.
   * @param in  The input stream positioned at the first line of the CSV.
   */
  void read_index(std::istream& in);

  /**
   * @brief Read a single field from a CSV line.
   *
   * Handles quoted fields (double-quote and pipe delimiters), escaped
   * characters, and doubled-quote escaping within quoted fields.
   *
   * @param in  The input stream positioned within a CSV line.
   * @return The trimmed field value.
   */
  string read_field(std::istream& in);

  /**
   * @brief Read the next non-comment line from the CSV file.
   *
   * Skips lines beginning with `#` (comments).  Returns nullptr at EOF.
   *
   * @param in  The input stream.
   * @return Pointer to the internal line buffer, or nullptr at end of file.
   */
  char* next_line(std::istream& in);

  /**
   * @brief Parse one CSV data row into a complete transaction.
   *
   * Creates a transaction with two postings and returns it.  The caller
   * takes ownership of the returned pointer.  Returns nullptr when no
   * more data rows are available.
   *
   * @param rich_data  If true, attach metadata tags recording the import
   *                   date and original CSV line text.
   * @return A heap-allocated transaction, or nullptr at end of file.
   */
  xact_t* read_xact(bool rich_data);

  const std::string& get_last_line() const { return context.linebuf; } ///< Last line read from CSV
  path get_pathname() const { return context.pathname; }               ///< CSV file path
  std::size_t get_linenum() const { return context.linenum; }          ///< Current line number
};

} // namespace ledger
