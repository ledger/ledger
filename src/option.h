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
 * @addtogroup expr
 */

/**
 * @file   option.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Basic type and macros for handling command-line options
 */
#pragma once

/**
 * @defgroup options Option Macro System
 * @ingroup expr
 *
 * @brief Macro-based system for declaring, parsing, and dispatching
 *        command-line options.
 *
 * @section opt_overview System Overview
 *
 * Ledger's command-line options are declared as data members of scope classes
 * (primarily report_t) using a family of preprocessor macros.  Each option
 * expands into a small struct that inherits from option_t<T>, with an instance
 * named `<name>handler` embedded as a member of the parent class.  This
 * approach gives each option its own handler_thunk() override for custom
 * side-effect logic, while keeping declarations concise.
 *
 * @section opt_core Core Template: option_t\<T\>
 *
 * @code
 *   template <typename T>
 *   class option_t {
 *     const char* name;          // internal name (underscores, trailing _ = wants arg)
 *     string::size_type name_len;
 *     const char ch;             // single-character shortcut ('\0' if none)
 *     bool handled;              // true once on() has been called
 *     optional<string> source;   // where the option was set from
 *   public:
 *     T* parent;                 // owning scope, set by OPT macros during lookup
 *     string value;              // the option's string argument value
 *     bool wants_arg;            // true if name ends with '_'
 *
 *     void on(whence);           // activate boolean option
 *     void on(whence, str);      // activate with argument value
 *     void off();                // deactivate option
 *     operator bool();           // returns handled
 *     string str();              // returns value (asserts handled)
 *     string desc();             // returns --long-form description
 *
 *     // Override these for custom behavior:
 *     virtual void handler_thunk(const optional<string>& whence);
 *     virtual void handler_thunk(const optional<string>& whence, const string& str);
 *   };
 * @endcode
 *
 * Key behavior:
 * - `on(whence)` calls handler_thunk(whence), then sets handled=true.
 * - `on(whence, str)` calls handler_thunk(whence, str); if the thunk did not
 *   modify value, sets value=str.  Then sets handled=true.
 * - `wants_arg` is auto-detected from a trailing underscore in the name.
 * - `desc()` converts underscores to hyphens for display (e.g., "sort_all_"
 *   becomes "--sort-all").
 *
 * @section opt_macros Macro Inventory by Family
 *
 * @subsection opt_structure Structure Macros (building blocks)
 *
 * | Macro | Expansion | Purpose |
 * |-------|-----------|---------|
 * | `BEGIN(type, name)` | `struct name##option_t : public option_t<type>` | Opens the option struct
 * declaration | | `CTOR(type, name)` | `name##option_t() : option_t<type>(#name)` | Basic
 * constructor | | `CTOR_(type, name, base)` | `name##option_t() : option_t<type>(#name), base` |
 * Constructor with extra member initializer | | `DECL1(type, name, vartype, var, value)` | declares
 * `vartype var;` + constructor | Adds an extra member variable to the option struct | | `END(name)`
 * | `name##handler` | Closes the struct and declares the member instance |
 *
 * @subsection opt_handler Handler Override Macros
 *
 * | Macro | Expansion | Purpose |
 * |-------|-----------|---------|
 * | `DO()` | `void handler_thunk(whence) override` | Override for boolean (no-arg) options |
 * | `DO_(var)` | `void handler_thunk(whence, var) override` | Override for options taking an
 * argument |
 *
 * @subsection opt_convenience Convenience Declaration Macros
 *
 * These combine BEGIN/CTOR/END into single-line declarations:
 *
 * | Macro | Usage Pattern | Purpose |
 * |-------|---------------|---------|
 * | `OPTION(type, name)` | `OPTION(report_t, color);` | Simple option, no custom handler logic |
 * | `OPTION_(type, name, body)` | `OPTION_(report_t, actual, DO() { ... });` | Option with custom
 * DO()/DO_() handler | | `OPTION_CTOR(type, name, body)` | `OPTION_CTOR(report_t, amount_,
 * DECL1(...) { } DO_(...) { ... });` | Fully custom body with extra members, custom CTOR, and
 * handler |
 *
 * @subsection opt_lookup Lookup Macros (used in lookup_option)
 *
 * These macros are used in the lookup_option() method to match a command-line
 * string to the corresponding option handler:
 *
 * | Macro | Purpose |
 * |-------|---------|
 * | `OPT(name)` | Exact name match; sets parent pointer and returns handler |
 * | `OPT_(name)` | Match by full name or single-character shortcut |
 * | `OPT_ALT(name, alt)` | Match by primary name or an alternative name |
 * | `OPT_CH(name)` | Single-character-only match (no long form) |
 *
 * @subsection opt_access Access and Cross-Reference Macros
 *
 * | Macro | Expansion | Purpose |
 * |-------|-----------|---------|
 * | `HANDLER(name)` | `name##handler` | Access the option's member instance |
 * | `HANDLED(name)` | `name##handler` | Same; tests if option is set (via bool conversion) |
 * | `OTHER(name)` | `parent->HANDLER(name)` | Access another option on the same parent; used in
 * DO()/DO_() bodies for cross-option side effects | | `COPY_OPT(name, other)` | copy constructor
 * helper | Copies one option's handler from another instance |
 *
 * @subsection opt_factory Factory Macros
 *
 * | Macro | Purpose |
 * |-------|---------|
 * | `MAKE_OPT_HANDLER(type, x)` | Wraps the option's handler() method as an expr_t functor |
 * | `MAKE_OPT_FUNCTOR(type, x)` | Wraps the option's operator() as an expr_t functor |
 *
 * @section opt_naming Naming Conventions
 *
 * - **Trailing underscore** = option takes a string argument.
 *   Example: `limit_` declares `--limit VALUE`.
 * - **No trailing underscore** = boolean flag.
 *   Example: `collapse` declares `--collapse` (on/off).
 * - **Underscores in name** map to **hyphens on CLI**.
 *   Example: `sort_all_` becomes `--sort-all VALUE`.
 *   The `is_eq()` helper function handles this mapping during lookup.
 * - **Member name**: The option instance is always `name##handler`.
 *   Example: `OPTION(report_t, color)` creates member `colorhandler`.
 *
 * @section opt_example Step-by-Step: Adding a New Command-Line Option
 *
 * Suppose you want to add `--my-filter VALUE` to report_t:
 *
 * **Step 1.** Declare the option in report.h using an OPTION macro.
 * Since it takes an argument, use a trailing underscore in the name:
 * @code
 *   // In report_t's member declarations:
 *   OPTION_(report_t, my_filter_,
 *     DO_(str) {
 *       // Custom handler: also activate --limit with a predicate
 *       OTHER(limit_).on(whence, string("account=~/") + str + "/");
 *     });
 * @endcode
 * This expands to a struct `my_filter_option_t` with an instance
 * `my_filter_handler` as a member of report_t.
 *
 * **Step 2.** Register the option in report_t::lookup_option() (report.cc):
 * @code
 *   OPT(my_filter_);
 * @endcode
 * Or, to also support `-m` as a shortcut, use:
 * @code
 *   OPT_(my_filter_);
 *   // and set ch='m' via a custom CTOR if desired.
 * @endcode
 *
 * **Step 3.** (Optional) If you need a filter handler in the pipeline,
 * add it to chain_pre_post_handlers() or chain_post_handlers() in chain.cc:
 * @code
 *   if (report.HANDLED(my_filter_))
 *     handler.reset(new my_filter_handler_t(handler, report.HANDLER(my_filter_).str()));
 * @endcode
 *
 * **Step 4.** Add the option to report_options() for --options output:
 * @code
 *   HANDLER(my_filter_).report(out);
 * @endcode
 *
 * **Step 5.** For a simple boolean flag instead, omit the trailing underscore:
 * @code
 *   OPTION(report_t, my_flag);  // creates --my-flag with no argument
 * @endcode
 * Test it with `report.HANDLED(my_flag)` which returns true if the user
 * passed `--my-flag`.
 */

#include "scope.h"

namespace ledger {

using namespace boost::placeholders;

class call_scope_t;

/**
 * @brief Core template for declaring and managing a single command-line option.
 *
 * Each option in ledger is an instance of option_t<T> (or a subclass generated
 * by the OPTION macros), where T is the owning scope class (typically
 * report_t).  The template provides:
 *
 * - **State tracking**: whether the option has been activated (handled), what
 *   source set it (command line, environment variable, init file), and the
 *   string value provided as its argument.
 * - **Activation protocol**: on(whence) for boolean flags, on(whence, str)
 *   for options taking a value.  Both call the virtual handler_thunk() which
 *   subclasses override for custom side-effect logic (e.g., --basis also
 *   activating --revalued).
 * - **CLI integration**: is_eq() maps CLI hyphens to internal underscores,
 *   desc() produces the --long-form for error messages and --options output.
 * - **Expression bridge**: handler() and operator() adapt the option for use
 *   as an expr_t functor, so options can be called from value expressions.
 *
 * @tparam T The parent scope type (e.g., report_t, session_t) that owns
 *           this option as a member variable.
 */
template <typename T>
class option_t {
protected:
  const char* name;            ///< Internal name with underscores (trailing _ means wants arg)
  string::size_type name_len;  ///< Cached strlen(name) for fast comparisons
  const char ch;               ///< Single-character shortcut, or '\0' if none
  bool handled;                ///< True once on() has been called (option is active)
  optional<string> source;     ///< Where the option was set: "--opt", "$ENV", "?normalize", etc.

  option_t& operator=(const option_t&);

public:
  T* parent;        ///< Owning scope; set by OPT macros during lookup before handler_thunk runs
  string value;     ///< The string argument value (empty for boolean flags)
  bool wants_arg;   ///< True if the option takes an argument (name ends with '_')

  /// @brief Construct an option with the given internal name and optional shortcut.
  /// @param _name  Internal name string (e.g., "sort_all_"); trailing '_' sets wants_arg.
  /// @param _ch    Single-character CLI shortcut (e.g., 'S'), or '\0' for none.
  option_t(const char* _name, const char _ch = '\0')
      : name(_name), name_len(std::strlen(name)), ch(_ch), handled(false), parent(nullptr), value(),
        wants_arg(name_len > 0 ? name[name_len - 1] == '_' : false) {
    DEBUG("option.names", "Option: " << name);
    TRACE_CTOR(option_t, "const char *, const char");
  }
  option_t(const option_t& other)
      : name(other.name), name_len(other.name_len), ch(other.ch), handled(other.handled),
        source(other.source), parent(nullptr), value(other.value), wants_arg(other.wants_arg) {
    TRACE_CTOR(option_t, "copy");
  }

  virtual ~option_t() { TRACE_DTOR(option_t); }

  /// @brief Print this option's current value and source for --options output.
  /// Only produces output if the option is handled and has a known source.
  void report(std::ostream& out) const {
    if (handled && source) {
      out.width(24);
      out << std::right << desc();
      if (wants_arg) {
        out << " = ";
        out.width(42);
        out << std::left << value;
      } else {
        out.width(45);
        out << ' ';
      }
      out << std::left << *source << '\n';
    }
  }

  /// @brief Return the CLI display form (e.g., "--sort-all (-S)").
  /// Converts internal underscores to hyphens and appends the shortcut if present.
  string desc() const {
    std::ostringstream out;
    out << "--";
    for (const char* p = name; *p; p++) {
      if (*p == '_') {
        if (*(p + 1))
          out << '-';
      } else {
        out << *p;
      }
    }
    if (ch)
      out << " (-" << ch << ")";
    return out.str();
  }

  /// @brief Test whether this option has been activated.
  operator bool() const { return handled; }

  /// @brief Return the option's string value.  Asserts that the option is handled.
  /// @throws std::runtime_error if the value is empty (no argument was provided).
  string str() const {
    assert(handled);
    if (value.empty())
      throw_(std::runtime_error, _f("No argument provided for %1%") % desc());
    return value;
  }

  /// @brief Activate a boolean (no-argument) option.
  /// Calls handler_thunk(whence) for subclass-specific side effects, then marks
  /// the option as handled and records the source.
  /// @param whence  Describes who set this option (e.g., "--actual", "$LEDGER_ACTUAL").
  void on(const char* whence) { on(string(whence)); }
  void on(const optional<string>& whence) {
    handler_thunk(whence);

    handled = true;
    source = whence;
  }

  /// @brief Activate an option that takes a string argument.
  /// Calls handler_thunk(whence, str) for subclass-specific side effects.
  /// If the thunk did not modify value, sets value = str.  Then marks as handled.
  /// @param whence  Describes who set this option.
  /// @param str     The argument value (e.g., "monthly", "amount > 100").
  void on(const char* whence, const string& str) { on(string(whence), str); }
  void on(const optional<string>& whence, const string& str) {
    string before = value; // NOLINT(bugprone-unused-local-non-trivial-variable)

    handler_thunk(whence, str);

    if (value == before)
      value = str;

    handled = true;
    source = whence;
  }

  /// @brief Deactivate this option, clearing its value and source.
  void off() {
    handled = false;
    value = "";
    source = none;
  }

  /// @brief Override point for boolean option side effects (called by on(whence)).
  virtual void handler_thunk(const optional<string>&) {}
  /// @brief Override point for argument option side effects (called by on(whence, str)).
  virtual void handler_thunk(const optional<string>&, const string&) {}

  /// @brief Entry point when the option is invoked via the expression engine.
  /// Validates argument count and types, then delegates to on().
  /// @param args  Call scope with args[0] = whence string, args[1] = value (if wants_arg).
  /// @return true on success.
  value_t handler(call_scope_t& args) {
    // NOLINTBEGIN(bugprone-branch-clone)
    if (wants_arg) {
      if (args.size() < 2)
        throw_(std::runtime_error, _f("No argument provided for %1%") % desc());
      else if (args.size() > 2)
        throw_(std::runtime_error, _f("Too many arguments provided for %1%") % desc());
      else if (!args[0].is_string())
        throw_(std::runtime_error, _f("Context argument for %1% not a string") % desc());
      on(args.get<string>(0), args.get<string>(1));
    } else if (args.size() < 1) {
      throw_(std::runtime_error, _f("No argument provided for %1%") % desc());
    } else if (!args[0].is_string()) {
      throw_(std::runtime_error, _f("Context argument for %1% not a string") % desc());
    } else {
      on(args.get<string>(0));
    }
    // NOLINTEND(bugprone-branch-clone)
    return true;
  }

  /// @brief Functor interface: set the option if args are provided, or query its value.
  /// When called with arguments, delegates to handler().  When called with no args,
  /// returns the current value (for wants_arg options) or handled status (for booleans).
  virtual value_t operator()(call_scope_t& args) {
    if (!args.empty()) {
      args.push_front(string_value("?expr"));
      return handler(args);
    } else if (wants_arg) {
      return string_value(value);
    } else {
      return handled;
    }
  }
};

/*--- Structure Macros (building blocks for option declarations) ---*/

#define BEGIN(type, name) struct name##option_t : public option_t<type>  ///< Opens the option struct

#define CTOR(type, name) name##option_t() : option_t<type>(#name)  ///< Basic constructor
#define CTOR_(type, name, base) name##option_t() : option_t<type>(#name), base  ///< Ctor with extra init
#define DECL1(type, name, vartype, var, value)                                                     \
  vartype var;                                                                                     \
  name##option_t() : option_t<type>(#name), var value  ///< Declares extra member var + constructor

/*--- Handler Override Macros ---*/

#define DO() virtual void handler_thunk([[maybe_unused]] const optional<string>& whence) override  ///< Override for boolean (no-arg) options
#define DO_()                                                                                      \
  virtual void handler_thunk([[maybe_unused]] const optional<string>& whence,                      \
                             [[maybe_unused]] const string& str) override  ///< Override for options taking an argument

/*--- Instance and Copy Macros ---*/

#define END(name) name##handler  ///< Closes the struct and declares the member instance

#define COPY_OPT(name, other) name##handler((other).name##handler)  ///< Copy one option's handler from another instance

/*--- Factory Macros (wrap option methods as expression functors) ---*/

#define MAKE_OPT_HANDLER(type, x) expr_t::op_t::wrap_functor(bind(&option_t<type>::handler, x, _1))  ///< Wrap handler() as expr_t functor

#define MAKE_OPT_FUNCTOR(type, x)                                                                  \
  expr_t::op_t::wrap_functor(bind(&option_t<type>::operator(), x, _1))  ///< Wrap operator() as expr_t functor

/// @brief Test whether CLI name @a p matches internal option name @a n.
/// Maps hyphens in @a p to underscores in @a n, and ignores a trailing
/// underscore in @a n (which indicates the option takes an argument).
inline bool is_eq(const char* p, const char* n) {
  // Test whether p matches n, substituting - in p for _ in n.
  for (; *p && *n; p++, n++) {
    if (!(*p == '-' && *n == '_') && *p != *n)
      return false;
  }
  // Ignore any trailing underscore
  return *p == *n || (!*p && *n == '_' && !*(n + 1));
}

/*--- Lookup Macros (used in lookup_option() to match CLI strings to handlers) ---*/

#define OPT(name)                                                                                  \
  if (is_eq(p, #name))                                                                             \
  return ((name##handler).parent = this, &(name##handler))  ///< Exact name match

#define OPT_ALT(name, alt)                                                                         \
  if (is_eq(p, #name) || is_eq(p, #alt))                                                           \
  return ((name##handler).parent = this, &(name##handler))  ///< Match by primary or alternative name

#define OPT_(name)                                                                                 \
  if (!*(p + 1) || ((name##handler).wants_arg && *(p + 1) == '_' && !*(p + 2)) || is_eq(p, #name)) \
  return ((name##handler).parent = this, &(name##handler))  ///< Match by full name or single-char shortcut

#define OPT_CH(name)                                                                               \
  if (!*(p + 1) || ((name##handler).wants_arg && *(p + 1) == '_' && !*(p + 2)))                    \
  return ((name##handler).parent = this, &(name##handler))  ///< Single-character-only match

/*--- Access and Cross-Reference Macros ---*/

#define HANDLER(name) name##handler     ///< Access the option's member instance
#define HANDLED(name) HANDLER(name)     ///< Same as HANDLER; tests if option is set via bool conversion

/*--- Convenience Declaration Macros ---*/

/// @brief Declare a simple option with no custom handler logic.
#define OPTION(type, name)                                                                         \
  BEGIN(type, name) {                                                                              \
    CTOR(type, name) {}                                                                            \
  }                                                                                                \
  END(name)

/// @brief Declare an option with a custom DO()/DO_() handler body.
#define OPTION_(type, name, ...) BEGIN(type, name){CTOR(type, name){} __VA_ARGS__} END(name)

/// @brief Declare an option with fully custom body (extra members, custom CTOR, handlers).
#define OPTION_CTOR(type, name, ...) BEGIN(type, name){__VA_ARGS__} END(name)

/// @brief Access another option on the same parent scope from within a DO()/DO_() body.
/// Sets the sibling option's parent pointer, then returns its handler for chaining (e.g.,
/// OTHER(limit_).on(whence, "actual") inside --actual's handler to also set --limit).
#define OTHER(name)                                                                                \
  parent->HANDLER(name).parent = parent;                                                           \
  parent->HANDLER(name)

/// @brief Look up and invoke a named option in the given scope.
/// @param whence  Source description for provenance tracking (e.g., "--limit", "$LEDGER_LIMIT").
/// @param name    Option name (hyphens, no leading dashes) to look up.
/// @param scope   The scope to search for the option handler.
/// @param arg     The option's argument value, or nullptr for boolean flags.
/// @param varname Display name for error messages.
/// @return true if the option was found and processed, false otherwise.
bool process_option(const string& whence, const string& name, scope_t& scope, const char* arg,
                    const string& varname);

/// @brief Scan environment variables for options matching a tag prefix.
/// Variables matching the prefix (e.g., "LEDGER_") are converted from
/// LEDGER_OPTION_NAME=value to --option-name value and processed.
/// @param envp  The environment variable array (from main's envp).
/// @param tag   The prefix to match (e.g., "LEDGER_").
/// @param scope The scope in which to look up and activate options.
void process_environment(const char** envp, const string& tag, scope_t& scope);

/// @brief Parse command-line arguments, processing options and collecting non-option args.
/// Handles --long-option, --long-option=value, -x short options, and bundled
/// short options (-abc).  A bare "--" stops option processing.
/// @param args   The argument list (typically from argv, minus the program name).
/// @param scope  The scope in which to look up and activate options.
/// @return The list of non-option arguments (command name, query terms, etc.).
strings_list process_arguments(strings_list args, scope_t& scope);

/// @brief Exception type for command-line option errors (illegal option, missing argument).
class option_error : public std::runtime_error {
public:
  explicit option_error(const string& why) noexcept : std::runtime_error(why) {}
  ~option_error() noexcept override {}
};

} // namespace ledger
