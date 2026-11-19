#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

#include <system.hh>

#include <chrono>
#include <limits>
#include <new>
#include <regex>
#include <thread>
#include <type_traits>

#include "utils.h"

using namespace ledger;

namespace {

// Send what the timers log to a string for the length of one test.
struct timer_log_fixture {
  std::ostringstream log;
  std::ostream* saved_stream;

  timer_log_fixture() : saved_stream(_log_stream) { _log_stream = &log; }
  ~timer_log_fixture() { _log_stream = saved_stream; }

  // The milliseconds finish_timer logged after DESCRIPTION.
  long logged_ms(const std::string& description) const {
    const std::string text = log.str();
    std::size_t pos = text.find(description + " ");
    BOOST_REQUIRE(pos != std::string::npos);
    pos += description.size() + 1;
    if (text[pos] == '(')
      ++pos;
    return std::stol(text.substr(pos));
  }
};

} // namespace

BOOST_FIXTURE_TEST_SUITE(timers, timer_log_fixture)

BOOST_AUTO_TEST_CASE(testFinishTimerKeepsStoppedTime)
{
  // A timer stopped after 50ms and started again must report at least 50ms
  // when it is finished while running: the earlier segment still counts.
  _log_buffer << "Time spent in t_utils:";
  start_timer("t_utils_accumulate", LOG_TRACE);
  std::this_thread::sleep_for(std::chrono::milliseconds(50));
  stop_timer("t_utils_accumulate");

  _log_buffer << "Time spent in t_utils:";
  start_timer("t_utils_accumulate", LOG_TRACE);
  finish_timer("t_utils_accumulate");

  BOOST_CHECK_GE(logged_ms("Time spent in t_utils:"), 50L);
}

BOOST_AUTO_TEST_CASE(testFinishTimerWithEmptyDescription)
{
  // Guard, not a failing-first test: an empty description has no last
  // character to compare with ':'.  The unfixed code reads description[-1],
  // undefined behavior that aborts only under hardened standard-library
  // assertions.  The time is logged in parentheses, as for any description
  // without a trailing colon.
  start_timer("t_utils_empty", LOG_TRACE);
  finish_timer("t_utils_empty");

  BOOST_CHECK(std::regex_search(log.str(), std::regex("\\[TRACE\\]  \\([0-9]+ms\\)\\n$")));
}

BOOST_AUTO_TEST_SUITE_END()

namespace {

std::vector<string> split(const char* line) {
  strings_list args = split_arguments(line);
  return {args.begin(), args.end()};
}

auto says(const string& text) {
  return [text](const std::logic_error& err) { return string(err.what()) == text; };
}

} // namespace

BOOST_AUTO_TEST_SUITE(utils)

BOOST_AUTO_TEST_CASE(testSplitArgumentsWhitespace)
{
  std::vector<string> want{"bal", "Food", "Cash"};
  std::vector<string> got = split("  bal  Food\tCash\r\n");
  BOOST_CHECK_EQUAL_COLLECTIONS(got.begin(), got.end(), want.begin(), want.end());

  BOOST_CHECK(split("").empty());
  BOOST_CHECK(split(" \t\r\n").empty());
}

BOOST_AUTO_TEST_CASE(testSplitArgumentsQuotes)
{
  // Quotes group words and are removed; the other kind of quote is
  // literal inside them; quoted text joins the bare text it touches.
  std::vector<string> want{"a b", "c d", "a\"b", "a'b", "xy zw"};
  std::vector<string> got = split("'a b' \"c d\" 'a\"b' \"a'b\" x'y z'w");
  BOOST_CHECK_EQUAL_COLLECTIONS(got.begin(), got.end(), want.begin(), want.end());

  // An empty quoted argument yields no argument at all.
  std::vector<string> want_empty{"a", "b"};
  std::vector<string> got_empty = split("a '' \"\" b");
  BOOST_CHECK_EQUAL_COLLECTIONS(got_empty.begin(), got_empty.end(), want_empty.begin(),
                                want_empty.end());
}

BOOST_AUTO_TEST_CASE(testSplitArgumentsBackslash)
{
  // A backslash escapes the next character outside quotes and inside
  // double quotes, and is literal inside single quotes.
  std::vector<string> want{"a b", "a\"b", "a\\b", "a\\"};
  std::vector<string> got = split("a\\ b \"a\\\"b\" 'a\\b' 'a\\'");
  BOOST_CHECK_EQUAL_COLLECTIONS(got.begin(), got.end(), want.begin(), want.end());
}

BOOST_AUTO_TEST_CASE(testSplitArgumentsErrors)
{
  BOOST_CHECK_EXCEPTION(split_arguments("abc\\"), std::logic_error,
                        says("Invalid use of backslash"));
  BOOST_CHECK_EXCEPTION(split_arguments("'abc"), std::logic_error,
                        says("Unterminated string, expected '''"));
  BOOST_CHECK_EXCEPTION(split_arguments("\"abc"), std::logic_error,
                        says("Unterminated string, expected '\"'"));
}

BOOST_AUTO_TEST_CASE(testSplitArgumentsLongWord)
{
  // An argument has no length limit; it used to be copied into a
  // 4096-byte stack buffer.
  const string word(10000, 'x');
  const string quoted(6000, 'q');
  const string line = "bal " + word + " '" + quoted + "' end";
  std::vector<string> want{"bal", word, quoted, "end"};
  std::vector<string> got = split(line.c_str());
  BOOST_CHECK_EQUAL_COLLECTIONS(got.begin(), got.end(), want.begin(), want.end());
}

BOOST_AUTO_TEST_SUITE_END()

// AddressSanitizer aborts on an impossible allocation (allocation-size-too-big)
// unless allocator_may_return_null=1, so the allocation-failure cases cannot
// run under it; they run in every other build.
#if defined(__SANITIZE_ADDRESS__)
#define LEDGER_TEST_ASAN 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define LEDGER_TEST_ASAN 1
#endif
#endif
#ifndef LEDGER_TEST_ASAN
#define LEDGER_TEST_ASAN 0
#endif

namespace {

// volatile keeps the compiler from folding or warning about the constant size.
volatile std::size_t impossible_size = std::numeric_limits<std::size_t>::max();

void allocate_and_free(std::size_t size) {
  ::operator delete(::operator new(size));
}

void allocate_array_and_free(std::size_t size) {
  ::operator delete[](::operator new[](size));
}

int new_handler_calls = 0;

void counting_new_handler() {
  ++new_handler_calls;
  std::set_new_handler(nullptr); // the next failure throws
}

} // namespace

BOOST_AUTO_TEST_SUITE(utils)

#if !LEDGER_TEST_ASAN

BOOST_AUTO_TEST_CASE(testOperatorNewThrowsBadAlloc)
{
  BOOST_CHECK_THROW(allocate_and_free(impossible_size), std::bad_alloc);
}

BOOST_AUTO_TEST_CASE(testOperatorNewArrayThrowsBadAlloc)
{
  BOOST_CHECK_THROW(allocate_array_and_free(impossible_size), std::bad_alloc);
}

BOOST_AUTO_TEST_CASE(testOperatorNewCallsNewHandler)
{
  new_handler_calls = 0;
  std::new_handler previous = std::set_new_handler(counting_new_handler);
  BOOST_CHECK_THROW(allocate_and_free(impossible_size), std::bad_alloc);
  std::set_new_handler(previous);
  BOOST_CHECK_EQUAL(new_handler_calls, 1);
}

#endif // !LEDGER_TEST_ASAN

BOOST_AUTO_TEST_CASE(testOperatorNewZeroSizeIsUnique)
{
  void* a = ::operator new(std::size_t(0));
  void* b = ::operator new(std::size_t(0));
  BOOST_CHECK(a != nullptr);
  BOOST_CHECK(b != nullptr);
  BOOST_CHECK(a != b);
  ::operator delete(a);
  ::operator delete(b);
}

BOOST_AUTO_TEST_SUITE_END()

// A signal handler may only write a volatile std::sig_atomic_t
// ([support.signal]); check_for_signal() polls it at safe points.
static_assert(std::is_same_v<decltype(caught_signal), volatile std::sig_atomic_t>,
              "caught_signal must be volatile std::sig_atomic_t");

BOOST_AUTO_TEST_SUITE(utils)

BOOST_AUTO_TEST_CASE(testSignalHandlersSetCaughtSignal)
{
  caught_signal = NONE_CAUGHT;
  BOOST_CHECK_NO_THROW(check_for_signal());

  sigint_handler(SIGINT);
  BOOST_CHECK_EQUAL(static_cast<int>(caught_signal), static_cast<int>(INTERRUPTED));
  BOOST_CHECK_EXCEPTION(check_for_signal(), std::runtime_error, [](const std::runtime_error& e) {
    return std::string(e.what()) == "Interrupted by user (use Control-D to quit)";
  });

  caught_signal = NONE_CAUGHT;
  sigpipe_handler(SIGPIPE);
  BOOST_CHECK_EQUAL(static_cast<int>(caught_signal), static_cast<int>(PIPE_CLOSED));
  BOOST_CHECK_EXCEPTION(check_for_signal(), std::runtime_error, [](const std::runtime_error& e) {
    return std::string(e.what()) == "Pipe terminated";
  });

  caught_signal = NONE_CAUGHT;
}

BOOST_AUTO_TEST_SUITE_END()

// Spelled ledger::optional so the file compiles whether ledger's
// optional is boost::optional or std::optional.
using opt_int = ledger::optional<int>;

BOOST_AUTO_TEST_SUITE(utils)

BOOST_AUTO_TEST_CASE(testScopedAssignRestoresOnScopeExit)
{
  bool flag = true;
  {
    scoped_assign<bool> guard(flag, false);
    BOOST_CHECK(!flag);
    BOOST_CHECK(guard.saved());
  }
  BOOST_CHECK(flag);
}

BOOST_AUTO_TEST_CASE(testScopedAssignRestoresWhenUnwinding)
{
  opt_int year = 2020;
  auto draft = [&year] {
    scoped_assign<opt_int> guard(year, opt_int());
    BOOST_CHECK(!year);
    throw std::runtime_error("draft failed");
  };
  BOOST_CHECK_THROW(draft(), std::runtime_error);
  BOOST_REQUIRE(year);
  BOOST_CHECK_EQUAL(*year, 2020);
}

BOOST_AUTO_TEST_CASE(testScopedAssignSaveOnlyUndoesWrites)
{
  opt_int year = 2020;
  {
    scoped_assign<opt_int> guard(year);
    BOOST_REQUIRE(year);
    BOOST_CHECK_EQUAL(*year, 2020);
    year = 2023; // an included file's `year 2023`
  }
  BOOST_REQUIRE(year);
  BOOST_CHECK_EQUAL(*year, 2020);

  opt_int unset;
  {
    scoped_assign<opt_int> guard(unset);
    unset = 1999;
  }
  BOOST_CHECK(!unset);
}

BOOST_AUTO_TEST_CASE(testScopedAssignNestsInReverseOrder)
{
  int depth = 0;
  {
    scoped_assign<int> outer(depth, 1);
    {
      scoped_assign<int> inner(depth, 2);
      BOOST_CHECK_EQUAL(depth, 2);
      BOOST_CHECK_EQUAL(inner.saved(), 1);
    }
    BOOST_CHECK_EQUAL(depth, 1);
  }
  BOOST_CHECK_EQUAL(depth, 0);
}

BOOST_AUTO_TEST_CASE(testScopedAssignIsNoncopyable)
{
  BOOST_CHECK(!std::is_copy_constructible_v<scoped_assign<bool>>);
  BOOST_CHECK(!std::is_copy_assignable_v<scoped_assign<bool>>);
}

BOOST_AUTO_TEST_SUITE_END()
