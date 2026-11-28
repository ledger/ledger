#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE gpgme
#include <boost/test/unit_test.hpp>

#include <system.hh>

// Keep this include order.  The unit-test compile line puts -I src ahead
// of the gpgme include directory, so the `#include <gpgme.h>` inside
// <gpgme++/data.h> also names src/gpgme.h; including it here first lets its
// #pragma once make that nested include a no-op (checked with gpgme++ 2.0).
#include "gpgme.h"
#include "session.h"
#include "journal.h"
#include "context.h"
#include "account.h"
#include "scope.h"

#include <gpgme++/global.h>

using namespace ledger;

// data_streambuffer_t serves decrypted text through a 4096-byte get area
// filled ahead of the reader.  These tests build the decrypted Data from
// memory, so they need neither gpg nor a key: positions reported by tellg()
// must count the bytes the reader consumed, not the bytes buffered.

namespace {

std::shared_ptr<std::istream> memory_stream(const std::string& text) {
  auto data = std::make_shared<GpgME::Data>(text.data(), text.size(), true);
  data->rewind();
  return std::shared_ptr<std::istream>(new decrypted_stream_t(data));
}

std::string alphabet_text(std::size_t size) {
  std::string text;
  for (std::size_t i = 0; i < size; ++i)
    text += static_cast<char>('a' + i % 26);
  return text;
}

} // namespace

struct gpgme_fixture {
  session_t session;
  scope_t* saved_default_scope;

  gpgme_fixture() {
    GpgME::initializeLibrary(0);
    set_session_context(&session);
    saved_default_scope = scope_t::default_scope;
    scope_t::default_scope = &session;
  }

  ~gpgme_fixture() {
    scope_t::default_scope = saved_default_scope;
    set_session_context(nullptr);
  }

  // Parse TEXT through a decrypted stream and return the number of
  // transactions read.
  std::size_t parse_decrypted(const std::string& text) {
    session.journal.reset(new journal_t);
    session.parsing_context.push(memory_stream(text));
    parse_context_t& ctx = session.parsing_context.get_current();
    ctx.journal = session.journal.get();
    ctx.master = session.journal->master;
    std::size_t count = 0;
    try {
      count = session.journal->read(session.parsing_context, NO_HASHES);
    } catch (...) {
      session.parsing_context.pop();
      throw;
    }
    session.parsing_context.pop();
    return count;
  }
};

BOOST_FIXTURE_TEST_SUITE(gpgme_stream, gpgme_fixture)

BOOST_AUTO_TEST_CASE(testTellgKeepsBufferedData)
{
  std::shared_ptr<std::istream> in = memory_stream("abcdefgh\n");
  in->get();
  in->get();
  in->get();
  BOOST_CHECK_EQUAL(std::streamoff(3), std::streamoff(in->tellg()));
  BOOST_CHECK_EQUAL('d', in->get());
}

BOOST_AUTO_TEST_CASE(testSeekgReturnsToTelledPosition)
{
  std::shared_ptr<std::istream> in = memory_stream("abcdefgh\n");
  in->get();
  in->get();
  std::streampos saved = in->tellg();
  in->get();
  in->get();
  in->get();
  in->seekg(saved);
  BOOST_CHECK_EQUAL('c', in->get());
}

BOOST_AUTO_TEST_CASE(testSeekgRelativeToCurrent)
{
  std::shared_ptr<std::istream> in = memory_stream("abcdefgh\n");
  in->get();
  in->get();
  in->seekg(2, std::ios::cur);
  BOOST_CHECK_EQUAL('e', in->get());
  BOOST_CHECK_EQUAL(std::streamoff(5), std::streamoff(in->tellg()));
}

BOOST_AUTO_TEST_CASE(testPositionsAcrossBufferBoundary)
{
  const std::string text = alphabet_text(10000);
  std::shared_ptr<std::istream> in = memory_stream(text);
  for (int i = 0; i < 5000; ++i)
    in->get();
  BOOST_CHECK_EQUAL(std::streamoff(5000), std::streamoff(in->tellg()));
  BOOST_CHECK_EQUAL(text[5000], in->get());

  in->seekg(4095);
  BOOST_CHECK_EQUAL(text[4095], in->get());
  BOOST_CHECK_EQUAL(text[4096], in->get());
  BOOST_CHECK_EQUAL(std::streamoff(4097), std::streamoff(in->tellg()));
  in->seekg(-2, std::ios::cur);
  BOOST_CHECK_EQUAL(text[4095], in->get());

  in->seekg(0, std::ios::end);
  BOOST_CHECK_EQUAL(std::streamoff(10000), std::streamoff(in->tellg()));
  BOOST_CHECK_EQUAL(std::char_traits<char>::eof(), in->get());
}

BOOST_AUTO_TEST_CASE(testAccountSubDirectiveInDecryptedJournal)
{
  // The indented line after `account` makes the parser tellg()/seekg()
  // while the whole journal sits in the stream's buffer.
  std::size_t count = parse_decrypted("account Assets:Cash\n"
                                      "    note cash on hand\n"
                                      "2024/01/01 Grocery\n"
                                      "    Expenses:Food    $10.00\n"
                                      "    Assets:Cash\n");
  BOOST_CHECK_EQUAL(1u, count);
  account_t* cash = session.journal->find_account("Assets:Cash", false);
  BOOST_REQUIRE(cash != nullptr);
  BOOST_REQUIRE(cash->note);
  BOOST_CHECK_EQUAL(std::string("cash on hand"), *cash->note);
}

BOOST_AUTO_TEST_CASE(testNestedAccountInDecryptedJournal)
{
  // A nested account (issue #877) peeks at the indentation twice per line.
  std::size_t count = parse_decrypted("account Assets\n"
                                      "    account Cash\n"
                                      "        note cash on hand\n"
                                      "2024/01/01 Grocery\n"
                                      "    Expenses:Food    $10.00\n"
                                      "    Assets:Cash\n");
  BOOST_CHECK_EQUAL(1u, count);
  account_t* cash = session.journal->find_account("Assets:Cash", false);
  BOOST_REQUIRE(cash != nullptr);
  BOOST_REQUIRE(cash->note);
  BOOST_CHECK_EQUAL(std::string("cash on hand"), *cash->note);
}

BOOST_AUTO_TEST_CASE(testAccountSubDirectivesAcrossBufferBoundary)
{
  // 200 account directives (about 9 KB) span three stream buffers.
  std::string text;
  for (int i = 0; i < 200; ++i)
    text += "account Expenses:Cat" + std::to_string(i) + "\n    note category " +
            std::to_string(i) + "\n";
  text += "2024/01/01 Grocery\n"
          "    Expenses:Cat199    $10.00\n"
          "    Assets:Cash\n";

  BOOST_CHECK_EQUAL(1u, parse_decrypted(text));
  for (int i : {0, 1, 90, 91, 199}) {
    account_t* cat = session.journal->find_account("Expenses:Cat" + std::to_string(i), false);
    BOOST_REQUIRE_MESSAGE(cat != nullptr, "Expenses:Cat" << i);
    BOOST_REQUIRE(cat->note);
    BOOST_CHECK_EQUAL("category " + std::to_string(i), *cat->note);
  }
}

BOOST_AUTO_TEST_SUITE_END()
