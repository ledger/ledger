#include "xml.h"
#include "journal.h"
#include "datetime.h"
#include "error.h"

#include <iostream>
#include <sstream>
#include <cstring>

extern "C" {
#include <xmlparse.h>           // expat XML parser
}

namespace ledger {

static XML_Parser    current_parser;
static unsigned int  count;

static journal_t *   curr_journal;
static entry_t *     curr_entry;
static commodity_t * curr_comm;
static std::string   comm_flags;

static std::string   data;
static bool          ignore;
static std::string   have_error;

static void startElement(void *userData, const char *name, const char **attrs)
{
  if (ignore)
    return;

  if (std::strcmp(name, "entry") == 0) {
    assert(! curr_entry);
    curr_entry = new entry_t;
  }
  else if (std::strcmp(name, "transaction") == 0) {
    assert(curr_entry);
    curr_entry->add_transaction(new transaction_t);
  }
  else if (std::strcmp(name, "commodity") == 0) {
    if (std::string(attrs[0]) == "flags")
      comm_flags = attrs[1];
  }
  else if (std::strcmp(name, "total") == 0) {
    ignore = true;
  }
}

static void endElement(void *userData, const char *name)
{
  if (ignore) {
    if (std::strcmp(name, "total") == 0)
      ignore = false;
    return;
  }

  if (std::strcmp(name, "entry") == 0) {
    assert(curr_entry);
    if (curr_journal->add_entry(curr_entry)) {
      count++;
    } else {
      account_t * acct = curr_journal->find_account("<Unknown>");
      curr_entry->add_transaction(new transaction_t(acct));
      if (curr_journal->add_entry(curr_entry)) {
	count++;
      } else {
	delete curr_entry;
	have_error = "Entry does not balance";
      }
    }
    curr_entry = NULL;
  }
  else if (std::strcmp(name, "en:date") == 0) {
    quick_parse_date(data.c_str(), &curr_entry->date);
  }
  else if (std::strcmp(name, "en:cleared") == 0) {
    curr_entry->state = entry_t::CLEARED;
  }
  else if (std::strcmp(name, "en:pending") == 0) {
    curr_entry->state = entry_t::PENDING;
  }
  else if (std::strcmp(name, "en:code") == 0) {
    curr_entry->code = data;
  }
  else if (std::strcmp(name, "en:payee") == 0) {
    curr_entry->payee = data;
  }
  else if (std::strcmp(name, "tr:account") == 0) {
    curr_entry->transactions.back()->account = curr_journal->find_account(data);
  }
  else if (std::strcmp(name, "commodity") == 0) {
    assert(! curr_comm);
    curr_comm = commodity_t::find_commodity(data, true);
    curr_comm->flags |= COMMODITY_STYLE_SUFFIXED;
    if (! comm_flags.empty())
      for (std::string::size_type i = 0, l = comm_flags.length(); i < l; i++)
	switch (comm_flags[i]) {
	case 'P': curr_comm->flags &= ~COMMODITY_STYLE_SUFFIXED; break;
	case 'S': curr_comm->flags |= COMMODITY_STYLE_SEPARATED; break;
	case 'T': curr_comm->flags |= COMMODITY_STYLE_THOUSANDS; break;
	case 'E': curr_comm->flags |= COMMODITY_STYLE_EUROPEAN; break;
	}
  }
  else if (std::strcmp(name, "quantity") == 0) {
    curr_entry->transactions.back()->amount.parse(data);
    if (curr_comm) {
      std::string::size_type i = data.find('.');
      if (i != std::string::npos) {
	int precision = data.length() - i - 1;
	if (precision > curr_comm->precision)
	  curr_comm->precision = precision;
      }
      curr_entry->transactions.back()->amount.set_commodity(*curr_comm);
      curr_comm = NULL;
    }
  }
  else if (std::strcmp(name, "tr:amount") == 0) {
    curr_comm = NULL;
  }
}

static void dataHandler(void *userData, const char *s, int len)
{
  if (! ignore)
    data = std::string(s, len);
}

bool xml_parser_t::test(std::istream& in) const
{
  char buf[256];

  in.getline(buf, 255);
  if (std::strncmp(buf, "<?xml version=\"1.0\"?>", 21) != 0) {
    in.seekg(0, std::ios::beg);
    return false;
  }

  in.getline(buf, 255);
  if (! std::strstr(buf, "<ledger>")) {
    in.seekg(0, std::ios::beg);
    return false;
  }

  in.seekg(0, std::ios::beg);
  return true;
}

unsigned int xml_parser_t::parse(std::istream&	 in,
				     journal_t *	 journal,
				     account_t *	 master,
				     const std::string * original_file)
{
  char buf[BUFSIZ];

  count        = 0;
  curr_journal = journal;
  curr_entry   = NULL;
  curr_comm    = NULL;
  ignore       = false;

  XML_Parser parser = XML_ParserCreate(NULL);
  current_parser = parser;

  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  while (! in.eof()) {
    in.getline(buf, BUFSIZ - 1);
    bool result;
    try {
      result = XML_Parse(parser, buf, std::strlen(buf), in.eof());
    }
    catch (const std::exception& err) {
      unsigned long line = XML_GetCurrentLineNumber(parser);
      XML_ParserFree(parser);
      throw parse_error(original_file ? *original_file : "<xml>", line,
			err.what());
    }

    if (! have_error.empty()) {
      unsigned long line = XML_GetCurrentLineNumber(parser);
      parse_error err(original_file ? *original_file : "<xml>", line,
		      have_error);
      std::cerr << "Error: " << err.what() << std::endl;
    }

    if (! result) {
      unsigned long line = XML_GetCurrentLineNumber(parser);
      const char *  err  = XML_ErrorString(XML_GetErrorCode(parser));
      XML_ParserFree(parser);
      throw parse_error(original_file ? *original_file : "<xml>", line, err);
    }
  }

  XML_ParserFree(parser);

  return count;
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(xml_parse_overloads,
				       xml_parser_t::parse, 2, 4)

void export_xml() {
  class_< xml_parser_t, bases<parser_t> > ("XmlParser")
    .def("test", &xml_parser_t::test)
    .def("parse", &xml_parser_t::parse, xml_parse_overloads())
    ;
}

#endif // USE_BOOST_PYTHON
