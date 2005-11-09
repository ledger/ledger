#ifndef _PARSER_H
#define _PARSER_H

#include <iostream>
#include <string>

namespace ledger {

class account_t;
class journal_t;

class parser_t
{
 public:
  virtual ~parser_t() {}

  virtual bool test(std::istream& in) const = 0;

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL) = 0;
};

bool register_parser(parser_t * parser);
bool unregister_parser(parser_t * parser);

unsigned int parse_journal(std::istream&       in,
			   journal_t *	       journal,
			   account_t *	       master        = NULL,
			   const std::string * original_file = NULL);

unsigned int parse_journal_file(const std::string&  path,
				journal_t *	    journal,
				account_t *	    master        = NULL,
				const std::string * original_file = NULL);

unsigned int parse_ledger_data(journal_t *	  journal,
			       const std::string& data_file,
			       const std::string& init_file	  = "",
			       const std::string& price_db	  = "",
			       bool		  use_cache	  = false,
			       const std::string& cache_file	  = "",
			       bool *		  cache_dirty	  = NULL,
			       parser_t *         cache_parser	  = NULL,
			       parser_t *         xml_parser	  = NULL,
			       parser_t *         stdin_parser	  = NULL,
			       const std::string& default_account = "");

class config_t;
unsigned int parse_ledger_data(journal_t * journal, config_t& config);

void initialize_parser_support();
void shutdown_parser_support();

} // namespace ledger

#endif // _PARSER_H
