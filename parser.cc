#include "parser.h"

#include <fstream>

namespace ledger {

parsers_list parser_t::parsers;

unsigned int parser_t::parse_file(const std::string&  path,
				  journal_t *	      journal,
				  account_t *	      master,
				  const std::string * original_file)
{
  journal->sources.push_back(path);

  if (access(path.c_str(), R_OK) == -1)
    return 0;

  std::ifstream stream(path.c_str());

  if (! master)
    master = journal->master;
  if (! original_file)
    original_file = &path;

  for (parsers_list::iterator i = parser_t::parsers.begin();
       i != parser_t::parsers.end();
       i++)
    if ((*i)->test(stream))
      return (*i)->parse(stream, journal, master, original_file);

  return 0;
}

} // namespace ledger
