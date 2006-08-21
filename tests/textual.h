#ifndef __TESTTEXTUALJOURNAL_H
#define __TESTTEXTUALJOURNAL_H

#include <cxxtest/TestSuite.h>

#include <textual.h>
#include <config.h>

using namespace std;
using namespace ledger;

class TestTextualJournal : public CxxTest::TestSuite
{
public:
    void testEmptyFileIsTextualFile()
    {
	stringstream j(stringstream::in);

	j << "2005/10/15 Something" << endl;
	j << "  A $ 42" << endl;
	j << "  B" << endl;

	textual_parser_t textualParser;
	TS_ASSERT(textualParser.test(j));
	TS_ASSERT(j.good());
	TS_ASSERT_EQUALS(0, j.tellg());

	config_t config;
	std::auto_ptr<journal_t> journal(new journal_t);
	textualParser.parse(j, config, journal.get());
    }
};

#endif // __TESTTEXTUALJOURNAL_H
