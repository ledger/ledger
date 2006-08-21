#ifndef __TESTFILEFORMAT_H
#define __TESTFILEFORMAT_H

#include <cxxtest/TestSuite.h>

#include <textual.h>
#include <xml.h>
#include <binary.h>
#include <gnucash.h>
#include <qif.h>

using namespace std;
using namespace ledger;

class TestFileFormat : public CxxTest::TestSuite
{
public:
    void testEmptyFileIsTextualFile()
    {
	stringstream emptyStream(stringstream::in);
	textual_parser_t textualParser;
	TS_ASSERT(textualParser.test(emptyStream));
	TS_ASSERT(emptyStream.good());
	TS_ASSERT_EQUALS(0, emptyStream.tellg());
    }

    void testEmptyFileIsNotXMLFile()
    {
	stringstream emptyStream(stringstream::in);
	xml_parser_t xmlParser;
	TS_ASSERT(!xmlParser.test(emptyStream));
	TS_ASSERT(emptyStream.good());
	TS_ASSERT_EQUALS(0, emptyStream.tellg());
    }

    void testEmptyFileIsNotBinaryFile()
    {
	stringstream emptyStream(stringstream::in);
	binary_parser_t binaryParser;
	TS_ASSERT(!binaryParser.test(emptyStream));
	TS_ASSERT(emptyStream.good());
	TS_ASSERT_EQUALS(0, emptyStream.tellg());
    }

    void testEmptyFileIsNotGnuCashFile()
    {
	stringstream emptyStream(stringstream::in);
	gnucash_parser_t gnucashParser;
	TS_ASSERT(!gnucashParser.test(emptyStream));
	TS_ASSERT(emptyStream.good());
	TS_ASSERT_EQUALS(0, emptyStream.tellg());
    }

    void testEmptyFileIsNotQIFFile()
    {
	stringstream emptyStream(stringstream::in);
	qif_parser_t qifParser;
	TS_ASSERT(!qifParser.test(emptyStream));
	TS_ASSERT(emptyStream.good());
	TS_ASSERT_EQUALS(0, emptyStream.tellg());
    }

};

#endif // __TESTFILEFORMAT_H
