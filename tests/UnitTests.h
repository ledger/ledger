#ifndef _UNITTESTS_H
#define _UNITTESTS_H

#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/Exception.h>
#include <cppunit/Portability.h>

#define assertDoublesEqual(x,y,z,w) CPPUNIT_ASSERT_DOUBLES_EQUAL_MESSAGE(x,y,z,w)
#define assertEquals(x,y)	    CPPUNIT_ASSERT_EQUAL(x,y)
#define assertEqualsMessage(x,y,z)  CPPUNIT_ASSERT_EQUAL_MESSAGE(x,y,z)
#define assertMessage(x,y)	    CPPUNIT_ASSERT_MESSAGE(x,y)
#define assertThrow(x,y)	    CPPUNIT_ASSERT_THROW(x,y)

#endif /* _UNITTESTS_H */
