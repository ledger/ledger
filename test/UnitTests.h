#ifndef _UNITTESTS_H
#define _UNITTESTS_H

#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/Exception.h>
#include <cppunit/Portability.h>

#define assertDoublesEqual(x,y,z,w) CPPUNIT_ASSERT_DOUBLES_EQUAL_MESSAGE(x,y,z,w)
#define assertEqual(x,y)            CPPUNIT_ASSERT_EQUAL(x,y)
#define assertNotEqual(x,y)         CPPUNIT_ASSERT((x) != (y))
#define assertTrue(x)               CPPUNIT_ASSERT(x)
#define assertFalse(x)              CPPUNIT_ASSERT(! (x))
#define assertValid(x)              CPPUNIT_ASSERT((x).valid())
#define assertEqualMessage(x,y,z)   CPPUNIT_ASSERT_EQUAL_MESSAGE(x,y,z)
#define assertMessage(x,y)          CPPUNIT_ASSERT_MESSAGE(x,y)
#define assertThrow(x,y)            CPPUNIT_ASSERT_THROW(x,y)

#define internalAmount(x)           amount_t::exact(x)

#endif /* _UNITTESTS_H */
