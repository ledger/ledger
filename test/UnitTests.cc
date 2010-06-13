#include <system.hh>

#include <cppunit/CompilerOutputter.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunit/TextTestProgressListener.h>
#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/XmlOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>

#include "UnitTests.h"

#include "utils.h"

// Create the CppUnit registry

CPPUNIT_REGISTRY_ADD_TO_DEFAULT("Framework");

// Create a sample test, which acts both as a template, and a
// verification that the basic framework is functioning.

class UnitTests : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE( UnitTests );
  CPPUNIT_TEST( testInitialization );
  CPPUNIT_TEST_SUITE_END();

public:
  UnitTests() {}
  virtual ~UnitTests() {}

  virtual void setUp() {}
  virtual void tearDown() {}

  void testInitialization() {
    assertEqual(std::string("Hello, world!"),
                std::string("Hello, world!"));
  }

private:
  UnitTests( const UnitTests &copy );
  void operator =( const UnitTests &copy );
};

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(UnitTests, "framework");

// Create the various runners and commence running the tests!

int main(int argc, char* argv[])
{
  int index = 1;

  if (argc > index && std::string(argv[index]) == "--verify") {
#if defined(VERIFY_ON)
    ledger::verify_enabled = true;
#endif
    index++;
  }

  // Retreive test path from command line first argument. Default to
  // "" which resolves to the top level suite.
  std::string testPath = ((argc > index) ? std::string(argv[index]) :
                          std::string(""));

  // Create the event manager and test controller
  CPPUNIT_NS::TestResult controller;

  // Add a listener that collects test results
  CPPUNIT_NS::TestResultCollector result;
  controller.addListener(&result);

  // Add a listener that print dots as test run.
#if 1
  CPPUNIT_NS::TextTestProgressListener progress;
#else
  CPPUNIT_NS::BriefTestProgressListener progress;
#endif
  controller.addListener(&progress);

  // Add the top suite to the test runner
  CPPUNIT_NS::TestRunner runner;
  runner.addTest(CPPUNIT_NS::TestFactoryRegistry::getRegistry().makeTest());
  try {
#if defined(VERIFY_ON)
    IF_VERIFY()
      ledger::initialize_memory_tracing();
#endif

    runner.run(controller, testPath);

#if defined(VERIFY_ON)
    IF_VERIFY()
      ledger::shutdown_memory_tracing();
#endif

#if 1
    // Print test in a compiler compatible format.
    CPPUNIT_NS::CompilerOutputter outputter(&result, CPPUNIT_NS::stdCOut());
    outputter.write();
#else
    // Uncomment this for XML output
    std::ofstream file("tests.xml");
    CPPUNIT_NS::XmlOutputter xml(&result, file);
    xml.setStyleSheet("report.xsl");
    xml.write();
    file.close();
#endif
  }
  catch (std::invalid_argument &e) { // Test path not resolved
    CPPUNIT_NS::stdCOut() << "\nERROR: " << e.what() << "\n";
    return 0;
  }

  return result.wasSuccessful() ? 0 : 1;
}
