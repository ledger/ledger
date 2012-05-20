if(HAVE_BOOST_PYTHON)
  set(TEST_PYTHON_FLAGS "--python")
endif()

if(PYTHONINTERP_FOUND)
  file(GLOB REGRESSION_TESTS *.test)
  foreach(TestFile ${REGRESSION_TESTS})
    get_filename_component(TestFile_Name ${TestFile} NAME_WE)
    string(FIND ${TestFile_Name} "_py" TestFile_IsPythonTest)
    if((NOT TestFile_IsPythonTest) OR HAVE_BOOST_PYTHON)
      add_test(RegressionTest_${TestFile_Name}
        ${PYTHON_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/RegressTests.py 
        ${LEDGER_LOCATION} ${PROJECT_SOURCE_DIR} 
        ${TestFile} ${TEST_PYTHON_FLAGS})
      set_target_properties(check 
        PROPERTIES DEPENDS RegressionTest_${TestFile_Name})
    endif()
  endforeach()
endif()
