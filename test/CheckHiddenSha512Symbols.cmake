# Usage: cmake -DNM=<nm> -DLIBRARY=<file> -P CheckHiddenSha512Symbols.cmake
#
# Fail when LIBRARY has a global symbol named SHA512, SHA512_Init,
# SHA512_Update or SHA512_Final.  Those are OpenSSL's names, with OpenSSL's
# signatures; ledger's own SHA-512 (src/sha512.cc) has different ones and must
# keep them internal, so that a process that loads libledger or the Python
# module beside libcrypto never binds a call meant for one to the other.

if(NOT NM OR NOT LIBRARY)
  message(FATAL_ERROR "usage: cmake -DNM=<nm> -DLIBRARY=<file> -P ${CMAKE_CURRENT_LIST_FILE}")
endif()

execute_process(COMMAND "${NM}" -g "${LIBRARY}"
  OUTPUT_VARIABLE symbols
  RESULT_VARIABLE status)
if(NOT status EQUAL 0)
  message(FATAL_ERROR "${NM} -g ${LIBRARY} exited with status ${status}")
endif()

string(REGEX MATCHALL "[ \t]_?SHA512(_Init|_Update|_Final)?\n" exported "${symbols}\n")
if(exported)
  message(FATAL_ERROR "${LIBRARY} exports OpenSSL-named symbols:\n${exported}")
endif()
