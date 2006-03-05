#include "ledger.h"

using namespace ledger;

namespace ledger {
  parser_t * binary_parser_ptr  = NULL;
  parser_t * xml_parser_ptr     = NULL;
  parser_t * gnucash_parser_ptr = NULL;
  parser_t * ofx_parser_ptr     = NULL;
  parser_t * qif_parser_ptr     = NULL;
  parser_t * textual_parser_ptr = NULL;
}

namespace {
  binary_parser_t  binary_parser;
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
  xml_parser_t	   xml_parser;
  gnucash_parser_t gnucash_parser;
#endif
#ifdef HAVE_LIBOFX
  ofx_parser_t	   ofx_parser;
#endif
  qif_parser_t	   qif_parser;
  textual_parser_t textual_parser;

  static class startup {
  public:
    startup();
    ~startup();
  } _startup;

  startup::startup()
  {
    std::ios::sync_with_stdio(false);

    initialize_parser_support();

    register_parser(&binary_parser);   binary_parser_ptr  = &binary_parser;
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
    register_parser(&xml_parser);      xml_parser_ptr	  = &xml_parser;
    register_parser(&gnucash_parser);  gnucash_parser_ptr = &gnucash_parser;
#endif
#ifdef HAVE_LIBOFX
    register_parser(&ofx_parser);      ofx_parser_ptr	  = &ofx_parser;
#endif
    register_parser(&qif_parser);      qif_parser_ptr	  = &qif_parser;
    register_parser(&textual_parser);  textual_parser_ptr = &textual_parser;
  }

  startup::~startup()
  {
    if (! ledger::do_cleanup)
      return;
    shutdown_parser_support();
  }
}
