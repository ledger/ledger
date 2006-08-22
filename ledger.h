#ifndef _LEDGER_H
#define _LEDGER_H

//////////////////////////////////////////////////////////////////////
//
// Ledger Accounting Tool
//
//   A command-line tool for general double-entry accounting.
//
// Copyright (c) 2003,2004 John Wiegley <johnw@newartisans.com>
//

#include <amount.h>
#include <balance.h>
#include <value.h>

#include <journal.h>

#include <datetime.h>
#include <format.h>
#include <emacs.h>
#include <csv.h>
#include <quotes.h>
#include <valexpr.h>
#include <walk.h>
#include <derive.h>
#include <reconcile.h>
#include <error.h>
#include <option.h>

#include <parser.h>
#include <textual.h>
#include <binary.h>
#include <xml.h>
#include <gnucash.h>
#include <qif.h>
#include <ofx.h>

namespace ledger {
  extern parser_t * binary_parser_ptr;
  extern parser_t * xml_parser_ptr;
  extern parser_t * gnucash_parser_ptr;
  extern parser_t * ofx_parser_ptr;
  extern parser_t * qif_parser_ptr;
  extern parser_t * textual_parser_ptr;
}

#include <config.h>
#include <report.h>

#endif // _LEDGER_H
