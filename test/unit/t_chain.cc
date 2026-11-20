#include <system.hh>

#include "chain.h"

using namespace ledger;

// The budget-mode flags belong to the chain-construction API: chain.cc reads
// report_t::budget_flags to decide whether to insert budget_posts.  They must
// be defined once, in chain.h, so that this file (which includes neither
// report.h nor filters.h) compiles.  The values are fixed: report.cc tests
// `budget_flags & ~BUDGET_WRAP_VALUES` and budget_posts tests single bits.
static_assert(BUDGET_NO_BUDGET == 0x00, "BUDGET_NO_BUDGET must be 0");
static_assert(BUDGET_BUDGETED == 0x01, "BUDGET_BUDGETED must be bit 0");
static_assert(BUDGET_UNBUDGETED == 0x02, "BUDGET_UNBUDGETED must be bit 1");
static_assert(BUDGET_WRAP_VALUES == 0x04, "BUDGET_WRAP_VALUES must be bit 2");
