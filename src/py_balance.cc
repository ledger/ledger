/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

using namespace boost::python;
using namespace ledger;

unsigned int balance_len(balance_t& bal)
{
  return bal.amounts.size();
}

amount_t balance_getitem(balance_t& bal, int i)
{
  std::size_t len = bal.amounts.size();

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, "Index out of range");
    throw_error_already_set();
  }

  int x = i < 0 ? len + i : i;
  balance_t::amounts_map::iterator elem = bal.amounts.begin();
  while (--x >= 0)
    elem++;

  return (*elem).second;
}

unsigned int balance_pair_len(balance_pair_t& bal_pair)
{
  return balance_len(bal_pair.quantity);
}

amount_t balance_pair_getitem(balance_pair_t& bal_pair, int i)
{
  return balance_getitem(bal_pair.quantity, i);
}

void export_balance()
{
  class_< balance_t > ("Balance")
    .def(init<balance_t>())
    .def(init<amount_t>())
    .def(init<long>())
    .def(init<unsigned long>())
    .def(init<double>())

    .def(self += self)
    .def(self += other<amount_t>())
    .def(self += long())
    .def(self +  self)
    .def(self +  other<amount_t>())
    .def(self +  long())
    .def(self -= self)
    .def(self -= other<amount_t>())
    .def(self -= long())
    .def(self -  self)
    .def(self -  other<amount_t>())
    .def(self -  long())
    .def(self *= self)
    .def(self *= other<amount_t>())
    .def(self *= long())
    .def(self *  self)
    .def(self *  other<amount_t>())
    .def(self *  long())
    .def(self /= self)
    .def(self /= other<amount_t>())
    .def(self /= long())
    .def(self /  self)
    .def(self /  other<amount_t>())
    .def(self /  long())
    .def(- self)

    .def(self <  self)
    .def(self <  other<amount_t>())
    .def(self <  long())
    .def(self <= self)
    .def(self <= other<amount_t>())
    .def(self <= long())
    .def(self >  self)
    .def(self >  other<amount_t>())
    .def(self >  long())
    .def(self >= self)
    .def(self >= other<amount_t>())
    .def(self >= long())
    .def(self == self)
    .def(self == other<amount_t>())
    .def(self == long())
    .def(self != self)
    .def(self != other<amount_t>())
    .def(self != long())
    .def(! self)

    .def(self_ns::str(self))

    .def("__abs__", &balance_t::abs)
    .def("__len__", balance_len)
    .def("__getitem__", balance_getitem)

    .def("valid",  &balance_t::valid)

    .def("realzero", &balance_t::realzero)
    .def("amount", &balance_t::amount)
    .def("value",  &balance_t::value)
    .def("price",  &balance_t::price)
    .def("date",  &balance_t::date)
    .def("strip_annotations", &balance_t::strip_annotations)
    .def("write",  &balance_t::write)
    .def("round",  &balance_t::round)
    .def("negate", &balance_t::negate)
    .def("negated", &balance_t::negated)
    ;

  class_< balance_pair_t > ("BalancePair")
    .def(init<balance_pair_t>())
    .def(init<balance_t>())
    .def(init<amount_t>())
    .def(init<long>())
    .def(init<unsigned long>())
    .def(init<double>())

    .def(self += self)
    .def(self += other<balance_t>())
    .def(self += other<amount_t>())
    .def(self += long())
    .def(self +  self)
    .def(self +  other<balance_t>())
    .def(self +  other<amount_t>())
    .def(self +  long())
    .def(self -= self)
    .def(self -= other<balance_t>())
    .def(self -= other<amount_t>())
    .def(self -= long())
    .def(self -  self)
    .def(self -  other<balance_t>())
    .def(self -  other<amount_t>())
    .def(self -  long())
    .def(self *= self)
    .def(self *= other<balance_t>())
    .def(self *= other<amount_t>())
    .def(self *= long())
    .def(self *  self)
    .def(self *  other<balance_t>())
    .def(self *  other<amount_t>())
    .def(self *  long())
    .def(self /= self)
    .def(self /= other<balance_t>())
    .def(self /= other<amount_t>())
    .def(self /= long())
    .def(self /  self)
    .def(self /  other<balance_t>())
    .def(self /  other<amount_t>())
    .def(self /  long())
    .def(- self)

    .def(self <  self)
    .def(self <  other<balance_t>())
    .def(self <  other<amount_t>())
    .def(self <  long())
    .def(self <= self)
    .def(self <= other<balance_t>())
    .def(self <= other<amount_t>())
    .def(self <= long())
    .def(self >  self)
    .def(self >  other<balance_t>())
    .def(self >  other<amount_t>())
    .def(self >  long())
    .def(self >= self)
    .def(self >= other<balance_t>())
    .def(self >= other<amount_t>())
    .def(self >= long())
    .def(self == self)
    .def(self == other<balance_t>())
    .def(self == other<amount_t>())
    .def(self == long())
    .def(self != self)
    .def(self != other<balance_t>())
    .def(self != other<amount_t>())
    .def(self != long())
    .def(! self)

    .def(self_ns::str(self))

    .def("__abs__", &balance_pair_t::abs)
    .def("__len__", balance_pair_len)
    .def("__getitem__", balance_pair_getitem)

    .def("valid",  &balance_pair_t::valid)

    .def("realzero", &balance_pair_t::realzero)
    .def("amount", &balance_pair_t::amount)
    .def("value",  &balance_pair_t::value)
    .def("price",  &balance_pair_t::price)
    .def("date",  &balance_pair_t::date)
    .def("strip_annotations", &balance_pair_t::strip_annotations)
    .def("write",  &balance_pair_t::write)
    .def("round",  &balance_pair_t::round)
    .def("negate", &balance_pair_t::negate)
    .def("negated", &balance_pair_t::negated)

    .add_property("cost",
		  make_getter(&balance_pair_t::cost,
			      return_value_policy<reference_existing_object>()))
    ;
}
