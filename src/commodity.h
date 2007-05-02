/**
 * @file   commodity.h
 * @author John Wiegley
 * @date   Wed Apr 18 22:05:53 2007
 * 
 * @brief  Types for handling commodities.
 * 
 * This file contains one of the most basic types in Ledger:
 * commodity_t, and its derived cousin, annotated_commodity_t.
 */

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

#ifndef _COMMODITY_H
#define _COMMODITY_H

namespace ledger {

#define COMMODITY_STYLE_DEFAULTS   0x0000
#define COMMODITY_STYLE_SUFFIXED   0x0001
#define COMMODITY_STYLE_SEPARATED  0x0002
#define COMMODITY_STYLE_EUROPEAN   0x0004
#define COMMODITY_STYLE_THOUSANDS  0x0008
#define COMMODITY_STYLE_NOMARKET   0x0010
#define COMMODITY_STYLE_BUILTIN    0x0020

typedef std::map<const moment_t, amount_t>  history_map;
typedef std::pair<const moment_t, amount_t> history_pair;

class commodity_base_t;

typedef std::map<const string, commodity_base_t *>  base_commodities_map;
typedef std::pair<const string, commodity_base_t *> base_commodities_pair;

class commodity_base_t
{
 public:
  friend class commodity_t;
  friend class annotated_commodity_t;

  typedef unsigned long ident_t;

  ident_t	ident;
  string	name;
  string	note;
  unsigned char precision;
  unsigned char flags;
  amount_t *	smaller;
  amount_t *	larger;

  commodity_base_t()
    : precision(0), flags(COMMODITY_STYLE_DEFAULTS),
      smaller(NULL), larger(NULL), history(NULL) {
    TRACE_CTOR(commodity_base_t, "");
  }

  commodity_base_t(const commodity_base_t&) {
    TRACE_CTOR(commodity_base_t, "copy");
    assert(0);
  }

  commodity_base_t(const string& _symbol,
		   unsigned int	_precision = 0,
		   unsigned int _flags	   = COMMODITY_STYLE_DEFAULTS)
    : precision(_precision), flags(_flags),
      smaller(NULL), larger(NULL), symbol(_symbol), history(NULL) {
    TRACE_CTOR(commodity_base_t, "const string&, unsigned int, unsigned int");
  }

  ~commodity_base_t() {
    TRACE_DTOR(commodity_base_t);
    if (history) checked_delete(history);
    if (smaller) checked_delete(smaller);
    if (larger)  checked_delete(larger);
  }

  static base_commodities_map commodities;
  static commodity_base_t * create(const string& symbol);

  string symbol;

  struct history_t {
    history_map	prices;
    ptime	last_lookup;
    history_t() : last_lookup() {}
  };
  history_t * history;

  void	   add_price(const moment_t& date, const amount_t& price);
  bool	   remove_price(const moment_t& date);
  amount_t value(const moment_t& moment = now);

  class updater_t {
   public:
    virtual ~updater_t() {}
    virtual void operator()(commodity_base_t& commodity,
			    const moment_t&   moment,
			    const moment_t&   date,
			    const moment_t&   last,
			    amount_t&         price) = 0;
  };
  friend class updater_t;

  static updater_t * updater;
};

typedef std::map<const string, commodity_t *>  commodities_map;
typedef std::pair<const string, commodity_t *> commodities_pair;

typedef std::vector<commodity_t *> commodities_array;

class commodity_t
{
  friend class annotated_commodity_t;

 public:
  // This map remembers all commodities that have been defined.

  static commodities_map     commodities;
  static commodities_array * commodities_by_ident;
  static bool		     commodities_sorted;
  static commodity_t *	     null_commodity;
  static commodity_t *	     default_commodity;

  static commodity_t * create(const string& symbol);
  static commodity_t * find(const string& name);
  static commodity_t * find_or_create(const string& symbol);

  static bool needs_quotes(const string& symbol);

  static void make_alias(const string& symbol,
			 commodity_t * commodity);

  // These are specific to each commodity reference

  typedef unsigned long ident_t;

  ident_t	     ident;
  commodity_base_t * base;
  string	     qualified_symbol;
  bool		     annotated;

 public:
  explicit commodity_t() : base(NULL), annotated(false) {
    TRACE_CTOR(commodity_t, "");
  }
  commodity_t(const commodity_t& o)
    : ident(o.ident), base(o.base),
      qualified_symbol(o.qualified_symbol), annotated(o.annotated) {
    TRACE_CTOR(commodity_t, "copy");
  }
  virtual ~commodity_t() {
    TRACE_DTOR(commodity_t);
  }

  operator bool() const {
    return this != null_commodity;
  }
  virtual bool operator==(const commodity_t& comm) const {
    if (comm.annotated)
      return comm == *this;
    return base == comm.base;
  }

  string base_symbol() const {
    return base->symbol;
  }
  string symbol() const {
    return qualified_symbol;
  }

  void write(std::ostream& out) const {
    out << symbol();
  }

  string name() const {
    return base->name;
  }
  void set_name(const string& arg) {
    base->name = arg;
  }

  string note() const {
    return base->note;
  }
  void set_note(const string& arg) {
    base->note = arg;
  }

  unsigned char precision() const {
    return base->precision;
  }
  void set_precision(unsigned char arg) {
    base->precision = arg;
  }

  unsigned char flags() const {
    return base->flags;
  }
  void set_flags(unsigned char arg) {
    base->flags = arg;
  }
  void add_flags(unsigned char arg) {
    base->flags |= arg;
  }
  void drop_flags(unsigned char arg) {
    base->flags &= ~arg;
  }

  amount_t * smaller() const {
    return base->smaller;
  }
  void set_smaller(const amount_t& arg) {
    if (base->smaller)
      checked_delete(base->smaller);
    base->smaller = new amount_t(arg);
  }

  amount_t * larger() const {
    return base->larger;
  }
  void set_larger(const amount_t& arg) {
    if (base->larger)
      checked_delete(base->larger);
    base->larger = new amount_t(arg);
  }

  commodity_base_t::history_t * history() const {
    return base->history;
  }

  void add_price(const moment_t& date, const amount_t& price) {
    return base->add_price(date, price);
  }
  bool remove_price(const moment_t& date) {
    return base->remove_price(date);
  }
  amount_t value(const moment_t& moment = now) const {
    return base->value(moment);
  }

  bool valid() const;
};

class annotated_commodity_t : public commodity_t
{
 public:
  const commodity_t * ptr;

  optional<amount_t> price;
  optional<moment_t> date;
  optional<string>   tag;

  explicit annotated_commodity_t() {
    TRACE_CTOR(annotated_commodity_t, "");
    annotated = true;
  }
  virtual ~annotated_commodity_t() {
    TRACE_DTOR(annotated_commodity_t);
  }

  virtual bool operator==(const commodity_t& comm) const;

  void write_annotations(std::ostream& out) const {
    annotated_commodity_t::write_annotations(out, price, date, tag);
  }

  static void write_annotations(std::ostream&		  out,
				const optional<amount_t>& price,
				const optional<moment_t>& date,
				const optional<string>&	  tag);

 private:
  static commodity_t * create(const commodity_t&	comm,
			      const optional<amount_t>& price,
			      const optional<moment_t>&	date,
			      const optional<string>&	tag,
			      const string&		mapping_key);

  static commodity_t * find_or_create(const commodity_t&	comm,
				      const optional<amount_t>& price,
				      const optional<moment_t>&	date,
				      const optional<string>&	tag);

  friend class amount_t;
};

inline std::ostream& operator<<(std::ostream& out, const commodity_t& comm) {
  out << comm.symbol();
  return out;
}

struct compare_amount_commodities {
  bool operator()(const amount_t * left, const amount_t * right) const;
};

} // namespace ledger

#endif // _COMMODITY_H
