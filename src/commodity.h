/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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

/**
 * @addtogroup math
 */

/**
 * @file   commodity.h
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief  Types for handling commodities
 *
 * This file contains one of the most basic types in Ledger:
 * commodity_t, and its annotated cousin, annotated_commodity_t.
 */
#ifndef _COMMODITY_H
#define _COMMODITY_H

#include "amount.h"

namespace ledger {

class keep_details_t;

DECLARE_EXCEPTION(commodity_error, std::runtime_error);

/**
 * @brief Brief
 *
 * Long.
 */
struct price_point_t
{
  datetime_t when;
  amount_t   price;
};

/**
 * @brief Brief
 *
 * Long.
 */
class commodity_t
  : public delegates_flags<uint_least16_t>,
    public equality_comparable1<commodity_t, noncopyable>
{
  friend class commodity_pool_t;

public:
  class base_t : public noncopyable, public supports_flags<uint_least16_t>
  {
    base_t();

  public:
    typedef std::map<const datetime_t, amount_t> history_map;

    struct history_t
    {
      history_map prices;
      ptime	  last_lookup;

      void add_price(commodity_t&	source,
		     const datetime_t&	date,
		     const amount_t&	price,
		     const bool		reflexive = true);
      bool remove_price(const datetime_t& date);

      optional<price_point_t>
      find_price(const optional<datetime_t>&   moment = none,
		 const optional<datetime_t>&   oldest = none
#if defined(DEBUG_ON)
		 , const int indent = 0
#endif
		 ) const;
    };

    typedef std::map<commodity_t *, history_t> history_by_commodity_map;

    struct varied_history_t
    {
      history_by_commodity_map histories;

      void add_price(commodity_t&	source,
		     const datetime_t&	date,
		     const amount_t&	price,
		     const bool		reflexive = true);
      bool remove_price(const datetime_t& date, commodity_t& commodity);

      optional<price_point_t>
      find_price(const commodity_t&	       source,
		 const optional<commodity_t&>& commodity = none,
		 const optional<datetime_t>&   moment    = none,
		 const optional<datetime_t>&   oldest    = none
#if defined(DEBUG_ON)
		 , const int indent = 0
#endif
		 ) const;
      optional<price_point_t>
      find_price(const commodity_t&		   source,
		 const std::vector<commodity_t *>& commodities,
		 const optional<datetime_t>&	   moment = none,
		 const optional<datetime_t>&       oldest = none
#if defined(DEBUG_ON)
		 , const int indent = 0
#endif
		 ) const;

      optional<history_t&>
      history(const optional<commodity_t&>& commodity = none);
      optional<history_t&>
      history(const std::vector<commodity_t *>& commodities);
    };

#define COMMODITY_STYLE_DEFAULTS  0x000
#define COMMODITY_STYLE_SUFFIXED  0x001
#define COMMODITY_STYLE_SEPARATED 0x002
#define COMMODITY_STYLE_EUROPEAN  0x004
#define COMMODITY_STYLE_THOUSANDS 0x008
#define COMMODITY_NOMARKET        0x010
#define COMMODITY_BUILTIN         0x020
#define COMMODITY_WALKED          0x040
#define COMMODITY_KNOWN           0x080
#define COMMODITY_PRIMARY         0x100

    string		       symbol;
    amount_t::precision_t      precision;
    optional<string>	       name;
    optional<string>	       note;
    optional<varied_history_t> varied_history;
    optional<amount_t>	       smaller;
    optional<amount_t>	       larger;

    mutable bool               searched;

  public:
    explicit base_t(const string& _symbol)
      : supports_flags<uint_least16_t>(COMMODITY_STYLE_DEFAULTS),
	symbol(_symbol), precision(0), searched(false) {
      TRACE_CTOR(base_t, "const string&");
    }
    ~base_t() {
      TRACE_DTOR(base_t);
    }
  };

public:
  static bool symbol_needs_quotes(const string& symbol);

  typedef base_t::history_t		   history_t;
  typedef base_t::history_map		   history_map;
  typedef base_t::varied_history_t	   varied_history_t;
  typedef base_t::history_by_commodity_map history_by_commodity_map;

  shared_ptr<base_t> base;

  commodity_pool_t * parent_;
  optional<string>   qualified_symbol;
  optional<string>   mapping_key_;
  bool		     annotated;

public:
  explicit commodity_t(commodity_pool_t *	 _parent,
		       const shared_ptr<base_t>& _base)
    : delegates_flags<uint_least16_t>(*_base.get()), base(_base),
      parent_(_parent), annotated(false) {
    TRACE_CTOR(commodity_t, "");
  }
  virtual ~commodity_t() {
    TRACE_DTOR(commodity_t);
  }

  operator bool() const;

  bool is_annotated() const {
    return annotated;
  }

  virtual bool operator==(const commodity_t& comm) const {
    if (comm.annotated)
      return comm == *this;
    return base.get() == comm.base.get();
  }

  virtual commodity_t& referent() {
    return *this;
  }
  virtual const commodity_t& referent() const {
    return *this;
  }
  virtual commodity_t& strip_annotations(const keep_details_t&) {
    return *this;
  }
  virtual void write_annotations(std::ostream&) const {}

  commodity_pool_t& parent() const {
    return *parent_;
  }

  string base_symbol() const {
    return base->symbol;
  }
  string symbol() const {
    return qualified_symbol ? *qualified_symbol : base_symbol();
  }

  string mapping_key() const {
    if (mapping_key_)
      return *mapping_key_;
    else
      return base_symbol();
  }

  optional<string> name() const {
    return base->name;
  }
  void set_name(const optional<string>& arg = none) {
    base->name = arg;
  }

  optional<string> note() const {
    return base->note;
  }
  void set_note(const optional<string>& arg = none) {
    base->note = arg;
  }

  amount_t::precision_t precision() const {
    return base->precision;
  }
  void set_precision(amount_t::precision_t arg) {
    base->precision = arg;
  }

  optional<amount_t> smaller() const {
    return base->smaller;
  }
  void set_smaller(const optional<amount_t>& arg = none) {
    base->smaller = arg;
  }

  optional<amount_t> larger() const {
    return base->larger;
  }
  void set_larger(const optional<amount_t>& arg = none) {
    base->larger = arg;
  }

  optional<varied_history_t&> varied_history() {
    if (base->varied_history)
      return *base->varied_history;
    return none;
  }

  optional<history_t&> history(const optional<commodity_t&>& commodity);
  optional<history_t&> history(const std::vector<commodity_t *>& commodities);

public:
  // These methods provide a transparent pass-through to the underlying
  // base->varied_history object.

  void add_price(const datetime_t& date, const amount_t& price,
		 const bool reflexive = true) {
    if (! base->varied_history)
      base->varied_history = varied_history_t();

    base->varied_history->add_price(*this, date, price, reflexive);
  }
  bool remove_price(const datetime_t& date, commodity_t& commodity) {
    if (base->varied_history)
      base->varied_history->remove_price(date, commodity);
    return false;
  }

  optional<price_point_t>
  find_price(const optional<commodity_t&>& commodity = none,
	     const optional<datetime_t>&   moment    = none,
	     const optional<datetime_t>&   oldest    = none
#if defined(DEBUG_ON)
	     , const int indent = 0
#endif
	     ) const {
    if (base->varied_history && ! has_flags(COMMODITY_WALKED)) {
      const_cast<commodity_t&>(*this).add_flags(COMMODITY_WALKED);
      optional<price_point_t> point =
	base->varied_history->find_price(*this, commodity, moment, oldest
#if defined(DEBUG_ON)
					 , indent
#endif
					 );
      const_cast<commodity_t&>(*this).drop_flags(COMMODITY_WALKED);
      return point;
    }
    return none;
  }    

  optional<price_point_t>
  find_price(const std::vector<commodity_t *>& commodities,
	     const optional<datetime_t>&       moment = none,
	     const optional<datetime_t>&       oldest = none
#if defined(DEBUG_ON)
	     , const int indent = 0
#endif
	     ) const {
    if (base->varied_history)
      return base->varied_history->find_price(*this, commodities, moment, oldest
#if defined(DEBUG_ON)
					      , indent
#endif
					      );
    return none;
  }

  // Methods to exchange one commodity for another, while recording the
  // factored price.

  static void exchange(commodity_t&	 commodity,
		       const amount_t&   per_unit_cost,
		       const datetime_t& moment);

  struct cost_breakdown_t {
    amount_t amount;
    amount_t final_cost;
    amount_t basis_cost;
  };

  static cost_breakdown_t exchange(const amount_t&	       amount,
				   const amount_t&	       cost,
				   const bool		       is_per_unit = false,
				   const optional<datetime_t>& moment	   = none,
				   const optional<string>&     tag	   = none);

  // Methods related to parsing, reading, writing, etc., the commodity
  // itself.

  static void parse_symbol(std::istream& in, string& symbol);
  static void parse_symbol(char *& p, string& symbol);
  static string parse_symbol(std::istream& in) {
    string temp;
    parse_symbol(in, temp);
    return temp;
  }

  void print(std::ostream& out) const {
    out << symbol();
  }

  bool valid() const;
};

inline std::ostream& operator<<(std::ostream& out, const commodity_t& comm) {
  comm.print(out);
  return out;
}

/**
 * @brief Brief
 *
 * Long.
 */
struct annotation_t : public equality_comparable<annotation_t>
{
  optional<amount_t> price;
  optional<date_t>   date;
  optional<string>   tag;

  explicit annotation_t(const optional<amount_t>& _price = none,
			const optional<date_t>&   _date  = none,
			const optional<string>&   _tag   = none)
    : price(_price), date(_date), tag(_tag) {
    TRACE_CTOR(annotation_t, "const optional<amount_t>& + date_t + string");
  }
  annotation_t(const annotation_t& other)
    : price(other.price), date(other.date), tag(other.tag) {
    TRACE_CTOR(annotation_t, "copy");
  }

  ~annotation_t() {
    TRACE_DTOR(annotation_t);
  }

  operator bool() const {
    return price || date || tag;
  }

  bool operator==(const annotation_t& rhs) const {
    return (price == rhs.price &&
	    date  == rhs.date &&
	    tag   == rhs.tag);
  }

  void parse(std::istream& in);
  void print(std::ostream& out) const {
    out << "price " << (price ? price->to_string() : "NONE") << " "
	<< "date "  << (date  ? *date : date_t()) << " "
	<< "tag "   << (tag   ? *tag  : "NONE");
  }

  bool valid() const {
    assert(*this);
    return true;
  }
};

struct keep_details_t
{
  bool keep_price;
  bool keep_date;
  bool keep_tag;
  bool keep_base;

  explicit keep_details_t(bool _keep_price = false,
			  bool _keep_date  = false,
			  bool _keep_tag   = false,
			  bool _keep_base  = false)
    : keep_price(_keep_price),
      keep_date(_keep_date),
      keep_tag(_keep_tag),
      keep_base(_keep_base)
  {
    TRACE_CTOR(keep_details_t, "bool, bool, bool, bool");
  }
  keep_details_t(const keep_details_t& other)
    : keep_price(other.keep_price), keep_date(other.keep_date),
      keep_tag(other.keep_tag), keep_base(other.keep_base) {
    TRACE_CTOR(keep_details_t, "copy");
  }

  ~keep_details_t() throw() {
    TRACE_DTOR(keep_details_t);
  }

  bool keep_all() const {
    return keep_price && keep_date && keep_tag;
  }
  bool keep_all(const commodity_t& comm) const {
    return ! comm.annotated || (keep_price && keep_date && keep_tag);
  }

  bool keep_any() const {
    return keep_price || keep_date || keep_tag;
  }
  bool keep_any(const commodity_t& comm) const {
    return comm.annotated && (keep_price || keep_date || keep_tag);
  }
};

inline std::ostream& operator<<(std::ostream& out, const annotation_t& details) {
  details.print(out);
  return out;
}

/**
 * @brief Brief
 *
 * Long.
 */
class annotated_commodity_t
  : public commodity_t,
    public equality_comparable<annotated_commodity_t,
	   equality_comparable2<annotated_commodity_t, commodity_t,
				noncopyable> >
{
public:
  commodity_t * ptr;
  annotation_t  details;

  explicit annotated_commodity_t(commodity_t * _ptr,
				 const annotation_t& _details)
    : commodity_t(_ptr->parent_, _ptr->base), ptr(_ptr), details(_details) {
    TRACE_CTOR(annotated_commodity_t, "");
    annotated = true;
  }
  virtual ~annotated_commodity_t() {
    TRACE_DTOR(annotated_commodity_t);
  }

  virtual bool operator==(const commodity_t& comm) const;
  virtual bool operator==(const annotated_commodity_t& comm) const {
    return *this == static_cast<const commodity_t&>(comm);
  }

  virtual commodity_t& referent() {
    return *ptr;
  }
  virtual const commodity_t& referent() const {
    return *ptr;
  }

  virtual commodity_t& strip_annotations(const keep_details_t& what_to_keep);

  virtual void write_annotations(std::ostream& out) const {
    annotated_commodity_t::write_annotations(out, details);
  }

  static void write_annotations(std::ostream&	    out,
				const annotation_t& info);
};

inline annotated_commodity_t&
as_annotated_commodity(commodity_t& commodity) {
  return downcast<annotated_commodity_t>(commodity);
}
inline const annotated_commodity_t&
as_annotated_commodity(const commodity_t& commodity) {
  return downcast<const annotated_commodity_t>(commodity);
}


/**
 * @brief Brief
 *
 * Long.
 */
struct compare_amount_commodities {
  bool operator()(const amount_t * left, const amount_t * right) const;
};

/**
 * @brief Brief
 *
 * Long.
 */
class commodity_pool_t : public noncopyable
{
  /**
   * The commodities collection in commodity_pool_t maintains pointers to all
   * the commodities which have ever been created by the user, whether
   * explicitly by calling the create methods of commodity_pool_t, or
   * implicitly by parsing a commoditized amount.
   */
  typedef std::map<string, commodity_t *> commodities_map;

public:
  commodities_map commodities;

  commodity_t *	null_commodity;
  commodity_t *	default_commodity;

public:
  boost::function<optional<amount_t>
		  (commodity_t&		       commodity,
		   const optional<datetime_t>& date,
		   const optional<datetime_t>& moment,
		   const optional<datetime_t>& last)> get_quote;

  explicit commodity_pool_t();

  ~commodity_pool_t() {
    TRACE_DTOR(commodity_pool_t);
    foreach (commodities_map::value_type pair, commodities)
      checked_delete(pair.second);
  }

  commodity_t * create(const string& symbol);
  commodity_t * find(const string& name);
  commodity_t * find_or_create(const string& symbol);

  commodity_t * create(const string& symbol, const annotation_t& details);
  commodity_t * find(const string& symbol, const annotation_t& details);
  commodity_t * find_or_create(const string& symbol,
			       const annotation_t& details);

  commodity_t * create(commodity_t&	   comm,
		       const annotation_t& details,
		       const string&	   mapping_key);

  commodity_t * find_or_create(commodity_t&	   comm,
			       const annotation_t& details);

  void parse_commodity_price(char * optarg);
};

} // namespace ledger

#endif // _COMMODITY_H
