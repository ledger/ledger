/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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

namespace ledger {

struct keep_details_t;
class commodity_pool_t;

DECLARE_EXCEPTION(commodity_error, std::runtime_error);

struct price_point_t
{
  datetime_t when;
  amount_t   price;

  price_point_t() {}
  price_point_t(datetime_t _when, amount_t _price)
    : when(_when), price(_price) {}

  bool operator==(const price_point_t& other) const {
    return when == other.when && price == other.price;
  }

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & when;
    ar & price;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

class commodity_t
  : public delegates_flags<uint_least16_t>,
    public equality_comparable1<commodity_t, noncopyable>
{
public:
  typedef std::map<const datetime_t, amount_t> history_map;

  struct history_t
  {
    history_map prices;

    void add_price(commodity_t&      source,
                   const datetime_t& date,
                   const amount_t&   price,
                   const bool        reflexive = true);
    bool remove_price(const datetime_t& date);

    optional<price_point_t>
    find_price(const optional<datetime_t>&   moment = none,
               const optional<datetime_t>&   oldest = none
#if defined(DEBUG_ON)
               , const int indent = 0
#endif
               ) const;

#if defined(HAVE_BOOST_SERIALIZATION)
  private:
    /** Serialization. */

    friend class boost::serialization::access;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int /* version */) {
      ar & prices;
    }
#endif // HAVE_BOOST_SERIALIZATION
  };

  typedef std::map<commodity_t *, history_t> history_by_commodity_map;

  struct varied_history_t
  {
    history_by_commodity_map histories;

    void add_price(commodity_t&      source,
                   const datetime_t& date,
                   const amount_t&   price,
                   const bool        reflexive = true);
    bool remove_price(const datetime_t& date, commodity_t& commodity);

    optional<price_point_t>
    find_price(const commodity_t&            source,
               const optional<commodity_t&>& commodity = none,
               const optional<datetime_t>&   moment    = none,
               const optional<datetime_t>&   oldest    = none
#if defined(DEBUG_ON)
               , const int indent = 0
#endif
               ) const;

    optional<history_t&>
    history(const optional<commodity_t&>& commodity = none);

#if defined(HAVE_BOOST_SERIALIZATION)
  private:
    /** Serialization. */

    friend class boost::serialization::access;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int /* version */) {
      ar & histories;
    }
#endif // HAVE_BOOST_SERIALIZATION
  };

protected:
  friend class commodity_pool_t;
  friend class annotated_commodity_t;

  class base_t : public noncopyable, public supports_flags<uint_least16_t>
  {
  public:
#define COMMODITY_STYLE_DEFAULTS         0x000
#define COMMODITY_STYLE_SUFFIXED         0x001
#define COMMODITY_STYLE_SEPARATED        0x002
#define COMMODITY_STYLE_DECIMAL_COMMA    0x004
#define COMMODITY_STYLE_THOUSANDS        0x008
#define COMMODITY_NOMARKET               0x010
#define COMMODITY_BUILTIN                0x020
#define COMMODITY_WALKED                 0x040
#define COMMODITY_KNOWN                  0x080
#define COMMODITY_PRIMARY                0x100
#define COMMODITY_SAW_ANNOTATED          0x200
#define COMMODITY_SAW_ANN_PRICE_FLOAT    0x400
#define COMMODITY_SAW_ANN_PRICE_FIXATED  0x800

    string                     symbol;
    amount_t::precision_t      precision;
    optional<string>           name;
    optional<string>           note;
    optional<varied_history_t> varied_history;
    optional<amount_t>         smaller;
    optional<amount_t>         larger;

    typedef std::pair<optional<datetime_t>,
                      optional<datetime_t> > optional_time_pair_t;
    typedef std::pair<optional_time_pair_t,
                      commodity_t *> time_and_commodity_t;
    typedef std::map<time_and_commodity_t,
                     optional<price_point_t> > memoized_price_map;

    static const std::size_t   max_price_map_size = 16;
    mutable memoized_price_map price_map;

    mutable bool               searched;

  public:
    explicit base_t(const string& _symbol)
      : supports_flags<uint_least16_t>
        (commodity_t::decimal_comma_by_default ?
         static_cast<uint_least16_t>(COMMODITY_STYLE_DECIMAL_COMMA) :
         static_cast<uint_least16_t>(COMMODITY_STYLE_DEFAULTS)),
        symbol(_symbol), precision(0), searched(false) {
      TRACE_CTOR(base_t, "const string&");
    }
    virtual ~base_t() {
      TRACE_DTOR(base_t);
    }

#if defined(HAVE_BOOST_SERIALIZATION)
  private:
    base_t() {
      TRACE_CTOR(base_t, "");
    }

    /** Serialization. */

    friend class boost::serialization::access;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int /* version */) {
      ar & boost::serialization::base_object<supports_flags<uint_least16_t> >(*this);
      ar & symbol;
      ar & precision;
      ar & name;
      ar & note;
      ar & varied_history;
      ar & smaller;
      ar & larger;
    }
#endif // HAVE_BOOST_SERIALIZATION
  };

  shared_ptr<base_t> base;

  commodity_pool_t * parent_;
  optional<string>   qualified_symbol;
  optional<string>   mapping_key_;
  bool               annotated;

  explicit commodity_t(commodity_pool_t *        _parent,
                       const shared_ptr<base_t>& _base)
    : delegates_flags<uint_least16_t>(*_base.get()), base(_base),
      parent_(_parent), annotated(false) {
    TRACE_CTOR(commodity_t, "commodity_pool_t *, shared_ptr<base_t>");
  }

public:
  static bool decimal_comma_by_default;

  virtual ~commodity_t() {
    TRACE_DTOR(commodity_t);
  }

  operator bool() const;

  virtual bool operator==(const commodity_t& comm) const {
    if (comm.annotated)
      return comm == *this;
    return base.get() == comm.base.get();
  }

  static bool symbol_needs_quotes(const string& symbol);

  virtual commodity_t& referent() {
    return *this;
  }
  virtual const commodity_t& referent() const {
    return *this;
  }

  bool has_annotation() const {
    return annotated;
  }

  virtual commodity_t& strip_annotations(const keep_details_t&) {
    return *this;
  }
  virtual void write_annotations(std::ostream&, bool) const {}

  commodity_pool_t& pool() const {
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
  optional<const varied_history_t&> varied_history() const {
    if (base->varied_history)
      return *base->varied_history;
    return none;
  }

  optional<history_t&> history(const optional<commodity_t&>& commodity);

  // These methods provide a transparent pass-through to the underlying
  // base->varied_history object.

  void add_price(const datetime_t& date, const amount_t& price,
                 const bool reflexive = true) {
    if (! base->varied_history)
      base->varied_history = varied_history_t();
    base->varied_history->add_price(*this, date, price, reflexive);
    DEBUG("commodity.prices.find", "Price added, clearing price_map");
    base->price_map.clear();    // a price was added, invalid the map
  }
  bool remove_price(const datetime_t& date, commodity_t& commodity) {
    if (base->varied_history) {
      base->varied_history->remove_price(date, commodity);
      DEBUG("commodity.prices.find", "Price removed, clearing price_map");
      base->price_map.clear();  // a price was added, invalid the map
    }
    return false;
  }

  optional<price_point_t>
  find_price(const optional<commodity_t&>& commodity = none,
             const optional<datetime_t>&   moment    = none,
             const optional<datetime_t>&   oldest    = none,
             const bool                    nested    = false
#if defined(DEBUG_ON)
             , const int indent = 0
#endif
             ) const;    

  optional<price_point_t>
  check_for_updated_price(const optional<price_point_t>& point,
                          const optional<datetime_t>&    moment,
                          const optional<commodity_t&>&  in_terms_of);

  // Methods related to parsing, reading, writing, etc., the commodity
  // itself.

  static void parse_symbol(std::istream& in, string& symbol);
  static void parse_symbol(char *& p, string& symbol);
  static string parse_symbol(std::istream& in) {
    string temp;
    parse_symbol(in, temp);
    return temp;
  }

  void print(std::ostream& out, bool elide_quotes = false) const;
  bool valid() const;

  struct compare_by_commodity {
    bool operator()(const amount_t * left, const amount_t * right) const;
  };

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  supports_flags<uint_least16_t> temp_flags;

protected:
  explicit commodity_t()
    : delegates_flags<uint_least16_t>(temp_flags), parent_(NULL),
      annotated(false) {
    TRACE_CTOR(commodity_t, "");
  }

private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & boost::serialization::base_object<delegates_flags<uint_least16_t> >(*this);
    ar & base;
    ar & parent_;
    ar & qualified_symbol;
    ar & mapping_key_;
    ar & annotated;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

inline std::ostream& operator<<(std::ostream& out, const commodity_t& comm) {
  comm.print(out);
  return out;
}

void to_xml(std::ostream& out, const commodity_t& comm,
            bool commodity_details = false);

} // namespace ledger

#endif // _COMMODITY_H
