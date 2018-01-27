/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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
 * @file   annotate.h
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief  Types for annotating commodities
 *
 * Long.
 */
#ifndef _ANNOTATE_H
#define _ANNOTATE_H

#include "expr.h"

namespace ledger {

struct annotation_t : public supports_flags<>,
                      public equality_comparable<annotation_t>
{
#define ANNOTATION_PRICE_CALCULATED      0x01
#define ANNOTATION_PRICE_FIXATED         0x02
#define ANNOTATION_PRICE_NOT_PER_UNIT    0x04
#define ANNOTATION_DATE_CALCULATED       0x08
#define ANNOTATION_TAG_CALCULATED        0x10
#define ANNOTATION_VALUE_EXPR_CALCULATED 0x20

  optional<amount_t> price;
  optional<date_t>   date;
  optional<string>   tag;
  optional<expr_t>   value_expr;

  explicit annotation_t(const optional<amount_t>& _price      = none,
                        const optional<date_t>&   _date       = none,
                        const optional<string>&   _tag        = none,
                        const optional<expr_t>&   _value_expr = none)
    : supports_flags<>(), price(_price), date(_date), tag(_tag),
      value_expr(_value_expr) {
    TRACE_CTOR(annotation_t,
               "optional<amount_t> + date_t + string + expr_t");
  }
  annotation_t(const annotation_t& other)
    : supports_flags<>(other.flags()),
      price(other.price), date(other.date), tag(other.tag),
      value_expr(other.value_expr)
  {
    TRACE_CTOR(annotation_t, "copy");
  }
  ~annotation_t() {
    TRACE_DTOR(annotation_t);
  }

  operator bool() const {
    return price || date || tag || value_expr;
  }

  bool operator<(const annotation_t& rhs) const;
  bool operator==(const annotation_t& rhs) const {
    return (price == rhs.price &&
            date  == rhs.date  &&
            tag   == rhs.tag   &&
            (value_expr && rhs.value_expr ?
             value_expr->text() == rhs.value_expr->text() :
             value_expr == rhs.value_expr));
  }

  void parse(std::istream& in);
  void print(std::ostream& out, bool keep_base = false,
             bool no_computed_annotations = false) const;

  bool valid() const {
    assert(*this);
    return true;
  }
};

void put_annotation(property_tree::ptree& pt, const annotation_t& details);

struct keep_details_t
{
  bool keep_price;
  bool keep_date;
  bool keep_tag;
  bool only_actuals;

  explicit keep_details_t(bool _keep_price   = false,
                          bool _keep_date    = false,
                          bool _keep_tag     = false,
                          bool _only_actuals = false)
    : keep_price(_keep_price),
      keep_date(_keep_date),
      keep_tag(_keep_tag),
      only_actuals(_only_actuals)
  {
    TRACE_CTOR(keep_details_t, "bool, bool, bool, bool");
  }
  keep_details_t(const keep_details_t& other)
    : keep_price(other.keep_price), keep_date(other.keep_date),
      keep_tag(other.keep_tag), only_actuals(other.only_actuals) {
    TRACE_CTOR(keep_details_t, "copy");
  }
  ~keep_details_t() throw() {
    TRACE_DTOR(keep_details_t);
  }

  bool keep_all() const {
    return keep_price && keep_date && keep_tag && ! only_actuals;
  }
  bool keep_all(const commodity_t& comm) const;

  bool keep_any() const {
    return keep_price || keep_date || keep_tag;
  }
  bool keep_any(const commodity_t& comm) const;
};

inline std::ostream& operator<<(std::ostream&       out,
                                const annotation_t& details) {
  details.print(out);
  return out;
}

class annotated_commodity_t
  : public commodity_t,
    public equality_comparable<annotated_commodity_t,
           equality_comparable2<annotated_commodity_t, commodity_t,
                                noncopyable> >
{
protected:
  friend class commodity_pool_t;

  commodity_t * ptr;

  explicit annotated_commodity_t(commodity_t * _ptr,
                                 const annotation_t& _details)
    : commodity_t(_ptr->parent_, _ptr->base), ptr(_ptr), details(_details) {
    annotated = true;
    qualified_symbol = _ptr->qualified_symbol;
    TRACE_CTOR(annotated_commodity_t, "commodity_t *, annotation_t");
  }

public:
  annotation_t  details;

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

  virtual optional<expr_t> value_expr() const {
    if (details.value_expr)
      return details.value_expr;
    return commodity_t::value_expr();
  }

  optional<price_point_t>
  virtual find_price(const commodity_t * commodity = NULL,
                     const datetime_t&   moment    = datetime_t(),
                     const datetime_t&   oldest    = datetime_t()) const;

  virtual commodity_t& strip_annotations(const keep_details_t& what_to_keep);

  virtual void print(std::ostream& out, bool elide_quotes = false,
                     bool print_annotations = false) const {
    if (print_annotations) {
      std::ostringstream buf;
      commodity_t::print(buf, elide_quotes);
      write_annotations(buf);
      out << buf.str();
    } else {
      commodity_t::print(out, elide_quotes);
    }
  }

  virtual void write_annotations(std::ostream& out,
                                 bool no_computed_annotations = false) const;
};

inline annotated_commodity_t&
as_annotated_commodity(commodity_t& commodity) {
  return downcast<annotated_commodity_t>(commodity);
}
inline const annotated_commodity_t&
as_annotated_commodity(const commodity_t& commodity) {
  return downcast<const annotated_commodity_t>(commodity);
}

} // namespace ledger

#endif // _ANNOTATE_H
