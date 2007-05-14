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

#ifndef _NODE_H
#define _NODE_H

#include "value.h"
//#include "parser.h"

namespace ledger {
namespace xml {

#define XML_NODE_IS_PARENT 0x1

DECLARE_EXCEPTION(conversion_error);

class parent_node_t;
class document_t;

class node_t : public supports_flags<>, public noncopyable
{
public:
  typedef uint_fast16_t nameid_t;

  nameid_t name_id_;

protected:
  document_t&		   document_;
  optional<parent_node_t&> parent_;

  typedef std::pair<nameid_t, string> attr_pair;

  typedef multi_index_container<
    attr_pair,
    multi_index::indexed_by<
      multi_index::sequenced<>,
      multi_index::hashed_unique<
	multi_index::member<attr_pair, nameid_t, &attr_pair::first> >
    >
  > attributes_t;

  optional<attributes_t> attributes;

public:
  node_t(nameid_t _name_id, document_t& _document,
	 const optional<parent_node_t&>& _parent = none, flags_t _flags = 0)
    : supports_flags<>(_flags), name_id_(_name_id),
      document_(_document), parent_(_parent) {
    TRACE_CTOR(node_t, "document_t&, parent_node_t&, nameid_t, flags_t");
  }

  virtual ~node_t() {
    TRACE_DTOR(node_t);
  }

  bool is_parent_node() const {
    return has_flags(XML_NODE_IS_PARENT);
  }
  parent_node_t& as_parent_node() {
    if (! is_parent_node())
      throw_(std::logic_error, "Request to cast leaf node to a parent node");
    return *polymorphic_downcast<parent_node_t *>(this);
  }    
  const parent_node_t& as_parent_node() const {
    if (! is_parent_node())
      throw_(std::logic_error, "Request to cast leaf node to a parent node");
    return *polymorphic_downcast<const parent_node_t *>(this);
  }    

  virtual value_t to_value() const		 = 0;
  virtual void    print(std::ostream& out) const = 0;
  virtual void    print_attributes(std::ostream& out) const;

  const char * name() const;
  nameid_t name_id() const {
    return name_id_;
  }

  document_t& document() const {
    return document_;
  }
  optional<parent_node_t&> parent() const {
    return parent_;
  }

  void set_attr(const nameid_t _name_id, const char * value) {
    if (! attributes)
      attributes = attributes_t();
    attributes->push_back(attr_pair(_name_id, value));
  }
  optional<const string&> get_attr(const nameid_t _name_id) {
    if (attributes) {
      typedef attributes_t::nth_index<1>::type attributes_by_name;

      attributes_by_name& name_index = attributes->get<1>();
      attributes_by_name::iterator i = name_index.find(_name_id);
      if (i != name_index.end())
	return (*i).second;
    }
    return none;
  }
};

class parent_node_t : public node_t
{
  typedef multi_index_container<
    node_t *,
    multi_index::indexed_by<
      multi_index::sequenced<>,
      multi_index::hashed_non_unique<
	multi_index::member<node_t, nameid_t, &node_t::name_id_> >,
      multi_index::hashed_unique<multi_index::identity<node_t *> >
    >
  > children_t;

  children_t children;

public:
  typedef children_t::nth_index<0>::type children_by_order;
  typedef children_t::nth_index<1>::type children_by_nameid;
  typedef children_t::nth_index<2>::type children_by_ptr;

  parent_node_t(nameid_t _name_id, document_t& _document,
		const optional<parent_node_t&>& _parent = none)
    : node_t(_name_id, _document, _parent, XML_NODE_IS_PARENT) {
    TRACE_CTOR(parent_node_t, "document_t *, parent_node_t *");
  }
  virtual ~parent_node_t() {
    TRACE_DTOR(parent_node_t);
    clear_children();
  }

  template <typename T>
  T * create_child(nameid_t _name_id) {
    T * child = new T(_name_id, document(), *this);
    children.push_back(child);
    return child;
  }

  void delete_child(node_t * child) {
    children_by_ptr& ptr_index = children.get<2>();
    children_by_ptr::iterator i = ptr_index.find(child);
    if (i == ptr_index.end())
      throw_(std::logic_error, "Request to delete node which is not a child");
    node_t * ptr = *i;
    ptr_index.erase(i);
    checked_delete(ptr);
  }

  struct match_nameid {
    nameid_t nameid;
    match_nameid(nameid_t _nameid) : nameid(_nameid) {}
    bool operator()(const node_t * node) const {
      return node->name_id() == nameid;
    }
  };

  typedef children_by_order::iterator iterator;
  typedef children_by_order::iterator const_iterator;

  children_by_order::iterator begin() {
    return children.get<0>().begin();
  }
  children_by_order::const_iterator begin() const {
    return children.get<0>().begin();
  }
  children_by_order::iterator end() {
    return children.get<0>().end();
  }
  children_by_order::const_iterator end() const {
    return children.get<0>().end();
  }

  children_by_nameid::iterator begin(nameid_t _name_id) {
    return std::find_if(children.get<1>().begin(),
			children.get<1>().end(), match_nameid(_name_id));
  }
  children_by_nameid::const_iterator begin(nameid_t _name_id) const {
    return std::find_if(children.get<1>().begin(),
			children.get<1>().end(), match_nameid(_name_id));
  }
  children_by_nameid::iterator end(nameid_t) {
    return children.get<1>().end();
  }
  children_by_nameid::const_iterator end(nameid_t) const {
    return children.get<1>().end();
  }

  void clear_children() {
    typedef children_t::nth_index<0>::type children_by_index;

    children_by_index& child_index = children.get<0>();
    for (children_by_index::iterator i = child_index.begin();
	 i != child_index.end();
	 i++)
      checked_delete(*i);

    children.clear();
  }

  virtual value_t to_value() const {
    throw_(std::logic_error, "Cannot convert parent node to a value");
  }

  void print(std::ostream& out) const;
};

class terminal_node_t : public node_t
{
  string data;

public:
  terminal_node_t(nameid_t _name_id, document_t& _document,
		  const optional<parent_node_t&>& _parent = none)
    : node_t(_name_id, _document, _parent)
  {
    TRACE_CTOR(terminal_node_t, "document_t *, parent_node_t *");
  }
  virtual ~terminal_node_t() {
    TRACE_DTOR(terminal_node_t);
  }

  const char * text() const {
    return data.c_str();
  }
  void set_text(const string& _data) {
    data = _data;
  }
  void set_text(const char * _data) {
    data = _data;
  }

  virtual value_t to_value() const {
    return text();
  }

  void print(std::ostream& out) const;
};

} // namespace xml
} // namespace ledger

#endif // _NODE_H
