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

#ifndef _BUILDER_H
#define _BUILDER_H

#include "document.h"

namespace ledger {
namespace xml {

/**
 * @class builder_t
 *
 * @brief Represents an interface for building a data hierarchy.
 *
 * This interface is much like .NET's XmlWriter facility.  It
 * abstracts the kind of hierarchy we're building, instead focusing
 * only on the relationships.
 */
class builder_t
{
public:
  struct position_t
  {
    typedef uint_least32_t file_pos_t;
    typedef uint_least32_t file_line_t;

    file_pos_t	offset;
    file_line_t linenum;

    position_t() : offset(0), linenum(0) {}

    explicit position_t(file_pos_t  _offset,
			file_line_t _linenum)
      : offset(_offset), linenum(_linenum) {}
  };

protected:
  position_t current_position;

public:
  virtual ~builder_t() {}

  virtual void     set_start_position(std::istream& in) {}
  virtual void     set_position(const position_t& position) {}
  virtual position_t& position() { return current_position; }

  virtual void     push_attr(const string&  name,
			     const string& value) = 0;
  virtual void     push_attr(const node_t::nameid_t name_id,
			     const string& value) = 0;

  virtual void     begin_node(const string& name, bool terminal = false)     = 0;
  virtual void     begin_node(const node_t::nameid_t name_id, bool terminal = false) = 0;

  virtual void     push_node(const string& name,
			     const optional<position_t>& end_pos = none) = 0;
  virtual void     push_node(const node_t::nameid_t name_id,
			     const optional<position_t>& end_pos = none) = 0;

  virtual node_t * current_node() = 0;

  virtual void     append_text(const string& text) = 0;

  virtual node_t * end_node(const string& name,
			     const optional<position_t>& end_pos = none) = 0;
  virtual node_t * end_node(const node_t::nameid_t name_id,
			     const optional<position_t>& end_pos = none) = 0;
};

/**
 * @class xml_builder_t
 *
 * @brief Build a generic node_t hierarchy.
 *
 * This builder can be used to parse ordinary XML into a document
 * object structure which can then be traversed in memory.
 */
class document_builder_t : public builder_t
{
protected:
  typedef std::list<std::pair<node_t::nameid_t, string> > attrs_list;

  document_t& document_;
  attrs_list  current_attrs;
  node_t *    current;
  string      current_text;

public:
  document_builder_t(document_t& _document)
    : document_(_document), current(&document_) {}

  virtual void     push_attr(const string&  name,
			     const string& value) {
    push_attr(document().register_name(name), value);
  }
  virtual void     push_attr(const node_t::nameid_t name_id,
			     const string& value) {
    current_attrs.push_back(attrs_list::value_type(name_id, value.c_str()));
  }

  virtual void     begin_node(const string& name, bool terminal = false) {
    begin_node(document().register_name(name), terminal);
  }
  virtual void     begin_node(const node_t::nameid_t name_id,
			      bool terminal = false) {
    if (terminal)
      current = current->as_parent_node().create_child<terminal_node_t>(name_id);
    else
      current = current->as_parent_node().create_child<parent_node_t>(name_id);

    foreach (const attrs_list::value_type& pair, current_attrs)
      current->set_attr(pair.first, pair.second.c_str());
    current_attrs.clear();
  }

  virtual void     push_node(const string& name,
			     const optional<position_t>& end_pos = none) {
    begin_node(name, true);
    end_node(name, end_pos);
  }
  virtual void     push_node(const node_t::nameid_t name_id,
			     const optional<position_t>& end_pos = none) {
    begin_node(name_id, true);
    end_node(name_id, end_pos);
  }

  virtual document_t& document() {
    return document_;
  }
  virtual node_t * current_node() {
    return current;
  }

  virtual void     append_text(const string& text) {
    assert(! current->is_parent_node());
    polymorphic_downcast<terminal_node_t *>(current)->set_text(text);
  }

  virtual node_t * end_node(const string& name,
			    const optional<position_t>& end_pos = none) {
    return current = &*current->parent();
  }
  virtual node_t * end_node(const node_t::nameid_t name_id,
			    const optional<position_t>& end_pos = none) {
    return current = &*current->parent();
  }
};

/**
 * @class xml_writer_t
 *
 * @brief Create textual XML on the given output stream.
 *
 * This builder, rather than manipulating data structures in memory,
 * simply streams its contents on the fly to the given output stream.
 * It uses only enough memory to remember the currently push
 * attributes and text.
 */
class xml_writer_t : public builder_t
{
  typedef std::list<std::pair<string, string> > attrs_list;

  attrs_list	current_attrs;
  std::ostream& outs;

public:
  xml_writer_t(std::ostream& _outs) : outs(_outs) {
    outs << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
    begin_node("ledger");
  }
  ~xml_writer_t() {
    end_node("ledger");
  }

  virtual void     push_attr(const string&  name,
			     const string& value) {
    current_attrs.push_back(attrs_list::value_type(name, value));
  }
  virtual void     push_attr(const node_t::nameid_t name_id,
			     const string& value) {
    push_attr("hello", value);
  }

  virtual void     begin_node(const string& name, bool terminal = false) {
    outs << '<' << name;
    foreach (const attrs_list::value_type& attr, current_attrs)
      outs << ' ' << attr.first << "=\"" << attr.second << "\"";
    current_attrs.clear();
    outs << '>';
  }
  virtual void     begin_node(const node_t::nameid_t name_id, bool terminal = false) {
    begin_node("hello");
  }

  virtual void     push_node(const string& name,
			     const optional<position_t>& end_pos = none) {
    begin_node(name, true);
    end_node(name, end_pos);
  }
  virtual void     push_node(const node_t::nameid_t name_id,
			     const optional<position_t>& end_pos = none) {
    push_node("hello", end_pos);
  }

  virtual node_t * current_node() { return NULL; }

  virtual void     append_text(const string& text) {
    outs << text;
  }

  virtual node_t * end_node(const string& name,
			    const optional<position_t>& end_pos = none) {
    outs << "</" << name << '>';
    return NULL;
  }
  virtual node_t * end_node(const node_t::nameid_t name_id,
			    const optional<position_t>& end_pos = none) {
    end_node("hello", end_pos);
    return NULL;
  }
};

} // namespace xml
} // namespace ledger

#endif // _BUILDER_H
