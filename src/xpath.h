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

#ifndef _XPATH_H
#define _XPATH_H

#include "document.h"

namespace ledger {
namespace xml {

class xpath_t
{
public:
  struct op_t;
  typedef intrusive_ptr<op_t> ptr_op_t;

  static void initialize();
  static void shutdown();

  DECLARE_EXCEPTION(parse_error);
  DECLARE_EXCEPTION(compile_error);
  DECLARE_EXCEPTION(calc_error);

public:
  class scope_t;

  typedef function<value_t (scope_t *)> function_t;

#define MAKE_FUNCTOR(x) \
  xml::xpath_t::wrap_functor(bind(&x, this, _1))

  static ptr_op_t wrap_value(const value_t& val);
  static ptr_op_t wrap_functor(const function_t& fobj);

public:
  class scope_t : public noncopyable
  {
    typedef std::map<const string, ptr_op_t> symbol_map;
    symbol_map symbols;

  public:
    scope_t *		parent;
    value_t::sequence_t args;

    enum kind_t { NORMAL, STATIC, ARGUMENT } kind;

    scope_t(scope_t * _parent = NULL, kind_t _kind = NORMAL)
      : parent(_parent), kind(_kind) {
      TRACE_CTOR(xpath_t::scope_t, "scope *, kind_t");
    }

    virtual ~scope_t() {
      TRACE_DTOR(xpath_t::scope_t);
    }

  public:
    virtual void define(const string& name, ptr_op_t def);
    virtual optional<value_t> resolve(const string& name,
				      scope_t * locals = NULL) {
      if (parent)
	return parent->resolve(name, locals);
      return none;
    }
    virtual ptr_op_t lookup(const string& name);

    void define(const string& name, const function_t& def);

    friend struct op_t;
  };

  class function_scope_t : public scope_t
  {
    const node_t& node;
    std::size_t	  index;
    std::size_t	  size;

  public:
    function_scope_t(const value_t::sequence_t& _sequence,
		     const node_t& _node, std::size_t _index,
		     scope_t * _parent = NULL)
      : scope_t(_parent, STATIC), node(_node), index(_index),
	size(_sequence.size()) {}

    function_scope_t(const node_t& _node, std::size_t _index,
		     std::size_t _size, scope_t * _parent = NULL)
      : scope_t(_parent, STATIC), node(_node), index(_index),
	size(_size) {}

    virtual optional<value_t> resolve(const string& name,
				      scope_t * locals = NULL);
  };

#define XPATH_PARSE_NORMAL     0x00
#define XPATH_PARSE_PARTIAL    0x01
#define XPATH_PARSE_RELAXED    0x02
#define XPATH_PARSE_NO_MIGRATE 0x04
#define XPATH_PARSE_NO_REDUCE  0x08
#define XPATH_PARSE_ALLOW_DATE 0x10

  typedef uint_least8_t flags_t;

private:
  struct token_t
  {
    enum kind_t {
      IDENT,			// [A-Za-z_][-A-Za-z0-9_:]*
      VALUE,			// any kind of literal value
      AT_SYM,			// @
      DOLLAR,			// $
      DOT,			// .
      DOTDOT,			// ..
      LPAREN,			// (
      RPAREN,			// )
      LBRACKET,			// (
      RBRACKET,			// )
      EXCLAM,			// !
      NEQUAL,			// !=
      MINUS,			// -
      PLUS,			// +
      STAR,			// *
      POWER,			// **
      SLASH,			// /
      EQUAL,			// =
      ASSIGN,			// :=
      LESS,			// <
      LESSEQ,			// <=
      GREATER,			// >
      GREATEREQ,		// >=
      AMPER,			// &
      PIPE,			// |
      QUESTION,			// ?
      COLON,			// :
      COMMA,			// ,
      KW_AND,
      KW_OR,
      KW_DIV,
      KW_MOD,
      KW_UNION,
      TOK_EOF,
      UNKNOWN
    } kind;

    char	 symbol[3];
    value_t	 value;
    unsigned int length;

    token_t() : kind(UNKNOWN), length(0) {
      TRACE_CTOR(xpath_t::token_t, "");
    }

    token_t(const token_t& other) {
      assert(false);
      TRACE_CTOR(xpath_t::token_t, "copy");
      *this = other;
    }

    ~token_t() {
      TRACE_DTOR(xpath_t::token_t);
    }

    token_t& operator=(const token_t& other) {
      if (&other == this)
	return *this;
      assert(false);
      return *this;
    }

    void clear() {
      kind   = UNKNOWN;
      length = 0;
      value  = 0L;

      symbol[0] = '\0';
      symbol[1] = '\0';
      symbol[2] = '\0';
    }

    void parse_ident(std::istream& in);

    void next(std::istream& in, flags_t flags);
    void rewind(std::istream& in);

    void unexpected();
    static void unexpected(char c, char wanted = '\0');
  };

public:
  class path_t
  {
  public:
    typedef function<void (const value_t&)> visitor_t;
    typedef function<bool (const node_t&, scope_t *)> predicate_t;

  private:
    struct value_appender_t {
      value_t::sequence_t& sequence;
      value_appender_t(value_t::sequence_t& _sequence)
	: sequence(_sequence) {}
      void operator()(const value_t& val) {
	sequence.push_back(val);
      }
    };

    ptr_op_t path_expr;

    template <typename NodeType>
    void walk_elements(NodeType&	start,
		       const ptr_op_t&	element,
		       const bool	recurse,
		       scope_t *	scope,
		       const visitor_t& func);

    template <typename NodeType>
    void check_element(NodeType&	start,
		       const ptr_op_t&	element,
		       scope_t *	scope,
		       std::size_t	index,
		       std::size_t	size,
		       const visitor_t& func);

  public:
    path_t(const xpath_t& xpath) : path_expr(xpath.ptr) {}
    path_t(const ptr_op_t& _path_expr) : path_expr(_path_expr) {}

    value_t find_all(node_t& start, scope_t * scope) {
      value_t result = value_t::sequence_t();
      visit(start, scope, value_appender_t(result.as_sequence_lval()));
      return result;
    }
    value_t find_all(const node_t& start, scope_t * scope) {
      value_t result = value_t::sequence_t();
      visit(start, scope, value_appender_t(result.as_sequence_lval()));
      return result;
    }

    void visit(node_t& start, scope_t * scope, const visitor_t& func) {
      if (path_expr)
	walk_elements<node_t>(start, path_expr, false, scope, func);
    }
    void visit(const node_t& start, scope_t * scope, const visitor_t& func) {
      if (path_expr)
	walk_elements<const node_t>(start, path_expr, false, scope, func);
    }
  };

  template <typename NodeType>
  class path_iterator_t
  {
    typedef NodeType * pointer;
    typedef NodeType&  reference;

    path_t    path;
    reference start;
    scope_t * scope;

    mutable value_t::sequence_t sequence;
    mutable bool searched;

    struct node_appender_t {
      value_t::sequence_t& sequence;
      node_appender_t(value_t::sequence_t& _sequence)
	: sequence(_sequence) {}
      void operator()(const value_t& node) {
	sequence.push_back(node);
      }
    };

  public:
    typedef value_t::sequence_t::iterator       iterator;
    typedef value_t::sequence_t::const_iterator const_iterator;

    path_iterator_t(const xpath_t& path_expr,
		    reference _start, scope_t * _scope)
      : path(path_expr), start(_start), scope(_scope),
	searched(false) {
    }

    iterator begin() {
      if (! searched) {
	path.visit(start, scope, node_appender_t(sequence));
	searched = true;
      }
      return sequence.begin();
    }
    const_iterator begin() const {
      return const_cast<path_iterator_t *>(this)->begin();
    }

    iterator end() { return sequence.end(); }
    const_iterator end() const { return sequence.end(); }
  };

  struct op_t : public noncopyable
  {
    enum kind_t {
      VOID,
      VALUE,

      NODE_ID,
      NODE_NAME,
      ATTR_ID,
      ATTR_NAME,
      FUNC_NAME,
      VAR_NAME,

      ARG_INDEX,

      CONSTANTS,		// constants end here

      FUNCTION,

      TERMINALS,		// terminals end here

      O_NOT,
      O_NEG,

      O_UNION,

      O_ADD,
      O_SUB,
      O_MUL,
      O_DIV,

      O_NEQ,
      O_EQ,
      O_LT,
      O_LTE,
      O_GT,
      O_GTE,

      O_AND,
      O_OR,

      O_QUES,
      O_COLON,

      O_COMMA,

      O_DEFINE,
      O_EVAL,
      O_ARG,

      O_FIND,
      O_RFIND,
      O_PRED,

      LAST			// operators end here
    };

    kind_t	  kind;
    mutable short refc;
    ptr_op_t	  left_;

    variant<unsigned int,	 // used by ARG_INDEX and O_ARG
	    value_t,		 // used by constant VALUE
	    string,		 // used by constant SYMBOL
	    function_t,		 // used by terminal FUNCTION
	    node_t::nameid_t,	 // used by NODE_NAME and ATTR_NAME
	    ptr_op_t>		 // used by all binary operators
      data;

    op_t(const kind_t _kind) : kind(_kind), refc(0){
      TRACE_CTOR(xpath_t::op_t, "const kind_t");
    }
    ~op_t() {
      TRACE_DTOR(xpath_t::op_t);

      DEBUG("ledger.xpath.memory", "Destroying " << this);
      assert(refc == 0);
    }

    op_t& operator=(const op_t&);

    bool is_long() const {
      return data.type() == typeid(unsigned int);
    }
    unsigned int& as_long() {
      assert(kind == ARG_INDEX || kind == O_ARG);
      return boost::get<unsigned int>(data);
    }
    const unsigned int& as_long() const {
      return const_cast<op_t *>(this)->as_long();
    }
    void set_long(unsigned int val) {
      data = val;
    }

    bool is_value() const {
      return kind == VALUE;
    }
    value_t& as_value() {
      assert(kind == VALUE);
      return boost::get<value_t>(data);
    }
    const value_t& as_value() const {
      return const_cast<op_t *>(this)->as_value();
    }
    void set_value(const value_t& val) {
      data = val;
    }

    bool is_string() const {
      return data.type() == typeid(string);
    }
    string& as_string() {
      assert(kind == NODE_NAME || kind == ATTR_NAME || kind == FUNC_NAME);
      return boost::get<string>(data);
    }
    const string& as_string() const {
      return const_cast<op_t *>(this)->as_string();
    }
    void set_string(const string& val) {
      data = val;
    }

    bool is_function() const {
      return kind == FUNCTION;
    }
    function_t& as_function() {
      assert(kind == FUNCTION);
      return boost::get<function_t>(data);
    }
    const function_t& as_function() const {
      return const_cast<op_t *>(this)->as_function();
    }
    void set_function(const function_t& val) {
      data = val;
    }

    bool is_name() const {
      return data.type() == typeid(node_t::nameid_t);
    }
    node_t::nameid_t& as_name() {
      assert(kind == NODE_ID || kind == ATTR_ID);
      return boost::get<node_t::nameid_t>(data);
    }
    const node_t::nameid_t& as_name() const {
      return const_cast<op_t *>(this)->as_name();
    }
    void set_name(const node_t::nameid_t& val) {
      data = val;
    }

#if 0
    bool is_path() const {
      return kind == PATH;
    }
    path_t& as_path() {
      assert(kind == PATH);
      return boost::get<path_t>(data);
    }
    const path_t& as_path() const {
      return const_cast<op_t *>(this)->as_path();
    }
    void set_path(const path_t& val) {
      data = val;
    }
#endif

    ptr_op_t& as_op() {
      assert(kind > TERMINALS);
      return boost::get<ptr_op_t>(data);
    }
    const ptr_op_t& as_op() const {
      return const_cast<op_t *>(this)->as_op();
    }

    void acquire() const {
      DEBUG("ledger.xpath.memory",
	     "Acquiring " << this << ", refc now " << refc + 1);
      assert(refc >= 0);
      refc++;
    }
    void release() const {
      DEBUG("ledger.xpath.memory",
	     "Releasing " << this << ", refc now " << refc - 1);
      assert(refc > 0);
      if (--refc == 0)
	checked_delete(this);
    }

    ptr_op_t& left() {
      return left_;
    }
    const ptr_op_t& left() const {
      assert(kind > TERMINALS);
      return left_;
    }
    void set_left(const ptr_op_t& expr) {
      assert(kind > TERMINALS);
      left_ = expr;
    }

    ptr_op_t& right() {
      assert(kind > TERMINALS);
      return as_op();
    }
    const ptr_op_t& right() const {
      assert(kind > TERMINALS);
      return as_op();
    }
    void set_right(const ptr_op_t& expr) {
      assert(kind > TERMINALS);
      data = expr;
    }

    static ptr_op_t new_node(kind_t kind, ptr_op_t left = NULL,
			     ptr_op_t right = NULL);

    ptr_op_t copy(ptr_op_t left = NULL, ptr_op_t right = NULL) const;
    ptr_op_t compile(const node_t& context, scope_t * scope, bool resolve = false);

    void append_value(value_t::sequence_t& result_seq, value_t& value);

    static ptr_op_t defer_sequence(value_t::sequence_t& result_seq);

    bool print(std::ostream&   out,
	       document_t&     document,
	       const bool      relaxed	  = true,
	       const ptr_op_t& op_to_find = NULL,
	       unsigned long * start_pos  = NULL,
	       unsigned long * end_pos	  = NULL) const;

    void dump(std::ostream& out, const int depth) const;

    friend inline void intrusive_ptr_add_ref(xpath_t::op_t * op) {
      op->acquire();
    }
    friend inline void intrusive_ptr_release(xpath_t::op_t * op) {
      op->release();
    }
  };

  class op_predicate
  {
    ptr_op_t op;
  public:
    op_predicate(ptr_op_t _op) : op(_op) {}

    bool operator()(const node_t& node, scope_t * scope) {
      xpath_t result(op->compile(node, scope, true));
      return result.ptr->as_value().to_boolean();
    }
  };

public:
  ptr_op_t ptr;

  xpath_t& operator=(ptr_op_t _expr) {
    expr = "";
    ptr	 = _expr;
    return *this;
  }

#ifdef THREADSAFE
  mutable token_t   lookahead;
#else
  static  token_t * lookahead;
#endif
  mutable bool	    use_lookahead;

  token_t& next_token(std::istream& in, flags_t tflags) const {
    if (use_lookahead)
      use_lookahead = false;
    else
#ifdef THREADSAFE
      lookahead.next(in, tflags);
#else
      lookahead->next(in, tflags);
#endif
#ifdef THREADSAFE
    return lookahead;
#else
    return *lookahead;
#endif
  }
  void push_token(const token_t& tok) const {
#ifdef THREADSAFE
    assert(&tok == &lookahead);
#else
    assert(&tok == lookahead);
#endif
    use_lookahead = true;
  }
  void push_token() const {
    use_lookahead = true;
  }

  ptr_op_t parse_value_term(std::istream& in, flags_t flags) const;
  ptr_op_t parse_predicate_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_path_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_unary_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_union_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_mul_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_add_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_logic_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_and_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_or_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_querycolon_expr(std::istream& in, flags_t flags) const;
  ptr_op_t parse_value_expr(std::istream& in, flags_t flags) const;

  ptr_op_t parse_expr(std::istream& in,
		      flags_t flags = XPATH_PARSE_RELAXED) const;

  ptr_op_t parse_expr(const string& str,
		      flags_t tflags = XPATH_PARSE_RELAXED) const
  {
    std::istringstream stream(str);
#if 0
    try {
#endif
      return parse_expr(stream, tflags);
#if 0
    }
    catch (error * err) {
      err->context.push_back
	(new line_context(str, (long)stream.tellg() - 1,
			  "While parsing value expression:"));
      throw err;
    }
#endif
  }

  ptr_op_t parse_expr(const char * p,
		      flags_t tflags = XPATH_PARSE_RELAXED) const {
    return parse_expr(string(p), tflags);
  }

  bool print(std::ostream&   out,
	     document_t&     document,
	     const bool      relaxed,
	     const ptr_op_t  op_to_find,
	     unsigned long * start_pos,
	     unsigned long * end_pos) const {
    if (ptr)
      ptr->print(out, document, relaxed, op_to_find, start_pos, end_pos);
    return true;
  }

public:
  string  expr;
  flags_t flags;		// flags used to parse `expr'

  xpath_t() : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR(xpath_t, "");
  }
  xpath_t(ptr_op_t _ptr) : ptr(_ptr), use_lookahead(false) {
    TRACE_CTOR(xpath_t, "ptr_op_t");
  }

  xpath_t(const string& _expr, flags_t _flags = XPATH_PARSE_RELAXED)
    : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR(xpath_t, "const string&, flags_t");
    if (! _expr.empty())
      parse(_expr, _flags);
  }
  xpath_t(std::istream& in, flags_t _flags = XPATH_PARSE_RELAXED)
    : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR(xpath_t, "std::istream&, flags_t");
    parse(in, _flags);
  }
  xpath_t(const xpath_t& other)
    : ptr(other.ptr), use_lookahead(false),
      expr(other.expr), flags(other.flags) {
    TRACE_CTOR(xpath_t, "copy");
  }
  virtual ~xpath_t() {
    TRACE_DTOR(xpath_t);
  }

  xpath_t& operator=(const string& _expr) {
    parse(_expr);
    return *this;
  }
  xpath_t& operator=(const xpath_t& _expr);
  xpath_t& operator=(xpath_t& _xpath) {
    ptr	  = _xpath.ptr;
    expr  = _xpath.expr;
    flags = _xpath.flags;
    use_lookahead = false;
    return *this;
  }

  operator ptr_op_t() throw() {
    return ptr;
  }

  operator bool() const throw() {
    return ptr != NULL;
  }
  operator string() const throw() {
    return expr;
  }

  void parse(const string& _expr, flags_t _flags = XPATH_PARSE_RELAXED) {
    expr  = _expr;
    flags = _flags;
    ptr	  = parse_expr(_expr, _flags);
  }
  void parse(std::istream& in, flags_t _flags = XPATH_PARSE_RELAXED) {
    expr  = "";
    flags = _flags;
    ptr   = parse_expr(in, _flags);
  }

  void compile(const node_t& context, scope_t * scope = NULL) {
    if (ptr.get())
      ptr = ptr->compile(context, scope);
  }

  virtual value_t calc(const node_t& context, scope_t * scope = NULL) const;

  static value_t eval(const string& _expr, const node_t& context,
		      scope_t * scope = NULL) {
    return xpath_t(_expr).calc(context, scope);
  }

  path_iterator_t<node_t>
  find_all(node_t& start, scope_t * scope) {
    return path_iterator_t<node_t>(*this, start, scope);
  }
  path_iterator_t<const node_t>
  find_all(const node_t& start, scope_t * scope) {
    return path_iterator_t<const node_t>(*this, start, scope);
  }

  void visit(node_t& start, scope_t * scope, const path_t::visitor_t& func) {
    path_t(*this).visit(start, scope, func);
  }
  void visit(const node_t& start, scope_t * scope, const
	     path_t::visitor_t& func) {
    path_t(*this).visit(start, scope, func);
  }

  void print(std::ostream& out, xml::document_t& document) const {
    print(out, document, true, NULL, NULL, NULL);
  }

  void dump(std::ostream& out) const {
    if (ptr)
      ptr->dump(out, 0);
  }

  friend class scope_t;
};

} // namespace xml

template <typename T>
inline T * get_ptr(xml::xpath_t::scope_t * locals, unsigned int idx) {
  assert(locals->args.size() > idx);
  T * ptr = locals->args[idx].as_pointer<T>();
  assert(ptr);
  return ptr;
}

template <typename T>
inline T * get_node_ptr(xml::xpath_t::scope_t * locals, unsigned int idx) {
  assert(locals->args.size() > idx);
  T * ptr = polymorphic_downcast<T *>(locals->args[idx].as_xml_node_mutable());
  assert(ptr);
  return ptr;
}

class xml_command
{
 public:
  value_t operator()(xml::xpath_t::scope_t * locals) {
    std::ostream *    out = get_ptr<std::ostream>(locals, 0);
    xml::document_t * doc = get_node_ptr<xml::document_t>(locals, 1);
    doc->print(*out);
    return true;
  }
};

} // namespace ledger

#endif // _XPATH_H
