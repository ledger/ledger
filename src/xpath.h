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
  class call_scope_t;

  typedef function<value_t (call_scope_t&)> function_t;

#define MAKE_FUNCTOR(x) \
  xml::xpath_t::op_t::wrap_functor(bind(&x, this, _1))
#define WRAP_FUNCTOR(x) \
  xml::xpath_t::op_t::wrap_functor(x)

public:
  class scope_t : public noncopyable
  {
  public:
    enum type_t {
      CHILD_SCOPE,
      SYMBOL_SCOPE,
      CALL_SCOPE,
      CONTEXT_SCOPE,
      SELECTION_SCOPE,
      PREDICATE_SCOPE
    } type_;

    explicit scope_t(type_t _type) : type_(_type) {
      TRACE_CTOR(xpath_t::scope_t, "type_t");
    }
    virtual ~scope_t() {
      TRACE_DTOR(xpath_t::scope_t);
    }

    const type_t type() const {
      return type_;
    }

    virtual void     define(const string& name, ptr_op_t def) = 0;
            void     define(const string& name, const value_t& val);
    virtual ptr_op_t lookup(const string& name) = 0;
            value_t  resolve(const string& name) {
      return lookup(name)->calc(*this);
    }

    virtual optional<scope_t&> find_scope(const type_t _type,
					  bool skip_this = false) = 0;
    virtual optional<scope_t&> find_first_scope(const type_t _type1,
						const type_t _type2,
						bool skip_this = false) = 0;

    template <typename T>
    T& find_scope(bool skip_this = false) {
      assert(false);
    }
    template <typename T>
    optional<T&> maybe_find_scope(bool skip_this = false) {
      assert(false);
    }
  };

  class child_scope_t : public scope_t
  {
    scope_t * parent;

  public:
    explicit child_scope_t(type_t _type = CHILD_SCOPE)
      : scope_t(_type), parent(NULL) {
      TRACE_CTOR(xpath_t::child_scope_t, "type_t");
    }
    explicit child_scope_t(scope_t& _parent, type_t _type = CHILD_SCOPE)
      : scope_t(_type), parent(&_parent) {
      TRACE_CTOR(xpath_t::child_scope_t, "scope_t&, type_t");
    }
    virtual ~child_scope_t() {
      TRACE_DTOR(xpath_t::child_scope_t);
    }
  public:
    virtual void     define(const string& name, ptr_op_t def) {
      if (parent)
	parent->define(name, def);
    }
    virtual ptr_op_t lookup(const string& name) {
      if (parent)
	return parent->lookup(name);
      return ptr_op_t();
    }

    virtual optional<scope_t&> find_scope(type_t _type,
					  bool skip_this = false) {
      for (scope_t * ptr = (skip_this ? parent : this); ptr; ) {
	if (ptr->type() == _type)
	  return *ptr;

	ptr = polymorphic_downcast<child_scope_t *>(ptr)->parent;
      }
      return none;
    }

    virtual optional<scope_t&> find_first_scope(const type_t _type1,
						const type_t _type2,
						bool skip_this = false) {
      for (scope_t * ptr = (skip_this ? parent : this); ptr; ) {
	if (ptr->type() == _type1 || ptr->type() == _type2)
	  return *ptr;

	ptr = polymorphic_downcast<child_scope_t *>(ptr)->parent;
      }
      return none;
    }
  };

  class symbol_scope_t : public child_scope_t
  {
    typedef std::map<const string, ptr_op_t> symbol_map;
    symbol_map symbols;

  public:
    explicit symbol_scope_t()
      : child_scope_t(SYMBOL_SCOPE) {
      TRACE_CTOR(xpath_t::symbol_scope_t, "");
    }
    explicit symbol_scope_t(scope_t& _parent)
      : child_scope_t(_parent, SYMBOL_SCOPE) {
      TRACE_CTOR(xpath_t::symbol_scope_t, "scope_t&");
    }
    virtual ~symbol_scope_t() {
      TRACE_DTOR(xpath_t::symbol_scope_t);
    }

    virtual void     define(const string& name, ptr_op_t def);
            void     define(const string& name, const value_t& val) {
	      scope_t::define(name, val);
	    }
    virtual ptr_op_t lookup(const string& name);
  };

  class call_scope_t : public child_scope_t
  {
    value_t args;

  public:
    explicit call_scope_t(scope_t& _parent)
      : child_scope_t(_parent, CALL_SCOPE) {
      TRACE_CTOR(xpath_t::call_scope_t, "scope_t&");
    }
    virtual ~call_scope_t() {
      TRACE_DTOR(xpath_t::call_scope_t);
    }

    void set_args(const value_t& _args) {
      args = _args;
    }

    value_t& value() {
      return args;
    }

    value_t& operator[](const int index) {
      return args[index];
    }
    const value_t& operator[](const int index) const {
      return args[index];
    }

    void push_back(const value_t& val) {
      args.push_back(val);
    }
    void pop_back() {
      args.pop_back();
    }

    const std::size_t size() const {
      return args.size();
    }
  };

  class context_scope_t : public child_scope_t
  {
  public:
    value_t     current_element;
    std::size_t element_index;
    std::size_t sequence_size;

    explicit context_scope_t(scope_t&	        _parent,
			     const value_t&     _element        = NULL_VALUE,
			     const std::size_t  _element_index  = 0,
			     const std::size_t  _sequence_size  = 0)
      : child_scope_t(_parent, CONTEXT_SCOPE), current_element(_element), 
	element_index(_element_index), sequence_size(_sequence_size)
    {
      TRACE_CTOR(xpath_t::context_scope_t, "scope_t&, const value_t&, ...");
    }
    virtual ~context_scope_t() {
      TRACE_DTOR(xpath_t::context_scope_t);
    }

    const std::size_t index() const {
      return element_index;
    }
    const std::size_t size() const {
      return sequence_size;
    }

    value_t& value() {
      return current_element;
    }
    node_t& xml_node() {
      assert(current_element.is_xml_node());
      return *current_element.as_xml_node();
    }
  };

  class selection_scope_t : public child_scope_t
  {
  public:
    ptr_op_t selection_path;
    bool     recurse;

    explicit selection_scope_t(scope_t&	       _parent,
			       const ptr_op_t& _selection_path = NULL,
			       const bool      _recurse        = false)
      : child_scope_t(_parent, SELECTION_SCOPE),
	selection_path(_selection_path), recurse(_recurse)
    {
      TRACE_CTOR(xpath_t::selection_scope_t,
		 "scope_t&, const ptr_op_t&, const bool");
    }
    virtual ~selection_scope_t() {
      TRACE_DTOR(xpath_t::selection_scope_t);
    }
  };

  typedef function<bool (scope_t&)> predicate_t;

  class predicate_scope_t : public child_scope_t
  {
  public:
    predicate_t predicate;

    explicit predicate_scope_t(scope_t&	_parent,
			       const predicate_t& _predicate = predicate_t())
      : child_scope_t(_parent, PREDICATE_SCOPE), predicate(_predicate)
    {
      TRACE_CTOR(xpath_t::predicate_scope_t, "scope_t&, const predicate_t&");
    }
    virtual ~predicate_scope_t() {
      TRACE_DTOR(xpath_t::predicate_scope_t);
    }
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
      VALUE,			// any kind of literal value

      IDENT,			// [A-Za-z_][-A-Za-z0-9_:]*
      DOLLAR,			// $
      AT_SYM,			// @

      DOT,			// .
      DOTDOT,			// ..
      SLASH,			// /

      LPAREN,			// (
      RPAREN,			// )
      LBRACKET,			// [
      RBRACKET,			// ]

      EQUAL,			// =
      NEQUAL,			// !=
      LESS,			// <
      LESSEQ,			// <=
      GREATER,			// >
      GREATEREQ,		// >=

      MINUS,			// -
      PLUS,			// +
      STAR,			// *
      KW_DIV,

      EXCLAM,			// !
      KW_AND,
      KW_OR,
      KW_MOD,

      PIPE,			// |
      KW_UNION,

      COMMA,			// ,

      TOK_EOF,
      UNKNOWN
    } kind;

    char	symbol[3];
    value_t	value;
    std::size_t length;

    explicit token_t() : kind(UNKNOWN), length(0) {
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
      value  = NULL_VALUE;

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
  class path_iterator_t
  {
    typedef node_t * pointer;
    typedef node_t&  reference;

    xpath_t& path_expr;
    scope_t& scope;

    mutable value_t::sequence_t sequence;
    mutable bool searched;

  public:
    typedef value_t::sequence_t::iterator       iterator;
    typedef value_t::sequence_t::const_iterator const_iterator;

    path_iterator_t(xpath_t& _path_expr, scope_t& _scope)
      : path_expr(_path_expr), scope(_scope), searched(false) {}

    iterator begin() {
      if (! searched) {
	sequence = path_expr.calc(scope).to_sequence();
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
      VALUE,

      FUNC_NAME,
      VAR_NAME,
      ARG_INDEX,

      NODE_ID,
      NODE_NAME,
      ATTR_ID,
      ATTR_NAME,

      CONSTANTS,		// constants end here

      FUNCTION,

      TERMINALS,		// terminals end here

      O_CALL,
      O_ARG,

      O_FIND,
      O_RFIND,
      O_PRED,

      O_NEQ,
      O_EQ,
      O_LT,
      O_LTE,
      O_GT,
      O_GTE,

      O_ADD,
      O_SUB,
      O_MUL,
      O_DIV,
      O_NEG,

      O_NOT,
      O_AND,
      O_OR,

      O_UNION,

      O_COMMA,

      LAST			// operators end here
    };

    kind_t	  kind;
    mutable short refc;
    ptr_op_t	  left_;

    variant<unsigned int,	 // used by ARG_INDEX and O_ARG
	    value_t,		 // used by constant VALUE
	    string,		 // used by constants SYMBOL, *_NAME
	    function_t,		 // used by terminal FUNCTION
	    node_t::nameid_t,	 // used by NODE_ID and ATTR_ID
	    ptr_op_t>		 // used by all binary operators
      data;

    explicit op_t(const kind_t _kind) : kind(_kind), refc(0){
      TRACE_CTOR(xpath_t::op_t, "const kind_t");
    }
    ~op_t() {
      TRACE_DTOR(xpath_t::op_t);

      DEBUG("ledger.xpath.memory", "Destroying " << this);
      assert(refc == 0);
    }

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

    static ptr_op_t new_node(kind_t _kind, ptr_op_t _left = NULL,
			     ptr_op_t _right = NULL);
    ptr_op_t copy(ptr_op_t _left = NULL, ptr_op_t _right = NULL) const {
      return new_node(kind, _left, _right);
    }

    static ptr_op_t wrap_value(const value_t& val);
    static ptr_op_t wrap_functor(const function_t& fobj);

    ptr_op_t compile(scope_t& scope);
    value_t  current_value(scope_t& scope);
    node_t&  current_xml_node(scope_t& scope);
    value_t  calc(scope_t& scope);

    struct print_context_t
    {
      scope_t&        scope;
      const bool      relaxed;
      const ptr_op_t& op_to_find;
      unsigned long * start_pos;
      unsigned long * end_pos;

      print_context_t(scope_t& _scope,
		      const bool      _relaxed	  = false,
		      const ptr_op_t& _op_to_find = ptr_op_t(),
		      unsigned long * _start_pos  = NULL,
		      unsigned long * _end_pos	  = NULL)
	: scope(_scope), relaxed(_relaxed), op_to_find(_op_to_find),
	  start_pos(_start_pos), end_pos(_end_pos) {}
    };

    bool print(std::ostream& out, print_context_t& context) const;
    void dump(std::ostream& out, const int depth) const;

    friend inline void intrusive_ptr_add_ref(xpath_t::op_t * op) {
      op->acquire();
    }
    friend inline void intrusive_ptr_release(xpath_t::op_t * op) {
      op->release();
    }
  };

  class op_predicate {
    ptr_op_t op;
  public:
    explicit op_predicate(ptr_op_t _op) : op(_op) {}
    bool operator()(scope_t& scope) {
      predicate_scope_t null_predicate(scope);
      return op->calc(null_predicate).to_boolean();
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

  bool print(std::ostream& out, op_t::print_context_t& context) const {
    if (ptr)
      ptr->print(out, context);
    return true;
  }

public:
  string  expr;
  flags_t flags;		// flags used to parse `expr'

  explicit xpath_t() : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR(xpath_t, "");
  }
  explicit xpath_t(ptr_op_t _ptr) : ptr(_ptr), use_lookahead(false) {
    TRACE_CTOR(xpath_t, "ptr_op_t");
  }

  explicit xpath_t(const string& _expr, flags_t _flags = XPATH_PARSE_RELAXED)
    : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR(xpath_t, "const string&, flags_t");
    if (! _expr.empty())
      parse(_expr, _flags);
  }
  explicit xpath_t(std::istream& in, flags_t _flags = XPATH_PARSE_RELAXED)
    : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR(xpath_t, "std::istream&, flags_t");
    parse(in, _flags);
  }
  xpath_t(const xpath_t& other)
    : ptr(other.ptr), use_lookahead(false),
      expr(other.expr), flags(other.flags) {
    TRACE_CTOR(xpath_t, "copy");
  }
  ~xpath_t() {
    TRACE_DTOR(xpath_t);
  }

#if 0
  xpath_t& operator=(const string& _expr) {
    parse(_expr);
    return *this;
  }
#endif
  xpath_t& operator=(const xpath_t& _expr);

#if 0
  operator ptr_op_t() throw() {
    return ptr;
  }
  operator bool() const throw() {
    return ptr != NULL;
  }
  operator string() const throw() {
    return expr;
  }
#endif

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

  void compile(scope_t& scope) {
    if (ptr.get())
      ptr = ptr->compile(scope);
  }

  value_t calc(scope_t& scope) const {
    if (ptr.get())
      return ptr->calc(scope);
    return NULL_VALUE;
  }

  static value_t eval(const string& _expr, scope_t& scope) {
    return xpath_t(_expr).calc(scope);
  }

  path_iterator_t find_all(scope_t& scope) {
    return path_iterator_t(*this, scope);
  }

  void print(std::ostream& out, scope_t& scope) const {
    op_t::print_context_t context(scope);
    print(out, context);
  }

  void dump(std::ostream& out) const {
    if (ptr)
      ptr->dump(out, 0);
  }
};

inline xpath_t::ptr_op_t
xpath_t::op_t::new_node(kind_t _kind, ptr_op_t _left, ptr_op_t _right) {
  ptr_op_t node(new op_t(_kind));
  node->set_left(_left);
  node->set_right(_right);
  return node;
}

inline xpath_t::ptr_op_t
xpath_t::op_t::wrap_value(const value_t& val) {
  xpath_t::ptr_op_t temp(new xpath_t::op_t(xpath_t::op_t::VALUE));
  temp->set_value(val);
  return temp;
}

inline xpath_t::ptr_op_t
xpath_t::op_t::wrap_functor(const function_t& fobj) {
  xpath_t::ptr_op_t temp(new xpath_t::op_t(xpath_t::op_t::FUNCTION));
  temp->set_function(fobj);
  return temp;
}

template<>
inline xpath_t::symbol_scope_t&
xpath_t::scope_t::find_scope<xpath_t::symbol_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(SYMBOL_SCOPE, skip_this);
  assert(scope);
  return downcast<symbol_scope_t>(*scope);
}

template<>
inline xpath_t::call_scope_t&
xpath_t::scope_t::find_scope<xpath_t::call_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(CALL_SCOPE, skip_this);
  assert(scope);
  return downcast<call_scope_t>(*scope);
}

template<>
inline xpath_t::context_scope_t&
xpath_t::scope_t::find_scope<xpath_t::context_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(CONTEXT_SCOPE, skip_this);
  assert(scope);
  return downcast<context_scope_t>(*scope);
}

template<>
inline optional<xpath_t::selection_scope_t&>
xpath_t::scope_t::maybe_find_scope<xpath_t::selection_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(SELECTION_SCOPE, skip_this);
  if (scope)
    return downcast<selection_scope_t>(*scope);
  else
    return none;
}

template<>
inline optional<xpath_t::predicate_scope_t&>
xpath_t::scope_t::maybe_find_scope<xpath_t::predicate_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(PREDICATE_SCOPE, skip_this);
  if (scope)
    return downcast<predicate_scope_t>(*scope);
  else
    return none;
}

#define FIND_SCOPE(scope_type, scope_ref) \
  downcast<xml::xpath_t::scope_t>(scope_ref).find_scope<scope_type>()
#define MAYBE_FIND_SCOPE(scope_type, scope_ref) \
  downcast<xml::xpath_t::scope_t>(scope_ref).maybe_find_scope<scope_type>()

#define CALL_SCOPE(scope_ref) \
  FIND_SCOPE(xml::xpath_t::call_scope_t, scope_ref)
#define SYMBOL_SCOPE(scope_ref) \
  FIND_SCOPE(xml::xpath_t::symbol_scope_t, scope_ref)
#define CONTEXT_SCOPE(scope_ref) \
  FIND_SCOPE(xml::xpath_t::context_scope_t, scope_ref)
#define SELECTION_SCOPE(scope_ref) \
  MAYBE_FIND_SCOPE(xml::xpath_t::selection_scope_t, scope_ref)
#define PREDICATE_SCOPE(scope_ref) \
  MAYBE_FIND_SCOPE(xml::xpath_t::predicate_scope_t, scope_ref)

} // namespace xml

value_t xml_command(xml::xpath_t::call_scope_t& args);

} // namespace ledger

#endif // _XPATH_H
