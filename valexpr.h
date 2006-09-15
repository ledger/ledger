#ifndef _VALEXPR_H
#define _VALEXPR_H

#include "value.h"
#include "error.h"
#include "mask.h"

#include <map>
#include <list>
#include <deque>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>

namespace ledger {

class valexpr_t
{
 public:
  struct node_t;

  class parse_error : public error {
  public:
    parse_error(const std::string& reason,
		  error_context * ctxt = NULL) throw()
      : error(reason, ctxt) {}
    virtual ~parse_error() throw() {}
  };

  class compile_error : public error {
  public:
    compile_error(const std::string& reason,
		  error_context * ctxt = NULL) throw()
      : error(reason, ctxt) {}
    virtual ~compile_error() throw() {}
  };

  class calc_error : public error {
  public:
    calc_error(const std::string& reason,
		     error_context * ctxt = NULL) throw()
      : error(reason, ctxt) {}
    virtual ~calc_error() throw() {}
  };

  class context : public error_context {
  public:
    const valexpr_t& valexpr;
    const node_t *   err_node;

    context(const valexpr_t& _valexpr,
	    const node_t *   _err_node,
	    const std::string& desc = "") throw();
    virtual ~context() throw();

    virtual void describe(std::ostream& out) const throw();
  };

 public:
  class scope_t;

  class functor_t {
    std::string fname;
  public:
    bool wants_args;

    functor_t(const std::string& _fname, bool _wants_args = false)
      : fname(_fname), wants_args(_wants_args) {}
    virtual ~functor_t() {}

    virtual void operator()(value_t& result, scope_t * locals) = 0;
    virtual std::string name() const { return fname; }
  };

  template <typename T, typename U>
  class member_functor_t : public functor_t {
  public:
    T * ptr;
    U T::*dptr;

    member_functor_t(const std::string& name, T * _ptr, U T::*_dptr)
      : functor_t(name, false), ptr(_ptr), dptr(_dptr) {}

    virtual void operator()(value_t& result, scope_t * locals) {
      result = ptr->*dptr;
    }
  };

  template <typename T>
  class memfun_functor_t : public functor_t {
  public:
    T * ptr;
    void (T::*mptr)(value_t& result);

    memfun_functor_t(const std::string& name, T * _ptr,
		     void (T::*_mptr)(value_t& result))
      : functor_t(name, false), ptr(_ptr), mptr(_mptr) {}

    virtual void operator()(value_t& result, scope_t * locals = NULL) {
      (ptr->*mptr)(result);
    }
  };

  template <typename T>
  class memfun_args_functor_t : public functor_t {
  public:
    T * ptr;
    void (T::*mptr)(value_t& result, scope_t * locals);

    memfun_args_functor_t(const std::string& name, T * _ptr,
			  void (T::*_mptr)(value_t& result, scope_t * locals))
      : functor_t(name, true), ptr(_ptr), mptr(_mptr) {}

    virtual void operator()(value_t& result, scope_t * locals) {
      (ptr->*mptr)(result, locals);
    }
  };

  static node_t * wrap_value(const value_t& val);
  static node_t * wrap_functor(functor_t * fobj);
  static node_t * wrap_mask(const std::string& pattern);

  template <typename T, typename U>
  static node_t *
  make_functor(const std::string& name = "<data>", T * ptr, U T::*mptr) {
    return wrap_functor(new member_functor_t<T, U>(name, ptr, mptr));
  }

  template <typename T>
  static node_t *
  make_functor(const std::string& fname = "<func>", T * ptr,
	       void (T::*mptr)(value_t& result)) {
    return wrap_functor(new memfun_functor_t<T>(fname, ptr, mptr));
  }

  template <typename T>
  static node_t *
  make_functor(const std::string& fname = "<func>", T * ptr,
	       void (T::*mptr)(value_t& result, scope_t * locals)) {
    return wrap_functor(new memfun_args_functor_t<T>(fname, ptr, mptr));
  }

#define MAKE_FUNCTOR(cls, name)				\
  valexpr_t::make_functor(#name, this, &cls::name)

 public:
  class scope_t
  {
    typedef std::map<const std::string, node_t *>  symbol_map;
    typedef std::pair<const std::string, node_t *> symbol_pair;

    symbol_map symbols;

    scope_t(const scope_t&);
    scope_t& operator=(const scope_t&);

   public:
    scope_t * parent;

    typedef std::deque<value_t> args_list;

    args_list args;
    bool      arg_scope;

    scope_t(scope_t * _parent = NULL, bool _arg_scope = false)
      : parent(_parent), arg_scope(_arg_scope) {
      TRACE_CTOR("valexpr_t::scope_t(scope *)");
    }

    virtual ~scope_t() {
      TRACE_DTOR("valexpr_t::scope_t");
      for (symbol_map::iterator i = symbols.begin();
	   i != symbols.end();
	   i++)
	(*i).second->release();
    }

   public:
    virtual void define(const std::string& name, node_t * def);
    virtual node_t * lookup(const std::string& name);

    void define(const std::string& name, functor_t * def);

    friend struct node_t;
  };

#define PARSE_VALEXPR_NORMAL	 0x00
#define PARSE_VALEXPR_PARTIAL	 0x01
#define PARSE_VALEXPR_RELAXED	 0x02
#define PARSE_VALEXPR_NO_MIGRATE 0x04
#define PARSE_VALEXPR_NO_REDUCE  0x08
#define PARSE_VALEXPR_REGEXP     0x10

 private:
  struct token_t
  {
    enum kind_t {
      IDENT,			// [A-Za-z_][A-Za-z0-9_.]*
      VALUE,			// [[-] Commodity [-]] Number [Commodity]
      REGEXP,			// /regexp/
      LPAREN,			// (
      RPAREN,			// )
      EXCLAM,			// !
      NEQUAL,			// !=
      MINUS,			// -
      PLUS,			// +
      STAR,			// *
      POWER,			// **
      SLASH,			// /
      ASSIGN,			// =
      EQUAL,			// ==
      LESS,			// <
      LESSEQ,			// <=
      GREATER,			// >
      GREATEREQ,		// >=
      AMPER,			// &
      PIPE,			// |
      QUESTION,			// ?
      COLON,			// :
      COMMA,			// ,
      MATCH,			// =~
      PERCENT,			// %
      TOK_EOF,
      UNKNOWN
    } kind;

    char	 symbol[3];
    value_t	 value;
    unsigned int length;

    token_t() : kind(UNKNOWN), length(0) {
      TRACE_CTOR("valexpr_t::token_t()");
    }

    token_t(const token_t& other) {
      assert(0);
      TRACE_CTOR("valexpr_t::token_t(copy)");
      *this = other;
    }

    ~token_t() {
      TRACE_DTOR("valexpr_t::token_t");
    }

    token_t& operator=(const token_t& other) {
      if (&other == this)
	return *this;
      assert(0);
    }

    void clear() {
      kind   = UNKNOWN;
      length = 0;

      symbol[0] = '\0';
      symbol[1] = '\0';
      symbol[2] = '\0';
    }

    void parse_ident(std::istream& in);
    void next(std::istream& in, unsigned short flags);
    void rewind(std::istream& in);
    void unexpected();

    static void unexpected(char c, char wanted = '\0');
  };

 public:
  struct node_t
  {
    enum kind_t {
      VALUE,
      SYMBOL,
      ARG_INDEX,

      CONSTANTS,		// constants end here

      FUNCTOR,
      MASK,

      TERMINALS,		// terminals end here

      O_NOT,
      O_NEG,

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

      O_MATCH,

      O_DEFINE,
      O_EVAL,
      O_ARG,

      O_PERC,

      LAST			// operators end here
    };

    kind_t	  kind;
    mutable short refc;
    node_t *      left;

    union {
      value_t *	   valuep;	// used by constant VALUE
      unsigned int arg_index;	// used by ARG_INDEX and O_ARG
      functor_t *  functor;	// used by terminal FUNCTOR
      mask_t *	   mask;	// used by terminal MASK
      node_t *	   right;	// used by all operators
    };

    node_t(const kind_t _kind)
      : kind(_kind), refc(0), left(NULL), right(NULL) {
      TRACE_CTOR("valexpr_t::node_t(const kind_t)");
    }
    node_t(const node_t&);
    ~node_t();

    node_t& operator=(const node_t&);

    bool constant() const {
      return kind == VALUE;
    }
    void get_value(value_t& result) const;
    value_t value() const {
      value_t temp;
      get_value(temp);
      return temp;
    }

    functor_t * functor_obj() const {
      if (kind == FUNCTOR)
	return functor;
      else
	return NULL;
    }

    void release() const {
      DEBUG_PRINT("ledger.valexpr.memory",
		  "Releasing " << this << ", refc now " << refc - 1);
      assert(refc > 0);
      if (--refc == 0)
	delete this;
    }
    node_t * acquire() {
      DEBUG_PRINT("ledger.valexpr.memory",
		  "Acquiring " << this << ", refc now " << refc + 1);
      assert(refc >= 0);
      refc++;
      return this;
    }
    const node_t * acquire() const {
      DEBUG_PRINT("ledger.valexpr.memory",
		  "Acquiring " << this << ", refc now " << refc + 1);
      assert(refc >= 0);
      refc++;
      return this;
    }

    void set_left(node_t * expr) {
      assert(kind > TERMINALS);
      if (left)
	left->release();
      left = expr ? expr->acquire() : NULL;
    }

    void set_right(node_t * expr) {
      assert(kind > TERMINALS);
      if (right)
	right->release();
      right = expr ? expr->acquire() : NULL;
    }

    static node_t * new_node(kind_t kind, node_t * left = NULL,
			     node_t * right = NULL);

    node_t * copy(node_t * left = NULL,
		  node_t * right = NULL) const;
    node_t * compile(scope_t * scope);
    node_t * lookup(scope_t * scope) const;

    bool write(std::ostream&   out,
	       const bool      relaxed	    = true,
	       const node_t *  node_to_find = NULL,
	       unsigned long * start_pos    = NULL,
	       unsigned long * end_pos	    = NULL) const;

    void dump(std::ostream& out, const int depth) const;
  };

 private:
  node_t * ptr;

  valexpr_t(node_t * _ptr) : ptr(_ptr), use_lookahead(false) {
    TRACE_CTOR("valexpr_t(node_t *)");
  }

  valexpr_t& operator=(node_t * _expr) {
    expr = "";
    reset(_expr);
    return *this;
  }

  node_t& operator*() throw() {
    return *ptr;
  }
  const node_t& operator*() const throw() {
    return *ptr;
  }
  node_t * operator->() throw() {
    return ptr;
  }
  const node_t * operator->() const throw() {
    return ptr;
  }

  node_t * get() throw() { return ptr; }
  const node_t * get() const throw() { return ptr; }

  node_t * release() throw() {
    node_t * tmp = ptr;
    ptr = 0;
    return tmp;
  }

  void reset(node_t * p = 0) throw() {
    if (p != ptr) {
      if (ptr)
	ptr->release();
      ptr = p;
    }
  }

#ifdef THREADED
  mutable token_t lookahead;
#else
  static  token_t lookahead;
#endif
  mutable bool    use_lookahead;

  token_t& next_token(std::istream& in, unsigned short flags) const {
    if (use_lookahead)
      use_lookahead = false;
    else
      lookahead.next(in, flags);
    return lookahead;
  }
  void push_token(const token_t& tok) const {
    assert(&tok == &lookahead);
    use_lookahead = true;
  }

  node_t * parse_value_term(std::istream& in, unsigned short flags) const;
  node_t * parse_unary_expr(std::istream& in, unsigned short flags) const;
  node_t * parse_mul_expr(std::istream& in, unsigned short flags) const;
  node_t * parse_add_expr(std::istream& in, unsigned short flags) const;
  node_t * parse_logic_expr(std::istream& in, unsigned short flags) const;
  node_t * parse_boolean_expr(std::istream& in, unsigned short flags) const;
  node_t * parse_value_expr(std::istream& in, unsigned short flags) const;

  node_t * parse_expr(std::istream& in,
		      unsigned short flags = PARSE_VALEXPR_RELAXED) const;

  node_t * parse_expr(const std::string& str,
		      unsigned short flags = PARSE_VALEXPR_RELAXED) const
  {
    std::istringstream stream(str);
    try {
      return parse_expr(stream, flags);
    }
    catch (error * err) {
      err->context.push_back
	(new line_context(str, (long)stream.tellg() - 1,
			  "While parsing value expression:"));
      throw err;
    }
  }

  node_t * parse_expr(const char * p,
		      unsigned short flags = PARSE_VALEXPR_RELAXED) const {
    return parse_expr(std::string(p), flags);
  }

  bool write(std::ostream&   out,
	     const bool      relaxed,
	     const node_t *  node_to_find,
	     unsigned long * start_pos,
	     unsigned long * end_pos) const {
    ptr->write(out, relaxed, node_to_find, start_pos, end_pos);
  }

 public:
  std::string    expr;
  unsigned short flags;		// flags used to parse `expr'

  valexpr_t() : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR("valexpr_t");
  }

  valexpr_t(const std::string& _expr,
	    unsigned short _flags = PARSE_VALEXPR_RELAXED)
    : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR("valexpr_t(const std::string&, unsigned short)");
    if (! _expr.empty())
      parse(_expr, _flags);
  }
  valexpr_t(std::istream& in, unsigned short _flags = PARSE_VALEXPR_RELAXED)
    : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR("valexpr_t(std::istream&, unsigned short)");
    parse(in, _flags);
  }
  valexpr_t(const valexpr_t& other)
    : ptr(other.ptr ? other.ptr->acquire() : NULL),
      use_lookahead(false), expr(other.expr), flags(other.flags) {
    TRACE_CTOR("valexpr_t(copy)");
  }
  virtual ~valexpr_t() {
    TRACE_DTOR("valexpr_t");
    if (ptr)
      ptr->release();
  }

  valexpr_t& operator=(const std::string& _expr) {
    parse(_expr);
    return *this;
  }
  valexpr_t& operator=(const valexpr_t& _expr);
  valexpr_t& operator=(valexpr_t& _expr) {
    expr  = _expr.expr;
    flags = _expr.flags;
    reset(_expr.get());
    return *this;
  }

  operator node_t *() throw() {
    return ptr;
  }

  operator bool() const throw() {
    return ptr != NULL;
  }
  operator std::string() const throw() {
    return expr;
  }

  void parse(const std::string& _expr,
	     unsigned short _flags = PARSE_VALEXPR_RELAXED) {
    expr  = _expr;
    flags = _flags;
    reset(parse_expr(_expr, _flags)->acquire());
  }

  void parse(std::istream& in,
	     unsigned short _flags = PARSE_VALEXPR_RELAXED) {
    expr  = "";
    flags = _flags;
    reset(parse_expr(in, _flags)->acquire());
  }

  void compile(scope_t * scope = NULL) {
    if (ptr) {
      node_t * compiled = ptr->compile(scope);
      if (compiled == ptr)
	compiled->release();
      reset(compiled);
    }
  }

  virtual void calc(value_t& result, scope_t * scope = NULL) const;
  virtual value_t calc(scope_t * scope = NULL) const {
    value_t temp;
    calc(temp, scope);
    return temp;
  }

  void write(std::ostream& out) const {
    write(out, true, NULL, NULL, NULL);
  }
  void dump(std::ostream& out) const {
    if (ptr)
      ptr->dump(out, 0);
  }

  friend class scope_t;
};

} // namespace ledger

#endif // _VALEXPR_H
