#ifndef _VALEXPR_H
#define _VALEXPR_H

#include "value.h"
#include "error.h"
#include "mask.h"

#include <map>
#include <list>
#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>

namespace ledger {

class valexpr_t
{
  struct node_t;

 public:
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
  public:
    virtual ~functor_t() {}
    virtual value_t operator()(scope_t * args) = 0;

    virtual std::string name() const {
      return "<func>";
    }
  };

 private:
  static node_t * wrap_value(const value_t& val);
  static node_t * wrap_functor(functor_t * fobj);
  static node_t * wrap_mask(const std::string& pattern);

 public:
  class scope_t
  {
    scope_t * parent;

    typedef std::map<const std::string, node_t *>  symbol_map;
    typedef std::pair<const std::string, node_t *> symbol_pair;

    symbol_map symbols;

  public:
    typedef std::vector<node_t *> args_list;

    args_list args;
    bool      arg_scope;

    scope_t(scope_t * _parent = NULL)
      : parent(_parent), arg_scope(false) {}

    virtual ~scope_t() {
      for (symbol_map::iterator i = symbols.begin();
	   i != symbols.end();
	   i++)
	(*i).second->release();

      for (args_list::iterator i = args.begin();
	   i != args.end();
	   i++)
	(*i)->release();
    }

   private:
    virtual void define(const std::string& name, node_t * def);
    virtual node_t * lookup(const std::string& name) const;

    friend struct node_t;

   public:
    void define(const std::string& name, functor_t * def);
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

    union {
      value_t * value;
      char      symbol[3];
    };

    unsigned int length;

    token_t() : kind(UNKNOWN), value(NULL), length(0) {}

    token_t(const token_t& other)
      : kind(other.kind), length(other.length) {
      switch (kind) {
      case REGEXP:
      case IDENT:
      case VALUE:
	value = new value_t(*other.value);
	break;
      default:
	std::memcpy(symbol, other.symbol, 3);
	break;
      }
    }

    ~token_t() {
      switch (kind) {
      case REGEXP:
      case IDENT:
      case VALUE:
	assert(value);
	delete value;
	break;
      default:
	break;
      }
    }

    void parse_ident(std::istream& in);
    static token_t next(std::istream& in, unsigned short flags);
    token_t token_t::rewind(std::istream& in);
    void unexpected();
    static void unexpected(char c, char wanted = '\0');
  };

  struct node_t
  {
    enum kind_t {
      VALUE,
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
      O_LOOKUP,
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
      DEBUG_PRINT("ledger.memory.ctors", "ctor node_t " << this);
    }
    node_t(const node_t&) {
      DEBUG_PRINT("ledger.memory.ctors", "ctor node_t (copy) " << this);
      assert(0);
    }
    ~node_t();

    bool constant() const {
      return kind == VALUE;
    }
    value_t value() const;

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
	       const bool      relaxed,
	       const node_t *  node_to_find,
	       unsigned long * start_pos,
	       unsigned long * end_pos) const;

    void dump(std::ostream& out, const int depth) const;
  };

  node_t * ptr;

 private:
  valexpr_t(node_t * _ptr) : ptr(_ptr) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor valexpr_t");
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

  mutable std::list<token_t> token_stack;

  token_t next_token(std::istream& in, unsigned short flags) const {
    if (token_stack.size() > 0) {
      token_t tok = token_stack.back();
      token_stack.pop_back();
      return tok;
    } else {
      return token_t::next(in, flags);
    }
  }
  void push_token(const token_t& tok) const {
    token_stack.push_back(tok);
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

  valexpr_t() : ptr(NULL) {}

  valexpr_t(const std::string& _expr,
	    unsigned short _flags = PARSE_VALEXPR_RELAXED)
    : ptr(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor valexpr_t");
    if (! _expr.empty())
      parse(_expr, _flags);
    else
      ptr = NULL;
  }
  valexpr_t(const valexpr_t& other)
    : ptr(other.ptr ? other.ptr->acquire() : NULL),
      expr(other.expr), flags(other.flags) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor valexpr_t");
  }
  virtual ~valexpr_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor valexpr_t");
    if (ptr)
      ptr->release();
  }

  valexpr_t& operator=(const std::string& _expr) {
    parse(_expr);
    return *this;
  }
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
