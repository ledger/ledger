#ifndef _XPATH_H
#define _XPATH_H

#include "xml.h"

namespace ledger {
namespace xml {

class xpath_t
{
public:
  struct op_t;

  static void initialize();
  static void shutdown();

  DECLARE_EXCEPTION(parse_error);
  DECLARE_EXCEPTION(compile_error);
  DECLARE_EXCEPTION(calc_error);

#if 0
  class context : public error_context {
  public:
    const xpath_t& xpath;
    const op_t *   err_node;

    context(const xpath_t& _xpath,
	    const op_t *   _err_node,
	    const string& desc = "") throw();
    virtual ~context() throw();

    virtual void describe(std::ostream& out) const throw();
  };
#endif

public:
  class scope_t;

  class functor_t {
  protected:
    string fname;
  public:
    bool wants_args;

    functor_t(const string& _fname, bool _wants_args = false)
      : fname(_fname), wants_args(_wants_args) {}
    virtual ~functor_t() {}

    virtual void operator()(value_t& result, scope_t * locals) = 0;
    virtual string name() const { return fname; }
  };

  template <typename T, typename U>
  class member_functor_t : public functor_t {
  public:
    T * ptr;
    U T::*dptr;

    member_functor_t(const string& _name, T * _ptr, U T::*_dptr)
      : functor_t(_name, false), ptr(_ptr), dptr(_dptr) {}

    virtual void operator()(value_t& result, scope_t * locals) {
      assert(ptr);
      assert(dptr);
      result = ptr->*dptr;
    }
  };

  template <typename T>
  class member_functor_t<T, string> : public functor_t {
  public:
    T * ptr;
    string T::*dptr;

    member_functor_t(const string& _name, T * _ptr, string T::*_dptr)
      : functor_t(_name, false), ptr(_ptr), dptr(_dptr) {}

    virtual void operator()(value_t& result, scope_t * locals) {
      assert(ptr);
      assert(dptr);
      result.set_string(ptr->*dptr);
    }
  };

  template <typename T>
  class memfun_functor_t : public functor_t {
  public:
    T * ptr;
    void (T::*mptr)(value_t& result);

    memfun_functor_t(const string& _name, T * _ptr,
		     void (T::*_mptr)(value_t& result))
      : functor_t(_name, false), ptr(_ptr), mptr(_mptr) {}

    virtual void operator()(value_t& result,
			    scope_t * locals = NULL) {
      assert(ptr);
      assert(mptr);
      assert(locals || locals == NULL);
      (ptr->*mptr)(result);
    }
  };

  template <typename T>
  class memfun_args_functor_t : public functor_t {
  public:
    T * ptr;
    void (T::*mptr)(value_t& result, scope_t * locals);

    memfun_args_functor_t(const string& _name, T * _ptr,
			  void (T::*_mptr)(value_t& result, scope_t * locals))
      : functor_t(_name, true), ptr(_ptr), mptr(_mptr) {}

    virtual void operator()(value_t& result, scope_t * locals) {
      assert(ptr);
      assert(mptr);
      (ptr->*mptr)(result, locals);
    }
  };

  static op_t * wrap_value(const value_t& val);
  static op_t * wrap_sequence(value_t::sequence_t * val);
  static op_t * wrap_functor(functor_t * fobj);
#if 0
  static op_t * wrap_mask(const string& pattern);
#endif

  template <typename T, typename U>
  static op_t *
  make_functor(const string& name, T * ptr, U T::*mptr) {
    return wrap_functor(new member_functor_t<T, U>(name, ptr, mptr));
  }

  template <typename T>
  static op_t *
  make_functor(const string& fname, T * ptr,
	       void (T::*mptr)(value_t& result)) {
    return wrap_functor(new memfun_functor_t<T>(fname, ptr, mptr));
  }

  template <typename T>
  static op_t *
  make_functor(const string& fname, T * ptr,
	       void (T::*mptr)(value_t& result, scope_t * locals)) {
    return wrap_functor(new memfun_args_functor_t<T>(fname, ptr, mptr));
  }

#define MAKE_FUNCTOR(cls, name)				\
  xml::xpath_t::make_functor(#name, this, &cls::name)

public:
  class scope_t
  {
    typedef std::map<const string, op_t *>  symbol_map;
    typedef std::pair<const string, op_t *> symbol_pair;

    symbol_map symbols;

    scope_t(const scope_t&);
    scope_t& operator=(const scope_t&);

  public:
    scope_t * parent;
    value_t   args;

    enum kind_t { NORMAL, STATIC, ARGUMENT } kind;

    scope_t(scope_t * _parent = NULL, kind_t _kind = NORMAL)
      : parent(_parent), kind(_kind) {
      TRACE_CTOR(xpath_t::scope_t, "scope *, kind_t");
    }

    virtual ~scope_t() {
      TRACE_DTOR(xpath_t::scope_t);
      for (symbol_map::iterator i = symbols.begin();
	   i != symbols.end();
	   i++)
	(*i).second->release();
    }

  public:
    virtual void define(const string& name, op_t * def);
    virtual bool resolve(const string& name, value_t& result,
			 scope_t * locals = NULL) {
      if (parent)
	return parent->resolve(name, result, locals);
      return false;
    }
    virtual op_t * lookup(const string& name);

    void define(const string& name, functor_t * def);

    friend struct op_t;
  };

  class function_scope_t : public scope_t
  {
    value_t::sequence_t * sequence;
    value_t * value;
    int index;

  public:
    function_scope_t(value_t::sequence_t * _sequence, value_t * _value,
		     int _index, scope_t * _parent = NULL)
      : scope_t(_parent, STATIC),
	sequence(_sequence), value(_value), index(_index) {}

    virtual bool resolve(const string& name, value_t& result,
			 scope_t * locals = NULL);
  };

#define XPATH_PARSE_NORMAL     0x00
#define XPATH_PARSE_PARTIAL    0x01
#define XPATH_PARSE_RELAXED    0x02
#define XPATH_PARSE_NO_MIGRATE 0x04
#define XPATH_PARSE_NO_REDUCE  0x08
#if 0
#define XPATH_PARSE_REGEXP     0x10
#endif
#define XPATH_PARSE_ALLOW_DATE 0x20

private:
  struct token_t
  {
    enum kind_t {
      IDENT,			// [A-Za-z_][-A-Za-z0-9_:]*
      VALUE,			// any kind of literal value
#if 0
      REGEXP,			// /regexp/  jww (2006-09-24): deprecate
				// in favor of a "match" function
#endif
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
#if 0
      MATCH,			// =~
      NMATCH,			// !~
      PERCENT,			// %
#endif
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
      assert(0);
      TRACE_CTOR(xpath_t::token_t, "copy");
      *this = other;
    }

    ~token_t() {
      TRACE_DTOR(xpath_t::token_t);
    }

    token_t& operator=(const token_t& other) {
      if (&other == this)
	return *this;
      assert(0);
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
    void next(std::istream& in, unsigned short flags);
    void rewind(std::istream& in);
    void unexpected();

    static void unexpected(char c, char wanted = '\0');
  };

public:
  struct op_t
  {
    enum kind_t {
      VOID,
      VALUE,

      NODE_NAME,
      NODE_ID,
      FUNC_NAME,
      ATTR_NAME,
      VAR_NAME,

      ARG_INDEX,

      CONSTANTS,		// constants end here

      FUNCTOR,
#if 0
      MASK,
#endif

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

#if 0
      O_MATCH,
      O_NMATCH,
#endif

      O_DEFINE,
      O_EVAL,
      O_ARG,

#if 0
      O_PERC,
#endif

      O_FIND,
      O_RFIND,
      O_PRED,

      LAST			// operators end here
    };

    kind_t	  kind;
    mutable short refc;
    op_t *	  left;

    union {
      value_t *	    valuep;	// used by constant VALUE
      string * name;	// used by constant SYMBOL
      unsigned int  arg_index;	// used by ARG_INDEX and O_ARG
      functor_t *   functor;	// used by terminal FUNCTOR
      unsigned int  name_id;	// used by NODE_NAME and ATTR_NAME
#if 0
      mask_t *	    mask;	// used by terminal MASK
#endif
      op_t *	    right;	// used by all operators
    };

    op_t(const kind_t _kind)
      : kind(_kind), refc(0), left(NULL), right(NULL) {
      TRACE_CTOR(xpath_t::op_t, "const kind_t");
    }
    op_t(const op_t&);
    ~op_t();

    op_t& operator=(const op_t&);

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
      DEBUG("ledger.xpath.memory",
	     "Releasing " << this << ", refc now " << refc - 1);
      assert(refc > 0);
      if (--refc == 0)
	checked_delete(this);
    }
    op_t * acquire() {
      DEBUG("ledger.xpath.memory",
	     "Acquiring " << this << ", refc now " << refc + 1);
      assert(refc >= 0);
      refc++;
      return this;
    }
    const op_t * acquire() const {
      DEBUG("ledger.xpath.memory",
	     "Acquiring " << this << ", refc now " << refc + 1);
      assert(refc >= 0);
      refc++;
      return this;
    }

    void set_left(op_t * expr) {
      assert(kind > TERMINALS);
      if (left)
	left->release();
      left = expr ? expr->acquire() : NULL;
    }

    void set_right(op_t * expr) {
      assert(kind > TERMINALS);
      if (right)
	right->release();
      right = expr ? expr->acquire() : NULL;
    }

    static op_t * new_node(kind_t kind, op_t * left = NULL,
			   op_t * right = NULL);

    op_t * copy(op_t * left = NULL,
		op_t * right = NULL) const;
    op_t * compile(value_t * context, scope_t * scope,
		   bool resolve = false);

    void find_values(value_t * context, scope_t * scope,
		     value_t::sequence_t& result_seq, bool recursive);
    bool test_value(value_t * context, scope_t * scope, int index = 0);

    void append_value(value_t& value, value_t::sequence_t& result_seq);

    static op_t * defer_sequence(value_t::sequence_t& result_seq);

    bool write(std::ostream&   out,
	       const bool      relaxed	  = true,
	       const op_t *    op_to_find = NULL,
	       unsigned long * start_pos  = NULL,
	       unsigned long * end_pos	  = NULL) const;

    void dump(std::ostream& out, const int depth) const;
  };

public:
  op_t * ptr;

  xpath_t& operator=(op_t * _expr) {
    expr = "";
    reset(_expr);
    return *this;
  }

  op_t& operator*() throw() {
    return *ptr;
  }
  const op_t& operator*() const throw() {
    return *ptr;
  }
  op_t * operator->() throw() {
    return ptr;
  }
  const op_t * operator->() const throw() {
    return ptr;
  }

  op_t * get() throw() { return ptr; }
  const op_t * get() const throw() { return ptr; }

  op_t * release() throw() {
    op_t * tmp = ptr;
    ptr = 0;
    return tmp;
  }

  void reset(op_t * p = 0) throw() {
    if (p != ptr) {
      if (ptr)
	ptr->release();
      ptr = p;
    }
  }

#ifdef THREADSAFE
  mutable token_t   lookahead;
#else
  static  token_t * lookahead;
#endif
  mutable bool	    use_lookahead;

  token_t& next_token(std::istream& in, unsigned short tflags) const {
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

  op_t * parse_value_term(std::istream& in, unsigned short flags) const;
  op_t * parse_predicate_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_path_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_unary_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_union_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_mul_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_add_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_logic_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_and_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_or_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_querycolon_expr(std::istream& in, unsigned short flags) const;
  op_t * parse_value_expr(std::istream& in, unsigned short flags) const;

  op_t * parse_expr(std::istream& in,
		    unsigned short flags = XPATH_PARSE_RELAXED) const;

  op_t * parse_expr(const string& str,
		    unsigned short tflags = XPATH_PARSE_RELAXED) const
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

  op_t * parse_expr(const char * p,
		    unsigned short tflags = XPATH_PARSE_RELAXED) const {
    return parse_expr(string(p), tflags);
  }

  bool write(std::ostream&   out,
	     const bool      relaxed,
	     const op_t *  op_to_find,
	     unsigned long * start_pos,
	     unsigned long * end_pos) const {
    if (ptr)
      ptr->write(out, relaxed, op_to_find, start_pos, end_pos);
    return true;
  }

public:
  string    expr;
  unsigned short flags;		// flags used to parse `expr'

  xpath_t() : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR(xpath_t, "");
  }
  xpath_t(op_t * _ptr) : ptr(_ptr), use_lookahead(false) {
    TRACE_CTOR(xpath_t, "op_t *");
  }

  xpath_t(const string& _expr,
	  unsigned short _flags = XPATH_PARSE_RELAXED)
    : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR(xpath_t, "const string&, unsigned short");
    if (! _expr.empty())
      parse(_expr, _flags);
  }
  xpath_t(std::istream& in, unsigned short _flags = XPATH_PARSE_RELAXED)
    : ptr(NULL), use_lookahead(false), flags(0) {
    TRACE_CTOR(xpath_t, "std::istream&, unsigned short");
    parse(in, _flags);
  }
  xpath_t(const xpath_t& other)
    : ptr(other.ptr ? other.ptr->acquire() : NULL),
      use_lookahead(false), expr(other.expr), flags(other.flags) {
    TRACE_CTOR(xpath_t, "copy");
  }
  virtual ~xpath_t() {
    TRACE_DTOR(xpath_t);
    reset(NULL);
  }

  xpath_t& operator=(const string& _expr) {
    parse(_expr);
    return *this;
  }
  xpath_t& operator=(const xpath_t& _expr);
  xpath_t& operator=(xpath_t& _xpath) {
    ptr	  = _xpath.ptr->acquire();
    expr  = _xpath.expr;
    flags = _xpath.flags;
    use_lookahead = false;
    return *this;
  }

  operator op_t *() throw() {
    return ptr;
  }

  operator bool() const throw() {
    return ptr != NULL;
  }
  operator string() const throw() {
    return expr;
  }

  void parse(const string& _expr, unsigned short _flags = XPATH_PARSE_RELAXED) {
    expr  = _expr;
    flags = _flags;
    op_t * tmp = parse_expr(_expr, _flags);
    assert(tmp);
    reset(tmp ? tmp->acquire() : NULL);
  }
  void parse(std::istream& in, unsigned short _flags = XPATH_PARSE_RELAXED) {
    expr  = "";
    flags = _flags;
    op_t * tmp = parse_expr(in, _flags);
    assert(tmp);
    reset(tmp ? tmp->acquire() : NULL);
  }

  void compile(const string& _expr, scope_t * scope = NULL,
	       unsigned short _flags = XPATH_PARSE_RELAXED) {
    parse(_expr, _flags);
    // jww (2006-09-24): fix
    compile((node_t *)NULL, scope);
  }
  void compile(std::istream& in, scope_t * scope = NULL,
	       unsigned short _flags = XPATH_PARSE_RELAXED) {
    parse(in, _flags);
    // jww (2006-09-24): fix
    compile((node_t *)NULL, scope);
  }

  void compile(document_t * document, scope_t * scope = NULL) {
    if (! document) {
      document_t tdoc;
      compile(tdoc.top, scope);
    } else {
      compile(document->top, scope);
    }
  }
  void compile(node_t * top_node, scope_t * scope = NULL) {
    if (ptr) {
      value_t noderef(top_node);
      op_t * compiled = ptr->compile(&noderef, scope);
      if (compiled == ptr)
	compiled->release();
      else
	reset(compiled);
    }
  }

  virtual void calc(value_t& result, node_t * node, scope_t * scope = NULL) const;

  virtual value_t calc(document_t * document, scope_t * scope = NULL) const {
    if (! ptr)
      return 0L;
    value_t temp;
    calc(temp, document ? document->top : NULL, scope);
    return temp;
  }
  virtual value_t calc(node_t * tcontext, scope_t * scope = NULL) const {
    if (! ptr)
      return 0L;
    value_t temp;
    calc(temp, tcontext, scope);
    return temp;
  }

  static void eval(value_t& result, const string& _expr,
		   document_t * document, scope_t * scope = NULL) {
    xpath_t temp(_expr);
    temp.calc(result, document->top, scope);
  }
  static value_t eval(const string& _expr, document_t * document,
		      scope_t * scope = NULL) {
    xpath_t temp(_expr);
    return temp.calc(document, scope);
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

inline std::ostream& operator<<(std::ostream& out, const xpath_t::op_t& op) {
  op.write(out);
  return out;
};

} // namespace xml

template <typename T>
inline T * get_ptr(xml::xpath_t::scope_t * locals, unsigned int idx) {
  assert(locals->args.size() > idx);
  T * ptr = static_cast<T *>(locals->args[idx].to_pointer());
  assert(ptr);
  return ptr;
}

class xml_command : public xml::xpath_t::functor_t
{
 public:
  xml_command() : xml::xpath_t::functor_t("xml") {}

  virtual void operator()(value_t&, xml::xpath_t::scope_t * locals) {
    std::ostream *    out = get_ptr<std::ostream>(locals, 0);
    xml::document_t * doc = get_ptr<xml::document_t>(locals, 1);

    doc->write(*out);
  }
};

} // namespace ledger

#endif // _XPATH_H
