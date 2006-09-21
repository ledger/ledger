#ifndef _XPATH_H
#define _XPATH_H

#include "valexpr.h"

namespace ledger {

class xpath_t
{
  // Traversal tree node interface
  struct traversable : public valexpr_t::scope_t
  {
    traversable(valexpr_t::scope_t * parent = NULL)
      : valexpr_t::scope_t(parent, false, true) {}

    virtual traversable * current() {
      return this;
    }
    virtual const traversable * current() const {
      return this;
    }

    virtual const char * name() const = 0;

    virtual traversable *& parent() = 0;
    virtual const traversable * parent() const = 0;

    virtual traversable *& next() = 0;
    virtual const traversable * next() const = 0;

    virtual traversable *& children() = 0;
    virtual const traversable * children() const = 0;

    virtual int position();
    virtual int last();

    virtual bool resolve(const std::string& name, value_t& result,
			 scope_t * locals = NULL);
  };

  struct pointer_t : public traversable
  {
    traversable * ptr;
    traversable * parent_ptr;
    traversable * next_ptr;
    traversable * children_ptr;

    pointer_t(traversable * _ptr, traversable * _parent = NULL)
      : traversable(_parent), ptr(_ptr), parent_ptr(_parent),
	next_ptr(NULL), children_ptr(NULL) {}

    virtual const char * name() const {
      return ptr->name();
    }

    virtual traversable *& parent() {
      return parent_ptr;
    }
    virtual const traversable * parent() const {
      return parent_ptr;
    }

    virtual traversable *& next() {
      return next_ptr;
    }
    virtual const traversable * next() const {
      return next_ptr;
    }

    virtual traversable *& children() {
      return children_ptr;
    }
    virtual const traversable * children() const {
      return children_ptr;
    }
  };

  struct predicate_t
  {
    bool      sort;
    valexpr_t valexpr;

    predicate_t() : sort(false) {}
  };

  struct element_t
  {
    struct element_t * next;

    enum direction_t {
      NONE, ALL, NODE, ATTRIBUTE, ROOT, PARENT, CURRENT
    } target;

    bool recurse;

    std::string name;
    std::list<predicate_t> predicates;

    element_t() : next(NULL), target(NONE), recurse(false) {
      TRACE_CTOR("xpath_t::element_t()");
    }

    element_t(const std::string& _name, bool attribute = false)
      : next(NULL), target(attribute ? ATTRIBUTE : NODE),
	recurse(false), name(_name) {
      TRACE_CTOR("xpath_t::element_t(const std::string&)");
    }

    ~element_t() {
      TRACE_DTOR("xpath_t::element_t");
      if (next)
	delete next;
    }

  private:
    element_t(const element_t&);
    element_t& operator=(const element_t&);
  };

 private:
  std::list<const element_t *> paths;

  xpath_t(const xpath_t&);
  xpath_t& operator=(const xpath_t&);

 public:
  std::string path;

  xpath_t(const std::string& _path) : path(_path) {
    TRACE_CTOR("xpath_t()");
    if (! path.empty())
      parse(path);
  }

  ~xpath_t() {
    TRACE_DTOR("xpath_t");
  }

  void clear();
  void parse(const std::string& expr);

  static pointer_t * find(const std::string& expr, traversable * node) {
    xpath_t xpath(expr);
    return xpath.find(node);
  }
  pointer_t * find(traversable * node);

  void eval(value_t& result, traversable * node);

  template <typename T>
  struct callback_t {
    virtual void operator()(traversable * item) {
      (*this)(dynamic_cast<T *>(item));
    }
    virtual void operator()(T * item) = 0;
  };

  template <typename T>
  static void select_all(traversable * node, callback_t<T>& callback)
  {
    traversable * child = node->children();
    while (child) {
      traversable * _next = child->next();
      select_all(child, callback); // allowed to destroy `child'!
      child = _next;
    }
    callback(node);
  }

  template <typename T>
  static void select(const std::string& expr, traversable * node,
		     callback_t<T>& callback)
  {
    xpath_t xpath(expr);
    xpath.select(node, callback);
  }

  template <typename T>
  void select(traversable * node, callback_t<T>& callback)
  {
  }

 private:
  static const element_t * parse_subselector(const char *& p);

  pointer_t * find(const element_t * element, traversable * node);

 public:
  void write(std::ostream& out);
};

template <>
struct xpath_t::callback_t<xpath_t::traversable> {
  virtual void operator()(xpath_t::traversable * item) = 0;
};

} // namespace ledger

#endif // _XPATH_H
