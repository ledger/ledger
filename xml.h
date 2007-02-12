#ifndef _XML_H
#define _XML_H

#include "value.h"
#include "debug.h"

extern "C" {
#if defined(HAVE_EXPAT)
#include <expat.h>           // expat XML parser
#elif defined(HAVE_XMLPARSE)
#include <xmlparse.h>        // expat XML parser
#endif
}

namespace ledger {

class journal_t;

namespace xml {

class node_t;

class document_t
{
  const char ** builtins;
  const int	builtins_size;

  typedef std::deque<std::string> names_array;

  names_array names;

  typedef std::map<std::string, int>  names_map;
  typedef std::pair<std::string, int> names_pair;

  names_map names_index;

 public:
  journal_t * journal;
  node_t * top;

  enum special_names_t {
    CURRENT, PARENT, ROOT, ALL
  };

  // Ids 0-9 are reserved.  10-999 are for "builtin" names.  1000+ are
  // for dynamically registered names.

  document_t(journal_t * _journal = NULL, const char ** _builtins = NULL,
	     const int _builtins_size = 0);

  int register_name(const std::string& name);
  int lookup_name_id(const std::string& name) const;
  const char * lookup_name(int id) const;

  void write(std::ostream& out) const;
};

#define XML_NODE_IS_PARENT 0x1

class parent_node_t;

class node_t
{
public:
  int		  name_id;
#ifdef THREADSAFE
  document_t *	  document;
#else
  static document_t * document;
#endif
  parent_node_t * parent;
  node_t *	  next;
  node_t *	  prev;
  unsigned int	  flags;
  void *	  info;

  typedef std::map<std::string, std::string>  attrs_map;
  typedef std::pair<std::string, std::string> attrs_pair;

  attrs_map * attrs;

  node_t(document_t * _document, parent_node_t * _parent = NULL,
	 unsigned int _flags = 0);

  virtual ~node_t() {
    TRACE_DTOR("node_t");
    if (parent) extract();
    if (attrs) delete attrs;
  }

  void extract();

  virtual const char * text() const {
    assert(0);
  }

  const char * name() const {
    return document->lookup_name(name_id);
  }
  int set_name(const char * _name) {
    name_id = document->register_name(_name);
    return name_id;
  }
  int set_name(int _name_id) {
    name_id = _name_id;
    return name_id;
  }

  void set_attr(const char * n, const char * v) {
    if (! attrs)
      attrs = new attrs_map;
    std::pair<attrs_map::iterator, bool> result =
      attrs->insert(attrs_pair(n, v));
    assert(result.second);
  }
  const char * get_attr(const char * n) {
    if (attrs) {
      attrs_map::iterator i = attrs->find(n);
      if (i != attrs->end())
	return (*i).second.c_str();
    }
    return NULL;
  }

  virtual void write(std::ostream& out, int depth = 0) const = 0;

private:
  node_t(const node_t&);
  node_t& operator=(const node_t&);
};

class parent_node_t : public node_t
{
public:
  node_t * children;
  node_t * last_child;

  parent_node_t(document_t * _document, parent_node_t * _parent = NULL)
    : node_t(_document, _parent, XML_NODE_IS_PARENT),
      children(NULL), last_child(NULL)
  {
    TRACE_CTOR("parent_node_t(document_t *, node_t *)");
  }
  virtual ~parent_node_t() {
    TRACE_DTOR("parent_node_t");
    if (children) clear();
  }

  virtual void clear();
  virtual void add_child(node_t * node);

  void write(std::ostream& out, int depth = 0) const;

private:
  parent_node_t(const parent_node_t&);
  parent_node_t& operator=(const parent_node_t&);
};

class terminal_node_t : public node_t
{
public:
  std::string data;

  terminal_node_t(document_t * _document, parent_node_t * _parent = NULL)
    : node_t(_document, _parent)
  {
    TRACE_CTOR("terminal_node_t(document_t *, node_t *)");
  }

  virtual const char * text() const {
    return data.c_str();
  }

  void write(std::ostream& out, int depth = 0) const;

private:
  terminal_node_t(const node_t&);
  terminal_node_t& operator=(const node_t&);
};

#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

class parser_t
{
 public:
  document_t *	      document;
  XML_Parser	      parser;
  std::string	      have_error;
  const char *	      pending;
  node_t::attrs_map * pending_attrs;
  bool                handled_data;

  std::list<parent_node_t *> node_stack;

  parser_t() : document(NULL), pending(NULL), pending_attrs(NULL),
	       handled_data(false) {}

  virtual bool         test(std::istream& in) const;
  virtual document_t * parse(std::istream& in,
			     const char ** builtins = NULL,
			     const int builtins_size = 0);
};

class parse_error : public error {
 public:
  parse_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~parse_error() throw() {}
};

#endif

} // namespace xml
} // namespace ledger

#endif // _XML_H
