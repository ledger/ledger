#ifndef _XML_H
#define _XML_H

#include "value.h"
#include "debug.h"

#include <map>
#include <deque>
#include <string>

extern "C" {
#if defined(HAVE_EXPAT)
#include <expat.h>           // expat XML parser
#elif defined(HAVE_XMLPARSE)
#include <xmlparse.h>        // expat XML parser
#endif
}

namespace xml {

class node_t;

class document_t
{
  const char ** builtins;
  const int	builtins_size;

  typedef std::deque<std::string> names_array;

  names_array names;

  typedef std::map<const char *, int>  names_map;
  typedef std::pair<const char *, int> names_pair;

  names_map names_index;

 public:
  node_t * top;

  enum special_names_t {
    CURRENT, PARENT, ROOT, ALL
  };

  // Ids 0-9 are reserved.  10-999 are for "builtin" names.  1000+ are
  // for dynamically registered names.

  document_t(const char ** _builtins = NULL, const int _builtins_size = 0)
    : builtins(_builtins), builtins_size(_builtins_size), top(NULL) {
  }

  int register_name(const std::string& name) {
    int index = lookup_name(name);
    if (index != -1)
      return index;

    names.push_back(name);
    index = names.size() - 1;

    std::pair<names_map::iterator, bool> result =
      names_index.insert(names_pair(names.back().c_str(), index));
    assert(result.second);

    return index + 1000;
  }

  int lookup_name(const std::string& name) const
  {
    if (builtins) {
      int first = 0;
      int last  = builtins_size;
      while (first <= last) {
	int mid = (first + last) / 2; // compute mid point.

	int result;
	if ((result = (int)name[0] - (int)builtins[mid][0]) == 0)
	  result = std::strcmp(name.c_str(), builtins[mid]);

	if (result > 0)
	  first = mid + 1;		// repeat search in top half.
	else if (result < 0)
	  last = mid - 1;		// repeat search in bottom half.
	else
	  return mid;
      }
    }

    names_map::const_iterator i = names_index.find(name.c_str());
    if (i != names_index.end())
      return (*i).second;

    return -1;
  }

  const char * lookup_name(int id) const {
    if (id < 1000 && builtins) {
      assert(id >= 10);
      return builtins[id - 10];
    } else {
      return names[id - 1000].c_str();
    }
  }

  void write(std::ostream& out) const;
};

#define XML_NODE_IS_PARENT 0x1

class parent_node_t;

class node_t
{
public:
  int		  name_id;
  document_t *	  document;
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

#if 0
class format_xml_entries : public format_entries
{
  bool show_totals;
 public:
  format_xml_entries(std::ostream& output_stream,
		     const bool _show_totals = false)
    : format_entries(output_stream, ""), show_totals(_show_totals) {
    output_stream << "<?xml version=\"1.0\"?>\n"
		  << "<ledger version=\"2.5\">\n";
  }

  virtual void flush() {
    format_entries::flush();
    output_stream << "</ledger>" << std::endl;
  }

  virtual void format_last_entry();
};
#endif

} // namespace xml

#endif // _XML_H
