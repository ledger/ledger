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

class node_t
{
public:
  document_t * document;

  node_t * parent;

  node_t * next;
  node_t * prev;

  node_t * children;
  node_t * last_child;

  int name_id;

  std::string data;

  node_t(document_t * _document, node_t * _parent = NULL)
    : document(_document),
      parent(_parent),

      next(NULL),
      prev(NULL),

      children(NULL),
      last_child(NULL)
  {
    TRACE_CTOR("node_t(document_t *, node_t *)");
    if (parent)
      parent->add_child(this);
  }
  virtual ~node_t() {
    TRACE_DTOR("node_t");
    if (parent) extract();
    if (children) clear();
  }

  virtual void extract();
  virtual void clear();

  virtual void add_child(node_t * node);

  virtual const char * text() const {
    return data.c_str();
  }

  virtual const char * name() const {
    return document->lookup_name(name_id);
  }
  virtual int set_name(const char * _name) {
    name_id = document->register_name(_name);
    return name_id;
  }
  virtual int set_name(int _name_id) {
    name_id = _name_id;
    return name_id;
  }

  virtual int position();
  virtual int last();

  virtual bool resolve(const std::string& func, value_t& result) {
    if (func == "text") {
      result.set_string(text());
      return true;
    }
    return false;
  }

  void write(std::ostream& out, int depth = 0) const;

private:
  node_t(const node_t&);
  node_t& operator=(const node_t&);
};

#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

class parser_t
{
 public:
  document_t * document;
  std::list<node_t *> node_stack;

  XML_Parser  parser;
  std::string have_error;

  parser_t() : document(NULL) {}

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
