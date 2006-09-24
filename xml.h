#ifndef _XML_H
#define _XML_H

#include "debug.h"

#include <map>
#include <string>

namespace xml {

class node_t;

class tree_t
{
  typedef std::map<std::string, unsigned int>  names_map;
  typedef std::pair<std::string, unsigned int> names_pair;

  names_map    names;
  unsigned int next_index;

 public:
  node_t * top;

  enum special_names_t {
    CURRENT,
    PARENT,
    ROOT,
    ALL
  };

  tree_t() : top(NULL), next_index(0) {
    register_name(".");
    register_name("..");
    register_name("/");
    register_name("*");
  }

  unsigned int register_name(const std::string& name) {
    std::pair<names_map::iterator, bool> result =
      names.insert(names_pair(name, next_index++));
    if (! result.second) {
      next_index--;
      return lookup_name(name);
    }
    return next_index - 1;
  }

  unsigned int lookup_name(const std::string& name) const {
    names_map::const_iterator i = names.find(name);
    if (i != names.end())
      return (*i).second;
  }

  const std::string& lookup_name(unsigned int id) const {
    for (names_map::const_iterator i = names.begin();
	 i != names.end();
	 i++)
      if ((*i).second == id)
	return (*i).first;
  }
};

class node_t
{
public:
  tree_t * tree;

  node_t * parent;

  node_t * next;
  node_t * prev;

  node_t * children;
  node_t * last_child;

  const char * name;
  unsigned int name_id;

  node_t(tree_t * _tree, node_t * _parent = NULL)
    : tree(_tree),

      parent(_parent),

      next(NULL),
      prev(NULL),

      children(NULL),
      last_child(NULL)
  {
    TRACE_CTOR("node_t(tree_t *, node_t *)");
    if (parent)
      parent->add_child(this);
  }
  virtual ~node_t();

  virtual int position();
  virtual int last();

  virtual void extract();
  virtual void clear();

  virtual void add_child(node_t * node);

#if 0
  virtual bool resolve(const std::string& name, value_t& result,
		       scope_t * locals = NULL);
#endif

private:
  node_t(const node_t&);
  node_t& operator=(const node_t&);
};

#if 0
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

class xml_parser_t : public parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL);
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
#endif

} // namespace xml

#endif // _XML_H
