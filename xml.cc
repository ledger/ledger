#ifdef USE_PCH
#include "pch.h"
#else
#include "xml.h"
#include "datetime.h"
#include "error.h"

#include <iostream>
#include <sstream>
#include <cstring>
#endif

namespace ledger {
namespace xml {

document_t::document_t(journal_t *   _journal,
		       const char ** _builtins,
		       const int     _builtins_size)
  : builtins(_builtins), builtins_size(_builtins_size),
    journal(_journal), top(NULL)
{
  if (! journal)
    top = new terminal_node_t(this);
}

int document_t::register_name(const std::string& name)
{
  int index = lookup_name_id(name);
  if (index != -1)
    return index;

  names.push_back(name);
  index = names.size() - 1;

  std::cerr << this << " Inserting name: " << names.back() << std::endl;
  std::pair<names_map::iterator, bool> result =
    names_index.insert(names_pair(names.back(), index));
  assert(result.second);

  return index + 1000;
}

int document_t::lookup_name_id(const std::string& name) const
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

  std::cerr << this << " Finding name: " << name << std::endl;
  names_map::const_iterator i = names_index.find(name);
  if (i != names_index.end())
    return (*i).second + 1000;

  return -1;
}

const char * document_t::lookup_name(int id) const
{
  if (id < 1000) {
    switch (id) {
    case CURRENT:
      return "CURRENT";
    case PARENT:
      return "PARENT";
    case ROOT:
      return "ROOT";
    case ALL:
      return "ALL";
    default:
      assert(id >= 10);
      assert(builtins);
      return builtins[id - 10];
    }
  } else {
    return names[id - 1000].c_str();
  }
}

void document_t::write(std::ostream& out) const
{
  if (top) {
    out << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    top->write(out);
  }
}

#ifndef THREADSAFE
document_t * node_t::document;
#endif

node_t::node_t(document_t * _document, parent_node_t * _parent,
	       unsigned int _flags)
  : name_id(-1),
    parent(_parent),
    next(NULL), prev(NULL), flags(_flags), info(NULL), attrs(NULL)
{
  TRACE_CTOR("node_t(document_t *, node_t *)");
#ifdef THREADSAFE
  document = _document;
#else
  if (! document)
    document = _document;
  else
    assert(document == _document);
#endif
  if (parent)
    parent->add_child(this);
}

void node_t::extract()
{
  if (prev)
    prev->next = next;

  if (parent) {
    if (parent->children == this)
      parent->children = next;

    if (parent->last_child == this)
      parent->last_child = prev;

    parent = NULL;
  }

  if (next)
    next->prev = prev;

  next = NULL;
  prev = NULL;
}

void parent_node_t::clear()
{
  node_t * child = children;
  while (child) {
    node_t * next = child->next;
    delete child;
    child = next;
  }
}

void parent_node_t::add_child(node_t * node)
{
  if (children == NULL) {
    assert(last_child == NULL);
    children = node;
    node->prev = NULL;
  } else {
    assert(last_child != NULL);
    last_child->next = node;
    node->prev = last_child;
  }

  node->parent = this;

  while (node->next) {
    node_t * next_node = node->next;
    assert(next_node->prev == node);
    next_node->parent = this;
    node = next_node;
  }

  last_child = node;
}

void parent_node_t::write(std::ostream& out, int depth) const
{
  for (int i = 0; i < depth; i++) out << "  ";
  out << '<' << name() << ">\n";

  for (node_t * child = children; child; child = child->next)
    child->write(out, depth + 1);

  for (int i = 0; i < depth; i++) out << "  ";
  out << "</" << name() << ">\n";
}

void terminal_node_t::write(std::ostream& out, int depth) const
{
  for (int i = 0; i < depth; i++) out << "  ";

  if (data.empty()) {
    out << '<' << name() << " />\n";
  } else {
    out << '<' << name() << ">"
	<< text()
	<< "</" << name() << ">\n";
  }
}

#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

template <typename T>
inline T * create_node(parser_t * parser)
{
  T * node = new T(parser->document, parser->node_stack.empty() ?
		   NULL : parser->node_stack.front());

  node->set_name(parser->pending);
  node->attrs = parser->pending_attrs;

  parser->pending	= NULL;
  parser->pending_attrs = NULL;

  return node;
}

static void startElement(void *userData, const char *name, const char **attrs)
{
  parser_t * parser = static_cast<parser_t *>(userData);

  DEBUG_PRINT("xml.parse", "startElement(" << name << ")");

  if (parser->pending) {
    parent_node_t * node = create_node<parent_node_t>(parser);
    if (parser->node_stack.empty())
      parser->document->top = node;
    parser->node_stack.push_front(node);
  }

  parser->pending = name;

  if (attrs) {
    for (const char ** p = attrs; *p; p += 2) {
      if (! parser->pending_attrs)
	parser->pending_attrs = new node_t::attrs_map;

      std::pair<node_t::attrs_map::iterator, bool> result
	= parser->pending_attrs->insert(node_t::attrs_pair(*p, *(p + 1)));
      assert(result.second);
    }
  }
}

static void endElement(void *userData, const char *name)
{
  parser_t * parser = static_cast<parser_t *>(userData);

  DEBUG_PRINT("xml.parse", "endElement(" << name << ")");

  if (parser->pending) {
    terminal_node_t * node = create_node<terminal_node_t>(parser);
    if (parser->node_stack.empty()) {
      parser->document->top = node;
      return;
    }
  }
  else if (! parser->handled_data) {
    assert(! parser->node_stack.empty());
    parser->node_stack.pop_front();
  }
  else {
    parser->handled_data = false;
  }
}

static void dataHandler(void *userData, const char *s, int len)
{
  parser_t * parser = static_cast<parser_t *>(userData);

  DEBUG_PRINT("xml.parse", "dataHandler(" << std::string(s, len) << ")");

  bool all_whitespace = true;
  for (int i = 0; i < len; i++) {
    if (! std::isspace(s[i])) {
      all_whitespace = false;
      break;
    }
  }

  // jww (2006-09-28): I currently do not support text nodes within a
  // node that has children.

  if (! all_whitespace) {
    terminal_node_t * node = create_node<terminal_node_t>(parser);

    node->data = std::string(s, len);
    parser->handled_data = true;

    if (parser->node_stack.empty()) {
      parser->document->top = node;
      return;
    }
  }
}

bool parser_t::test(std::istream& in) const
{
  char buf[80];

  in.getline(buf, 79);
  if (std::strncmp(buf, "<?xml", 5) != 0) {
    in.clear();
    in.seekg(0, std::ios::beg);
    return false;
  }

  in.clear();
  in.seekg(0, std::ios::beg);
  return true;
}

document_t * parser_t::parse(std::istream& in, const char ** builtins,
			     const int builtins_size)
{
  std::auto_ptr<document_t> doc(new document_t(NULL, builtins, builtins_size));

  document = doc.get();

  unsigned int offset = 2;
  parser = XML_ParserCreate(NULL);

  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);
  XML_SetUserData(parser, this);

  char buf[BUFSIZ];
  while (! in.eof()) {
    in.getline(buf, BUFSIZ - 1);
    std::strcat(buf, "\n");
    bool result;
    try {
      result = XML_Parse(parser, buf, std::strlen(buf), in.eof());
    }
    catch (const std::exception& err) {
      unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      XML_ParserFree(parser);
      throw new parse_error(err.what());
    }

    if (! have_error.empty()) {
      unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      parse_error err(have_error);
      std::cerr << "Error: " << err.what() << std::endl;
      have_error = "";
    }

    if (! result) {
      unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      const char *  err  = XML_ErrorString(XML_GetErrorCode(parser));
      XML_ParserFree(parser);
      throw new parse_error(err);
    }
  }

  XML_ParserFree(parser);

  document = NULL;
  return doc.release();
}

#endif // defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

void output_xml_string(std::ostream& out, const std::string& str)
{
  for (const char * s = str.c_str(); *s; s++) {
    switch (*s) {
    case '<':
      out << "&lt;";
      break;
    case '>':
      out << "&rt;";
      break;
    case '&':
      out << "&amp;";
      break;
    default:
      out << *s;
      break;
    }
  }
}

} // namespace xml
} // namespace ledger
