#ifdef USE_PCH
#include "pch.h"
#else
#include "xml.h"
#if 0
#include "journal.h"
#endif
#include "datetime.h"
#include "error.h"

#include <iostream>
#include <sstream>
#include <cstring>
#endif

namespace xml {

void document_t::write(std::ostream& out) const
{
  if (top) {
    out << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    top->write(out);
  }
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

void node_t::clear()
{
  node_t * child = children;
  while (child) {
    node_t * next = child->next;
    delete child;
    child = next;
  }
}

void node_t::add_child(node_t * node)
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
    next_node->prev = node;
    next_node->parent = this;
    node = next_node;
  }

  last_child = node;
}

int node_t::position()
{
  if (! parent) {
    assert(! next);
    return 1;
  }

  int index = 1;
  bool found = false;
  for (node_t * p = parent->children; p; p = p->next) {
    if (p == this) {
      found = true;
      break;
    }
    index++;
  }

  assert(found);
  return index;
}

int node_t::last()
{
  if (! parent) {
    assert(! next);
    return 1;
  }

  int count = 0;
  for (node_t * p = parent->children; p; p = p->next)
    count++;

  assert(count > 0);
  return count;
}

void node_t::write(std::ostream& out, int depth) const
{
  for (int i = 0; i < depth; i++) out << "  ";
  out << '<' << name() << ">";

  if (! data.empty()) {
    if (children) {
      out << '\n';
      for (int i = 0; i < depth + 1; i++) out << "  ";
      out << text() << '\n';
    } else {
      out << text();
    }
  } else {
    out << '\n';
  }

  for (node_t * child = children; child; child = child->next)
    child->write(out, depth + 1);

  if (data.empty() || children)
    for (int i = 0; i < depth; i++) out << "  ";

  out << "</" << name() << ">\n";
}

#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

static void startElement(void *userData, const char *name, const char **attrs)
{
  parser_t * parser = static_cast<parser_t *>(userData);

  DEBUG_PRINT("xml.parse", "startElement(" << name << ")");

  if (parser->node_stack.empty()) {
    parser->node_stack.push_front(new node_t(parser->document));
    parser->document->top = parser->node_stack.front();
  } else {
    parser->node_stack.push_front(new node_t(parser->document,
					     parser->node_stack.front()));
  }
  parser->node_stack.front()->set_name(name);
}

static void endElement(void *userData, const char *name)
{
  parser_t * parser = static_cast<parser_t *>(userData);

  DEBUG_PRINT("xml.parse", "endElement(" << name << ")");

  parser->node_stack.pop_front();
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

  if (! all_whitespace)
    parser->node_stack.front()->data = std::string(s, len);
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
  std::auto_ptr<document_t>
    doc(new document_t(builtins, builtins_size));

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
