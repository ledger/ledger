#ifndef _BUILDER_H
#define _BUILDER_H

#include "xml.h"

namespace ledger {

/**
 * @class builder_t
 *
 * @brief Represents an interface for building a data hierarchy.
 *
 * This interface is much like .NET's XmlWriter facility.  It
 * abstracts the kind of hierarchy we're building, instead focusing
 * only on the relationships.
 */
class builder_t
{
  virtual void     pushAttribute(const string& name,
				 const string& value)	= 0;
  virtual void     pushAttribute(const nameid_t name_id,
				 const string& value)	= 0;

  virtual void     beginNode(const string& name)	= 0;
  virtual void     beginNode(const nameid_t name_id)	= 0;

  virtual void     appendText(const string& text)	= 0;

  virtual node_t * endNode(const optional<string>& name = 
			   optional<string>())		= 0;
  virtual node_t * endNode(const nameid_t name_id)	= 0;
};

/**
 * @class xml_builder_t
 *
 * @brief Build a generic node_t hierarchy.
 *
 * This builder can be used to parse ordinary XML into a document
 * object structure which can then be traversed in memory.
 */
class xml_builder_t : public builder_t
{
};

/**
 * @class journal_builder_t
 *
 * @brief This custom builder creates an XML-mirrored Ledger journal.
 *
 * Rather than simply creating a node_t hierarchy, as xml_builder_t
 * does, this code creates the associated journal elements referred to
 * by those nodes, and then refers to those elements via minimalist
 * "shadow nodes".
 *
 * Thus, after building a <transaction> element, the element itself
 * will have no children, but instead will point to a transaction_t
 * object.  If later an XPath expression desires to traverse the
 * <transaction> element, all of the appropriate child nodes will be
 * constructed on the fly, as if they'd been created in the first
 * place by a regular xml_builder_t.
 */
class journal_builder_t : public xml_builder_t
{
};

/**
 * @class xml_writer_t
 *
 * @brief Create textual XML on the given output stream.
 *
 * This builder, rather than manipulating data structures in memory,
 * simply streams its contents on the fly to the given output stream.
 * It uses only enough memory to remember the currently push
 * attributes and text.
 */
class xml_writer_t : public builder_t
{
};

} // namespace ledger

#endif // _BUILDER_H
