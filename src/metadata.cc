/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <system.hh>

#include "metadata.h"

namespace ledger {

bool metadata_t::has_tag(const string& tag, bool) const {
  DEBUG("metadata", "Checking if holder has tag: " << tag);
  if (!metadata) {
    DEBUG("metadata", "Holder has no metadata at all");
    return false;
  }
  string_map::const_iterator i = metadata->find(tag);
#if DEBUG_ON
  if (SHOW_DEBUG("metadata")) {
    if (i == metadata->end())
      DEBUG("metadata", "Holder does not have this tag");
    else
      DEBUG("metadata", "Holder has the tag!");
  }
#endif
  return i != metadata->end();
}

bool metadata_t::has_tag(const mask_t& tag_mask, const std::optional<mask_t>& value_mask,
                         bool) const {
  if (metadata) {
    for (const string_map::value_type& data : *metadata) {
      if (tag_mask.match(data.first) &&
          (!value_mask || (data.second.first && value_mask->match(data.second.first->to_string()))))
        return true;
    }
  }
  return false;
}

std::optional<value_t> metadata_t::get_tag(const string& tag, bool) const {
  DEBUG("metadata", "Getting tag: " << tag);
  if (metadata) {
    string_map::const_iterator i = metadata->find(tag);
    if (i != metadata->end())
      return i->second.first;
  }
  return std::nullopt;
}

std::optional<value_t> metadata_t::get_tag(const mask_t& tag_mask,
                                           const std::optional<mask_t>& value_mask, bool) const {
  if (metadata) {
    for (const string_map::value_type& data : *metadata) {
      if (tag_mask.match(data.first) &&
          (!value_mask ||
           (data.second.first && value_mask->match(data.second.first->to_string())))) {
        return data.second.first;
      }
    }
  }
  return std::nullopt;
}

metadata_t::string_map::iterator metadata_t::set_tag(const string& tag,
                                                     const std::optional<value_t>& value,
                                                     const bool overwrite_existing) {
  assert(!tag.empty());

  if (!metadata)
    metadata.emplace();

  DEBUG("metadata", "Setting tag '" << tag << "' to value '"
                                    << (value ? *value : string_value("<none>")) << "'");

  std::optional<value_t> data = value;
  if (data && (data->is_null() || (data->is_string() && data->as_string().empty())))
    data = std::nullopt;

  string_map::iterator i = metadata->find(tag);
  if (i == metadata->end()) {
    auto [iter, inserted] = metadata->insert(string_map::value_type(tag, tag_data_t(data, false)));
    assert(inserted);
    return iter;
  }
  if (overwrite_existing)
    i->second = tag_data_t(data, false);
  return i;
}

std::vector<metadata_t::parsed_tag_t> metadata_t::scan_metadata_tags(const char* p) {
  std::vector<parsed_tag_t> found;

  // Skip any additional leading `;` characters (with interleaved whitespace).
  // The first `;` has already been consumed by the caller, but comments
  // written as `;;Key: value` or `;  ;Key: value` would otherwise see the
  // extra semicolon glom onto the tag name (fixes #1786).
  while (*p == ' ' || *p == '\t' || *p == ';')
    ++p;

  if (!std::strchr(p, ':'))
    return found;

  string str(p);
  string::size_type pos = 0;

  bool first = true;

  while (pos < str.length()) {
    pos = str.find_first_not_of(" \t", pos);
    if (pos == string::npos)
      break;

    string::size_type end = str.find_first_of(" \t", pos);
    if (end == string::npos)
      end = str.length();

    string token = str.substr(pos, end - pos);
    if (token.length() < 2) {
      pos = end;
      continue;
    }

    if (token[0] == ':' && token[token.length() - 1] == ':') { // a series of tags
      string tag_str(token, 1, token.length() - 2);
      std::vector<string> tag_names;
      boost::split(tag_names, tag_str, boost::is_any_of(":"));
      for (const string& tag_name : tag_names) {
        if (!tag_name.empty())
          found.push_back({tag_name, std::nullopt, false});
      }
    } else if (first && token[token.length() - 1] == ':') { // a metadata setting
      bool by_value = false;
      std::size_t index = 1;
      if (token[token.length() - 2] == ':') {
        by_value = true;
        index = 2;
      }
      string field(p + end);
      trim(field);
      found.push_back({token.substr(0, token.length() - index), field, by_value});
      break;
    }
    first = false;
    pos = end;
  }
  return found;
}

void metadata_t::parse_metadata_tags(const char* p, scope_t& outer_scope, scope_t& self_scope,
                                     bool overwrite_existing) {
  for (const parsed_tag_t& t : scan_metadata_tags(p)) {
    string_map::iterator i;
    if (!t.value) {
      i = set_tag(t.name, std::nullopt, overwrite_existing);
    } else if (t.by_value) {
      bind_scope_t bound_scope(outer_scope, self_scope);
      i = set_tag(t.name, expr_t(*t.value).calc(bound_scope), overwrite_existing);
    } else {
      i = set_tag(t.name, string_value(*t.value), overwrite_existing);
    }
    i->second.second = true;
  }
}

value_t metadata_has_tag(metadata_t& target, call_scope_t& args) {
  if (args.size() == 1) {
    if (args[0].is_string())
      return target.has_tag(args.get<string>(0));
    else if (args[0].is_mask())
      return target.has_tag(args.get<mask_t>(0));
    else
      throw_(std::runtime_error,
             _f("Expected string or mask for argument 1, but received %1%") % args[0].label());
  } else if (args.size() == 2) {
    if (args[0].is_mask() && args[1].is_mask())
      return target.has_tag(args.get<mask_t>(0), args.get<mask_t>(1));
    else
      throw_(std::runtime_error,
             _f("Expected masks for arguments 1 and 2, but received %1% and %2%") %
                 args[0].label() % args[1].label());
  } else if (args.size() == 0) {
    throw_(std::runtime_error, _("Too few arguments to function"));
  } else {
    throw_(std::runtime_error, _("Too many arguments to function"));
  }
  return false;
}

value_t metadata_get_tag(metadata_t& target, call_scope_t& args) {
  std::optional<value_t> val;

  if (args.size() == 1) {
    if (args[0].is_string())
      val = target.get_tag(args.get<string>(0));
    else if (args[0].is_mask())
      val = target.get_tag(args.get<mask_t>(0));
    else
      throw_(std::runtime_error,
             _f("Expected string or mask for argument 1, but received %1%") % args[0].label());
  } else if (args.size() == 2) {
    if (args[0].is_mask() && args[1].is_mask())
      val = target.get_tag(args.get<mask_t>(0), args.get<mask_t>(1));
    else
      throw_(std::runtime_error,
             _f("Expected masks for arguments 1 and 2, but received %1% and %2%") %
                 args[0].label() % args[1].label());
  } else if (args.size() == 0) {
    throw_(std::runtime_error, _("Too few arguments to function"));
  } else {
    throw_(std::runtime_error, _("Too many arguments to function"));
  }

  return val ? *val : NULL_VALUE;
}

void put_metadata(property_tree::ptree& st, const metadata_t::string_map& metadata) {
  for (const metadata_t::string_map::value_type& pair : metadata) {
    if (pair.second.first) {
      property_tree::ptree& vt(st.add("value", ""));
      vt.put("<xmlattr>.key", pair.first);
      put_value(vt, *pair.second.first);
    } else {
      st.add("tag", pair.first);
    }
  }
}

} // namespace ledger
