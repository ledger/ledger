/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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

#pragma once

#include <regex>
#include <string>
#include <utility>
#include <vector>

namespace ledger {

class account_t;

class accounts_map : public std::map<string, account_t *>
{
private:
    struct regex_alias_entry {
        string pattern;
        std::regex compiled_regex;
        account_t* account;
        
        regex_alias_entry(const string& p, account_t* a) 
            : pattern(p), compiled_regex(p), account(a) {}
    };
    
    // Separate storage for regex aliases to avoid checking them for exact matches
    mutable std::vector<regex_alias_entry> regex_aliases;
    mutable bool regex_aliases_dirty = true;
    
    // Check if a pattern contains regex special characters
    static bool is_regex_pattern(const string& pattern) {
        static const string regex_chars = ".*+?[]{}()^$\\|";
        return pattern.find_first_of(regex_chars) != string::npos;
    }
    
    // Build or rebuild the regex aliases list
    void rebuild_regex_aliases() const {
        if (!regex_aliases_dirty)
            return;
            
        regex_aliases.clear();
        for (const_iterator iter = begin(); iter != end(); ++iter) {
            if (is_regex_pattern(iter->first)) {
                try {
                    regex_aliases.emplace_back(iter->first, iter->second);
                } catch (const std::regex_error&) {
                    // If regex compilation fails, skip this alias
                }
            }
        }
        regex_aliases_dirty = false;
    }
    
public:
    /**
     * Method looking for account by it's alias
     */
    const_iterator find(const string& name) const {
        // First, try exact match (most common case)
        const_iterator it = map::find(name);
        if (it != end()) {
            return it;
        }
        
        // If no exact match and we have regex aliases, check them
        if (regex_aliases_dirty) {
            rebuild_regex_aliases();
        }
        
        // Check regex aliases (only patterns that actually contain regex chars)
        for (const auto& entry : regex_aliases) {
            std::smatch m;
            if (std::regex_match(name, m, entry.compiled_regex)) {
                // We need to return an iterator, so find the entry in the map
                const_iterator result = map::find(entry.pattern);
                if (result != end()) {
                    return result;
                }
            }
        }
        
        return end();
    }
    
    // Override insert to mark regex aliases as dirty
    std::pair<iterator, bool> insert(const value_type& val) {
        regex_aliases_dirty = true;
        return map::insert(val);
    }
    
    // Override erase to mark regex aliases as dirty
    size_type erase(const key_type& k) {
        regex_aliases_dirty = true;
        return map::erase(k);
    }
    
    void clear() {
        regex_aliases.clear();
        regex_aliases_dirty = true;
        map::clear();
    }
};

} // namespace