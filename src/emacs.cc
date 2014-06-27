/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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
#include <boost/algorithm/string.hpp>
#include "emacs.h"
#include "xact.h"
#include "post.h"
#include "account.h"

namespace ledger {

	void format_emacs_posts::write_xact(xact_t& xact)
	{
		if (xact.pos)
			out << "\"" << xact.pos->pathname.string() << "\" "
					<< xact.pos->beg_line << " ";
		else
			out << "\"\" " << -1 << " ";

		tm          when = gregorian::to_tm(xact.date());
		std::time_t date = std::mktime(&when);

		out << "(" << (date / 65536) << " " << (date % 65536) << " 0) ";

		if (xact.code)
			out << "\"" << *xact.code << "\" ";
		else
			out << "nil ";

		if (xact.payee.empty())
			out << "nil";
		else
			out << "\"" << xact.payee << "\"";

		out << "\n";
	}

	void format_emacs_posts::operator()(post_t& post)
	{
		if (! post.has_xdata() ||
				! post.xdata().has_flags(POST_EXT_DISPLAYED)) {
			if (! last_xact) {
				out << "((";
				write_xact(*post.xact);
			}
			else if (post.xact != last_xact) {
				out << ")\n (";
				write_xact(*post.xact);
			}
			else {
				out << "\n";
			}

			if (post.pos)
				out << "  (" << post.pos->beg_line << " ";
			else
				out << "  (" << -1 << " ";

			out << "\"" << post.reported_account()->fullname() << "\" \""
					<< post.amount << "\"";

			switch (post.state()) {
			case item_t::UNCLEARED:
				out << " nil";
				break;
			case item_t::CLEARED:
				out << " t";
				break;
			case item_t::PENDING:
				out << " pending";
				break;
			}

			if (post.cost)
				out << " \"" << *post.cost << "\"";
			if (post.note)
				out << " \"" << escape_string(*post.note) << "\"";
			out << ")";

			last_xact = post.xact;

			post.xdata().add_flags(POST_EXT_DISPLAYED);
		}
	}

	string format_emacs_posts::escape_string(string raw){
		replace_all(raw, "\\", "\\\\");
		replace_all(raw, "\"", "\\\"");
		return raw;
	}

} // namespace ledger
