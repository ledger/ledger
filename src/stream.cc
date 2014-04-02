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

#include "stream.h"

namespace ledger {

namespace {
  /**
   * @brief Forks a child process so that Ledger may handle running a
   * pager
   *
   * In order for the pager option to work, Ledger has to run the pager
   * itself, which requires Ledger to fork a new process in order to run
   * the pager.  This function does the necessary fork.  After the fork,
   * two processes exist.  One of them is exec'd to create the pager;
   * the other is still Ledger.
   *
   * This function returns only for the process that is still Ledger.
   *
   * @param pager_path Path to the pager command.
   *
   * @return The file descriptor of the pipe to the pager.  The caller
   * is responsible for cleaning this up.
   *
   * @exception std::logic_error Some problem was encountered, such as
   * failure to create a pipe or failure to fork a child process.
   */
  int do_fork(std::ostream ** os, const path& pager_path)
  {
#ifndef WIN32
    int pfd[2];

    int status = pipe(pfd);
    if (status == -1)
      throw std::logic_error(_("Failed to create pipe"));

    status = fork();
    if (status < 0) {
      throw std::logic_error(_("Failed to fork child process"));
    }
    else if (status == 0) {     // child
      // Duplicate pipe's reading end into stdin
      status = dup2(pfd[0], STDIN_FILENO);
      if (status == -1)
        perror("dup2");

      // Close unuseful file descriptors: the pipe's writing and reading
      // ends (the latter is not needed anymore, after the duplication).
      close(pfd[1]);
      close(pfd[0]);

      execlp("/bin/sh", "/bin/sh", "-c", pager_path.string().c_str(), NULL);

      // We should never, ever reach here
      perror("execlp: /bin/sh");
      exit(1);
    }
    else {                      // parent
      close(pfd[0]);
      typedef iostreams::stream<iostreams::file_descriptor_sink> fdstream;
#if BOOST_VERSION >= 104400
      *os = new fdstream(pfd[1], iostreams::never_close_handle);
#else // BOOST_VERSION >= 104400
      *os = new fdstream(pfd[1]);
#endif // BOOST_VERSION >= 104400
    }
    return pfd[1];
#else
    return 0;
#endif
  }
}

void output_stream_t::initialize(const optional<path>& output_file,
                                 const optional<path>& pager_path)
{
  if (output_file && *output_file != "-")
    os = new ofstream(*output_file);
  else if (pager_path)
    pipe_to_pager_fd = do_fork(&os, *pager_path);
  else
    os = &std::cout;
}

void output_stream_t::close()
{
#ifndef WIN32
  if (os != &std::cout) {
    checked_delete(os);
    os = &std::cout;
  }

  if (pipe_to_pager_fd != -1) {
    ::close(pipe_to_pager_fd);
    pipe_to_pager_fd = -1;

    int status;
    wait(&status);
    if (! WIFEXITED(status) || WEXITSTATUS(status) != 0)
      throw std::logic_error(_("Error in the pager"));
  }
#endif
}

} // namespace ledger
