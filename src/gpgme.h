/*
 * Copyright (c) 2020, Michael Raitza.  All rights reserved.
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

/**
 * @addtogroup extra
 */

/**
 * @file   gpgme.h
 * @author Michael Raitza
 *
 * @ingroup extra
 *
 * @brief A utility class for reading encrypted journal data
 */
#pragma once

#include <system.hh>

#include "utils.h"

#include <streambuf>
#include <istream>

#include <gpgme++/data.h>

namespace ledger {

  class data_streambuffer_t : public std::streambuf {
  public:
    GpgME::Data& data;

    /* Size of cbuf */
    const unsigned int bufsize;

    /* Backing character buffer */
    std::unique_ptr<char[]> cbuf;

    explicit data_streambuffer_t(GpgME::Data& _data);

    virtual int_type underflow();

  protected:
    virtual std::streambuf::pos_type seekpos(std::streambuf::pos_type sp, std::ios_base::openmode which);
    virtual std::streambuf::pos_type seekoff(std::streambuf::off_type off,
                                             std::ios_base::seekdir   dir,
                                             std::ios_base::openmode  which);
  };

  class decrypted_stream_t : public std::istream {
  public:
    std::shared_ptr<GpgME::Data> dec_d;
    std::FILE * file;

    /* Establishes an istream decrypting a file pointed to by FILENAME.

       Decryption is performed at object creation and only the decrypted Data
       buffer is retained as the backing store for the stream.

       Expects the input file to be unencrypted or encrypted in CMS or PGP
       format (includes asymmetrically and symmetrically encrypted content).

       Calls open_file(), setup_cipher_buffer() and decrypt() and throws
       exceptions noted in there on error. */
    decrypted_stream_t(path& filename);

    /* Established an istream serving the decrypted content in DEC_D.

       Make sure DEC_D is properly rewound. (Which it is not after decrypting.)

       Expects DEC_D was created by actually decrypting input data (usually a
       FILE object). Otherwise GpgME just hands over the reference from the
       buffer holding the "encrypted" input to DEC_D. Then, you must keep the
       original object around for the lifetime of this stream. */
    decrypted_stream_t(std::shared_ptr<GpgME::Data> dec_d);

    ~decrypted_stream_t();

    /* Opens file pointed to by FILENAME.

       Opens the file using fopen() in "rb" mode.

       Throws a runtime error when the file cannot be opened for reading. */
    static std::FILE * open_file(const path& filename);

    /* Returns a Data buffer connected to an open FILE object.

       Throws a runtime error when the content is neither PGPEncrypted,
       CMSEncrypted or Unknown, or when the buffer cannot be established. */
    static std::shared_ptr<GpgME::Data> setup_cipher_buffer(std::FILE * f);

    /* Returns a Data buffer of the plain text. Decrypts cipher text by
       establishing a proper decryption context, first. .

       Returns the input Data buffer when the encryption type is Unknown, which
       is considered unencrypted input.

       Throws a runtime error when the decryption fails or when the cipher text
       is neither PGPEncrypted nor CMSEncrypted. */
    static std::shared_ptr<GpgME::Data> decrypt(std::shared_ptr<GpgME::Data> enc_d);

    /* Returns an istream, which is either a decrypted_stream_t, given the file
       is encrypted, or an ifstream object.

       Use this to create the istream! The decrypted_stream_t is perfectly
       capable reading unencrypted data, but the file size and data pointers no
       longer match with a standard ifstream. */
    static std::istream* open_stream(const path& filename);
  };

} // namespace ledger
