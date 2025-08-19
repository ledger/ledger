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

#include <system.hh>

#include "gpgme.h"
#include "utils.h"

#include <iostream>
#include <sstream>
#include <cerrno>

#include <gpgme++/context.h>
#include <gpgme++/decryptionresult.h>

namespace ledger {

using GpgME::Context;
using GpgME::Data;
using GpgME::Protocol;
using namespace std;
using std::shared_ptr;
using std::unique_ptr;

constexpr static unsigned int _bufsize = 4096;

data_streambuffer_t::data_streambuffer_t(Data& _data) :
  data(_data),
  bufsize(_bufsize),
  cbuf(unique_ptr<char[]>(new char[_bufsize])) {}

streambuf::int_type data_streambuffer_t::underflow() {
  if (this->gptr() == this->egptr()) {

    auto buf = cbuf.get();
    auto size = data.read(buf, bufsize);
    if (size < 0)
      throw_(runtime_error, _f("Reading decrypted data: GPGME: %1%") % strerror(errno));
    else if (size == 0)
      return char_traits<char>::eof();

    this->setg(buf, buf, buf + size);
  }
  return this->gptr() == this->egptr()
    ? char_traits<char>::eof()
    : char_traits<char>::to_int_type(*this->gptr());
}

streambuf::pos_type data_streambuffer_t::seekpos(streambuf::pos_type sp, ios_base::openmode which = ios_base::in) {
  return this->seekoff(static_cast<streambuf::off_type>(sp), ios::beg, which);
}

streambuf::pos_type data_streambuffer_t::seekoff(streambuf::off_type off,
                                                 ios_base::seekdir   dir,
                                                 ios_base::openmode  which = ios_base::in) {
  streamoff pos = -1;
  if (dir == ios::beg)
    pos = static_cast<streambuf::pos_type>(data.seek(static_cast<streamoff>(off), SEEK_SET));
  else if (dir == ios::end)
    pos = static_cast<streambuf::pos_type>(data.seek(static_cast<streamoff>(off), SEEK_END));
  else if (dir == ios::cur)
    pos = static_cast<streambuf::pos_type>(data.seek(static_cast<streamoff>(off), SEEK_CUR));

  if (pos == -1)
    throw runtime_error("Unable to seek to position");

  auto buf = cbuf.get();
  // Trigger underflow on next read
  this->setg(buf, buf+1, buf+1);
  return pos;
}

FILE * decrypted_stream_t::open_file(const path& filename) {
  FILE * f = fopen(filename.c_str(), "rb");
  if (!f)
    throw_(runtime_error, _f("Could not open file: %1%") % strerror(errno));
  return f;
}

shared_ptr<Data> decrypted_stream_t::setup_cipher_buffer(FILE * f) {
  auto enc_d = make_shared<Data>(f);
  if (!enc_d)
    throw runtime_error("Unable to create cipher text buffer");

  if (enc_d->type() != Data::PGPEncrypted
      && enc_d->type() != Data::CMSEncrypted
      && enc_d->type() != Data::Unknown
      && enc_d->type() != Data::Invalid)
    throw_(runtime_error, _f("Unsupported encryption type: %1%") % enc_d->type());

  return enc_d;
}

static bool is_encrypted(shared_ptr<Data> enc_d) {
  if (enc_d->type() == Data::Unknown
      || enc_d->type() == Data::Invalid)
    return false;
  else
    return true;
}

shared_ptr<Data> decrypted_stream_t::decrypt(shared_ptr<Data> enc_d) {
  unique_ptr<Context> ctx;
  shared_ptr<Data> dec_d;

  if (enc_d->type() == Data::Unknown
      || enc_d->type() == Data::Invalid) {
    ctx = nullptr;
    dec_d = enc_d;
  } else {
    ctx = Context::create(enc_d->type() == Data::PGPEncrypted
                          ? Protocol::OpenPGP
                          : Protocol::CMS);
    if (!ctx)
      throw runtime_error("Unable to establish decryption context");

    ctx->setOffline(true);
    dec_d = make_shared<Data>();
    if (!dec_d)
      throw runtime_error("Unable to create plain text buffer");

    auto res = ctx->decrypt(*enc_d.get(), *dec_d.get());
    if (res.error())
      throw_(runtime_error, _f("Decryption error: %1%: %2%") % res.error().source() % res.error().asStdString());
  }
  return dec_d;
}

static inline void init_lib() {
  auto err = GpgME::initializeLibrary(0);
  if (err.code() != GPG_ERR_NO_ERROR)
    throw_(runtime_error, _f("%1%: %2%") % err.source() % err.asStdString());
}

istream* decrypted_stream_t::open_stream(const path& filename) {
  init_lib();

  unique_ptr<FILE, decltype(&fclose)> file(open_file(filename), &fclose);
  auto enc_d = setup_cipher_buffer(file.get());
  if (is_encrypted(enc_d)) {
    auto dec_d = decrypt(enc_d);
    dec_d.get()->rewind();
    return new decrypted_stream_t(dec_d);
  }
  return new ifstream(filename);
}

decrypted_stream_t::decrypted_stream_t(path& filename)
: istream(new data_streambuffer_t(*new Data())) {
  init_lib();

  file = open_file(filename);
  auto enc_d = setup_cipher_buffer(file);
  dec_d = decrypt(enc_d);
  dec_d.get()->rewind();

  if (is_encrypted(enc_d)) {
    fclose(file);
    file = nullptr;
  }

  set_rdbuf(new data_streambuffer_t(*dec_d.get()));
  clear();
}

decrypted_stream_t::decrypted_stream_t(shared_ptr<Data> dec_d)
  : istream(new data_streambuffer_t(*dec_d.get())),
    dec_d(dec_d),
    file(nullptr) {
  clear();
}

decrypted_stream_t::~decrypted_stream_t() {
  if (file)
    fclose(file);
}

} // namespace ledger
