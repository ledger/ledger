#ifndef _FLAGS_H
#define _FLAGS_H

template <typename T = boost::uint_least8_t>
class supports_flags
{
public:
  typedef T flags_t;

protected:
  flags_t flags_;

public:
  supports_flags() : flags_(0) {}
  supports_flags(const flags_t arg) : flags_(arg) {}

  flags_t flags() const {
    return flags_;
  }
  bool has_flags(const flags_t arg) const {
    return flags_ & arg;
  }

  void set_flags(const flags_t arg) {
    flags_ = arg;
  }
  void clear_flags() {
    flags_ = 0;
  }
  void add_flags(const flags_t arg) {
    flags_ |= arg;
  }
  void drop_flags(const flags_t arg) {
    flags_ &= ~arg;
  }
};

template <typename T = boost::uint_least8_t>
class delegates_flags : public boost::noncopyable
{
public:
  typedef T flags_t;

protected:
  supports_flags<T>& flags_;

public:
  delegates_flags() : flags_() {}
  delegates_flags(supports_flags<T>& arg) : flags_(arg) {}

  flags_t flags() const {
    return flags_.flags();
  }
  bool has_flags(const flags_t arg) const {
    return flags_.has_flags(arg);
  }

  void set_flags(const flags_t arg) {
    flags_.set_flags(arg);
  }
  void clear_flags() {
    flags_.clear_flags();
  }
  void add_flags(const flags_t arg) {
    flags_.add_flags(arg);
  }
  void drop_flags(const flags_t arg) {
    flags_.drop_flags(arg);
  }
};

#endif // _FLAGS_H
