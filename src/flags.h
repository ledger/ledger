#ifndef _FLAGS_H
#define _FLAGS_H

template <typename T = uint_least8_t>
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

#endif // _FLAGS_H
