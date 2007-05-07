#ifndef _SCOPE_EXECUTE_H
#define _SCOPE_EXECUTE_H

template <typename T>
class scoped_variable : public boost::noncopyable
{
  T&   var;
  T    prev;
  bool enabled;

public:
  explicit scoped_variable(T& _var)
    : var(_var), prev(var), enabled(true) {}
  explicit scoped_variable(T& _var, const T& value)
    : var(_var), prev(var), enabled(true) {
    var = value;
  }
  ~scoped_variable() {
    if (enabled)
      var = prev;
  }

  void clear() {
    enabled = false;
  }
};

template <typename T>
class scoped_execute : public boost::noncopyable
{
  typedef boost::function<void (T)> function_t;

  function_t code;
  T	     arg;
  bool       enabled;

public:
  explicit scoped_execute(const function_t& _code, T _arg)
    : code(_code), arg(_arg), enabled(true) {}

  ~scoped_execute() {
    if (enabled)
      code(arg);
  }

  void clear() {
    enabled = false;
  }
};

template <>
class scoped_execute<void> : public boost::noncopyable
{
  typedef boost::function<void ()> function_t;

  function_t code;
  bool       enabled;

public:
  explicit scoped_execute(const function_t& _code)
    : code(_code), enabled(true) {}

  ~scoped_execute() {
    if (enabled)
      code();
  }

  void clear() {
    enabled = false;
  }
};

#endif // _SCOPE_EXECUTE_H
