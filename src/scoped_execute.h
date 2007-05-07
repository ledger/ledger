#ifndef _SCOPED_EXECUTE_H
#define _SCOPED_EXECUTE_H

/**
 * @file   scoped_execute.h
 * @author John Wiegley
 * @date   Sun May  6 20:10:52 2007
 * 
 * @brief  Adds a facility to C++ for handling "scoped executions".
 * 
 * There are sometimes cases where you would like to guarantee that
 * something happens at the end of a scope, such as calling a function
 * to close a resource for you.
 *
 * The common idiom for this has become to write a helper class whose
 * destructor will call that function.  Of course, it must then be
 * passed information from the calling scope to hold onto as state
 * information, since the code within the class itself has no access
 * to its points of use.
 *
 * This type of solution is cumbersome enough that it's sometimes
 * avoided.  Take calling pthread_mutex_unlock(pthread_mutex_t *) for
 * example.  A typical snippet of safe C++ code might look like this:
 *
 * @code
 *   void foo(pthread_mutex_t * mutex) {
 *     if (pthread_mutex_lock(mutex) == 0) {
 *       try {
 *         // Do work that requires the mutex to be locked; then...
 *         pthread_mutex_unlock(mutex);
 *       }
 *       catch (std::logic_error& exc) {
 *         // This is an exception we actually handle, and then exit
 *         pthread_mutex_unlock(mutex);
 *       }
 *       catch (...) {
 *         // These are exceptions we do not handle, but still the
 *         // mutex must be unlocked
 *         pthread_mutex_unlock(mutex);
 *         throw;
 *       }
 *     }
 *   }
 * @endcode
 *
 * The alternative to this, as mentioned above, is to create a helper
 * class named pthread_scoped_lock, which might look like this:
 *
 * @code
 *   class pthread_scoped_lock : public boost::noncopyable {
 *     pthread_mutex_t * mutex;
 *   public:
 *     explicit pthread_scoped_lock(pthread_mutex_t * locked_mutex)
 *       : mutex(locked_mutex) {}
 *     ~pthread_scoped_lock() {
 *       pthread_mutex_unlock(mutex);
 *     }
 *   };
 * @endcode
 *
 * Although this helper class is just as much work as writing the code
 * above, it only needs to be written once.  Now the access code can
 * look like this:
 *
 * @code
 *   void foo(pthread_mutex_t * mutex) {
 *     if (pthread_mutex_lock(mutex) == 0) {
 *       pthread_scoped_lock(mutex);
 *       try {
 *         // Do work that requires the mutex to be locked
 *       }
 *       catch (std::logic_error& exc) {
 *         // This is an exception we actually handle, and then exit
 *       }
 *     }
 *   }
 * @endcode
 *
 * But what if it could be even easier?  That is what this file is
 * for, to provide a scoped_execute<> class which guarantees execution
 * of arbtirary code after a scope has terminated, without having to
 * resort to custom utility classes.  It relies on boost::bind to
 * declare pending function calls.  Here it what the above would look
 * like:
 *
 * @code
 *   void foo(pthread_mutex_t * mutex) {
 *     if (pthread_mutex_lock(mutex) == 0) {
 *       scoped_execute<void> unlock_mutex
 *         (boost::bind(pthread_mutex_unlock, mutex));
 *       try {
 *         // Do work that requires the mutex to be locked
 *       }
 *       catch (std::logic_error& exc) {
 *         // This is an exception we actually handle, and then exit
 *       }
 *     }
 *   }
 * @endcode
 *
 * The advantage here is that no helper class ever needs to created,
 * and hence no bugs from such helper classes can creep into the code.
 * The single call to boost::bind creates a closure binding that will
 * be invoked once the containing scope has terminated.
 *
 * Another kind of scoped_execute is useful for setting the values of
 * variables to a predetermined value upon completion of a scope.
 * Consider this example:
 *
 * @code
 *   bool foo_was_run;
 *
 *   void foo() {
 *     scoped_execute<bool&> set_success((_1 = true), foo_was_run);
 *     // do some code, and make sure foo_was_run is set to true
 *     // once the scope is exited -- however this happens.
 *   }
 * @endcode
 *
 * In this case, the Boost.Lambda library is used to create an
 * anonymous functor whose job is to set the global variable
 * `foo_was_run' to a predetermined value.
 *
 * Lastly, there is another helper class, `scoped_variable' whose job
 * is solely to return variables to the value they had at the moment
 * the scoped_variable class was instantiated.  For example, let's say
 * you have a `bar' variable that you want to work on, but you want to
 * guarantee that its value is restored upon exiting the scope.  This
 * can be useful in recursion, for "pushing" and "popping" variable
 * values during execution, for example:
 *
 * @code
 *   std::string bar = "Hello";
 *   void foo() {
 *     scoped_variable<std::string> restore_bar(bar);
 *     bar = "Goodbye";
 *     // do work with the changed bar; it gets restored upon exit
 *   }
 * @endcode
 *
 * As a shortcut, you can specify the new value for the pushed
 * variable as a second constructor argument:
 *
 * @code
 *   std::string bar = "Hello";
 *   void foo() {
 *     scoped_variable<std::string> restore_bar(bar, "Goodbye");
 *     // do work with the changed bar; it gets restored upon exit
 *   }
 * @endcode
 *
 * Finally, you can stop a scoped_execute or scoped_variable from
 * invoking its completion code by calling the `clear' method on the
 * object instance.  Once `clear' is called, the scoped execution
 * becomes inert and will do nothing when the enclosing scope is
 * exited.
 */

#include <boost/noncopyable.hpp>
#include <boost/function.hpp>

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

#endif // _SCOPED_EXECUTE_H
