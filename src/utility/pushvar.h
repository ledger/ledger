/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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
 * @file   scopevar.h
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
 * for, to provide a scopevar<> class which guarantees execution
 * of arbtirary code after a scope has terminated, without having to
 * resort to custom utility classes.  It relies on boost::bind to
 * declare pending function calls.  Here it what the above would look
 * like:
 *
 * @code
 *   void foo(pthread_mutex_t * mutex) {
 *     if (pthread_mutex_lock(mutex) == 0) {
 *       scopevar<void> unlock_mutex
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
 * Another kind of scopevar is useful for setting the values of
 * variables to a predetermined value upon completion of a scope.
 * Consider this example:
 *
 * @code
 *   bool foo_was_run;
 *
 *   void foo() {
 *     scopevar<bool&> set_success((_1 = true), foo_was_run);
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
 * Finally, you can stop a scopevar or scoped_variable from
 * invoking its completion code by calling the `clear' method on the
 * object instance.  Once `clear' is called, the scoped execution
 * becomes inert and will do nothing when the enclosing scope is
 * exited.
 */

#ifndef _SCOPEVAR_H
#define _SCOPEVAR_H

template <typename T>
class push_variable : public boost::noncopyable
{
  T&   var;
  T    prev;
  bool enabled;

public:
  explicit push_variable(T& _var)
    : var(_var), prev(var), enabled(true) {}
  explicit push_variable(T& _var, const T& value)
    : var(_var), prev(var), enabled(true) {
    var = value;
  }
  ~push_variable() {
    if (enabled)
      var = prev;
  }

  void clear() {
    enabled = false;
  }
};

#endif // _SCOPEVAR_H
