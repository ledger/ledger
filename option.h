#ifndef _OPTION_H
#define _OPTION_H

#include <list>
#include <string>
#include <exception>

struct option_handler {
  bool handled;
  option_handler() : handled(false) {}
  virtual ~option_handler() {}
  virtual void operator()(const char * arg = NULL) = 0;
};

struct option_t {
  char		   short_opt;
  std::string	   long_opt;
  bool		   wants_arg;
  option_handler * handler;

  option_t() : short_opt(0), wants_arg(false), handler(NULL) {}
};

class option_error : public std::exception {
  std::string reason;
 public:
  option_error(const std::string& _reason) throw() : reason(_reason) {}
  virtual ~option_error() throw() {}

  virtual const char* what() const throw() {
    return reason.c_str();
  }
};

void add_option_handler(std::list<option_t>& options, const std::string& label,
			const std::string& opt_chars, option_handler& option);
bool process_option(std::list<option_t>& options,
		    const std::string& opt, const char * arg = NULL);
void process_arguments(std::list<option_t>& options,
		       int argc, char ** argv, const bool anywhere,
		       std::list<std::string>& args);
void process_environment(std::list<option_t>& options,
			 char ** envp, const std::string& tag);

#endif // _OPTION_H
