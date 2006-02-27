#ifndef _OPTION_H
#define _OPTION_H

#include <list>
#include <string>
#include <exception>

typedef void (*handler_t)(const char * arg);

struct option_t {
  const char * long_opt;
  char	       short_opt;
  bool	       wants_arg;
  handler_t    handler;
  bool         handled;
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

bool process_option(option_t * options, const std::string& opt,
		    const char * arg = NULL);
void process_arguments(option_t * options, int argc, char ** argv,
		       const bool anywhere, std::list<std::string>& args);
void process_environment(option_t * options, char ** envp,
			 const std::string& tag);

#endif // _OPTION_H
