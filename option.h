#ifndef _OPTION_H
#define _OPTION_H

#include <deque>
#include <string>

struct option_handler {
  bool handled;
  option_handler() : handled(false) {}
  virtual void operator()(const char * arg = NULL) = 0;
};

struct option_t {
  char		   short_opt;
  std::string	   long_opt;
  bool		   wants_arg;
  option_handler * handler;

  option_t() : short_opt(0), wants_arg(false), handler(NULL) {}
};

void register_option(const std::string& label,
		     const std::string& opt_chars, option_handler& option);
bool process_option(const std::string& opt, const char * arg = NULL);
void process_arguments(int argc, char ** argv, const bool anywhere,
		       std::deque<std::string>& args);
void process_environment(char ** envp, const std::string& tag);

#endif // _OPTION_H
