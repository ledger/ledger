#ifndef _OPTION_H
#define _OPTION_H

#include <map>
#include <list>
#include <vector>
#include <string>

struct option_handler;

struct option_t {
  char		   short_opt;
  std::string	   long_opt;
  bool		   wants_arg;
  option_handler * handler;

  option_t() : short_opt(0), wants_arg(false) {}
};

typedef std::map<const std::string, option_handler *>  option_handler_map;
typedef std::pair<const std::string, option_handler *> option_handler_pair;

struct option_handler {
  bool handled;
  bool multiple;

  static std::vector<option_t> options;
  static option_handler_map    handlers;

  option_handler(const std::string& label,
		 const std::string& opt_chars,
		 const bool         _multiple);

  virtual void handle_option(const char * arg = NULL) = 0;
};

bool process_option(const std::string& opt, const char * arg = NULL);
void process_arguments(int argc, char ** argv, const bool anywhere,
		       std::list<std::string>& args);
void process_environment(char ** envp, const std::string& tag);

#define DEF_OPT_HANDLERS()				\
  std::vector<option_t> option_handler::options;	\
  option_handler_map    option_handler::handlers

#define OPT_BEGIN(tag, chars, multi)				\
  static struct tag ## _handler : public option_handler {	\
    tag ## _handler() : option_handler(#tag, chars, multi) {}	\
    virtual void handle_option(const char * optarg)

#define OPT_END(tag) } tag ## _handler_obj

#endif // _OPTION_H
