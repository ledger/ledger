class streamable {
public:
  void dump(std::ostream& out);

  void print(std::ostream& out);
  void parse(std::istream& in);

  void write(std::ostream& out);
  void read(std::istream& in);
  void read(char *& data);
};
