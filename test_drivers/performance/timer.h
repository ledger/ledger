#include <ctime>
#include <iostream>
struct timer {
    timer(std::ostream& report) : report(report)
       {start = std::clock();}

    void print_time()
       {
          using namespace std;
          clock_t now = clock();
          unsigned milliseconds = (now - start)*1000 / CLOCKS_PER_SEC;
          report << "Spent " << milliseconds << "ms here\n";
       }

    std::clock_t start;
    std::ostream& report;

private:
    // just to surpress a VC++ 8.0 warning
    timer& operator = (const timer&);
    timer(const timer&);
};
