// Programmer:  Mark Barkell
// Date: 2018-06-21
// Purpose: Boring little program to find the max number of characters
// in a line.

#include <iostream>
#include <fstream>
#include <cstdlib>

int main(int argc, const char** argv)
{
  if (argc != 2) {
    std::cerr << "This program takes only one argument.  The argument is the name of the file." << std::endl;
    return EXIT_FAILURE;
  }
  std::ifstream ifs(argv[1]);
  std::string line;
  std::size_t maxLineLen = 0;
  std::size_t prev = 0;
  std::string cs;
  while (ifs >> line) {
    maxLineLen = std::max(maxLineLen, line.size());
    if (prev < maxLineLen) {
      //std::cout << "New max: '" << line << "'" << std::endl;
      cs = line;
    }
    prev = maxLineLen;
  }
  std::cout << "max line is: '" << cs << "'" <<  std::endl;
  std::cout << maxLineLen << std::endl;
  return 0;
}
