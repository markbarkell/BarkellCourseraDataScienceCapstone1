// Programmer: Mark Barkell
// Purpose: Refresh my mind's memory and experiment with
#include <iostream>
#include <fstream>

int main()
{
  std::ifstream biBlogStream("./final/en_US/en_US.blogs.txt.bi.srt");
  std::string line;
  bool bResult = biBlogStream.good();
  std::cout << "Initial value " << bResult << std::endl;
  while ( (bResult = std::getline(biBlogStream, line).good()) ) {
    std::cout << bResult << " '" << line << "'" << std::endl;
  }
  return EXIT_SUCCESS;
}
