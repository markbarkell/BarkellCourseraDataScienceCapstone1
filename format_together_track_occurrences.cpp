// Programmer:  Mark Barkell
// Purpose:  Using R, C++, and Bash, I've already made
// many sorted files of tri-grams and bi-grams of data provided
// while taking the Coursera.org's John Hopkins Data Science Specialization
// Capstone.
//
// So, as a Data Scientist, I desire to take the files that I've made,
// create combined master file having all of those texts normalized to be the same
// record size.   The first few bytes will be the occurrence count.   The next so
// many bytes will be the actual text having been space padded.
//
// Notes:
// If I were ever going to maintain this software, then, I might make
// it more object oriented.  the amount of template manipulation is a
// bit higher than desired in the case of this particular program.

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <algorithm>
#include <sstream>
#include <vector>

namespace {

  uint32_t const desiredStringSize = 128 - sizeof(uint16_t);

  
  std::string formatLine(std::string const& in)
  {
    uint32_t const inSize =  in.size();
    uint32_t const r = std::min(desiredStringSize, inSize);
    std::string result = in.substr(0*desiredStringSize, r);
    std::ostringstream oss;
    oss << result << std::string(result.size(), ' ');
    result = oss.str();
    return result;
  }
  
  std::pair<std::string, bool> readLineFromStream(std::istream& iss)
  {
    std::string line;    
    bool ok = !!(std::getline(iss, line));
    line = formatLine(line);
    return std::make_pair(line, ok);
  }
  
  void readFiles(std::vector<std::istream*>& vis,
		 std::vector<std::pair<std::string, bool> >& lines)
  {
    auto iterLines = lines.begin();
    auto iterStream = vis.begin();
    
    for(
	;
	iterStream != vis.end() && iterLines != lines.end();
	++iterStream, ++iterLines) {
      if (iterLines->second) {
	bool second = std::getline((**iterStream), iterLines->first).good();
	auto first = formatLine(iterLines->first);
	*iterLines = std::make_pair(first, second);
      }
    
    }
    
  }

  bool checkContinueReading(
			    std::vector<std::pair<std::string, bool> > const&
			    lines) {
    bool c = std::any_of(lines.cbegin(), lines.cend(), [](auto i) {
	return i.second; }
      );
    return c;
  }


  std::string leastString(std::vector<std::pair<std::string, bool> > const& lines)
  {
    std::string minStr;
    for(auto iterLines = lines.begin(); iterLines != lines.cend(); ++iterLines) {
      if (minStr.empty()) {
	minStr = iterLines->first;
      }
      minStr = std::min(minStr, iterLines->first);
    }
    return minStr;
  }
  
  std::pair<std::string, uint16_t> leastStringAndCount
  (std::vector<std::istream*>& vis,
   std::vector<std::pair<std::string, bool> >& lines)
  {
    std::string minStr;
    uint16_t minCount = 0;
    
    do {

      auto minStr = leastString(lines);
      
      for(auto iterLines = lines.begin(); iterLines != lines.end(); ++iterLines) {
	auto cLine = iterLines->first;
	while (cLine == minStr) {
	  auto& curStream(*vis[iterLines - lines.cbegin()]);
	  auto result = readLineFromStream(curStream);
	  *iterLines = result;
	  cLine = result.first; 
	  ++minCount;
	  if (!result.second) {
	    break;
	  }
	}
      }
      
      if (minCount != 0) {
	return std::make_pair(minStr, minCount);
      }
    } while(checkContinueReading(lines));
    return std::make_pair(std::string(), 0);
  }
						       

  void writeStringAndCount(std::pair<std::string, uint16_t> const& cntAndStr,
  			   std::ofstream& outStream)
  {
    std::cout << cntAndStr.first << " " << cntAndStr.second << std::endl;
    outStream << cntAndStr.first;
    outStream.write(reinterpret_cast<char const*>(&(cntAndStr.second)),
		    sizeof(cntAndStr.second));
  }
  
  
  void combineStreams(std::vector<std::istream*>& vis, std::string const& outputName)
  {
    std::vector<std::pair<std::string, bool> > lines;
    std::ofstream outStream(outputName);
    lines.reserve(vis.size());
    for(auto iter = vis.cbegin(); iter != vis.cend(); ++iter) {
      lines.push_back(std::make_pair(std::string(), true));
    }
    bool continueReading = true;
    do {
      readFiles(vis, lines);
      auto cntAndStr = leastStringAndCount(vis, lines);
      continueReading = cntAndStr.second != 0;
      if (continueReading) {
      	writeStringAndCount(cntAndStr, outStream);
      }
    } while(continueReading);
    
  }
  
  void combineStreams()
  {

    std::ifstream biBlogStream("./final/en_US/en_US.blogs.txt.bi.srt");
    std::ifstream biTwitStream("./final/en_US/en_US.twitter.txt.bi.srt");
    std::ifstream biNewsStream("./final/en_US/en_US.news.txt.bi.srt");
    std::ifstream triBlogStream("./final/en_US/en_US.blogs.txt.tri.srt");
    std::ifstream triTwitStream("./final/en_US/en_US.twitter.txt.tri.srt");
    std::ifstream triNewsStream("./final/en_US/en_US.news.txt.tri.srt");
    std::vector<std::istream*> vis;
    vis.push_back(&biBlogStream);
    vis.push_back(&biTwitStream);
    vis.push_back(&biNewsStream);
    vis.push_back(&triBlogStream);
    vis.push_back(&triTwitStream);
    vis.push_back(&triNewsStream);
    combineStreams(vis, "btn.cnt");    
  }
}

int main(int argc, const char** argv)
{

  combineStreams();
  return EXIT_SUCCESS;
}
