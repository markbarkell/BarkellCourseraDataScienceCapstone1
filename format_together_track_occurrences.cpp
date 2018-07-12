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
#include <map>
#include <set>
#include <string>


namespace {

  
  
  uint32_t const desiredStringSize = 128 - sizeof(uint16_t);
  uint32_t const recordSize = 128;
  const char* cntFileName =  "btn.cnt";

  
  std::string formatLine(std::string const& in)
  {
    uint32_t const inSize =  in.size();
    uint32_t const r = std::min(desiredStringSize, inSize);
    std::string result = in.substr(0*desiredStringSize, r);
    std::ostringstream oss;
    oss << result << std::string(desiredStringSize - result.size(), ' ');
    result = oss.str();
    return result;
  }
  
  std::pair<std::string, bool> readLineFromStream(std::istream& iss)
  {
    std::string line;    
    bool ok = !!(std::getline(iss, line)) && !line.empty();
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
	bool second = std::getline((**iterStream), iterLines->first).eof();
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
      continueReading = cntAndStr.second != 0 && ' ' != cntAndStr.first[0];
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
    combineStreams(vis, cntFileName);    
  }
}

std::string buildSearchString(std::vector<std::string> const v)
{
  std::ostringstream oss;
  for(auto item : v) {
    oss << item << " ";
  }
  std::cout << "The search string is going to be: '" << oss.str() << "'" << std::endl;
  return (oss.str());
}


std::map<std::string, uint16_t> readCntFile()
{
  std::cout << "Debug:" << __func__ << ":" << __FILE__ << ":" << __LINE__ << std::endl;
  std::map<std::string, uint16_t> v;
   std::ifstream is(cntFileName);
   char buf[recordSize];
   long long cnt = 0;
   while (!is.eof() && is.good()) {
     is.read(buf, sizeof(buf)/sizeof(*buf));
     std::string value(buf, sizeof(buf)/sizeof(*buf) - sizeof(uint16_t));
     uint16_t count = *(reinterpret_cast<uint16_t*>(&(buf[desiredStringSize])));
     auto p = std::make_pair(value, count);
     v.insert(p);
     ++cnt;
     if (cnt % 1000000 == 0) {
       std::cout << "Debug:" << "READ " << cnt << " " << __func__ << ":" << __FILE__ << ":" << __LINE__ << std::endl;
     }
   }
   std::cout << "Debug:" << __func__ << ":" << __FILE__ << ":" << __LINE__ << std::endl;
   return v;
}

std::multimap<uint16_t, std::string> counts()
{
  std::multimap<uint16_t, std::string> v;
   std::ifstream is(cntFileName);
   char buf[recordSize];
   while (!is.eof() && is.good()) {
     is.read(buf, sizeof(buf)/sizeof(*buf));
     std::string value(buf, sizeof(buf)/sizeof(*buf) - sizeof(uint16_t));
     uint16_t count = *(reinterpret_cast<uint16_t*>(&(buf[desiredStringSize])));
     auto p = std::make_pair(count, value);
     v.insert(p);
   }
   return v;
}


std::string getLastWord(std::string const& full)
{
  bool wordBack = false;
  std::vector<char> v;
  for(auto citer = full.crbegin(); citer != full.crend(); ++citer) {
    if (*citer == ' ') {
      if (wordBack) {
	break;
      }
      continue;
    } else {
      wordBack = true;
    }
    v.push_back(*citer);
  }
  std::string r;
  for(auto vi = v.crbegin(); vi != v.crend(); ++vi) {
    r.push_back(*vi);
  }
  return r;
}

void predictFromString(std::string const& ss,
		       std::map<std::string, uint16_t> const& cntInfo,
		       std::set<std::string> const* endsWith = nullptr)
{

  //  std::cout << "Debug: " <<  __func__ << "," << __FILE__ << ":" << __LINE__ << std::endl
  //	    << "ss: '" << ss << "'" << std::endl
  //	    << "size: " << cntInfo.size() << std::endl;
  //  std::cout.flush();

  auto iter = cntInfo.lower_bound(ss);
  std::cout << "Finished lower bounds on " << ss << std::endl; std::cout.flush();
  if (iter != cntInfo.cend()) {
    std::cout << "Iter String Value '" << iter->first << "'" << std::endl;
    std::cout << "Iter String Count " << iter->second << std::endl;
    std::cout.flush();
    std::map<uint16_t, std::vector<std::string> > cnts;
    do  {
      cnts[iter->second].push_back(iter->first);
      ++iter;
    } while (iter != cntInfo.end() && iter->first.substr(0, ss.size()) == ss);
    for(auto citerMap = cnts.cbegin(); citerMap != cnts.cend(); ++citerMap)
      {
	bool doneOccurrences = false;
	for(auto iterV = citerMap->second.cbegin();
	    iterV != citerMap->second.cend(); ++iterV) {
	  std::string lastWord = getLastWord((*iterV));
	  if (!endsWith || endsWith && endsWith->find(lastWord) != endsWith->cend()) {
	    if (!doneOccurrences) {
	      std::cout << "occurrences: " << citerMap->first << ":" << std::endl;
	    }
	    doneOccurrences = true;
	    std::cout << (*iterV) << std::endl;
	  }

	}
      }
  }
  else {
    std::cout << "Nothing to report";
  }
}

void predictValues()
{


  auto cntInfo = readCntFile();
  while (true) {
    std::string word;
    std::vector<std::string> words;
    std::cout << "What is the list of words to search?" << std::endl;
    while(std::cin >> word && word != "-") {
      words.push_back(word);
    }
    std::string const ss = buildSearchString(words);
    predictFromString(ss, cntInfo);
  }
}

std::string oneendingspace(std::string const& s)
{
   std::ostringstream oss;
  for(auto iter = s.cbegin(); iter != s.cend(); ++iter)
    {
      if (iter + 1 < s.cend() && *(iter ) == ' ' && *(iter + 1) == ' ')
	{
	  break;
	}
      oss << *iter;
    }
  return oss.str();
}

std::string spacelobar(std::string const& s)
{
  std::ostringstream oss;
  for(auto iter = s.cbegin(); iter != s.cend(); ++iter)
    {
      if (iter + 1 < s.cend() && *(iter ) == ' ' && *(iter + 1) == ' ' )
	{
	  break;
	}
      oss << (*iter == ' ' ? '_' : *iter);
    }
  return oss.str();
}


int main(int argc, const char** argv)
{
  std::cout << "argc " << argc << std::endl;
  for(auto i = 0; i < argc; ++i) {
    std::cout << argv[i] << std::endl;
  }
  if (argc == 2 && std::string(argv[1]) == std::string("--generate")) {
    std::cout << "generating" << std::endl;
    combineStreams();
  }
  else if (argc == 3 && std::string(argv[1]) == std::string("--tokenizetexts")) {
    std::string top(argv[2]);
    int topCount;
    std::istringstream iss(top);
    std::ofstream ofs("mapping.txt");
    std::ofstream rofs("subs.R");
    iss >> topCount;
    auto model = counts();
    auto mmi = model.crbegin();
    rofs << "manysubs <- function(v) {" << std::endl;
    for(int i = 0; i < topCount && mmi != model.crend(); ++i, ++mmi) {
      auto lb = spacelobar(mmi->second);
      auto es = oneendingspace(mmi->second);
      rofs << "v <- gsub(\""
		<< es << "\",\""
		<< lb
		<< "\", v)" << std::endl;
      ofs << es << "|" << lb << std::endl;
    }
    rofs << "return (v)" << std::endl;
    rofs << "}" << std::endl;
  }
  else if (argc == 2 && std::string(argv[1]) == std::string("--endswith")) {    
    std::cout << "About read model from file" << std::endl;
    auto model = readCntFile();
   
    while (true) {
      std::cout << "About to populate the ending filter:  (dash to finish)"
		<< std::endl;
      std::set<std::string> endsWith;
      std::string e;
      while( (std::cin >> e) && e != "-")      {
	try { endsWith.insert(e); } catch (std::exception) { }
      }
      for(auto iter = endsWith.cbegin(); iter != endsWith.cend(); ++iter) {
	std::cout << "Possible ending with '" << (*iter) << "'" << std::endl;
      }
      std::cout << "Search string:" << std::endl;    
      std::cout.flush();
      std::string line;
      std::cin.ignore();
      std::getline(std::cin, line);
      std::cout << "Line start search '" << line << "'" << std::endl;
      predictFromString(line, model, &endsWith);
    
      std::cout.flush();
    }
    
  }
  else if (argc == 1) {
    std::cout << "predicting" << std::endl;
    predictValues();
  }

  return EXIT_SUCCESS;
}
