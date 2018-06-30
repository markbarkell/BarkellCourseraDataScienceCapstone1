// Programmer: Mark Barkell
// Purpose:  Keep track of strings and the occurrences thereof.
// Cavets: This file is really 

#ifndef STR_CNT_HPP_BARKELL_2018_06_30
#define  STR_CNT_HPP_BARKELL_2018_06_30

class StrCnt {
private:
  std::string const m_text;
  uint16_t const m_count;
public:
  StrCnt(std::string const& s, uint16_t cnt)
    : m_text(s),
    m_count(cnt)
  {
  }
  bool operator<(std::string const& s) const
  {
    return m_text < s;
  }
  bool operator>(std::string const& s) const
  {
    return !this->operator<(s) && !this->operator==(s); 
  }
  bool operator==(std::string const& s) const
  {
    return m_text == s;
  }
  std::string const& valueString() const { return m_text; }
  uint16_t const& associationCount() const { return m_count; }
};

#endif /* STR_CNT_HPP_BARKELL_2018_06_30 */
