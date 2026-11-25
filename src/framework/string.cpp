#include "string.hpp"

#include <stdint.h>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
#include <cctype>
#include <climits>

namespace mpas
{
namespace string
{

std::vector<std::string>
tokenize( std::string input, std::string delim, bool trimTokens )
{
  size_t pos = 0;
  size_t prev = 0;
  std::vector< std::string > tokens;
  std::string token;

  while ( ( pos = input.find( delim, prev ) ) != std::string::npos )
  {
    token = input.substr(prev, pos - prev);
    if ( trimTokens )
    {
      token = mpas::string::trim( token );
    }
    tokens.push_back( token );
    prev = pos + delim.length();
  }
  // get last token
  token = input.substr(prev);
  if ( trimTokens )
  {
    token = mpas::string::trim( token );
  }
  tokens.push_back( token );
  return tokens;
}

int32_t
numeric( std::string input )
{
  char * end = NULL;
  int64_t num = std::strtol( input.c_str(), &end, 10 );
  if ( (*end) == '\0' &&    // fully converted
        errno != ERANGE &&  // no out of range
        num != LONG_MAX &&
        num != LONG_MIN &&
        !( num == 0 && errno != 0 ) && // able to convert
        input.c_str() != end           // wasn't just blank
      )
  {
    return num;
  }

  return INT_MAX;
}

std::string
trim( const std::string &input )
{
  size_t first = input.find_first_not_of( " \t" );
  if ( std::string::npos == first ) 
  {
    return "";
  }
  size_t last = input.find_last_not_of( " \t" );
  return input.substr( first, last - first + 1 );
}

}
}
