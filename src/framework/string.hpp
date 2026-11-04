#ifndef __mpas_string_hpp__
#define __mpas_string_hpp__

#include <string>
#include <vector>

namespace mpas
{
namespace string
{
std::vector<std::string>
tokenize( std::string input, std::string delim, bool trimTokens = false );

std::string
trim( const std::string &input );

int32_t
numeric( std::string input );

template< typename T >
std::string
join( T input, std::string delim = "" )
{
  std::string result = "";
  for ( typename T::const_iterator it = input.begin(); it != input.end(); it++ )
  {
    result += (*it) + delim;
  }
  // remove last delim if possible
  if ( result.length() > delim.length() )
  {
    result.erase( result.length() - delim.length() );
  }
  return result;
}

}
}

#endif // __mpas_string_hpp__

