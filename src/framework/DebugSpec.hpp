#ifndef __mpas_DebugSpec_hpp__
#define __mpas_DebugSpec_hpp__

#include <stdint.h>
#include <climits>
#include <set>
#include <algorithm>

#include "string.hpp"

namespace mpas
{

struct DebugSpec
{
  uint8_t debugLevel_;
  std::set< std::string > tags_;

  DebugSpec( std::string spec )
  : debugLevel_( UCHAR_MAX )
  , tags_()
  {
    std::vector< std::string > tokens = mpas::string::tokenize( spec, "::", true );
    std::vector< std::string >::iterator found = tokens.end();
    for ( std::vector< std::string >::iterator it = tokens.begin();
          it != tokens.end();
          it++ )
    {
      // Try to find only first valid trace level in tokens
      std::string tok = mpas::string::trim( *it );
      int32_t canConvert = mpas::string::numeric( tok );
      if ( canConvert != INT_MAX )
      {
        debugLevel_ = static_cast< uint8_t >( canConvert );
        found = it;
      }
    }

    if ( found != tokens.end() )
    {
      // Remove it
      tokens.erase( found );
    }

    // Now copy to trace
    tags_ = std::set< std::string >( tokens.begin(), tokens.end() );

  }

  bool valid( uint8_t debugLevel, const std::set< std::string > &tags )
  {
    if ( debugLevel_ >= debugLevel )
    {
      if ( tags_.size() == 0 )
      {
        return true;
      }
      else
      {
        std::set< std::string > diff;
        // Check the intersection of the set to find if tags contains all of tags_
        std::set_difference( 
                            tags_.begin(), tags_.end(),
                            tags.begin(), tags.end(),
                            std::inserter( diff, diff.begin() )
                            );
        if ( diff.size() == 0 ) // all of tags_ is found in tags
        {
          return true;
        }

      }
    }
    return false;
  }

};

}

#endif // __mpas_DebugSpec_hpp__
