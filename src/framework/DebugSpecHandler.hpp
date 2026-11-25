#ifndef __mpas_DebugSpecHandler_hpp__
#define __mpas_DebugSpecHandler_hpp__
#include <stdint.h>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "DebugSpec.hpp"

namespace mpas
{

class DebugSpecHandler
{
public :
  DebugSpecHandler() : debugLevel_( 0 ) {}
  ~DebugSpecHandler() { debugSpecs_.clear(); }

  bool
  canLog( uint8_t debugLevel, const std::set< std::string > &tags );

  void
  setDefaultDebugLevel( uint8_t debugLevel ) { debugLevel_ = debugLevel; }

  void
  addDebugSpecs( std::string specs );

private :

  uint8_t                        debugLevel_; ///< Default debug level for which we do not need to process
  std::vector< mpas::DebugSpec > debugSpecs_; ///< our traces determining what can be logged
  std::unordered_map< 
    uint8_t,
    std::unordered_map< size_t, bool > > lookup_; ///< a lookup table to store pre-computed queries
};

}


extern "C"
{

void DebugSpecHandler_ctor( mpas::DebugSpecHandler **ppObj );
void DebugSpecHandler_dtor( mpas::DebugSpecHandler **ppObj );

void DebugSpecHandler_setDefaultDebugLevel( mpas::DebugSpecHandler *pObj, int debugLevel );
void DebugSpecHandler_addDebugSpecs( mpas::DebugSpecHandler *pObj, char *specs );
bool DebugSpecHandler_canLog    ( mpas::DebugSpecHandler *pObj, int debugLevel, char *tags, char *file );

}

#endif // __mpas_DebugSpecHandler_hpp__
