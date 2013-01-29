//------------------------------------------------------------------------------
#include "fileutils.h"

//------------------------------------------------------------------------------
#include <fstream>

//------------------------------------------------------------------------------
void readFileData( const std::string &str, std::string &data ){
  std::ifstream t( str );

  t.seekg( 0, std::ios::end );
  data.reserve( t.tellg() );
  t.seekg( 0, std::ios::beg );

  data.assign( std::istreambuf_iterator<char>(t),
	       std::istreambuf_iterator<char>());
}

//------------------------------------------------------------------------------
