#include <cassert>
#include <iostream>
#include <cstdlib>
#include "lua.hpp"

int main( int argc, char * argv[] ){
    // Lua Initialization
    lua_State * L = luaL_newstate();
    assert( L && "Can't create Lua State" );
    lua_gc(L, LUA_GCSTOP, 0);
    luaL_openlibs( L );
    lua_gc(L, LUA_GCRESTART, 0);

    // execute config file
    int ret = luaL_dofile( L, "config.lua" );
    if( ret != 0 ){
        const char * msg = lua_tostring(L, -1);
        if( msg ){
            std::cerr << "Error: " << msg << std::endl;
        }else{
            std::cerr << "Error: (error object is not a string)\n";
        }
        return EXIT_FAILURE;
    }

    // get variables    
    lua_getglobal( L, "MAX_ULTIMATE" );
    int val = lua_tointeger( L, -1 );
    std::cout << "MAX_ULTIMATE = " << val << std::endl;
    
    lua_close( L );
        
    return EXIT_SUCCESS;
}
