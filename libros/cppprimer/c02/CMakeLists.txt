cmake_minimum_required( VERSION 2.8 )
set( PROJECT_NAME "C++ Primer 01" )
project( ${PROJECT_NAME} )

add_definitions( -Wall -Wextra -std=c++11 )

file( GLOB EXE_MAIN_CPP "ex*.cpp" )

file( GLOB COMMON_CPP "*.cpp" )

list( REMOVE_ITEM COMMON_CPP ${EXE_MAIN_CPP} )

foreach( m ${EXE_MAIN_CPP} )
  get_filename_component( EXE_NAME ${m} NAME_WE )
  add_executable( ${EXE_NAME} ${m} ${COMMON_CPP} )
endforeach()
