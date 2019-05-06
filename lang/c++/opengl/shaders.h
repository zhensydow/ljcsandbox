//------------------------------------------------------------------------------
#ifndef SHADERS_H
#define SHADERS_H

//------------------------------------------------------------------------------
#include <string>
#include <vector>

#define GL_GLEXT_PROTOTYPES 1
#define GL3_PROTOTYPES 1
#include <GL/gl.h>

//------------------------------------------------------------------------------
GLuint createShader( GLenum shaderType, const std::string &filename );
GLuint createProgram( const std::vector<GLuint> &shaders );
GLuint loadProgram( const std::string &name );

//------------------------------------------------------------------------------
#endif//SHADERS_H

//------------------------------------------------------------------------------
