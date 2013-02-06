//------------------------------------------------------------------------------
#include <cstdlib>

#include <vector>
#include <list>
#include <algorithm>

#define GL_GLEXT_PROTOTYPES 1
#define GL3_PROTOTYPES 1
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glext.h>
#include <GL/glut.h>

#include "shaders.h"

//------------------------------------------------------------------------------
const float vertexPositions[] = {
  0.45f, 0.75f, 0.0f, 1.0f,
  -0.75f, -0.45f, 0.0f, 1.0f,
  -0.75f, 0.75f, 0.0f, 1.0f,

  0.75f, 0.75f, 0.0f, 1.0f,
  0.75f, 0.45f, 0.0f, 1.0f,
  -0.75f, -0.75f, 0.0f, 1.0f,

  0.75f, 0.45f, 0.0f, 1.0f,
  -0.45f, -0.75f, 0.0f, 1.0f,
  -0.75f, -0.75f, 0.0f, 1.0f,

  0.75f, 0.15f, 0.0f, 1.0f,
  0.75f, -0.75f, 0.0f, 1.0f,
  -0.15f, -0.75f, 0.0f, 1.0f,
};

const float vertexColors[] = {
    1.0f, 0.0f, 0.0f, 1.0f,
    0.0f, 1.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 1.0f, 1.0f,

    1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 1.0f, 0.0f, 1.0f,

    1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 1.0f, 0.0f, 1.0f,

    1.0f, 0.0f, 0.0f, 1.0f,
    0.0f, 1.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 1.0f, 1.0f,
};

GLuint posBufferObject = 0;
GLuint colBufferObject = 0;

GLint posloc = 0;
GLint colloc = 0;

GLuint myProgram = 0;

//------------------------------------------------------------------------------
void initializeVertexBuffer(){
  glGenBuffers( 1, &posBufferObject );
  glBindBuffer( GL_ARRAY_BUFFER, posBufferObject );
  glBufferData( GL_ARRAY_BUFFER, sizeof(vertexPositions), vertexPositions,
                GL_STATIC_DRAW );

  glGenBuffers( 1, &colBufferObject );

  glBindBuffer( GL_ARRAY_BUFFER, colBufferObject );
  glBufferData( GL_ARRAY_BUFFER, sizeof(vertexColors), vertexColors,
                GL_STATIC_DRAW );

  glBindBuffer( GL_ARRAY_BUFFER, 0 );
}

void initializeProgram(){
    std::vector<GLuint> shaders;
    std::list<std::string> available = {"1.20","3.30"};

    std::string glslv;

    const GLubyte * glslVersion = glGetString( GL_SHADING_LANGUAGE_VERSION );
    std::string sv(reinterpret_cast<const char*>(glslVersion));
    
    for( auto name: available ){
        if( 0 == sv.find(name) ){
            glslv = name;
            break;
        }
    }
    
    if( "" == glslv ){
        printf( "Invalid shader version: %s\n", sv.c_str());
        exit(EXIT_FAILURE);
    }

    std::string strVertexShader = "ex01."+glslv+".vert";
    std::string strFragmentShader = "ex01."+glslv+".frag";

    shaders.push_back( createShader( GL_VERTEX_SHADER, strVertexShader ) );
    shaders.push_back( createShader( GL_FRAGMENT_SHADER, strFragmentShader ) );

    myProgram = createProgram( shaders );

    std::for_each( shaders.begin(), shaders.end(), glDeleteShader );

    posloc = glGetAttribLocation( myProgram, "position");
    colloc = glGetAttribLocation( myProgram, "color");
}

//------------------------------------------------------------------------------
void renderScene(void) {
  glClearColor( 0.0f, 0.0f, 0.0f, 0.0f );
  glClear( GL_COLOR_BUFFER_BIT );

  glUseProgram( myProgram );

  glBindBuffer( GL_ARRAY_BUFFER, posBufferObject );
  glEnableVertexAttribArray( posloc );
  glVertexAttribPointer( posloc, 4, GL_FLOAT, GL_FALSE, 0, 0 );

  glBindBuffer( GL_ARRAY_BUFFER, colBufferObject );
  glEnableVertexAttribArray( colloc );
  glVertexAttribPointer( colloc, 4, GL_FLOAT, GL_FALSE, 0, 0 );

  glDrawArrays( GL_TRIANGLES, 0, 12 );

  glDisableVertexAttribArray( posloc );
  glDisableVertexAttribArray( colloc);
  glUseProgram( 0 );

  glutSwapBuffers();
}

void reshape( int w, int h ){
  glViewport( 0, 0, (GLsizei)w, (GLsizei)h );
}

//------------------------------------------------------------------------------
int main(int argc, char **argv ){
  // init GLUT and create Window
  glutInit( &argc, argv );
  glutInitDisplayMode( GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA );
  glutInitWindowSize( 320, 320 );
  glutCreateWindow( "Onion World" );

  initializeProgram();
  initializeVertexBuffer();

  // register callbacks
  glutDisplayFunc( renderScene );
  glutReshapeFunc( reshape );

  glutMainLoop();

  return EXIT_SUCCESS;
}

//------------------------------------------------------------------------------
