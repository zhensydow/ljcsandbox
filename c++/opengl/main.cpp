//------------------------------------------------------------------------------
#include <cstdlib>

#include <vector>
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

GLuint positionBufferObject = 0;
GLuint myProgram = 0;

std::string strVertexShader = "ex01.vert";
std::string strFragmentShader = "ex01.frag";

//------------------------------------------------------------------------------
void initializeVertexBuffer(){
  glGenBuffers( 1, &positionBufferObject );

  glBindBuffer( GL_ARRAY_BUFFER, positionBufferObject );
  glBufferData( GL_ARRAY_BUFFER, sizeof(vertexPositions), vertexPositions,
		GL_STATIC_DRAW );
  glBindBuffer( GL_ARRAY_BUFFER, 0 );
}

void initializeProgram(){
  std::vector<GLuint> shaders;

  shaders.push_back( createShader( GL_VERTEX_SHADER, strVertexShader ) );
  shaders.push_back( createShader( GL_FRAGMENT_SHADER, strFragmentShader ) );

  myProgram = createProgram( shaders );

  std::for_each( shaders.begin(), shaders.end(), glDeleteShader );
}

//------------------------------------------------------------------------------
void renderScene(void) {
  glClearColor( 0.0f, 0.0f, 0.0f, 0.0f );
  glClear( GL_COLOR_BUFFER_BIT );

  glUseProgram( myProgram );
  glBindBuffer( GL_ARRAY_BUFFER, positionBufferObject );
  glEnableVertexAttribArray( 0 );
  glVertexAttribPointer( 0, 4, GL_FLOAT, GL_FALSE, 0, 0 );

  glDrawArrays( GL_TRIANGLES, 0, 12 );

  glDisableVertexAttribArray( 0 );
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
