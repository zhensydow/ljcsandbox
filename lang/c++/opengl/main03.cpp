//------------------------------------------------------------------------------
#include <cstdlib>

#include <vector>

#define GL_GLEXT_PROTOTYPES 1
#define GL3_PROTOTYPES 1
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glext.h>
#include <GL/glut.h>

#include "shaders.h"

//------------------------------------------------------------------------------
const float vertexPositions[] = {
    0.25f, 0.0f, 0.75f, 1.0f,
    0.25f, -0.5f, 0.75f, 1.0f,
    -0.25f, 0.0f, 0.75f, 1.0f,

    -0.25f, 0.0f, 0.75f, 1.0f,
    -0.75f, 0.75f, 0.75f, 1.0f,
    0.0f, 0.5f, 0.75f, 1.0f,

    0.0f, 0.5f, 0.75f, 1.0f,
    0.25f, 0.5f, 0.75f, 1.0f,
    0.25f, 0.0f, 0.75f, 1.0f,

    -0.25f, 0.0f, 0.75f, 1.0f,
    0.0f, 0.5f, 0.75f, 1.0f,
    0.25f, 0.0f, 0.75f, 1.0f,
};

const float vertexColors[] = {
    0.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 0.0f, 1.0f,

    1.0f, 0.0f, 0.0f, 1.0f,
    1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f,

    1.0f, 0.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 1.0f, 1.0f,

    1.0f, 0.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 0.0f, 1.0f, 1.0f,
};

const float texCoords[] = {
    0.0f, 0.0f,
    0.5f, 0.0f,
    1.0f, 1.0f,

    0.0f, 0.0f,
    0.5f, 0.0f,
    1.0f, 1.0f,

    0.0f, 0.0f,
    0.5f, 0.0f,
    1.0f, 1.0f,
};

GLuint posBufferObject = 0;
GLuint colBufferObject = 0;
GLuint texBufferObject = 0;

GLint posLoc1 = 0;
GLint colLoc1 = 0;
GLint texLoc1 = 0;

GLint posLoc2 = 0;
GLint texLoc2 = 0;

GLint posLoc3 = 0;
GLint colLoc3 = 0;

GLuint myProgram1 = 0;
GLuint myProgram2 = 0;
GLuint myProgram3 = 0;

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

  glGenBuffers( 1, &texBufferObject );

  glBindBuffer( GL_ARRAY_BUFFER, texBufferObject );
  glBufferData( GL_ARRAY_BUFFER, sizeof(texCoords), texCoords,
                GL_STATIC_DRAW );

  glBindBuffer( GL_ARRAY_BUFFER, 0 );
}

void initializeProgram(){
    myProgram1 = loadProgram( "ex03a" );
    posLoc1 = glGetAttribLocation( myProgram1, "position" );
    colLoc1 = glGetAttribLocation( myProgram1, "color" );
    texLoc1 = glGetAttribLocation( myProgram1, "tex" );

    myProgram2 = loadProgram( "ex03b" );
    posLoc2 = glGetAttribLocation( myProgram2, "position" );
    texLoc2 = glGetAttribLocation( myProgram2, "tex" );

    GLint colLoc2 = glGetUniformLocation( myProgram2, "color" );

    glUseProgram( myProgram2 );
    glUniform3f( colLoc2, 0.8f, 0.8f, 0.8f );
    glUseProgram( 0 );

    myProgram3 = loadProgram( "ex03c" );
    posLoc3 = glGetAttribLocation( myProgram3, "position" );
    colLoc3 = glGetAttribLocation( myProgram3, "color" );
}

//------------------------------------------------------------------------------
void renderScene(void) {
    constexpr size_t nverts = 3 * 3;

    glClearColor( 0.0f, 0.0f, 0.0f, 0.0f );
    glClear( GL_COLOR_BUFFER_BIT );

    glUseProgram( myProgram1 );

    glBindBuffer( GL_ARRAY_BUFFER, posBufferObject );
    glEnableVertexAttribArray( posLoc1 );
    glVertexAttribPointer( posLoc1, 4, GL_FLOAT, GL_FALSE, 0, 0 );

    glBindBuffer( GL_ARRAY_BUFFER, colBufferObject );
    glEnableVertexAttribArray( colLoc1 );
    glVertexAttribPointer( colLoc1, 4, GL_FLOAT, GL_FALSE, 0, 0 );

    glBindBuffer( GL_ARRAY_BUFFER, texBufferObject );
    glEnableVertexAttribArray( texLoc1 );
    glVertexAttribPointer( texLoc1, 2, GL_FLOAT, GL_FALSE, 0, 0 );

    glDrawArrays( GL_TRIANGLES, 0, nverts );

    glDisableVertexAttribArray( posLoc1 );
    glDisableVertexAttribArray( colLoc1 );
    glDisableVertexAttribArray( texLoc1 );

    glUseProgram( myProgram2 );

    glBindBuffer( GL_ARRAY_BUFFER, posBufferObject );
    glEnableVertexAttribArray( posLoc2 );
    glVertexAttribPointer( posLoc2, 4, GL_FLOAT, GL_FALSE, 0, 0 );

    glBindBuffer( GL_ARRAY_BUFFER, texBufferObject );
    glEnableVertexAttribArray( texLoc2 );
    glVertexAttribPointer( texLoc2, 2, GL_FLOAT, GL_FALSE, 0, 0 );

    glDrawArrays( GL_TRIANGLES, 0, nverts );

    glDisableVertexAttribArray( posLoc2 );
    glDisableVertexAttribArray( texLoc2 );

    glUseProgram( myProgram3 );

    glBindBuffer( GL_ARRAY_BUFFER, posBufferObject );
    glEnableVertexAttribArray( posLoc3 );
    glVertexAttribPointer( posLoc3, 4, GL_FLOAT, GL_FALSE, 0, (void*)(4*nverts*sizeof(float)) );

    glBindBuffer( GL_ARRAY_BUFFER, colBufferObject );
    glEnableVertexAttribArray( colLoc3 );
    glVertexAttribPointer( colLoc3, 4, GL_FLOAT, GL_FALSE, 0, (void*)(4*nverts*sizeof(float)) );
    
    glDrawArrays( GL_TRIANGLES, 0, 3 );

    glDisableVertexAttribArray( posLoc3 );
    glDisableVertexAttribArray( colLoc3 );

    glUseProgram( 0 );

    glutSwapBuffers();
    glutPostRedisplay();
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
  glutCreateWindow( "Example03" );

  initializeProgram();
  initializeVertexBuffer();

  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);
  glFrontFace(GL_CW);

  // register callbacks
  glutDisplayFunc( renderScene );
  glutReshapeFunc( reshape );

  glutMainLoop();

  return EXIT_SUCCESS;
}

//------------------------------------------------------------------------------
