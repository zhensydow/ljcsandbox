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
    0.25f, 0.25f, -1.25f, 1.0f,
    0.25f, -0.25f, -1.25f, 1.0f,
    -0.25f, 0.25f, -1.25f, 1.0f,

     0.25f, -0.25f, -1.25f, 1.0f,
    -0.25f, -0.25f, -1.25f, 1.0f,
    -0.25f,  0.25f, -1.25f, 1.0f,

     0.25f,  0.25f, -2.75f, 1.0f,
    -0.25f,  0.25f, -2.75f, 1.0f,
     0.25f, -0.25f, -2.75f, 1.0f,

     0.25f, -0.25f, -2.75f, 1.0f,
    -0.25f,  0.25f, -2.75f, 1.0f,
    -0.25f, -0.25f, -2.75f, 1.0f,

    -0.25f,  0.25f,  -1.25f, 1.0f,
    -0.25f, -0.25f,  -1.25f, 1.0f,
    -0.25f, -0.25f, -2.75f, 1.0f,

    -0.25f,  0.25f,  -1.25f, 1.0f,
    -0.25f, -0.25f, -2.75f, 1.0f,
    -0.25f,  0.25f, -2.75f, 1.0f,

     0.25f,  0.25f,  -1.25f, 1.0f,
     0.25f, -0.25f, -2.75f, 1.0f,
     0.25f, -0.25f,  -1.25f, 1.0f,

     0.25f,  0.25f,  -1.25f, 1.0f,
     0.25f,  0.25f, -2.75f, 1.0f,
     0.25f, -0.25f, -2.75f, 1.0f,

     0.25f,  0.25f, -2.75f, 1.0f,
     0.25f,  0.25f,  -1.25f, 1.0f,
    -0.25f,  0.25f,  -1.25f, 1.0f,

     0.25f,  0.25f, -2.75f, 1.0f,
    -0.25f,  0.25f,  -1.25f, 1.0f,
    -0.25f,  0.25f, -2.75f, 1.0f,

     0.25f, -0.25f, -2.75f, 1.0f,
    -0.25f, -0.25f,  -1.25f, 1.0f,
     0.25f, -0.25f,  -1.25f, 1.0f,

     0.25f, -0.25f, -2.75f, 1.0f,
    -0.25f, -0.25f, -2.75f, 1.0f,
    -0.25f, -0.25f,  -1.25f, 1.0f,
};

const float vertexColors[] = {
    0.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 0.0f, 1.0f, 1.0f,

    0.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 0.0f, 1.0f, 1.0f,

    0.8f, 0.8f, 0.8f, 1.0f,
    0.8f, 0.8f, 0.8f, 1.0f,
    0.8f, 0.8f, 0.8f, 1.0f,

    0.8f, 0.8f, 0.8f, 1.0f,
    0.8f, 0.8f, 0.8f, 1.0f,
    0.8f, 0.8f, 0.8f, 1.0f,

    0.0f, 1.0f, 0.0f, 1.0f,
    0.0f, 1.0f, 0.0f, 1.0f,
    0.0f, 1.0f, 0.0f, 1.0f,

    0.0f, 1.0f, 0.0f, 1.0f,
    0.0f, 1.0f, 0.0f, 1.0f,
    0.0f, 1.0f, 0.0f, 1.0f,

    0.5f, 0.5f, 0.0f, 1.0f,
    0.5f, 0.5f, 0.0f, 1.0f,
    0.5f, 0.5f, 0.0f, 1.0f,

    0.5f, 0.5f, 0.0f, 1.0f,
    0.5f, 0.5f, 0.0f, 1.0f,
    0.5f, 0.5f, 0.0f, 1.0f,

    1.0f, 0.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 0.0f, 1.0f,

    1.0f, 0.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 0.0f, 1.0f,

    0.0f, 1.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 1.0f,

    0.0f, 1.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 1.0f,
};

float perspectiveMatrix[] = {
    0.0f, 0.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 0.0f, 0.0f,
};

float fFrustumScale = 1.0f;

GLuint posBufferObject = 0;
GLuint colBufferObject = 0;

GLuint perspectiveMatrixUnif = 0;

GLint posLoc = 0;
GLint colLoc = 0;

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
    myProgram = loadProgram( "ex02" );
    posLoc = glGetAttribLocation( myProgram, "position" );
    colLoc = glGetAttribLocation( myProgram, "color" );

    auto offsetUnif = glGetUniformLocation( myProgram, "offset" );

    perspectiveMatrixUnif = glGetUniformLocation( myProgram, "perspectiveMatrix" );

    float fzNear = 0.5f; 
    float fzFar = 3.0f;

    perspectiveMatrix[0] = fFrustumScale;
    perspectiveMatrix[5] = fFrustumScale;
    perspectiveMatrix[10] = (fzFar + fzNear) / (fzNear - fzFar);
    perspectiveMatrix[14] = (2 * fzFar * fzNear) / (fzNear - fzFar);
    perspectiveMatrix[11] = -1.0f;

    glUseProgram( myProgram );
    glUniform2f( offsetUnif, 1.5f, 0.5f );
    glUniformMatrix4fv( perspectiveMatrixUnif, 1, GL_FALSE, perspectiveMatrix );
    glUseProgram( 0 );
}

//------------------------------------------------------------------------------
void renderScene(void) {
    glClearColor( 0.0f, 0.0f, 0.0f, 0.0f );
    glClear( GL_COLOR_BUFFER_BIT );

    glUseProgram( myProgram );

    glBindBuffer( GL_ARRAY_BUFFER, posBufferObject );
    glEnableVertexAttribArray( posLoc );
    glVertexAttribPointer( posLoc, 4, GL_FLOAT, GL_FALSE, 0, 0 );

    glBindBuffer( GL_ARRAY_BUFFER, colBufferObject );
    glEnableVertexAttribArray( colLoc );
    glVertexAttribPointer( colLoc, 4, GL_FLOAT, GL_FALSE, 0, 0 );

    glDrawArrays( GL_TRIANGLES, 0, 36 );

    glDisableVertexAttribArray( posLoc );
    glDisableVertexAttribArray( colLoc);
    glUseProgram( 0 );

    glutSwapBuffers();
    glutPostRedisplay();
}

void reshape( int w, int h ){
    perspectiveMatrix[0] = fFrustumScale/(w/static_cast<float>(h));
    perspectiveMatrix[5] = fFrustumScale;
    
    glUseProgram( myProgram );
    glUniformMatrix4fv( perspectiveMatrixUnif, 1, GL_FALSE, perspectiveMatrix );
    glUseProgram( 0 );

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
