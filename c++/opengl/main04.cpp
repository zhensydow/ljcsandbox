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
    // object 1
    -0.8f, 0.2f, -1.75f,
    -0.8f, 0.0f, -1.25f,
    0.8f, 0.0f, -1.25f,
    0.8f, 0.2f, -1.75f,

    -0.8f, -0.2f, -1.75f,
    -0.8f, 0.0f, -1.25f,
    0.8f, 0.0f, -1.25f,
    0.8f, -0.2f, -1.75f,

    -0.8f, 0.2f, -1.75f,
    -0.8f, 0.0f, -1.25f,
    -0.8f, -0.2f, -1.75f,

    0.8f, 0.2f, -1.75f,
    0.8f, 0.0f, -1.25f,
    0.8f, -0.2f, -1.75f,

    -0.8f, -0.2f, -1.75f,
    -0.8f, 0.2f, -1.75f,
    0.8f, 0.2f, -1.75f,
    0.8f, -0.2f, -1.75f,

    // object 2
    0.2f, 0.8f, -1.75f,
    0.0f, 0.8f, -1.25f,
    0.0f, -0.8f, -1.25f,
    0.2f, -0.8f, -1.75f,

    -0.2f, 0.8f, -1.75f,
    0.0f, 0.8f, -1.25f,
    0.0f, -0.8f, -1.25f,
    -0.2f, -0.8f, -1.75f,

    0.2f, 0.8f, -1.75f,
    0.0f, 0.8f, -1.25f,
    -0.2f, 0.8f, -1.75f,

    0.2f, -0.8f, -1.75f,
    0.0f, -0.8f, -1.25f,
    -0.2f, -0.8f, -1.75f,

    -0.2f, 0.8f, -1.75f,
    0.2f, 0.8f, -1.75f,
    0.2f, -0.8f, -1.75f,
    -0.2f, -0.8f, -1.75f,
};

#define GREEN  0.7f, 0.7f, 1.0f, 1.0f
#define BLUE   0.0f, 0.5f, 0.0f, 1.0f
#define RED    1.0f, 0.0f, 0.0f, 1.0f
#define GREY   0.8f, 0.8f, 0.8f, 1.0f
#define BROWN  0.5f, 0.5f, 0.0f, 1.0f

const float vertexColors[] = {
    // object 1
    GREEN, GREEN, GREEN, GREEN,
    BLUE, BLUE, BLUE, BLUE,
    RED, RED, RED,
    GREY, GREY, GREY,
    BROWN, BROWN, BROWN, BROWN,

    // object 2
    RED, RED, RED, RED,
    BROWN, BROWN, BROWN, BROWN,
    BLUE, BLUE, BLUE,
    GREEN, GREEN, GREEN,
    GREY, GREY, GREY, GREY,
};

const GLshort indexData[] = {
    0, 2, 1, 3, 2, 0,
    4, 5, 6, 6, 7, 4,
    8, 9, 10,
    11, 13, 12,
    14, 16, 15, 17, 16, 14,
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
GLuint idxBufferObject = 0;

GLuint perspectiveMatrixUnif = 0;

GLint posLoc = 0;
GLint colLoc = 0;
GLint offsetUnif = 0;

GLuint myProgram = 0;

GLuint vaoObject1 = 0;
GLuint vaoObject2 = 0;

//------------------------------------------------------------------------------
void initializeProgram(){
    myProgram = loadProgram( "ex04" );
    posLoc = glGetAttribLocation( myProgram, "position" );
    colLoc = glGetAttribLocation( myProgram, "color" );

    offsetUnif = glGetUniformLocation( myProgram, "offset" );

    perspectiveMatrixUnif = glGetUniformLocation( myProgram, "perspectiveMatrix" );

    float fzNear = 0.5f;
    float fzFar = 3.0f;

    perspectiveMatrix[0] = fFrustumScale;
    perspectiveMatrix[5] = fFrustumScale;
    perspectiveMatrix[10] = (fzFar + fzNear) / (fzNear - fzFar);
    perspectiveMatrix[14] = (2 * fzFar * fzNear) / (fzNear - fzFar);
    perspectiveMatrix[11] = -1.0f;

    glUseProgram( myProgram );
    glUniformMatrix4fv( perspectiveMatrixUnif, 1, GL_FALSE, perspectiveMatrix );
    glUseProgram( 0 );
}

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

    glGenBuffers( 1, &idxBufferObject );
    glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, idxBufferObject );
    glBufferData( GL_ELEMENT_ARRAY_BUFFER, sizeof(indexData), indexData,
                  GL_STATIC_DRAW );
    glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, 0 );
}

void initializeVertexArrayObjects(){
    glGenVertexArrays( 1, &vaoObject1 );
    glBindVertexArray( vaoObject1 );

    glBindBuffer( GL_ARRAY_BUFFER, posBufferObject );
    glEnableVertexAttribArray( posLoc );
    glVertexAttribPointer( posLoc, 3, GL_FLOAT, GL_FALSE, 0, 0 );

    glBindBuffer( GL_ARRAY_BUFFER, colBufferObject );
    glEnableVertexAttribArray( colLoc );
    glVertexAttribPointer( colLoc, 4, GL_FLOAT, GL_FALSE, 0, 0);

    glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, idxBufferObject );

    glGenVertexArrays( 1, &vaoObject2 );
    glBindVertexArray( vaoObject2 );

    size_t offset = sizeof(float) * 3 * 18;

    glBindBuffer( GL_ARRAY_BUFFER, posBufferObject );
    glEnableVertexAttribArray( posLoc );
    glVertexAttribPointer( posLoc, 3, GL_FLOAT, GL_FALSE, 0, (void*)offset );

    offset = sizeof(float) * 4 * 18;

    glBindBuffer( GL_ARRAY_BUFFER, colBufferObject );
    glEnableVertexAttribArray( colLoc );
    glVertexAttribPointer( colLoc, 4, GL_FLOAT, GL_FALSE, 0, (void*)offset );

    glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, idxBufferObject );


    glBindVertexArray(0);
}

//------------------------------------------------------------------------------
void renderScene(void) {
    glClearColor( 0.0f, 0.0f, 0.0f, 0.0f );
    glClear( GL_COLOR_BUFFER_BIT );

    glUseProgram( myProgram );

    glBindVertexArray( vaoObject1 );
    glUniform3f( offsetUnif, 0.0f, 0.0f, 0.0f );
    glDrawElements( GL_TRIANGLES, 24, GL_UNSIGNED_SHORT, 0 );

    glBindVertexArray( vaoObject2 );
    glUniform3f( offsetUnif, 0.0f, 0.0f, 0.0f );
    glDrawElements( GL_TRIANGLES, 24, GL_UNSIGNED_SHORT, 0 );

    glBindVertexArray( 0 );
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
  initializeVertexArrayObjects();

  glEnable( GL_CULL_FACE );
  glCullFace( GL_BACK );
  glFrontFace( GL_CW );

  // register callbacks
  glutDisplayFunc( renderScene );
  glutReshapeFunc( reshape );

  glutMainLoop();

  return EXIT_SUCCESS;
}

//------------------------------------------------------------------------------
