#version 120

attribute vec4 position;
attribute vec4 color;
attribute vec2 tex;

varying vec4 theColor;
varying vec2 theTex;

void main(){
    gl_Position = position;
    theColor = color;
    theTex = tex;
}
