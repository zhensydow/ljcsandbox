#version 120

attribute vec4 position;
attribute vec4 color;

varying vec4 theColor;

void main(){
    gl_Position = position;
    theColor = color;
}
