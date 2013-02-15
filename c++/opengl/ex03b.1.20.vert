#version 120

attribute vec4 position;
attribute vec2 tex;

varying vec2 theTex;

void main(){
    gl_Position = position;
    theTex = tex;
}
