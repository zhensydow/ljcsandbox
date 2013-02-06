#version 120

attribute vec4 position;
attribute vec4 color;
uniform vec2 offset;

varying vec4 theColor;

void main(){
    gl_Position = position + vec4( offset.x, offset.y, 0.0f, 0.0f);
    theColor = color;
}
