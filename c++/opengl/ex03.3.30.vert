#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 color;
layout(location = 2) in vec2 tex;

smooth out vec4 theColor;
smooth out vec2 theTex;

void main(){
    gl_Position = position;
    theColor = color;
    theTex = tex;
}
