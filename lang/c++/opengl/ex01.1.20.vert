#version 120

attribute vec4 position;
attribute vec4 color;
uniform float loopDuration;
uniform float time;

varying vec4 theColor;

void main(){
    float timeScale = 3.14159f * 2.0f / loopDuration;

    float currTime = mod(time, loopDuration);
    vec4 totalOffset = vec4(
        cos(currTime * timeScale) * 0.5f,
        sin(currTime * timeScale) * 0.5f,
        0.0f,
        0.0f);

    gl_Position = position + totalOffset;
    theColor = color;
}
