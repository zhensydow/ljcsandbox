#version 120

varying vec4 theColor;

uniform float fragLoopDuration;
uniform float time;
uniform float w;
uniform float h;

const vec4 firstColor = vec4(0.0f, 1.0f, 0.0f, 1.0f);

void main(){
    float timeScale = 3.14159f * 2.0f / fragLoopDuration;

    float lerpx = gl_FragCoord.x / (w==0 ? 1 : w);
    float lerpy = gl_FragCoord.y / (h==0 ? 1 : h);

    float currTime = mod(time, fragLoopDuration);
    float currLerp = sin(currTime * timeScale);
  
    vec4 baseColor = mix( firstColor, theColor, currLerp );
    gl_FragColor = mix( vec4(0.0, 0.0, 0.0f, 1.0f ), baseColor, lerpy*lerpx );
}
