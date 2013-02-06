#version 120

varying vec4 theColor;

uniform float fragLoopDuration;
uniform float time;

const vec4 firstColor = vec4(0.0f, 1.0f, 0.0f, 1.0f);

void main(){
  float lerpx = gl_FragCoord.x / 200.0f;
  float lerpy = gl_FragCoord.y / 200.0f;

  float currTime = mod(time, fragLoopDuration);
  float currLerp = currTime / fragLoopDuration;
  
  vec4 baseColor = mix( firstColor, theColor, currLerp );
  gl_FragColor = mix( vec4(0.0, 0.0, 0.0f, 1.0f ), baseColor, lerpy*lerpx );
}
