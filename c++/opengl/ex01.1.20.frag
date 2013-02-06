#version 120

varying vec4 theColor;

void main(){
  float lerpx = gl_FragCoord.x / 200.0f;
  float lerpy = gl_FragCoord.y / 200.0f;

  gl_FragColor = mix( vec4(0.1, 0.1, 0.1f, 1.0f ), theColor, lerpy*lerpx );
}
