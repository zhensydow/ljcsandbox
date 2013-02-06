#version 330

smooth in vec4 theColor;

out vec4 outputColor;

void main(){
  float lerpx = gl_FragCoord.x / 200.0f;
  float lerpy = gl_FragCoord.y / 200.0f;

  outputColor = mix( vec4(0.1, 0.1, 0.1f, 1.0f ), theColor, lerpy*lerpx );
}
