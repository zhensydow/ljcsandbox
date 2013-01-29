#version 330

out vec4 outputColor;
void main(){
  float lerpy = gl_FragCoord.y / 500.0f;
  float lerpx = gl_FragCoord.x / 500.0f;

  float rc = mix( 0.0f, 1.0f, lerpy );
  float gc = mix( 0.0f, 1.0f, lerpx );
  outputColor =vec4(rc, gc, 0.2f, 1.0f );
}
