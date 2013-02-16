#version 330

smooth in vec2 theTex;

uniform vec3 color;

out vec4 outputColor;

const float thickness = 1.0;

void main(){
    vec2 px = dFdx(theTex);
    vec2 py = dFdy(theTex);
    float fx = (2.0*theTex.x)*px.x - px.y;
    float fy = (2.0*theTex.x)*py.x - py.y;
    float sd = (theTex.x*theTex.x - theTex.y)/sqrt(fx*fx + fy*fy);
    float alpha = thickness - abs(sd);
    if( alpha > 1 ){       // Inside
        alpha = 1;
    }else if( alpha < 0 ){  // Outside
        discard; //clip(-1);
    }
    outputColor = vec4( color.r, color.g, color.b, alpha );
}
