#version 120

varying vec4 theColor;
varying vec2 theTex;

void main(){
    vec2 px = dFdx(theTex);
    vec2 py = dFdy(theTex);
    float fx = (2.0*theTex.x)*px.x - px.y;
    float fy = (2.0*theTex.x)*py.x - py.y;
    float sd = (theTex.x*theTex.x - theTex.y)/sqrt(fx*fx + fy*fy);
    float alpha = 0.5 - sd;
    if( alpha > 1 ){       // Inside
        alpha = 1;
    }else if( alpha < 0 ){  // Outside
        discard; //clip(-1);
    }
    gl_FragColor = vec4( theColor.r, theColor.g, theColor.b, alpha );
}
