#if (RADIOSITY_ON)
#declare defaultAmbient = 0.0;
#else
#declare defaultAmbient = 0.2;
#end

#declare floorTex = texture { 
  pigment { color White } 
  finish { ambient defaultAmbient }
}
#declare ceilTex = texture {
  pigment { color White }
  finish { ambient defaultAmbient }
}
#declare wallTex = texture {
  pigment { color White }
  finish { ambient defaultAmbient }
}

#declare stairTex = texture {
  pigment { color White }
  finish { ambient defaultAmbient }
}

#declare sphereMat = material {
  texture {
    pigment { color rgb <1.0, 1.0, 1.0, 0.5> }
    finish { ambient defaultAmbient }
  }
}

#declare sphereInt = interior { 
  media {
    scattering { 1, Red extinction 0.25 }
  }
}
