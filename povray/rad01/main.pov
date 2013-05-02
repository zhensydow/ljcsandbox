#version 3.6;
#include "colors.inc"

#declare RADIOSITY_ON=1;
#declare WITH_LIGHTS=1;
#declare CLOSED_ROOM=1;
#declare FAST_PREVIEW=1; /* worst -1, 0, 1 best */

#include "rad01_mat.inc"

global_settings { 
  assumed_gamma 1.0
  ambient_light White
    max_trace_level select( FAST_PREVIEW, 2, 5, 10 )

#if (RADIOSITY_ON)
  radiosity {
#if (WITH_LIGHTS)
    brightness select( FAST_PREVIEW, 5.0, 2.0 )
    count select( FAST_PREVIEW, 10, 40, 100 )
    error_bound 0.15
    gray_threshold 0.0
    low_error_factor 0.2
    minimum_reuse 0.015
    nearest_count 10
    recursion_limit select( FAST_PREVIEW, 2, 3, 5 )
    adc_bailout 0.01
    max_sample 0.5
    media off
    normal off
    always_sample 1
    pretrace_start 0.08
    pretrace_end 0.01
#else
    brightness select( FAST_PREVIEW, 5.0, 2.2 )
    count select( FAST_PREVIEW, 50, 100, 300 )
    error_bound 0.15
    gray_threshold 0.0
    low_error_factor 0.2
    minimum_reuse 0.015
    nearest_count 10
    recursion_limit select( FAST_PREVIEW, 2, 3, 5 )
    adc_bailout 0.01
    max_sample -1.0
    media off
    normal off
    always_sample 1
    pretrace_start 0.08
    pretrace_end 0.01
#end
  }
#end
}

sky_sphere {
  pigment { color White }
}

camera { location <0, 2, -2.5> look_at  <0, 2,  4> }

#if (WITH_LIGHTS)
#declare AREA_SIZE = select( FAST_PREVIEW, 3, 20, 100 );
light_source { 
  <-5, 3.5, 3.5> color White
  area_light <0, 0, 0.5>, <0, 0.5, 0>, AREA_SIZE, AREA_SIZE
  adaptive 3
  circular
}
#end

sphere {
  <0, 2, 4>, 0.2
  texture {
    pigment { color Yellow }
  }
}

#declare WholeStair = cylinder { <0, -0.2, 7>, <0, 5, 7>, 3.5 }
#declare InnerStair = cylinder { <0, 0, 7>, <0, 4, 7> ,3.3 }

// FLOOR
box {
  <-3, -0.2, -3>, <3, 0.0, 9>
  texture { floorTex }
}

// CEIL
#if (CLOSED_ROOM)
difference {
  box { <-3, 3.0, -3>, <3, 3.2, 9> }
  object { WholeStair }
  texture { ceilTex }
}
#end

// WALLS
difference {
  box { <3.0, 0, -3>, <2.8, 3, 9> }
  object { InnerStair }
  texture { wallTex }
}

#if (CLOSED_ROOM)
box {
  <-3.0, 0, -3>, <-2.8, 3, 3>
  texture { wallTex }
}
  
difference {
  box { <-3.0, 0, 4>, <-2.8, 3, 9> }
  object { InnerStair }
  texture { wallTex }
}
#end

#if (CLOSED_ROOM)
box {
  <-3.0, 0, -3>, <3.0, 3, -2.8>
  texture { wallTex }
}
#end

// STAIRS
#declare STAIR_CENTER = 7;
#declare STAIR_HEIGHT = 0.2;

#declare Stair = box {
  <1.7, 0, -0.3>, <-1.7, STAIR_HEIGHT, 0.3>
  texture { stairTex }
}

cylinder {
  <0, 0, STAIR_CENTER>, <0, 5, STAIR_CENTER>, 1.5
  texture { wallTex  }
}


difference {
  object { WholeStair }
  object { InnerStair }
  box { <-3, 0, 3>, <3, 3, STAIR_CENTER> }
  texture { wallTex }
}

#declare countStairs = 0;
#while (countStairs < 20)
object { Stair 
	   translate <-1.7,0,0> 
	   rotate <0,10*countStairs -20,0> 
	   translate <0,STAIR_HEIGHT*countStairs,STAIR_CENTER> 
}
#declare countStairs = countStairs + 1;
#end
