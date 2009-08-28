// Persistence of Vision Ray Tracer Scene Description File
// File: ?.pov
// Vers: 3.6
// Desc: Checkered Floor Example
// Date: mm/dd/yy
// Auth: ?
//

#version 3.6;

#include "colors.inc"

global_settings {
  assumed_gamma 1.0
  max_trace_level 5
}

// ----------------------------------------

camera {
  location  <0.0, 0.5, -4.0>
  direction 1.5*z
  right     x*image_width/image_height
  look_at   <0.0, 0.0,  0.0>
}

sky_sphere {
  pigment {
    gradient y
    color_map {
      [0.0 rgb <0.6,0.7,1.0>]
      [0.7 rgb <0.0,0.1,0.8>]
    }
  }
}

light_source {
  <0, 0, 0>            // light's position (translated below)
  color rgb <1, 1, 1>  // light's color
  translate <-30, 30, -30>
}

// ----------------------------------------

plane {               // checkered floor
  y, -1
  texture
  {
    pigment {
      checker
      color rgb 1
      color blue 1
      scale 0.5
    }
    finish{
      diffuse 0.8
      ambient 0.1
    }
  }
}

sphere {              // reflective sphere
  0.0, 1
  texture {
    pigment {
      color rgb <0.8,0.8,1.0>
    }
    finish{
      diffuse 0.3
      ambient 0.0
      specular 0.6
      reflection {
        0.8
        metallic
      }
      conserve_energy
    }
  }
}

