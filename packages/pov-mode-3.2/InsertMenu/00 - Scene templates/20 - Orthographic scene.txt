// Persistence of Vision Ray Tracer Scene Description File
// File: ?.pov
// Vers: 3.6
// Desc: Orthographic Scene Example
//       useful for generating image_maps, heightfields, etc.
// Date: mm/dd/yy
// Auth: ?
//

#version 3.6;

global_settings {
  assumed_gamma 1.0
}

// ----------------------------------------

camera {
  orthographic
  location <0,0,1>     // position & direction of view
  look_at  <0,0,0>
  right 1*x            // horizontal size of view  \___ to be rendered at square size
  up 1*y               // vertical size of view    /
}

// ----------------------------------------

box {                  // this box fits exactly in view
  <-0.5, -0.5, 0>, <0.5, 0.5, 0>
  texture {
    pigment {
      agate
      color_map {
        [0.0 color rgb 0.0 ]
        [1.0 color rgb 1.0 ]
      }
    }
    finish {
      ambient 1.0
      diffuse 0.0
    }
  }
}

