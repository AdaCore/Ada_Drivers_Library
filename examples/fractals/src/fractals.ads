------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

package Fractals is

   subtype Base_Float is Float;

   type Coordinate is record
      X : Base_Float;
      Y : Base_Float;
   end record;

   type Screen is record
      X0, Y0, Width, Height : Base_Float;
   end record;

   type Fractal is abstract tagged null record;
   type Fractal_Ref is access all Fractal'Class;

   function Default_Screen (F : Fractal) return Screen is abstract;
   function Compute
     (F         : Fractal;
      Z0        : Coordinate;
      Max_Depth : Natural) return Natural
      is abstract;

   type Fractal_Mandelbrot is new Fractal with null record;

   overriding function Default_Screen (F : Fractal_Mandelbrot) return Screen
   is (-2.5, -1.0, 3.5, 2.0);

   overriding function Compute
     (F         : Fractal_Mandelbrot;
      Z0        : Coordinate;
      Max_Depth : Natural) return Natural;

   type Fractal_Julia is new Fractal with null record;

   overriding function Default_Screen (F : Fractal_Julia) return Screen
   is (-1.5, -1.0, 3.0, 2.0);

   overriding function Compute
     (F         : Fractal_Julia;
      Z0        : Coordinate;
      Max_Depth : Natural) return Natural;

   Mandelbrot : aliased Fractal_Mandelbrot;
   Julia      : aliased Fractal_Julia;

end Fractals;
