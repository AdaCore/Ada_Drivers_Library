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

package body Fractals is

   function Square (C : Coordinate) return Coordinate
   is ((C.X ** 2 - C.Y ** 2, 2.0 * C.X * C.Y))
   with Inline_Always;

   function Square (C : Coordinate) return Base_Float
   is (C.X ** 2 + C.Y ** 2)
   with Inline_Always;

   function "+" (C1, C2 : Coordinate) return Coordinate
   is ((C1.X + C2.X, C1.Y + C2.Y))
   with Inline_Always;

   -------------
   -- Compute --
   -------------

   overriding function Compute
     (F         : Fractal_Mandelbrot;
      Z0        : Coordinate;
      Max_Depth : Natural) return Natural
   is
      pragma Unreferenced (F);
      Z    : Coordinate := (0.0, 0.0);
      Iter : Natural := 0;
   begin
      while Square (Z) <= 4.0 and then Iter < Max_Depth loop
         Z := Square (Z) + Z0;
         Iter := Iter + 1;
      end loop;

      return Iter;
   end Compute;

   -------------
   -- Compute --
   -------------

   overriding function Compute
     (F         : Fractal_Julia;
      Z0        : Coordinate;
      Max_Depth : Natural) return Natural
   is
      pragma Unreferenced (F);
      C    : constant Coordinate := (-0.7, 0.27015);
      Z    : Coordinate := Z0;
      Iter : Natural := 0;
   begin
      while Square (Z) <= 4.0 and then Iter < Max_Depth loop
         Z := Square (Z) + C;
         Iter := Iter + 1;
      end loop;

      return Iter;
   end Compute;

end Fractals;
