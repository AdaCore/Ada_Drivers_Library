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
