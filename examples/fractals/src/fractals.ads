package Fractals is

   type Coordinate is record
      X : Float;
      Y : Float;
   end record;

   type Screen is record
      X0, Y0, Width, Height : Float;
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
