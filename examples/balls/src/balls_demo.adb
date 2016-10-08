------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  The file declares the main procedure for the demonstration. The LEDs
--  will blink "in a circle" on the board. The blue user button generates
--  an interrupt that changes the direction.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with System;
with Interfaces;            use Interfaces;
with STM32.Button;          use STM32.Button;
with STM32.Board;           use STM32.Board;
with STM32.RNG.Interrupts;  use STM32.RNG.Interrupts;

with HAL;                   use HAL;
with HAL.Bitmap;            use HAL.Bitmap;

with Bitmapped_Drawing;     use Bitmapped_Drawing;

procedure Balls_Demo is
   use STM32;

   pragma Priority (System.Priority'First);

   type HSV_Color is record
      Hue : Byte;
      Sat : Byte;
      Val : Byte;
   end record;

   function To_RGB (Col : HSV_Color) return Bitmap_Color;
   --  Translates a Hue/Saturation/Value color into RGB

   type Coordinate is (X, Y);

   type Vector is array (Coordinate) of Float;

   function "+" (V1, V2 : Vector) return Vector;
   function "-" (V1, V2 : Vector) return Vector;
   function "*" (V1, V2 : Vector) return Float;
   function "*" (V : Vector; Val : Float) return Vector;
   function "/" (V : Vector; Val : Float) return Vector;
   procedure Init_Balls;

   ---------
   -- "+" --
   ---------

   function "+" (V1, V2 : Vector) return Vector
   is
   begin
      return (X => V1 (X) + V2 (X),
              Y => V1 (Y) + V2 (Y));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (V1, V2 : Vector) return Vector
   is
   begin
      return (X => V1 (X) - V2 (X),
              Y => V1 (Y) - V2 (Y));
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (V1, V2 : Vector) return Float
   is
   begin
      return V1 (X) * V2 (X) + V1 (Y) * V2 (Y);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (V : Vector; Val : Float) return Vector
   is
   begin
      return (V (X) * Val, V (Y) * Val);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (V : Vector; Val : Float) return Vector
   is
   begin
      return (V (X) / Val, V (Y) / Val);
   end "/";

   type Moving_Object is record
      Center : Vector;
      Speed  : Vector;
      R      : Natural;
      Col    : HSV_Color;
      N_Hue  : Byte;
   end record;

   function I (F : Float) return Integer
     is (Integer (F));

   function Collides (M1, M2 : Moving_Object) return Boolean;
   procedure Handle_Collision (M1, M2 : in out Moving_Object);

   Objects      : array (1 .. 15) of Moving_Object;

   White_Background : Boolean := False;

   ------------
   -- To_RGB --
   ------------

   function To_RGB (Col : HSV_Color) return Bitmap_Color
   is
      V, S, H : UInt32;
      Region, FPart, p, q, t : UInt32;
      Ret     : Bitmap_Color;
   begin
      Ret.Alpha := 255;

      if Col.Sat = 0 then
         Ret.Red := Col.Val;
         Ret.Green := Col.Val;
         Ret.Blue  := Col.Val;

         return Ret;
      end if;

      V := UInt32 (Col.Val);
      S := UInt32 (Col.Sat);
      H := UInt32 (Col.Hue);

      --  Hue in the range 0 .. 5
      Region := H / 43;
      --  Division reminder, multiplied by 6 to make it in range 0 .. 255
      FPart  := (H - (Region * 43)) * 6;

      p := Shift_Right (V * (255 - S), 8);
      q := Shift_Right (V * (255 - Shift_Right (S * FPart, 8)), 8);
      t := Shift_Right (V * (255 - Shift_Right (S * (255 - FPart), 8)), 8);

      case Region is
         when 0 =>
            Ret.Red   := Byte (V);
            Ret.Green := Byte (t);
            Ret.Blue  := Byte (p);
         when 1 =>
            Ret.Red   := Byte (q);
            Ret.Green := Byte (V);
            Ret.Blue  := Byte (p);
         when 2 =>
            Ret.Red   := Byte (p);
            Ret.Green := Byte (V);
            Ret.Blue  := Byte (t);
         when 3 =>
            Ret.Red   := Byte (p);
            Ret.Green := Byte (q);
            Ret.Blue  := Byte (V);
         when 4 =>
            Ret.Red   := Byte (t);
            Ret.Green := Byte (p);
            Ret.Blue  := Byte (V);
         when others =>
            Ret.Red   := Byte (V);
            Ret.Green := Byte (p);
            Ret.Blue  := Byte (q);
      end case;

      return Ret;
   end To_RGB;

   -------------
   -- Colides --
   -------------

   function Collides (M1, M2 : Moving_Object) return Boolean
   is
      Dist_Min_Square : constant Integer :=
                          (M1.R + M2.R) ** 2;
      M1_M2           : constant Vector := M2.Center - M1.Center;
      Dist_Square     : constant Integer :=
                          (I (M1_M2 (X)) ** 2 + I (M1_M2 (Y)) ** 2);
   begin
      return Dist_Square < Dist_Min_Square;
   end Collides;

   ----------------------
   -- Handle_Colisiton --
   ----------------------

   procedure Handle_Collision (M1, M2 : in out Moving_Object)
   is
      --  Dist: distance vector between the two objects
      Dist  : Vector;

      --  Projx: Projection of the speed of Mx on Dist
      Tmp   : Float;
      Proj1 : Vector;
      Proj2 : Vector;

      Tan_Sp1 : Vector;
      Tan_Sp2 : Vector;

      Sp1     : Vector;
      Sp2     : Vector;

      --  Simplified mass: equal to R^2
      Mass1   : constant Float := Float (M1.R ** 2);
      Mass2   : constant Float := Float (M2.R ** 2);
      dT      : Float := 0.0;
      Old1, Old2 : Vector;

   begin
      --  Move back before collision
      --  If we don't, then the balls might still be touching themselves
      --  after we execute this procedure, so collision is called a second
      --  time afterwards.
      Old1 := M1.Center;
      Old2 := M2.Center;
      loop
         dT := dT + 0.2;
         M1.Center := Old1 - M1.Speed * dT;
         M2.Center := Old2 - M2.Speed * dT;
         exit when not Collides (M1, M2);
      end loop;

      Dist := (M2.Center - M1.Center);

      --  Calculate the projection vector of the speeds on Dist
      Tmp := (M1.Speed * Dist) / (Dist * Dist);
      Proj1 := Dist * Tmp;
      Tmp := (M2.Speed * Dist) / (Dist * Dist);
      Proj2 := Dist * Tmp;

      --  Now retrieve the speed that is tangantial to dist
      Tan_Sp1 := M1.Speed - Proj1;
      Tan_Sp2 := M2.Speed - Proj2;

      --  Transfer the projected speed from one object to the other
      Sp1 := (Proj1 * (Mass1 - Mass2) + Proj2 * 2.0 * Mass2) / (Mass1 + Mass2);
      Sp2 := (Proj2 * (Mass2 - Mass1) + Proj1 * 2.0 * Mass1) / (Mass1 + Mass2);

      --  Calculate the final speed of the objects:
      --  initial tangantial speed + calculated projected speed
      M1.Speed := Sp1 + Tan_Sp1;
      M2.Speed := Sp2 + Tan_Sp2;

      --  Now replay from the time of the impact with the new speed
      M1.Center := M1.Center + M1.Speed * dT;
      M2.Center := M2.Center + M2.Speed * dT;
      M1.N_Hue  := M1.N_Hue + 32;
      M2.N_Hue  := M2.N_Hue + 32;
   end Handle_Collision;

   ----------------
   -- Init_Balls --
   ----------------

   procedure Init_Balls
   is
      Size   : constant Integer :=
                 Natural'Min (LCD_Natural_Width, LCD_Natural_Height);
      R_Min  : constant Natural :=
                 Size / 24;
      R_Var  : constant UInt32 :=
                 UInt32 (R_Min) * 4 / 5;
      SP_Max : constant Integer :=
                 Size / 7;
   begin
      for J in Objects'Range loop
         loop
            declare
               O     : Moving_Object renames Objects (J);
               R     : constant Integer :=
                         Integer (RNG.Interrupts.Random mod R_Var) + R_Min;
               Col   : constant Byte :=
                         Byte (RNG.Interrupts.Random mod 255);
               X_Raw : constant UInt32 :=
                         (RNG.Interrupts.Random mod
                                    UInt32 (LCD_Natural_Width - 2 * R)) +
                         UInt32 (R);
               Y_Raw : constant UInt32 :=
                         (RNG.Interrupts.Random mod
                                    UInt32 (LCD_Natural_Height - 2 * R)) +
                         UInt32 (R);
               Sp_X  : constant Integer :=
                 Integer (RNG.Interrupts.Random mod UInt32 (SP_Max * 2 + 1)) -
                   SP_Max;
               Sp_Y  : constant Integer :=
                 Integer (RNG.Interrupts.Random mod UInt32 (SP_Max * 2 + 1)) -
                   SP_Max;
               Redo  : Boolean := False;

            begin
               O :=
                 (Center => (Float (Natural (X_Raw) + R),
                             Float (Natural (Y_Raw) + R)),
                  Speed  => (Float (Sp_X) / 10.0,
                             Float (Sp_Y) / 10.0),
                  R      => R,
                  Col    => (Hue => Col,
                             Sat => 255,
                             Val => 255),
                  N_Hue  => Col);

               for K in Objects'First .. J - 1 loop
                  if Collides (O, Objects (K)) then
                     Redo := True;
                     exit;
                  end if;
               end loop;

               exit when not Redo;
            end;
         end loop;
      end loop;
   end Init_Balls;

begin
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_1555);
   STM32.RNG.Interrupts.Initialize_RNG;
   STM32.Button.Initialize;

   Init_Balls;

   loop
      if STM32.Button.Has_Been_Pressed then
         White_Background := not White_Background;
      end if;

      Display.Get_Hidden_Buffer (1).Fill
        ((if White_Background then White else Black));

      for M of Objects loop
         if M.N_Hue /= M.Col.Hue then
            M.Col.Hue := M.Col.Hue + 1;
         end if;
      end loop;

      for O of Objects loop
         O.Center := O.Center + O.Speed;

         if (O.Speed (X) < 0.0 and then I (O.Center (X)) <= O.R)
           or else (O.Speed (X) > 0.0
                    and then I (O.Center (X)) + O.R >= LCD_Natural_Width - 1)
         then
            O.Speed (X) := -O.Speed (X);
            O.Center (X) := O.Center (X) + O.Speed (X);
         end if;

         if (O.Speed (Y) < 0.0 and then I (O.Center (Y)) <= O.R)
           or else (O.Speed (Y) > 0.0
                    and then I (O.Center (Y)) + O.R >= LCD_Natural_Height - 1)
         then
            O.Speed (Y) := -O.Speed (Y);
            O.Center (Y) := O.Center (Y) + O.Speed (Y);
         end if;
      end loop;

      for J in Objects'First .. Objects'Last - 1 loop
         for K in J + 1 .. Objects'Last loop
            if Collides (Objects (J), Objects (K)) then
               Handle_Collision (Objects (J), Objects (K));
            end if;
         end loop;
      end loop;

      for O of Objects loop
         Fill_Circle
           (Display.Get_Hidden_Buffer (1),
            (X => I (O.Center (X)),
             Y => I (O.Center (Y))),
            O.R,
            To_RGB (O.Col));
      end loop;

      Display.Update_Layer (1);
   end loop;

end Balls_Demo;
