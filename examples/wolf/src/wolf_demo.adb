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

with Last_Chance_Handler;
pragma Unreferenced (Last_Chance_Handler);


with Ada.Real_Time;         use Ada.Real_Time;

with Cortex_M.FPU;

with STM32.Button;
with STM32.Board;           use STM32.Board;
with STM32.SDRAM;
with HAL.Framebuffer;       use HAL.Framebuffer;
with HAL.Bitmap;            use HAL.Bitmap;

with Raycaster;             use Raycaster;
with Cos;                   use Cos;

--  A simple raycasting demo
--  Below, we're using the follwing conventions:
--
--  Coordinates:
--  +---> X
--  |
--  |
--  v y
--
--  with angles in tenth of degrees, anticlockwise.

procedure Wolf_Demo
is
   type Movement_Kind is (Move_To, Turn_Left, Turn_Right);

   type Movement (Kind : Movement_Kind := Move_To) is record
      case Kind is
         when Move_To =>
            X, Y : Float;
         when Turn_Left | Turn_Right =>
            Angle : Degree;
      end case;
   end record;

   Path : constant array (Natural range <>) of Movement :=
                 ((Move_To, 2.5, 2.5),
                  (Turn_Right, 0),
                  (Move_To, 5.5, 2.5),
                  (Turn_Left, 450),
                  (Move_To, 6.5, 1.5),
                  (Turn_Right, 0),
                  (Move_To, 14.5, 1.5),
                  (Turn_Right, 2700),
                  (Move_To, 14.5, 3.5),
                  (Turn_Right, 1800),
                  (Move_To, 9.5, 3.5),
                  (Turn_Left, 2700),
                  (Move_To, 9.5, 6.5),
                  (Turn_Left, 3150),
                  (Move_To, 11.5, 8.5),
                  (Turn_Left, 0),
                  (Move_To, 15.1, 8.5),
                  (Turn_Left, 1800),
                  (Move_To, 1.5, 8.5),
                  (Turn_Right, 900),
                  (Move_To, 1.5, 6.5),
                  (Turn_Right, 0),
                  (Move_To, 4.5, 6.5),
                  (Turn_Left, 450),
                  (Move_To, 5.5, 5.5),
                  (Turn_Left, 900),
                  (Move_To, 5.5, 2.5),
                  (Turn_Left, 1800),
                  (Move_To, 2.5, 2.5),
                  (Turn_Left, 2700),
                  (Move_To, 2.5, 3.5),
                  (Turn_Right, 900));

   Start : Time;

   procedure Do_Move_To (X, Y : Float);
   procedure Do_Turn (Angle : Degree; Clockwise : Boolean);

   ----------------
   -- Do_Move_To --
   ----------------

   procedure Do_Move_To (X, Y : Float)
   is
      Initial_X : constant Float := Current.X;
      Initial_Y : constant Float := Current.Y;
      Distance  : constant Float :=
                    Cortex_M.FPU.Sqrt
                      ((X - Initial_X) ** 2 + (Y - Initial_Y) ** 2);
      Total_T   : constant Time_Span :=
                    To_Time_Span (Duration (Distance) / 3.5); --  3.5 tiles/s
      Delta_T   : Time_Span;
      Ratio     : Float;

   begin
      loop
         Delta_T := Clock - Start;
         exit when Delta_T > Total_T;
         Ratio := Float (To_Duration (Delta_T) / To_Duration (Total_T));
         Current.X := Initial_X + (X - Initial_X) * Ratio;
         Current.Y := Initial_Y + (Y - Initial_Y) * Ratio;
         Draw;
      end loop;

      Start := Start + Total_T;

      Current.X := X;
      Current.Y := Y;
   end Do_Move_To;

   -------------
   -- Do_Turn --
   -------------

   procedure Do_Turn (Angle : Degree; Clockwise : Boolean)
   is
      Initial_Angle : constant Degree := Current.Angle;
      Delta_A       : constant Degree :=
                        (if Clockwise
                         then Initial_Angle - Angle
                         else Angle - Initial_Angle);
      Total_T       : constant Time_Span :=
                        To_Time_Span (Duration (Delta_A) / 2200.0); -- 220º/s
      Delta_T       : Time_Span;
      Ratio         : Float;

   begin
      loop
         Delta_T := Clock - Start;
         exit when Delta_T > Total_T;
         Ratio := Float (To_Duration (Delta_T) / To_Duration (Total_T));

         if Clockwise then
            Current.Angle := Initial_Angle - Degree (Float (Delta_A) * Ratio);
         else
            Current.Angle := Initial_Angle + Degree (Float (Delta_A) * Ratio);
         end if;
         Draw;
      end loop;

      Start := Start + Total_T;

      Current.Angle := Angle;
   end Do_Turn;

begin
   STM32.SDRAM.Initialize;
   STM32.Button.Initialize;
   Display.Initialize (HAL.Framebuffer.Landscape);
   Display.Initialize_Layer
     (Layer  => 1,
      Mode   => HAL.Bitmap.RGB_565);
   Display.Initialize_Layer
     (Layer  => 2,
      Mode   => HAL.Bitmap.AL_44,
      X      => 0,
      Y      => 10,
      Width  => 8 * 12,
      Height => 12);

   Display.Get_Hidden_Buffer (1).Fill (0);
   Display.Get_Hidden_Buffer (2).Fill (0);
   Display.Update_Layers;

   Initialize_Tables;

   Start := Clock;

   loop
      for Mov of Path loop
         case Mov.Kind is
         when Move_To =>
            Do_Move_To (Mov.X, Mov.Y);

         when Turn_Left =>
            Do_Turn (Mov.Angle, False);

         when Turn_Right =>
            Do_Turn (Mov.Angle, True);

         end case;
      end loop;
   end loop;

end Wolf_Demo;
