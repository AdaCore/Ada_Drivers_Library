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

with Ada.Real_Time;     use Ada.Real_Time;
with Ada.Text_IO;       use Ada.Text_IO;

with STM32.Board;       use STM32.Board;
with HAL.Touch_Panel;

package body TP is

   type Button is record
      CB   : Button_Callback;
      Area : Rect;
   end record;

   G_Buttons     : array (1 .. Max_Buttons) of Button;
   Last_Btn       : Natural := 0;
   Click_Started : Natural := 0;

   Slide_CB      : Slide_Callback := null;
   Slide_Started : Boolean := False;
   Slide_Vector  : Touch_Vector :=
                     (Start_Point => (0, 0),
                      End_Point   => (0, 0));

   procedure Init_Vector (V : in out Touch_Vector);

   task TP_Updater is
   end TP_Updater;

   ----------------
   -- TP_Updater --
   ----------------

   task body TP_Updater is
   begin
      loop
         if Last_Btn > 0 or else Slide_CB /= null then
            Update;
         end if;

         delay until Clock + Milliseconds (50);
      end loop;
   end TP_Updater;

   ---------------------
   -- Add_Button_Area --
   ---------------------

   procedure Add_Button_Area
     (Area     : Rect;
      Callback : Button_Callback)
   is
   begin
      if Last_Btn = G_Buttons'Last then
         raise Constraint_Error with "Maximum number of buttons reached";
      end if;

      Last_Btn := Last_Btn + 1;
      G_Buttons (Last_Btn) := (Callback, Area);
   end Add_Button_Area;

   -------------
   -- In_Area --
   -------------

   function In_Area
     (Pos  : HAL.Touch_Panel.TP_Touch_State;
      Area : Rect) return Boolean
   is (Pos.X >= Area.Position.X
       and then Pos.X <= (Area.Position.X + Area.Width)
       and then Pos.Y >= Area.Position.Y
       and then Pos.Y <= (Area.Position.Y + Area.Height));

   -----------------
   -- Init_Vector --
   -----------------

   procedure Init_Vector (V : in out Touch_Vector) is
   begin
      V := (Start_Point => (0, 0),
            End_Point   => (0, 0));
   end Init_Vector;

   ------------------------
   -- Set_Slide_Callback --
   ------------------------

   procedure Set_Slide_Callback
     (Callback : Slide_Callback)
   is
   begin
      Slide_CB := Callback;
      Init_Vector (Slide_Vector);
   end Set_Slide_Callback;

   ------------
   -- Update --
   ------------

   procedure Update is
      Touch_Data : constant HAL.Touch_Panel.TP_State :=
                     Touch_Panel.Get_All_Touch_Points;
   begin
      if Touch_Data'Length > 0 then
         if not Slide_Started then
            Click_Started := 0;
            for J in 1 .. Last_Btn loop
               if In_Area (Touch_Data (1), G_Buttons (J).Area) then
                  Click_Started := J;
                  exit;
               end if;
            end loop;
         end if;

         if Slide_CB /= null and then Click_Started = 0 then
            if not Slide_Started then
               Slide_Started := True;
               Slide_Vector.Start_Point := (X => Touch_Data (1).X,
                                            Y => Touch_Data (1).Y);
            end if;
         end if;

         Slide_Vector.End_Point := (X => Touch_Data (1).X,
                                    Y => Touch_Data (1).Y);

      elsif Click_Started /= 0 then
         G_Buttons (Click_Started).CB
           (Slide_Vector.End_Point.X -
              G_Buttons (Click_Started).Area.Position.X,
            Slide_Vector.End_Point.Y -
              G_Buttons (Click_Started).Area.Position.Y);
         Click_Started := 0;
         Ada.Text_IO.Put_Line ("X: " & Slide_Vector.End_Point.X'Img);
         Ada.Text_IO.Put_Line ("Y: " & Slide_Vector.End_Point.Y'Img);

      elsif Slide_Started then
         Slide_CB (Slide_Vector);
         Init_Vector (Slide_Vector);
         Slide_Started := False;
      end if;
   end Update;

end TP;
