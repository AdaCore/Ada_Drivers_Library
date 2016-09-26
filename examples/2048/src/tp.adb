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
