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

with Ada.Real_Time;
with Interfaces;               use Interfaces;
with STM32.Eth;
with LCD_Std_Out;              use LCD_Std_Out;

procedure Ethdemo is

   function Hex_Image (V : Unsigned_16) return String;

   subtype String4 is String (1 .. 4);

   Hex : constant array (Natural range 0 .. 15) of Character :=
           "0123456789ABCDEF";

   function Hex_Image (V : Unsigned_16) return String
   is
      Res : String4;
   begin
      for I in Res'Range loop
         Res (I) := Hex (Natural (Shift_Right (V, (4 - I) * 4) and 15));
      end loop;
      return Res;
   end Hex_Image;


begin

   Set_Font (Default_Font);
   Clear_Screen;
   Put_Line ("Hello");

   if False then
      declare
         use Ada.Real_Time;
         T : Time := Clock;
         P : constant Time_Span := Seconds (1);
      begin
         for I in 1 .. 10 loop
            T := T + P;
            delay until T;
            Put ("Time #");
            Put (Hex (I));
            New_Line;
         end loop;
      end;
   end if;

   STM32.Eth.Initialize_RMII;

   --  Wait until link is up.
   declare
      use STM32.Eth;
      use Ada.Real_Time;
      T : Time := Clock;
      P : constant Time_Span := Seconds (2);
      V : Unsigned_16;
   begin
      loop
         Read_MMI (1, V);
         exit when (V and 4) /= 0;
         Clear_Screen;
         Put_Line ("Eth link is down");
         delay until T;
         T := T + P;
      end loop;
   end;

   Clear_Screen;
   Put_Line ("Starting");

   STM32.Eth.Init_Mac;
   STM32.Eth.Start_Rx;

   declare
      Pkt_Count : Unsigned_16;
   begin
      Pkt_Count := 0;
      loop
         STM32.Eth.Wait_Packet;
         Pkt_Count := Pkt_Count + 1;
         Put (0, 0, Hex_Image (Pkt_Count));
      end loop;
   end;
end Ethdemo;
