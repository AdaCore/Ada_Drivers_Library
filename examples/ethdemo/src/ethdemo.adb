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
