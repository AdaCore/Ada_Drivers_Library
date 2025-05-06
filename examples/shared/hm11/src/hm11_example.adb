------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

--  Simple test/example for STM32F429disco with HM-11/cc2541 attached
--  It implements a simple "echo" server that can be connected from a phone

with Ada.Exceptions;
with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with Ada.Real_Time;  use Ada.Real_Time;

with HAL.UART;       use HAL.UART;
with STM32.USARTs;
with HM11;           use HM11;
with Drivers;        use Drivers;

--  Service
with STM32.Board;    use STM32.Board;
with HAL.Bitmap;     use HAL.Bitmap;
with BMP_Fonts;
with LCD_Std_Out;

procedure HM11_Example is

   Status       : UART_Status;

   Period       : constant Time_Span := Milliseconds (200);
   Next_Release : Time := Clock;
   BG           : constant Bitmap_Color := (Alpha => 255, others => 64);

   -----------
   -- Print --
   -----------

   procedure Print (Msg : String) is
   begin
      LCD_Std_Out.Put_Line (Msg);
      Display.Update_Layer (1, Copy_Back => True);
   end Print;

   --------------
   -- On_Error --
   --------------

   procedure On_Error (Msg : String) is
   begin
      Print (Msg);

      loop
         STM32.Board.Toggle (Red_LED);
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end On_Error;

   ------------------
   -- Check_Status --
   ------------------

   procedure Check_Status (Msg : String) is
   begin
      if Status = Ok then
         Print (Msg & " OK");
      else
         On_Error (Msg & " failed");
      end if;
   end Check_Status;

begin
   delay 2.0;

   Drivers.Init_UART;
   delay 1.0;
   Drivers.Initialize_DMA;
   delay 1.0;

   -- Init testing infrastructure
   STM32.Board.Initialize_LEDs;
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_8888);
   LCD_Std_Out.Set_Font (BMP_Fonts.Font12x12);
   LCD_Std_Out.Current_Background_Color := BG;
   Display.Hidden_Buffer (1).Set_Source (BG);
   Display.Hidden_Buffer (1).Fill;
   LCD_Std_Out.Clear_Screen;

   delay 1.0;
   Print ("Initialized");

   --  Tests --
   Test (Drivers.Driver, Status);
   Check_Status ("Test");

   delay 0.5;
   declare
      Result : MAC_Address;
      S : String (Result'Range) with Import, Address => Result'Address;
   begin
      Get_MAC_Address (Drivers.Driver, Result, Status);
      if Status /= Ok then
         On_Error ("Get_MAC_Address failed");
      end if;
      Print ("MAC_Address:" & S);
   end;

   delay 0.5;
   Set_Advertising_Type
     (Drivers.Driver, Advertising_ScanResponse_Connectable, Status);
   Check_Status ("Set_Advertising_Type");

   delay 0.5;
   Set_Bond_Mode (Drivers.Driver, Auth_With_PIN, Status);
   Check_Status ("Set_Bond_Mode");

   delay 0.5;
   Set_Notify_Information (Drivers.Driver, True, Status);
   Check_Status ("Set_Notify_Information");

   delay 0.5;
   Set_Module_Name (Drivers.Driver, "HMTest", Status);
   Check_Status ("Set_Module_Name");

   delay 0.5;
   Set_PIN_Code (Drivers.Driver, "000000", Status);
   Check_Status ("Set_PIN_Code");

   delay 0.5;
   Set_Work_Type (Drivers.Driver, Start_Immediately, Status);
   Check_Status ("Set_Work_Type");

   delay 0.5;
   Set_Role (Drivers.Driver, Peripheral, Status);
   Check_Status ("Set_Role");

   delay 1.0;
   Restart (Drivers.Driver, Status);
   Check_Status ("Restart");

   delay 2.0;
   Print ("Waiting connection");

   --  On this stage you can connect from your phone with
   --  Serial bluetooth terminal (for example)
   declare
      Data : String (When_Connected_Message'Range);
   begin
      loop
         DMA_Receive_Handler
           (UART'Access, Data'Address, Data'Length, Status, 120 * 1000);

         if Data = When_Connected_Message then
            Print ("Connected");
            exit;
         end if;

         Check_Status ("Connect loop");
      end loop;
   end;

   --  Waiting for data or disconnect
   --  On this stage you can send (1 .. 5) message from phone terminal
   --  and module will return this message back.
   --  For example if you send `12345` you will see `12345` returned.
   declare
      Data : String (When_Disconnected_Message'Range);
      Send : UART_Data_8b (Data'Range) with Import, Address => Data'Address;
   begin
      loop
         DMA_Receive_Handler
           (UART'Access, Data'Address, When_Disconnected_Message'Length,
            Status, 120 * 1000);

         if Data = When_Disconnected_Message then
            Print ("Disconnected");
            exit;

         else
            STM32.USARTs.Transmit (UART, Send, Status);
         end if;

         Check_Status ("Transmit loop");
      end loop;
   end;

   -- All is OK
   Print ("Done");

   loop
      STM32.Board.Toggle (Green_LED);
      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;

exception
   when E : others =>
      On_Error (Ada.Exceptions.Exception_Information (E));
end HM11_Example;
