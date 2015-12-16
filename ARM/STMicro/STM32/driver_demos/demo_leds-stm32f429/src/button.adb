------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with Ada.Real_Time; use Ada.Real_Time;
with STM32_Board;   use STM32_Board;
with STM32.GPIO;    use STM32.GPIO;
with STM32.SYSCFG;  use STM32.SYSCFG;

package body Button is

   ------------
   -- Button --
   ------------

   protected Button is
      pragma Interrupt_Priority;

      function Current_Direction return Directions;

   private
      procedure Interrupt_Handler;
      pragma Attach_Handler (Interrupt_Handler, User_Button_Interrupt);

      Direction : Directions := Clockwise;  -- arbitrary
      Last_Time : Time := Clock;
   end Button;

   Debounce_Time : constant Time_Span := Milliseconds (500);

   ------------
   -- Button --
   ------------

   protected body Button is

      function Current_Direction return Directions is
      begin
         return Direction;
      end Current_Direction;

      procedure Interrupt_Handler is
         Now : constant Time := Clock;
      begin
         Clear_External_Interrupt (User_Button_Pin);

         --  Debouncing
         if Now - Last_Time >= Debounce_Time then
            if Direction = Counterclockwise then
               Direction := Clockwise;
            else
               Direction := Counterclockwise;
            end if;
            Last_Time := Now;
         end if;
      end Interrupt_Handler;

   end Button;

   -----------------------
   -- Current_Direction --
   -----------------------

   function Current_Direction return Directions is
   begin
      return Button.Current_Direction;
   end Current_Direction;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Configure_User_Button_GPIO;
      Connect_External_Interrupt (User_Button_Port, User_Button_Pin);
      Configure_Trigger (User_Button_Port, User_Button_Pin, Interrupt_Rising_Edge);
   end Initialize;

begin
   Initialize;
end Button;
