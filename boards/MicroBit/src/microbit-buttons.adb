------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with nRF51.GPIO;    use nRF51.GPIO;
with MicroBit.Time; use MicroBit.Time;

package body MicroBit.Buttons is

   Points : constant array (Button_Id) of GPIO_Point := (MB_P5, MB_P11);
   States : array (Button_Id) of Button_State := (others => Released);
   Subscribers : array (1 .. 10) of Button_Callback := (others => null);

   procedure Initialize;
   procedure Tick_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Conf : GPIO_Configuration;
   begin
      Conf.Mode         := Mode_In;
      Conf.Resistors    := No_Pull;
      Conf.Input_Buffer := Input_Buffer_Connect;
      Conf.Sense        := Sense_Disabled;

      for Pt of Points loop
         Pt.Configure_IO (Conf);
      end loop;

      if not Tick_Subscribe (Tick_Handler'Access) then
         raise Program_Error;
      end if;

   end Initialize;

   ------------------
   -- Tick_Handler --
   ------------------

   procedure Tick_Handler is
      New_State : Button_State;
   begin

      for Id in Button_Id loop
         if not Set (Points (Id)) then
            New_State := Pressed;
         else
            New_State := Released;
         end if;

         if States (Id) /= New_State then
            for Sub of Subscribers loop

               if Sub /= null then
                  Sub.all (Id, New_State);
               end if;
            end loop;

            States (Id) := New_State;
         end if;
      end loop;
   end Tick_Handler;

   -----------
   -- State --
   -----------

   function State (Button : Button_Id) return Button_State is
   begin
      if Button =  Button_B then
         --  For a reason that I don't understand right now, the B button is a
         --  always detected as pressed (low). As far as I can see the code is
         --  the same as button A and probing the hardware didn't show any
         --  problem...
         raise Program_Error with "Button B is not working...";
      end if;
      return States (Button);
   end State;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (Callback : not null Button_Callback) return Boolean is
   begin
      for Subs of Subscribers loop
         if Subs = null then
            Subs := Callback;
            return True;
         end if;
      end loop;

      return False;
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   function Unsubscribe (Callback : not null Button_Callback) return Boolean is
   begin
      for Subs of Subscribers loop
         if Subs = Callback then
            Subs := null;
            return True;
         end if;
      end loop;
      return False;
   end Unsubscribe;

begin
   Initialize;
end MicroBit.Buttons;
