------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017-2020, AdaCore                      --
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

with nRF.Device;  use nRF.Device;
with nRF.GPIO;    use nRF.GPIO;
with NRF52_DK.Time; use NRF52_DK.Time;

package body NRF52_DK.Buttons is

   type Button_State_Array is array (Button_Id) of Button_State;
   Points : constant array (Button_Id) of GPIO_Point := (P13, P14, P15, P16);
   States : Button_State_Array := (others => Released);
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
      Conf.Resistors    := Pull_Up;
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
      Prev_States : constant Button_State_Array := States;
   begin
      --  Update all components of States array

      for Id in Button_Id loop
         if not Set (Points (Id)) then
            States (Id) := Pressed;
         else
            States (Id) := Released;
         end if;
      end loop;

      --  Notify changes to subscribers

      for Id in Button_Id loop
         if States (Id) /= Prev_States (Id) then
            for Sub of Subscribers loop

               if Sub /= null then
                  Sub.all (Id, States (Id));
               end if;
            end loop;
         end if;
      end loop;
   end Tick_Handler;

   -----------
   -- State --
   -----------

   function State (Button : Button_Id) return Button_State is
   begin
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
end NRF52_DK.Buttons;
