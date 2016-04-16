------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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

--  Based on ft5336.h from MCD Application Team

with Ada.Real_Time;        use Ada.Real_Time;
with Ada.Unchecked_Conversion;

with Interfaces.Bit_Types; use Interfaces, Interfaces.Bit_Types;

with STM32.Board;          use STM32.Board;
with STM32.Device;         use STM32.Device;
with STM32.LCD;            use STM32.LCD;
with FT5336;               use FT5336;
with HAL.I2C;              use HAL.I2C;
with HAL.Touch_Panel;      use HAL.Touch_Panel;

package body Touch_Panel is

   --  I2C Slave address of touchscreen FocalTech FT5336
   TP_ADDR  : constant := 16#70#;

   LL_Driver : FT5336.FT5336_Device (Port     => TP_I2C'Access,
                                     I2C_Addr => TP_ADDR);

   ----------------
   -- Initialize --
   ----------------

   function Initialize return Boolean is
   begin
      Initialize_I2C_GPIO (TP_I2C);

      --  Wait at least 200ms after power up before accessing the TP registers
      delay until Clock + Milliseconds (200);

      Configure_I2C (TP_I2C);

      LL_Driver.TP_Set_Use_Interrupts (False);

      return LL_Driver.Check_Id;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Ret : Boolean with Unreferenced;
   begin
      Ret := Initialize;
   end Initialize;

   ------------------
   -- Detect_Touch --
   ------------------

   function Detect_Touch return Natural is
   begin
      return Natural (LL_Driver.Active_Touch_Points);
   end Detect_Touch;

   ---------------
   -- Get_State --
   ---------------

   function Get_State return TP_State
   is
      Status  : I2C_Status := Ok;

      ---------------------
      -- Get_Touch_State --
      ---------------------

      function Get_Touch_State (Num : Byte) return TP_Touch_State
      is
         Pt  : TP_Touch_State :=
                 LL_Driver.Get_Touch_Point (Touch_Identifier (Num));
         Tmp : Natural;

      begin
         if Pt.X = 0 and then Pt.Y = 0 and then Pt.Weight = 0 then
            Status := Busy;
            return (0, 0, 0);
         end if;

         if STM32.LCD.SwapXY then
            Tmp  := Pt.X;
            Pt.X := STM32.LCD.Pixel_Width - Pt.Y - 1;
            Pt.Y := Tmp;
         end if;

         Status := Ok;

         return
           (X => Natural'Min (Natural'Max (0, Pt.X), Pixel_Width - 1),
            Y => Natural'Min (Natural'Max (0, Pt.Y), Pixel_Height - 1),
            Weight => Pt.Weight);
      end Get_Touch_State;

      N_Touch : constant Natural := Detect_Touch;
      State   : TP_State (1 .. N_Touch);

   begin
      if N_Touch = 0 then
         return (1 .. 0 => <>);
      end if;

      for J in State'Range loop
         State (J) := Get_Touch_State (Byte (J));
         if Status /= Ok then
            return (1 .. 0 => <>);
         end if;
      end loop;

      return State;
   end Get_State;

end Touch_Panel;
