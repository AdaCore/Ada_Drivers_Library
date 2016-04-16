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

--  Based on ft6x06.h from MCD Application Team

with Ada.Real_Time;        use Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Interfaces.Bit_Types; use Interfaces, Interfaces.Bit_Types;

with STM32.Board;          use STM32.Board;
with STM32.Device;         use STM32.Device;
with STM32.I2C;            use STM32.I2C;
with STM32.GPIO;           use STM32.GPIO;
with STM32.LCD;            use STM32.LCD;
with HAL.Touch_Panel;      use HAL.Touch_Panel;

with FT6x06;               use FT6x06;

package body Touch_Panel is

   --  I2C Slave address of touchscreen FocalTech FT5336
   TP_ADDR  : constant := 16#54#;

   Driver : FT6x06.FT6x06_Device (Port     => TP_I2C'Access,
                                  I2C_Addr => TP_ADDR);

   procedure My_Delay (Ms : Integer);
   --  Wait the specified number of milliseconds

   procedure TP_Init_Pins;
   --  Initializes the Touch Panel GPIO pins

   procedure TP_I2C_Config;
   --  Initializes the I2C bus

   function Get_Touch_State
     (Touch_Id : Byte;
      Status   : out Boolean) return TP_Touch_State;
   --  Retrieves the position and pressure information of the specified
   --  touch

   pragma Warnings (Off, "* is not referenced");


   pragma Warnings (On, "* is not referenced");

   --------------
   -- My_Delay --
   --------------

   procedure My_Delay (Ms : Integer) is
      Next_Start : Time := Clock;
      Period    : constant Time_Span := Milliseconds (Ms);
   begin
      Next_Start := Next_Start + Period;
      delay until Next_Start;
   end My_Delay;

   ---------------
   -- Init_Pins --
   ---------------

   procedure TP_Init_Pins
   is
      Pins : constant GPIO_Points := TP_Pins;
   begin
      Enable_Clock (Pins);

      Reset (TP_I2C);

      Configure_Alternate_Function (Pins, GPIO_AF_I2C);
      Configure_IO (Pins,
                    (Speed       => Speed_25MHz,
                     Mode        => Mode_AF,
                     Output_Type => Open_Drain,
                     Resistors   => Floating));
      Lock (Pins);

      Enable_Clock (TP_INT);

      Configure_IO (TP_INT,
                    (Speed       => Speed_50MHz,
                     Mode        => Mode_In,
                     Output_Type => Open_Drain,
                     Resistors   => Pull_Up));
      Lock (TP_INT);
   end TP_Init_Pins;

   -------------------
   -- TP_I2C_Config --
   -------------------

   procedure TP_I2C_Config
   is
      I2C_Conf : I2C_Configuration;
   begin
      Enable_Clock (TP_I2C);

      --  Wait at least 200ms after power up before accessing the TP registers
      My_Delay (200);

      if not TP_I2C.Port_Enabled then
         Reset (TP_I2C);

         I2C_Conf.Own_Address := 16#00#;
         I2C_Conf.Addressing_Mode := Addressing_Mode_7bit;
         I2C_Conf.General_Call_Enabled := False;
         I2C_Conf.Clock_Stretching_Enabled := True;

         I2C_Conf.Clock_Speed := 100_000;

         TP_I2C.Configure (I2C_Conf);
      end if;
   end TP_I2C_Config;

   ----------------
   -- Initialize --
   ----------------

   function Initialize return Boolean is
   begin
      TP_Init_Pins;
      TP_I2C_Config;

      Driver.TP_Set_Use_Interrupts (False);

      return Driver.Check_Id;
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

   function Detect_Touch return Natural
   is
   begin
      return Natural (Driver.Active_Touch_Points);
   end Detect_Touch;

   ---------------------
   -- Get_Touch_State --
   ---------------------

   function Get_Touch_State
     (Touch_Id : Byte;
      Status   : out Boolean) return TP_Touch_State
   is
      Pt  : TP_Touch_State :=
        Driver.Get_Touch_Point (Touch_Identifier (Touch_Id));
      Tmp : Natural;
   begin
      if Pt.X = 0 and then Pt.Y = 0 and then Pt.Weight = 0 then
         Status := False;
         return (0, 0, 0);
      end if;

      if STM32.LCD.SwapXY then
         Tmp  := Pt.X;
         Pt.X := STM32.LCD.Pixel_Width - Pt.Y - 1;
         Pt.Y := Tmp;
      end if;

      Status := True;

      Pt.X := Natural'Max (0, Pt.X);
      Pt.Y := Natural'Max (0, Pt.Y);
      Pt.X := Natural'Min (STM32.LCD.Pixel_Width - 1, Pt.X);
      Pt.Y := Natural'Min (STM32.LCD.Pixel_Height - 1, Pt.Y);

      --  ??? On the STM32F426, Y is returned reverted
      Pt.Y := STM32.LCD.LCD_Natural_Height - Pt.Y - 1;
      Pt.Y := Pt.Y * 11 / 10;

      return Pt;
   end Get_Touch_State;

   ---------------
   -- Get_State --
   ---------------

   function Get_State return TP_State
   is
      Status  : Boolean;
      N_Touch : constant Natural := Detect_Touch;
      State   : TP_State (1 .. N_Touch);

   begin
      if N_Touch = 0 then
         return (1 .. 0 => <>);
      end if;

      for J in State'Range loop
         State (J) := Get_Touch_State (Byte (J), Status);
         if not Status then
            return (1 .. 0 => <>);
         end if;
      end loop;

      return State;
   end Get_State;

end Touch_Panel;
