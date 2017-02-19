------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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

with HAL.GPIO;     use HAL.GPIO;
with STM32.I2C;    use STM32.I2C;
with STM32.GPIO;   use STM32.GPIO;
with STM32.Device; use STM32.Device;
with STM32.USARTs; use STM32.USARTs;

package body STM32.Generic_UNO_Header is

   Headers : aliased STM32_UNO_Headers;
   --  Singleton

   ------------
   -- Header --
   ------------

   function Header return not null Any_UNO_Headers is
   begin
      return Headers'Access;
   end Header;

   -------------
   -- As_GPIO --
   -------------

   overriding function As_GPIO
     (This : in out STM32_UNO_Headers;
      Pin  : UNO_Pin)
      return not null HAL.GPIO.Any_GPIO_Point
   is
   begin
      return This.Pins (Pin);
   end As_GPIO;

   --------------
   -- Pin_Mode --
   --------------

   overriding procedure Pin_Mode
     (This : in out STM32_UNO_Headers;
      Pin  : UNO_Pin;
      Mode : UNO_Pin_Mode)
   is
   begin
      Enable_Clock (This.Pins (Pin).all);
      case Mode is
         when OUTPUT =>
            if not This.Pins (Pin).Set_Mode (Output) then
               raise Program_Error with "Cannot configure as OUTPUT";
            end if;
         when INPUT =>
            if not This.Pins (Pin).Set_Pull_Resistor (Floating)
              or else
               not This.Pins (Pin).Set_Mode (Input)
            then
               raise Program_Error with "Cannot configure as INPUT";
            end if;
         when INPUT_PULLUP =>
            if not This.Pins (Pin).Set_Pull_Resistor (Pull_Up)
              or else
               not This.Pins (Pin).Set_Mode (Input)
            then
               raise Program_Error with "Cannot configure as INPUT_PULLUP";
            end if;
      end case;
   end Pin_Mode;

   ------------------
   -- Digital_Read --
   ------------------

   overriding function Digital_Read
     (This : in out STM32_UNO_Headers;
      Pin  : UNO_Pin)
      return Boolean
   is
   begin
      return This.Pins (Pin).Set;
   end Digital_Read;

   -------------------
   -- Digital_Write --
   -------------------

   overriding procedure Digital_Write
     (This : in out STM32_UNO_Headers;
      Pin  : UNO_Pin;
      Val  : Boolean)
   is
   begin
      if Val then
         This.Pins (Pin).Set;
      else
         This.Pins (Pin).Clear;
      end if;
   end Digital_Write;

   -----------------
   -- Analog_Read --
   -----------------

   overriding function Analog_Read
     (This : in out STM32_UNO_Headers;
      Pin  : UNO_Analog_Input_Pin)
      return Analog_Value
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error with "Unimplemented function Analog_Read";
      return Analog_Read (This => This, Pin => Pin);
   end Analog_Read;

   ------------------
   -- Analog_Write --
   ------------------

   overriding procedure Analog_Write
     (This : in out STM32_UNO_Headers;
      Pin  : UNO_PWM_Pin;
      Val  : Analog_Value)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error with "Unimplemented procedure Analog_Write";
   end Analog_Write;

   ------------------
   -- Serial_Begin --
   ------------------

   overriding procedure Serial_Begin
     (This : in out STM32_UNO_Headers;
      Baud : UInt32)
   is
      pragma Unreferenced (This);
      IO_Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (D0_Pt.all);
      Enable_Clock (D1_Pt.all);

      IO_Conf.Mode := Mode_AF;
      IO_Conf.Speed := Speed_50MHz;
      IO_Conf.Output_Type := Open_Drain;
      IO_Conf.Resistors := Floating;

      D0_Pt.Configure_IO (IO_Conf);
      D1_Pt.Configure_IO (IO_Conf);

      D0_Pt.Configure_Alternate_Function (UART_TX_AF);
      D1_Pt.Configure_Alternate_Function (UART_RX_AF);

      Enable_Clock (UART_Port.all);

      UART_Port.Set_Baud_Rate (Baud);
      UART_Port.Set_Mode (Tx_Rx_Mode);
      UART_Port.Set_Word_Length (Word_Length_8);
      UART_Port.Set_Parity (No_Parity);
      UART_Port.Set_Stop_Bits (Stopbits_1);
      UART_Port.Enable;
   end Serial_Begin;

   ----------------
   -- Serial_End --
   ----------------

   overriding procedure Serial_End
     (This : in out STM32_UNO_Headers)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error with "Unimplemented procedure Serial_End";
   end Serial_End;

   -----------------
   -- Serial_Port --
   -----------------

   overriding function Serial_Port
     (This : STM32_UNO_Headers)
      return not null HAL.UART.Any_UART_Port
   is
      pragma Unreferenced (This);
   begin
      return UART_Port;
   end Serial_Port;

   ----------------
   -- Wire_Begin --
   ----------------

   overriding procedure Wire_Begin
     (This : in out STM32_UNO_Headers)
   is
      pragma Unreferenced (This);
      I2C_Conf : I2C_Configuration;
      IO_Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (D14_Pt.all);
      Enable_Clock (D15_Pt.all);

      IO_Conf.Mode := Mode_AF;
      IO_Conf.Speed := Speed_50MHz;
      IO_Conf.Output_Type := Open_Drain;
      IO_Conf.Resistors := Floating;

      D14_Pt.Configure_IO (IO_Conf);
      D15_Pt.Configure_IO (IO_Conf);

      D14_Pt.Configure_Alternate_Function (SDA_AF);
      D15_Pt.Configure_Alternate_Function (SCL_AF);

      Enable_Clock (I2C_Port.all);

      I2C_Conf.Clock_Speed := 100_000;
      I2C_Conf.Own_Address := 16#00#;
      I2C_Conf.Addressing_Mode := Addressing_Mode_7bit;
      I2C_Conf.General_Call_Enabled := False;
      I2C_Conf.Clock_Stretching_Enabled := True;

      I2C_Port.Configure (I2C_Conf);
   end Wire_Begin;

   --------------
   -- Wire_End --
   --------------

   overriding procedure Wire_End
     (This : in out STM32_UNO_Headers)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error with "Unimplemented procedure Wire_End";
   end Wire_End;

   ---------------
   -- Wire_Port --
   ---------------

   overriding function Wire_Port
     (This : STM32_UNO_Headers)
      return not null HAL.I2C.Any_I2C_Port
   is
      pragma Unreferenced (This);
   begin
      return I2C_Port;
   end Wire_Port;

end STM32.Generic_UNO_Header;
