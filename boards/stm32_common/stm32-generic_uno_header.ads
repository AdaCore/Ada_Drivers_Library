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

with HAL.Arduino_Headers; use HAL.Arduino_Headers;
with HAL.GPIO;
with HAL.I2C;
with HAL.UART;

with STM32.GPIO;
with STM32.I2C;
with STM32.USARTs;

generic
   D0_Pt  : not null access STM32.GPIO.GPIO_Point;
   D1_Pt  : not null access STM32.GPIO.GPIO_Point;
   D2_Pt  : not null access STM32.GPIO.GPIO_Point;
   D3_Pt  : not null access STM32.GPIO.GPIO_Point;
   D4_Pt  : not null access STM32.GPIO.GPIO_Point;
   D5_Pt  : not null access STM32.GPIO.GPIO_Point;
   D6_Pt  : not null access STM32.GPIO.GPIO_Point;
   D7_Pt  : not null access STM32.GPIO.GPIO_Point;
   D8_Pt  : not null access STM32.GPIO.GPIO_Point;
   D9_Pt  : not null access STM32.GPIO.GPIO_Point;
   D10_Pt : not null access STM32.GPIO.GPIO_Point;
   D11_Pt : not null access STM32.GPIO.GPIO_Point;
   D12_Pt : not null access STM32.GPIO.GPIO_Point;
   D13_Pt : not null access STM32.GPIO.GPIO_Point;
   D14_Pt : not null access STM32.GPIO.GPIO_Point;
   D15_Pt : not null access STM32.GPIO.GPIO_Point;
   A0_Pt  : not null access STM32.GPIO.GPIO_Point;
   A1_Pt  : not null access STM32.GPIO.GPIO_Point;
   A2_Pt  : not null access STM32.GPIO.GPIO_Point;
   A3_Pt  : not null access STM32.GPIO.GPIO_Point;
   A4_Pt  : not null access STM32.GPIO.GPIO_Point;
   A5_Pt  : not null access STM32.GPIO.GPIO_Point;

   SCL_AF : GPIO_Alternate_Function;
   SDA_AF : GPIO_Alternate_Function;

   I2C_Port : not null access STM32.I2C.I2C_Port;

   UART_TX_AF : GPIO_Alternate_Function;
   UART_RX_AF : GPIO_Alternate_Function;

   UART_Port : not null access STM32.USARTs.USART;

package STM32.Generic_UNO_Header is

   function Header return not null Any_UNO_Headers;

private

   type Pin_Array is array (UNO_Pin) of not null access STM32.GPIO.GPIO_Point;

   type STM32_UNO_Headers is new UNO_Headers with record
      Pins : Pin_Array := (D0  => D0_Pt,
                           D1  => D1_Pt,
                           D2  => D2_Pt,
                           D3  => D3_Pt,
                           D4  => D4_Pt,
                           D5  => D5_Pt,
                           D6  => D6_Pt,
                           D7  => D7_Pt,
                           D8  => D8_Pt,
                           D9  => D9_Pt,
                           D10 => D10_Pt,
                           D11 => D11_Pt,
                           D12 => D12_Pt,
                           D13 => D13_Pt,
                           D14 => D14_Pt,
                           D15 => D15_Pt,
                           A0  => A0_Pt,
                           A1  => A1_Pt,
                           A2  => A2_Pt,
                           A3  => A3_Pt,
                           A4  => A4_Pt,
                           A5  => A5_Pt);
   end record;

   -- IO Pins --

   overriding
   function As_GPIO (This : in out STM32_UNO_Headers;
                     Pin  : UNO_Pin)
                     return not null HAL.GPIO.Any_GPIO_Point;

   overriding
   procedure Pin_Mode (This : in out STM32_UNO_Headers;
                       Pin  : UNO_Pin;
                       Mode : UNO_Pin_Mode);

   overriding
   function Digital_Read (This : in out STM32_UNO_Headers;
                          Pin  : UNO_Pin)
                          return Boolean;

   overriding
   procedure Digital_Write (This : in out STM32_UNO_Headers;
                            Pin  : UNO_Pin;
                            Val  : Boolean);

   overriding
   function Analog_Read (This : in out STM32_UNO_Headers;
                         Pin  : UNO_Analog_Input_Pin)
                         return Analog_Value;

   overriding
   procedure Analog_Write (This : in out STM32_UNO_Headers;
                           Pin  : UNO_PWM_Pin;
                           Val  : Analog_Value);

   -- Serial/UART --

   overriding
   procedure Serial_Begin (This : in out STM32_UNO_Headers;
                           Baud : UInt32);
   overriding
   procedure Serial_End (This : in out STM32_UNO_Headers);

   overriding
   function Serial_Port (This : STM32_UNO_Headers)
                         return not null HAL.UART.Any_UART_Port;

   -- Wire/I2c --

   overriding
   procedure Wire_Begin (This : in out STM32_UNO_Headers);
   overriding
   procedure Wire_End (This : in out STM32_UNO_Headers);

   overriding
   function Wire_Port (This : STM32_UNO_Headers)
                       return not null HAL.I2C.Any_I2C_Port;
end STM32.Generic_UNO_Header;
