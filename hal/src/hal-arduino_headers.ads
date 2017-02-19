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

with HAL.GPIO; use HAL.GPIO;
with HAL.I2C;  use HAL.I2C;
with HAL.UART; use HAL.UART;

package HAL.Arduino_Headers is

   type UNO_Pin is (D0, D1, D2, D4, D7, D8, D12, D13, D14, D15, D3, D5, D6,
                    D9, D10, D11, A0, A1, A2, A3, A4, A5);

   subtype UNO_Analog_Input_Pin is UNO_Pin range A0 .. A5;
   subtype UNO_PWM_Pin is UNO_Pin range D3 .. D11;

   type UNO_Pin_Mode is (OUTPUT, INPUT, INPUT_PULLUP);

   type Analog_Value is new UInt10;

   type UNO_Headers is limited interface;
   type Any_UNO_Headers is access all UNO_Headers'Class;

   -- IO Pins --

   function As_GPIO (This : in out UNO_Headers;
                     Pin  : UNO_Pin)
                     return not null Any_GPIO_Point is abstract;

   procedure Pin_Mode (This : in out UNO_Headers;
                       Pin  : UNO_Pin;
                       Mode : UNO_Pin_Mode) is abstract;

   function Digital_Read (THis : in out UNO_Headers;
                          Pin  : UNO_Pin)
                          return Boolean is abstract;

   procedure Digital_Write (This : in out UNO_Headers;
                            Pin  : UNO_Pin;
                            Val  : Boolean) is abstract;

   function Analog_Read (This : in out UNO_Headers;
                         Pin  : UNO_Analog_Input_Pin)
                         return Analog_Value is abstract;

   procedure Analog_Write (This : in out UNO_Headers;
                           Pin  : UNO_PWM_Pin;
                           Val  : Analog_Value) is abstract;

   -- Serial/UART --

   procedure Serial_Begin (This : in out UNO_Headers;
                           Baud : UInt32) is abstract;
   procedure Serial_End (This : in out UNO_Headers) is abstract;

   function Serial_Port (This : UNO_Headers)
                         return not null Any_UART_Port is abstract;

   -- Wire/I2c --

   procedure Wire_Begin (This : in out UNO_Headers) is abstract;
   procedure Wire_End (This : in out UNO_Headers) is abstract;

   function Wire_Port (This : UNO_Headers)
                       return not null Any_I2C_Port is abstract;

end HAL.Arduino_Headers;
