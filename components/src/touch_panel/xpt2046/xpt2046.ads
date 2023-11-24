------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

--  Generic driver for the XPT2046 touch panel

with HAL;
with HAL.GPIO;
with HAL.SPI;
with HAL.Touch_Panel; use HAL.Touch_Panel;

package XPT2046 is

   type XPT2046_Device
     (SPI : not null HAL.SPI.Any_SPI_Port;
      CS  : not null HAL.GPIO.Any_GPIO_Point) is
     limited new Touch_Panel_Device with private;

   use type HAL.UInt16;

   subtype Sensor_Value is HAL.UInt16 range 0 .. 2 ** 12 - 1;

   procedure Calibrate
     (This  : in out XPT2046_Device'Class;
      Min_X : Sensor_Value;
      Max_X : Sensor_Value;
      Min_Y : Sensor_Value;
      Max_Y : Sensor_Value);
   --  Usually, sensor values at the boundary points differ from the
   --  minimum/maximum. With the help of this procedure, we can calibrate
   --  the touch panel to return accurate values.

private

   type XPT2046_Device
     (SPI : not null HAL.SPI.Any_SPI_Port;
      CS  : not null HAL.GPIO.Any_GPIO_Point) is
     limited new HAL.Touch_Panel.Touch_Panel_Device with
   record
      LCD_Natural_Width  : Natural := 0;
      LCD_Natural_Height : Natural := 0;
      Swap               : Swap_State := 0;
      Min_X              : Sensor_Value := Sensor_Value'First;
      Max_X              : Sensor_Value := Sensor_Value'Last;
      Min_Y              : Sensor_Value := Sensor_Value'First;
      Max_Y              : Sensor_Value := Sensor_Value'Last;
   end record;

   overriding
   procedure Set_Bounds (This   : in out XPT2046_Device;
                         Width  : Natural;
                         Height : Natural;
                         Swap   : HAL.Touch_Panel.Swap_State);

   overriding
   function Active_Touch_Points (This : in out XPT2046_Device)
                                 return HAL.Touch_Panel.Touch_Identifier;

   overriding
   function Get_Touch_Point (This     : in out XPT2046_Device;
                             Touch_Id : HAL.Touch_Panel.Touch_Identifier)
                             return HAL.Touch_Panel.TP_Touch_State;

   overriding
   function Get_All_Touch_Points
     (This : in out XPT2046_Device) return HAL.Touch_Panel.TP_State;

   type Reference_Kind is (Differential, Single_Ended);
   --  XPT2046 can measure in two modes: differential and single-ended.
   --  Differential mode is better for X, Y, Z measurement, but temperature
   --  and battery voltage measurement could be done in single-ended mode only.

   Channel_Temp : constant := 0;
   Channel_Y    : constant := 1;
   Channel_Z1   : constant := 3;
   Channel_Z2   : constant := 4;
   Channel_X    : constant := 5;

   function Read_Sensor
     (This      : XPT2046_Device'Class;
      Channel   : Natural;
      Last      : Boolean := False;
      Reference : Reference_Kind := Differential) return Sensor_Value;
   --  Read ADC sensor value from given cannel using given reference mode.
   --  If Last is True then shutdown the power.

   function Read_Sensor_3
     (This      : XPT2046_Device'Class;
      Channel   : Natural;
      Last      : Boolean := False;
      Reference : Reference_Kind := Differential) return Sensor_Value;
   --  Read 3 values from the same channel and return a middle one.

end XPT2046;
