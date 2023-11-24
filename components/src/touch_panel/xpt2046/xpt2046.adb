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

with Ada.Unchecked_Conversion;

package body XPT2046 is

   type Power_Down_Mode is
     (Power_Down,     --  Power Down Between Conversions
      Reference_Off,  --  Reference off, ADC on
      ADC_Off,        --  Reference on, ADC off
      Always_On);

   for Power_Down_Mode use
     (Power_Down    => 0,
      Reference_Off => 1,
      ADC_Off       => 2,
      Always_On     => 3);

   type Conversion_Mode is (Use_12_Bits, Use_8_Bits);
   pragma Unreferenced (Use_8_Bits);

   type Control_Byte is record
      Power_Down : Power_Down_Mode;
      Reference  : Reference_Kind;
      Mode       : Conversion_Mode;
      Channel    : Natural range 0 .. 7;
      Start      : Boolean;
   end record;

   for Control_Byte use record
      Power_Down at 0 range 0 .. 1;
      Reference  at 0 range 2 .. 2;
      Mode       at 0 range 3 .. 3;
      Channel    at 0 range 4 .. 6;
      Start      at 0 range 7 .. 7;
   end record;

   function Cast is new Ada.Unchecked_Conversion (Control_Byte, HAL.UInt8);

   procedure Read_Sensors
     (This : XPT2046_Device'Class;
      X, Y, Z1, Z2 : out Sensor_Value);

   Z_Treshold : constant Sensor_Value := 3200;

   -------------------------
   -- Active_Touch_Points --
   -------------------------

   overriding
   function Active_Touch_Points
     (This : in out XPT2046_Device) return HAL.Touch_Panel.Touch_Identifier
   is
      X, Y, Z1, Z2 : Sensor_Value;
   begin
      This.Read_Sensors (X, Y, Z1, Z2);
      return (if Z2 - Z1 < Z_Treshold then 1 else 0);
   end Active_Touch_Points;

   ---------------
   -- Calibrate --
   ---------------

   procedure Calibrate
     (This  : in out XPT2046_Device'Class;
      Min_X : Sensor_Value;
      Max_X : Sensor_Value;
      Min_Y : Sensor_Value;
      Max_Y : Sensor_Value) is
   begin
      This.Min_X := Min_X;
      This.Max_X := Max_X;
      This.Min_Y := Min_Y;
      This.Max_Y := Max_Y;
   end Calibrate;

   --------------------------
   -- Get_All_Touch_Points --
   --------------------------

   overriding
   function Get_All_Touch_Points
     (This : in out XPT2046_Device) return HAL.Touch_Panel.TP_State
   is
      Result : constant TP_Touch_State := This.Get_Touch_Point (1);
   begin
      if Result = Null_Touch_State then
         return (1 .. 0 => <>);
      else
         return (1 => Result);
      end if;
   end Get_All_Touch_Points;

   ---------------------
   -- Get_Touch_Point --
   ---------------------

   overriding
   function Get_Touch_Point
     (This : in out XPT2046_Device;
      Touch_Id : HAL.Touch_Panel.Touch_Identifier)
      return HAL.Touch_Panel.TP_Touch_State
   is
      pragma Unreferenced (Touch_Id);

      Result : HAL.Touch_Panel.TP_Touch_State;
      X, Y, Z1, Z2 : Sensor_Value;
   begin
      This.Read_Sensors (X, Y, Z1, Z2);

      if Z2 - Z1 >= Z_Treshold or Z1 = 0 or X < This.Min_X then
         return Null_Touch_State;
      end if;

      Result.X := Natural (X - Sensor_Value'Min (This.Min_X, X))
        * This.LCD_Natural_Width / Natural (This.Max_X - This.Min_X);

      Result.Y := Natural (Y - Sensor_Value'Min (This.Min_Y, Y))
        * This.LCD_Natural_Height / Natural (This.Max_Y - This.Min_Y);

      Result.X := Natural'Min (Result.X, This.LCD_Natural_Width - 1);
      Result.Y := Natural'Min (Result.Y, This.LCD_Natural_Height - 1);

      if (This.Swap and Invert_X) /= 0 then
         Result.X := This.LCD_Natural_Width - 1 - Result.X;
      end if;

      if (This.Swap and Invert_Y) /= 0 then
         Result.Y := This.LCD_Natural_Height - 1 - Result.Y;
      end if;

      if (This.Swap and Swap_XY) /= 0 then
         declare
            Temp : constant Natural := Result.X;
         begin
            Result.X := Result.Y;
            Result.Y := Temp;
         end;
      end if;

      Result.Weight := Natural (X) * Natural (Z2 - Z1) / Natural (Z1);
      Result.Weight := Result.Weight / 500;
      Result.Weight := Natural'Max (4, Result.Weight);
      Result.Weight := Natural'Min (19, Result.Weight);
      Result.Weight := 20 - Result.Weight;

      return Result;
   end Get_Touch_Point;

   -----------------
   -- Read_Sensor --
   -----------------

   function Read_Sensor
     (This      : XPT2046_Device'Class;
      Channel   : Natural;
      Last      : Boolean := False;
      Reference : Reference_Kind := Differential) return Sensor_Value
   is
      use all type HAL.SPI.SPI_Status;

      Status   : HAL.SPI.SPI_Status;
      Response : HAL.SPI.SPI_Data_8b (1 .. 2);

      Mode : constant Power_Down_Mode :=
        (if Last then Power_Down else Reference_Off);
         --  elsif Reference = Differential then Reference_Off
         --  else Always_On);

      Control : constant Control_Byte :=
        (Power_Down => Mode,
         Reference  => Reference,
         Mode       => Use_12_Bits,
         Channel    => Channel,
         Start      => True);
   begin
      This.SPI.Transmit (HAL.SPI.SPI_Data_8b'(1 => Cast (Control)), Status);
      pragma Assert (Status = Ok);
      This.SPI.Receive (Response, Status);
      pragma Assert (Status = Ok);

      return HAL.Shift_Left (HAL.UInt16 (Response (1)) and 127, 5) +
        HAL.Shift_Right (HAL.UInt16 (Response (2)), 3);
   end Read_Sensor;

   -------------------
   -- Read_Sensor_3 --
   -------------------

   function Read_Sensor_3
     (This      : XPT2046_Device'Class;
      Channel   : Natural;
      Last      : Boolean := False;
      Reference : Reference_Kind := Differential) return Sensor_Value
   is
      List : array (1 .. 3) of Sensor_Value;
      Max  : Sensor_Value := Sensor_Value'First;
      Min  : Sensor_Value := Sensor_Value'Last;
   begin
      for J in 1 .. 3 loop
         List (J) := This.Read_Sensor
           (Channel,
            Last => J = 3 and Last,
            Reference => Reference);

         Min := Sensor_Value'Min (Min, List (J));
         Max := Sensor_Value'Max (Max, List (J));
      end loop;

      for X of List loop
         if X < Max and X > Min then
            return X;
         end if;
      end loop;

      if List (1) in List (2) | List (3) then
         return List (1);
      else
         return List (2);
      end if;
   end Read_Sensor_3;

   ------------------
   -- Read_Sensors --
   ------------------

   procedure Read_Sensors
     (This : XPT2046_Device'Class;
      X, Y, Z1, Z2 : out Sensor_Value) is
   begin
      This.CS.Clear;
      X  := This.Read_Sensor (Channel_X);
      Y  := This.Read_Sensor_3 (Channel_Y);  --  Y is so noisy
      Z1 := This.Read_Sensor (Channel_Z1);
      Z2 := This.Read_Sensor (Channel_Z2, Last => True);
      This.CS.Set;
   end Read_Sensors;

   ----------------
   -- Set_Bounds --
   ----------------

   overriding
   procedure Set_Bounds
     (This   : in out XPT2046_Device;
      Width  : Natural;
      Height : Natural;
      Swap   : HAL.Touch_Panel.Swap_State) is
   begin
      This.LCD_Natural_Width := Width;
      This.LCD_Natural_Height := Height;
      This.Swap := Swap;
   end Set_Bounds;

end XPT2046;
