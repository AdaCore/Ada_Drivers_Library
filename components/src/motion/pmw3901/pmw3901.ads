------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
--           Copyright (C) 2020, Simon Wright (simon@pushface.org)          --
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

--  The algorithms used here are derived from Pimoroni's code at
--  https://github.com/pimoroni/pmw3901-python.

with HAL.GPIO;
with HAL.SPI;
with HAL.Time;
with Interfaces;

package PMW3901 is

   SPI_Error : exception;

   type PMW3901_Flow_Sensor
     (Port   : not null HAL.SPI.Any_SPI_Port;
      CS     : not null HAL.GPIO.Any_GPIO_Point;
      Timing : not null HAL.Time.Any_Delays) is limited private;

   type Run_Mode is range 0 .. 3 with Size => 2;
   type Motion is record
      Frame_From_0    : Boolean;
      Mode            : Run_Mode;
      Raw_From_0      : Boolean;
      Motion_Occurred : Boolean;
      Observation     : HAL.UInt8;
      Delta_X         : Interfaces.Integer_16;
      Delta_Y         : Interfaces.Integer_16;
      S_Qual          : HAL.UInt8;
      Raw_Data_Sum    : HAL.UInt8;
      Max_Raw_Data    : HAL.UInt8;
      Min_Raw_Data    : HAL.UInt8;
      Shutter_Upper   : HAL.UInt8; --  Shutter is big-endian!
      Shutter_Lower   : HAL.UInt8;
   end record;

   function Is_Valid (M : Motion) return Boolean;

   function Is_Initialized (This : PMW3901_Flow_Sensor) return Boolean;

   procedure Initialize (This : in out PMW3901_Flow_Sensor)
   with
     Pre => not Is_Initialized (This);
   --  NB: does NOT init/configure SPI and GPIO IO, which must be done
   --  (elsewhere) prior to calling this routine.

   procedure Calibrate (This : in out PMW3901_Flow_Sensor)
   with Pre => Is_Initialized (This);

   function Read_Motion  (This : in out PMW3901_Flow_Sensor) return Motion
   with Pre => Is_Initialized (This);

private

   type PMW3901_Flow_Sensor
     (Port   : not null HAL.SPI.Any_SPI_Port;
      CS     : not null HAL.GPIO.Any_GPIO_Point;
      Timing : not null HAL.Time.Any_Delays) is limited record
         Initialized : Boolean := False;
      end record;

   function Is_Initialized (This : PMW3901_Flow_Sensor) return Boolean
     is (This.Initialized);

   for Motion use record
      Frame_From_0    at 0 range 0 .. 0;
      Mode            at 0 range 1 .. 2;
      Raw_From_0      at 0 range 4 .. 4;
      Motion_Occurred at 0 range 7 .. 7;
      Observation     at 1 range 0 .. 7;
      Delta_X         at 2 range 0 .. 15;
      Delta_Y         at 4 range 0 .. 15;
      S_Qual          at 6 range 0 .. 7;
      Raw_Data_Sum    at 7 range 0 .. 7;
      Max_Raw_Data    at 8 range 0 .. 7;
      Min_Raw_Data    at 9 range 0 .. 7;
      Shutter_Upper   at 10 range 0 .. 7;
      Shutter_Lower   at 11 range 0 .. 7;
   end record;
   for Motion'Size use 12 * 8;

end PMW3901;
