------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2021, AdaCore                           --
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

--  See LPS25H Datasheet DocID023722 Rev 6

with HAL.I2C;
with HAL.Time;

package LPS25H.I2C is

   type LPS25H_Barometric_Sensor_I2C
     (Port              : not null HAL.I2C.Any_I2C_Port;
      Slave_Address_Odd : Boolean;
      Timing            : not null HAL.Time.Any_Delays)
     is new LPS25H_Barometric_Sensor with private;
   --  See Datasheet section 5.2.1 for slave address adjustment. The
   --  address is given as 2#101_1101# if Slave_Address_Odd is true
   --  (SDO is high), 2#101_1100# if false. Using the Ada Drivers
   --  Library convention, where the 7 bits of the address used on the
   --  wire are in bits 1 .. 7, this is 16#BA#, 16#B8# respectively.

   overriding
   procedure Initialize (This : in out LPS25H_Barometric_Sensor_I2C);

   overriding
   procedure Get_Data
     (This   : in out LPS25H_Barometric_Sensor_I2C;
      Press  :    out Pressure;
      Temp   :    out Temperature;
      Asl    :    out Altitude;
      Status :    out Boolean);

private

   type LPS25H_Barometric_Sensor_I2C
     (Port              : not null HAL.I2C.Any_I2C_Port;
      Slave_Address_Odd : Boolean;
      Timing            : not null HAL.Time.Any_Delays)
      is new LPS25H_Barometric_Sensor with record
         I2C_Address : HAL.I2C.I2C_Address;
      end record;

end LPS25H.I2C;
