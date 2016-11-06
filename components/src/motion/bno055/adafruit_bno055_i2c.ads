------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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

--  This package provides a concrete subclass of the AdaFruit BNO055 IMU that
--  uses I2C for communication with the sensor. (USART-based communication is
--  an alternative.)

with HAL.I2C;  use HAL.I2C;
with Adafruit_BNO055;
with Interfaces;

package AdaFruit_BNO055_I2C is

   type BNO055_IMU (Port : I2C_Port_Ref; Device : I2C_Address) is
     new Adafruit_BNO055.BNO055_9DOF_IMU with private;

private

   use HAL;
   use Adafruit_BNO055; -- for Register_Address
   use Interfaces;

   type BNO055_IMU (Port : I2C_Port_Ref; Device : I2C_Address) is
     new Adafruit_BNO055.BNO055_9DOF_IMU with null record;

   overriding
   procedure Read
     (This     : in out BNO055_IMU;
      Register : Register_Address;
      Value    : out Unsigned_8);

   overriding
   procedure Write
     (This     : in out BNO055_IMU;
      Register : Register_Address;
      Value    : Unsigned_8);

   overriding
   procedure Read_Buffer
     (This     : in out BNO055_IMU;
      Register : Register_Address;
      Value    : out Byte_Buffer);

end AdaFruit_BNO055_I2C;
