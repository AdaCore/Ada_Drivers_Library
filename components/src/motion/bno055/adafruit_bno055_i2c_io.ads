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

--  This file contains the I2C communication routines used with the BNO055
--  IMU breakout board from AdaFruit. Note that the BNO055 also supports
--  serial communications as an alternative.

with Interfaces;    use Interfaces;

with STM32.I2C;   use STM32.I2C;
with STM32.GPIO;  use STM32.GPIO;
with HAL.I2C;    --  use HAL.I2C;
use STM32;

package AdaFruit_BNO055_I2C_IO is

   BNO055_Primary_Address   : constant HAL.I2C.I2C_Address := 16#28#;
   BNO055_Alternate_Address : constant HAL.I2C.I2C_Address := 16#29#;

   procedure Initialize
     (BNO055_Device : HAL.I2C.I2C_Address := BNO055_Primary_Address;
      Port          : access I2C_Port;
      SCL           : GPIO_Point;
      SCL_AF        : GPIO_Alternate_Function;
      SDA           : GPIO_Point;
      SDA_AF        : GPIO_Alternate_Function;
      Reset         : GPIO_Point);

   type Register_Address is new HAL.UInt16;

   procedure Read (Register : Register_Address;  Value : out Unsigned_8);

   procedure Write (Register : Register_Address;  Value : Unsigned_8);

   procedure Read_Buffer
     (Register : Register_Address;
      Value    : out HAL.I2C.I2C_Data);

   procedure Reset_BNO055_Via_Hardware;
   --  Requires the "Reset" GPIO pin specified to Initialize to be physically
   --  connected to the Reset pin on the AdaFruit BNO055 breakout board.

private

   Port   : access I2C_Port;
   Reset  : GPIO_Point;
   Device : HAL.I2C.I2C_Address;  -- the I2C address for the BNO055 itself

   Selected_Clock_Speed : constant := 10_000;

end AdaFruit_BNO055_I2C_IO;
