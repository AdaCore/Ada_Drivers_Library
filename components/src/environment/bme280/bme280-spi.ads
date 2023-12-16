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

--  This package offers a straightforward method for setting up the BME280
--  when connected via SPI, especially useful when the use of only one sensor
--  is required. If you need multiple sensors, it is preferable to use the
--  BME280.SPI_Sensors package, which provides the appropriate tagged type.

with HAL.GPIO;
with HAL.SPI;

with BME280.Generic_Sensor;

generic
   SPI_Port : not null HAL.SPI.Any_SPI_Port;
   SPI_CS   : not null HAL.GPIO.Any_GPIO_Point;
package BME280.SPI is

   procedure Read
     (Data    : out HAL.UInt8_Array;
      Success : out Boolean);
   --  Read registers starting from Data'First

   procedure Write
     (Address : Register_Address;
      Data    : HAL.UInt8;
      Success : out Boolean);
   --  Write the value to the BME280 chip register with given Address.

   package Sensor is new Generic_Sensor (Read, Write);

end BME280.SPI;
