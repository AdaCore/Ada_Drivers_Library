------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

--  Driver for the AdaFruit Charlie Wing matrix LED board.
--  See the IS31FL3731 package for API documentation.

with HAL;
with HAL.I2C;

with IS31FL3731;

package AdaFruit.CharlieWing is

   subtype X_Coord is IS31FL3731.X_Coord range 0 .. 15;
   subtype Y_Coord is IS31FL3731.Y_Coord range 0 .. 6;

   type Device
     (Port : not null HAL.I2C.Any_I2C_Port;
      AD   : HAL.UInt2)
   is new IS31FL3731.Device
   with private;

   overriding
   function LED_Address (This : Device;
                         X    : IS31FL3731.X_Coord;
                         Y    : IS31FL3731.Y_Coord)
                         return IS31FL3731.LED_Id
     with Pre => X in X_Coord and then Y in Y_Coord;
   --  LED address conversion specific to the LED arrangement of the Charlie
   --  Wing.

private

   type Device
     (Port : not null HAL.I2C.Any_I2C_Port;
      AD   : HAL.UInt2)
   is new IS31FL3731.Device (Port, AD)
   with null record;

end AdaFruit.CharlieWing;
