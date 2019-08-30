------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with System.Storage_Elements;

with HAL;     use HAL;
with HAL.SPI; use HAL.SPI;

package SiFive.SPI is

   type SPI_Controller
     (Base_Address : System.Storage_Elements.Integer_Address)
   is new HAL.SPI.SPI_Port
   with private;

   type Clock_Divisor is new UInt12;

   type Clock_Phase is
     (Leading_Edge,
      --  Data is sampled on the leading edge of SCK and shifted on the
      --  trailing edge of SCK.

      Trailing_Edge
      --  Data is shifted on the leading edge of SCK and sampled on the
      --  trailing edge of SCK.
     );

   type Clock_Polarity is
     (Inactive_Is_Logical_0,
      Inactive_Is_Logical_1);

   type SPI_Endianness is (MSB_First, LSB_First);

   procedure Configure (This       : in out SPI_Controller;
                        Divisor    : Clock_Divisor;
                        Polarity   : Clock_Polarity;
                        Phase      : Clock_Phase;
                        Endianness : SPI_Endianness);

   overriding
   function Data_Size (This : SPI_Controller)
                       return SPI_Data_Size
   is (Data_Size_8b);
   --  Only 8bit per frame supported

   overriding
   procedure Transmit
     (This    : in out SPI_Controller;
      Data    : SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Transmit
     (This    : in out SPI_Controller;
      Data    : SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out SPI_Controller;
      Data    : out SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out SPI_Controller;
      Data    : out SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000);

private

   type SPI_Controller
     (Base_Address : System.Storage_Elements.Integer_Address)
   is new HAL.SPI.SPI_Port
   with null record;

end SiFive.SPI;
