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

with System.Storage_Elements; use System.Storage_Elements;

package body SiFive.SPI is

   type Frame_Format_Register is record
      Proto  : UInt2;
      Endian : Bit;
      Dir    : Bit;
      Len    : UInt4;
   end record
     with Size => 32, Volatile_Full_Access;

   for Frame_Format_Register use record
      Proto  at 0 range  0 ..  1;
      Endian at 0 range  2 ..  2;
      Dir    at 0 range  3 ..  3;
      Len    at 0 range 16 .. 19;
   end record;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This       : in out SPI_Controller;
      Divisor    : Clock_Divisor;
      Polarity   : Clock_Polarity;
      Phase      : Clock_Phase;
      Endianness : SPI_Endianness)
   is
      SCKDIV  : UInt32
        with Volatile,
        Address => To_Address (This.Base_Address + 16#00#);
      SCKMODE  : UInt32
        with Volatile,
        Address => To_Address (This.Base_Address + 16#04#);

      FMT  : Frame_Format_Register
        with Volatile,
        Address => To_Address (This.Base_Address + 16#40#);

      FCTRL : UInt32
        with Volatile,
        Address => To_Address (This.Base_Address + 16#60#);

   begin
      --  Disable flash mode
      FCTRL := 0;

      SCKDIV := UInt32 (Divisor);
      SCKMODE := (case Phase is
                     when Leading_Edge  => 0,
                     when Trailing_Edge => 1)
                or
                 (case Polarity is
                     when Inactive_Is_Logical_0 => 0,
                     when Inactive_Is_Logical_1 => 2);

      FMT := (Proto  => 0, -- Only single mode is supported so far
              Endian => (case Endianness is
                            when MSB_First => 0,
                            when LSB_First => 1),
              Dir    => 0,
              Len    => 8 --  Only 8bit data supported
             );

   end Configure;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out SPI_Controller;
      Data    : SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1_000)
   is
      pragma Unreferenced (Timeout);

      TX_Data : UInt32
        with Volatile,
        Address => To_Address (This.Base_Address + 16#48#);
   begin
      for Elt of Data loop
         --  Wait while the FIFO is full
         while TX_Data /= 0 loop
            null;
         end loop;

         TX_Data := UInt32 (Elt);
      end loop;
      Status := Ok;
   end Transmit;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out SPI_Controller;
      Data    : SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1_000)
   is
   begin
      raise Program_Error with "Unimplemented procedure Transmit";
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out SPI_Controller;
      Data    : out SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1_000)
   is
      pragma Unreferenced (Timeout);

      RX_Data : UInt32
        with Volatile,
        Address => To_Address (This.Base_Address + 16#4C#);
   begin
      for Elt of Data loop
         --  Wait while the FIFO empty
         while (RX_Data and 2 ** 31) /= 0 loop
            null;
         end loop;

         Elt := UInt8 (RX_Data and 16#FF#);
      end loop;
      Status := Ok;
   end Receive;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out SPI_Controller;
      Data    : out SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1_000)
   is
   begin
      raise Program_Error with "Unimplemented procedure Receive";
   end Receive;

end SiFive.SPI;
