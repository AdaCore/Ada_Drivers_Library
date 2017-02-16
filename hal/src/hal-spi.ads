------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package HAL.SPI is

   type SPI_Status is
     (Ok,
      Err_Error,
      Err_Timeout,
      Busy);

   type SPI_Data_Size is
     (Data_Size_8b,
      Data_Size_16b);

   type SPI_Data_8b is array (Natural range <>) of UInt8;

   type SPI_Data_16b is array (Natural range <>) of UInt16;

   type SPI_Port is limited interface;

   type Any_SPI_Port is access all SPI_Port'Class;

   function Data_Size (This : SPI_Port) return SPI_Data_Size is abstract;

   procedure Transmit
     (This    : in out SPI_Port;
      Data    : SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (This) = Data_Size_8b;

   procedure Transmit
     (This    : in out SPI_Port;
      Data    : SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (This) = Data_Size_16b;

   procedure Receive
     (This    : in out SPI_Port;
      Data    : out SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (This) = Data_Size_8b;

   procedure Receive
     (This    : in out SPI_Port;
      Data    : out SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000) is abstract
     with
       Pre'Class => Data_Size (This) = Data_Size_16b;

end HAL.SPI;
