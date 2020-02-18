------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2018-2020, AdaCore                      --
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

with HAL;         use HAL;
with HAL.SPI;     use HAL.SPI;
with nRF.GPIO;    use nRF.GPIO;
with NRF_SVD.SPI;

package nRF.SPI_Master is

   type Clock_Speed is (SPI_125kbps, SPI_250kbps, SPI_500kbps,
                        SPI_1Mbps, SPI_2Mbps, SPI_4Mbps, SPI_8Mbps);
   --  nRF SPI device has a limited number of speeds available

   type Data_Bit_Order is (Most_Significant_First,
                           Least_Significant_First);

   type Clock_Phase is (Sample_Leading_Edge, Sample_Trailing_Edge);
   --  SPI Clock phase, also known as CPHA:
   --  CPHA = 0 -> Sample_Leading_Edge
   --  CPHA = 1 -> Sample_Trailing_Edge

   type Clock_Polarity is (Active_High, Active_Low);
   --  SPI Clock polarity, also known as CPOL:
   --  CPOL = 0 -> Active_High
   --  CPOL = 1 -> Active_Low

   type SPI_Master (Periph : not null access NRF_SVD.SPI.SPI_Peripheral) is
     new HAL.SPI.SPI_Port with private;
   --  nRF SPI Master device

   procedure Enable (This : in out SPI_Master)
     with Post => This.Enabled;
   --  Enable the device. Configuration cannot be changed while the device is
   --  enabled.

   procedure Disable (This : in out SPI_Master)
     with Post => not This.Enabled;
   --  Disable the device

   function Enabled (This : SPI_Master) return Boolean;
   --  Return True if the device is enabled

   procedure Configure (This            : in out SPI_Master;
                        SCK, MOSI, MISO : GPIO_Pin_Index;
                        Speed           : Clock_Speed;
                        Bit_Order       : Data_Bit_Order;
                        Polarity        : Clock_Polarity;
                        Phase           : Clock_Phase)
     with Pre => not This.Enabled;
   --  Configure the device's IO pins and protocol. Configuration must be done
   --  while the device is disabled.
   --
   --  SPI Mode | Polarity    | Phase
   --  ----------------------------------------------
   --   0       | Active_High | Sample_Leading_Edge
   --   1       | Active_High | Sample_Trailing_Edge
   --   2       | Active_Low  | Sample_Leading_Edge
   --   3       | Active_Low  | Sample_Trailing_Edge

   procedure Disconnect (This : in out SPI_Master)
     with Pre => not This.Enabled;
   --  Disconect the peripheral from the GPIO points

   ----------------------------------------
   -- Implementation of HAL.SPI.SPI_Port --
   ----------------------------------------

   overriding
   function Data_Size (This : SPI_Master) return SPI_Data_Size;

   overriding
   procedure Transmit
     (This    : in out SPI_Master;
      Data    : SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Transmit
     (This    : in out SPI_Master;
      Data    : SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out SPI_Master;
      Data    : out SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out SPI_Master;
      Data    : out SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000);

private

   type SPI_Master (Periph : not null access NRF_SVD.SPI.SPI_Peripheral) is
     new HAL.SPI.SPI_Port with null record;

end nRF.SPI_Master;
