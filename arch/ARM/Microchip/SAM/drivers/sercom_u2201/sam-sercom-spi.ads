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

with HAL.SPI; use HAL.SPI;
with HAL;     use HAL;

package SAM.SERCOM.SPI is

   type SPI_Device
   is new SERCOM_Device and HAL.SPI.SPI_Port
   with private;

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

   procedure Configure (This                : in out SPI_Device;
                        Baud                :        UInt8;
                        Data_Order          :        Data_Bit_Order;
                        Phase               :        Clock_Phase;
                        Polarity            :        Clock_Polarity;
                        DIPO                :        Pad_Id;
                        DOPO                :        Pad_Id;
                        Slave_Select_Enable :        Boolean)
     with Pre  => not This.Enabled and then not This.Configured,
          Post => not This.Enabled and then This.Configured;
   --  Configure SERCOM in SPI Master mode.
   --
   --  SPI Mode | Polarity    | Phase
   --  ----------------------------------------------
   --   0       | Active_High | Sample_Leading_Edge
   --   1       | Active_High | Sample_Trailing_Edge
   --   2       | Active_Low  | Sample_Leading_Edge
   --   3       | Active_Low  | Sample_Trailing_Edge

   procedure Enable_Receiver (This : in out SPI_Device);
   procedure Disable_Receiver (This : in out SPI_Device);

   -------------
   -- HAL.SPI --
   -------------

   overriding
   function Data_Size (This : SPI_Device) return SPI_Data_Size
   is (HAL.SPI.Data_Size_8b);

   overriding
   procedure Transmit
     (This    : in out SPI_Device;
      Data    :        SPI_Data_8b;
      Status  :    out SPI_Status;
      Timeout :        Natural := 1000);

   overriding
   procedure Transmit
     (This    : in out SPI_Device;
      Data    :        SPI_Data_16b;
      Status  :    out SPI_Status;
      Timeout :        Natural := 1000)
     with Pre => False;
   --  16bit transmit is not supported

   overriding
   procedure Receive
     (This    : in out SPI_Device;
      Data    :    out SPI_Data_8b;
      Status  :    out SPI_Status;
      Timeout :        Natural := 1000);

   overriding
   procedure Receive
     (This    : in out SPI_Device;
      Data    :    out SPI_Data_16b;
      Status  :    out SPI_Status;
      Timeout :        Natural := 1000)
     with Pre => False;
   --  16bit receive is not supported

private

   type SPI_Device
   is new SERCOM_Device and HAL.SPI.SPI_Port
   with null record;

end SAM.SERCOM.SPI;
