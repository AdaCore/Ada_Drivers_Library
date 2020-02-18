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

with NRF_SVD.SPI; use NRF_SVD.SPI;

package body nRF.SPI_Master is

   procedure Transfer (This     : in out SPI_Master;
                       Data_Out : UInt8;
                       Data_In  : out UInt8)
     with Inline_Always;
   --  Send and receive one byte of data

   --------------
   -- Transfer --
   --------------

   procedure Transfer (This     : in out SPI_Master;
                       Data_Out : UInt8;
                       Data_In  : out UInt8)
   is
   begin

      --  Set output byte
      This.Periph.TXD.TXD := Data_Out;

      --  Wait for the RX ready event
      while This.Periph.EVENTS_READY = 0 loop
         null;
      end loop;

      --  Get input data
      Data_In := This.Periph.RXD.RXD;

      --  Clear event
      This.Periph.EVENTS_READY := 0;
   end Transfer;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out SPI_Master) is
   begin
      This.Periph.ENABLE.ENABLE := Enabled;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out SPI_Master) is
   begin
      This.Periph.ENABLE.ENABLE := Disabled;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : SPI_Master) return Boolean
   is (This.Periph.ENABLE.ENABLE = Enabled);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This            : in out SPI_Master;
      SCK, MOSI, MISO : GPIO_Pin_Index;
      Speed           : Clock_Speed;
      Bit_Order       : Data_Bit_Order;
      Polarity        : Clock_Polarity;
      Phase           : Clock_Phase)
   is
   begin
      This.Periph.PSELSCK := UInt32 (SCK);
      This.Periph.PSELMOSI := UInt32 (MOSI);
      This.Periph.PSELMISO := UInt32 (MISO);

      This.Periph.FREQUENCY := (case Speed is
                                   when SPI_125kbps => 16#0200_0000#,
                                   when SPI_250kbps => 16#0400_0000#,
                                   when SPI_500kbps => 16#0800_0000#,
                                   when SPI_1Mbps   => 16#1000_0000#,
                                   when SPI_2Mbps   => 16#2000_0000#,
                                   when SPI_4Mbps   => 16#4000_0000#,
                                   when SPI_8Mbps   => 16#8000_0000#);

      This.Periph.CONFIG.ORDER := (case Bit_Order is
                                      when Most_Significant_First => Msbfirst,
                                      when Least_Significant_First => Lsbfirst);

      This.Periph.CONFIG.CPHA := (case Phase is
                                     when Sample_Leading_Edge => Leading,
                                     when Sample_Trailing_Edge => Trailing);

      This.Periph.CONFIG.CPOL := (case Polarity is
                                     when Active_High => Activehigh,
                                     when Active_Low => Activelow);
   end Configure;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (This : in out SPI_Master) is
   begin
      This.Periph.PSELSCK := UInt32'Last;
      This.Periph.PSELMOSI := UInt32'Last;
      This.Periph.PSELMISO := UInt32'Last;
   end Disconnect;

   ---------------
   -- Data_Size --
   ---------------

   overriding function Data_Size
     (This : SPI_Master)
      return SPI_Data_Size
   is (HAL.SPI.Data_Size_8b);

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out SPI_Master;
      Data    : SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
      Unused : UInt8;
   begin
      --  Clear any previous event
      This.Periph.EVENTS_READY := 0;

      for Elt of Data loop
         Transfer (This, Data_Out => Elt, Data_In => Unused);
      end loop;

      Status := HAL.SPI.Ok;
   end Transmit;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out SPI_Master;
      Data    : SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
      Unused : UInt8;
   begin
      --  Clear any previous event
      This.Periph.EVENTS_READY := 0;

      if This.Periph.CONFIG.ORDER = Msbfirst then
         for Elt of Data loop
            Transfer (This,
                      Data_Out => UInt8 (Shift_Left (Elt, 8)),
                      Data_In => Unused);
            Transfer (This,
                      Data_Out => UInt8 (Elt and 16#FF#),
                      Data_In => Unused);
         end loop;
      else
         for Elt of Data loop
            Transfer (This,
                      Data_Out => UInt8 (Elt and 16#FF#),
                      Data_In => Unused);
            Transfer (This,
                      Data_Out => UInt8 (Shift_Left (Elt, 8)),
                      Data_In => Unused);
         end loop;
      end if;

      Status := HAL.SPI.Ok;
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out SPI_Master;
      Data    : out SPI_Data_8b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
   begin
      --  Clear any previous event
      This.Periph.EVENTS_READY := 0;

      for Elt of Data loop
         Transfer (This, Data_Out => 0, Data_In => Elt);
      end loop;

      Status := HAL.SPI.Ok;
   end Receive;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out SPI_Master;
      Data    : out SPI_Data_16b;
      Status  : out SPI_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
      Byte : UInt8;
   begin
      --  Clear any previous event
      This.Periph.EVENTS_READY := 0;

      if This.Periph.CONFIG.ORDER = Msbfirst then
         for Elt of Data loop
            Transfer (This, Data_Out => 0, Data_In => Byte);
            Elt := Shift_Left (UInt16 (Byte), 8);
            Transfer (This, Data_Out => 0, Data_In => Byte);
            Elt := Elt or UInt16 (Byte);
         end loop;
      else
         for Elt of Data loop
            Transfer (This, Data_Out => 0, Data_In => Byte);
            Elt := Elt or UInt16 (Byte);
            Transfer (This, Data_Out => 0, Data_In => Byte);
            Elt := Shift_Left (UInt16 (Byte), 8);
         end loop;
      end if;

      Status := HAL.SPI.Ok;
   end Receive;

end nRF.SPI_Master;
