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

package body SAM.SERCOM.SPI is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This                : in out SPI_Device;
      Baud                :        UInt8;
      Data_Order          :        Data_Bit_Order;
      Phase               :        Clock_Phase;
      Polarity            :        Clock_Polarity;
      DIPO                :        Pad_Id;
      DOPO                :        Pad_Id;
      Slave_Select_Enable :        Boolean)
   is
   begin

      This.Reset;

      This.Periph.SERCOM_SPI.CTRLA :=
        (MODE => 3, -- SPI Master
         DOPO => UInt2 (DOPO),
         DIPO => UInt2 (DIPO),
         FORM => 0,
         CPHA => (case Phase is
                     when Sample_Leading_Edge => False,
                     when Sample_Trailing_Edge => True),
         CPOL => (case Polarity is
                     when Active_High => False,
                     when Active_Low  => True),
         DORD => (case Data_Order is
                     when Most_Significant_First  => False,
                     when Least_Significant_First => True),
         others => <>
        );

      This.Periph.SERCOM_SPI.BAUD := Baud;

      This.Periph.SERCOM_SPI.CTRLB :=
        (This.Periph.SERCOM_SPI.CTRLB
         with delta CHSIZE => 0,
                    MSSEN  => Slave_Select_Enable);

      --  Wait for CTRLB synchronization signal
      while This.Periph.SERCOM_SPI.SYNCBUSY.CTRLB loop
         null;
      end loop;

      This.Config_Done := True;
   end Configure;

   ---------------------
   -- Enable_Receiver --
   ---------------------

   procedure Enable_Receiver (This : in out SPI_Device) is
   begin
      This.Periph.SERCOM_SPI.CTRLB.RXEN := True;

      --  Wait for CTRLB synchronization signal
      while This.Periph.SERCOM_SPI.SYNCBUSY.CTRLB loop
         null;
      end loop;
   end Enable_Receiver;

   ----------------------
   -- Disable_Receiver --
   ----------------------

   procedure Disable_Receiver (This : in out SPI_Device) is
   begin
      This.Periph.SERCOM_SPI.CTRLB.RXEN := False;

      --  Wait for CTRLB synchronization signal
      while This.Periph.SERCOM_SPI.SYNCBUSY.CTRLB loop
         null;
      end loop;
   end Disable_Receiver;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out SPI_Device;
      Data    :        SPI_Data_8b;
      Status  :    out SPI_Status;
      Timeout :        Natural := 1_000)
   is
      pragma Unreferenced (Timeout);
   begin
      for Elt of Data loop
         This.Periph.SERCOM_SPI.DATA := UInt32 (Elt);

         while not This.Periph.SERCOM_SPI.INTFLAG.TXC loop
            null;
         end loop;
      end loop;

      Status := Ok;
   end Transmit;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out SPI_Device;
      Data    :        SPI_Data_16b;
      Status  :    out SPI_Status;
      Timeout :        Natural := 1_000)
   is
   begin
      raise Program_Error with "Unimplemented procedure Transmit 16b";
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out SPI_Device;
      Data    :    out SPI_Data_8b;
      Status  :    out SPI_Status;
      Timeout :        Natural := 1_000)
   is
      pragma Unreferenced (Timeout);
   begin
      for Elt of Data loop
         --  Write anything to trigger the transaction
         This.Periph.SERCOM_SPI.DATA := 0;

         while not This.Periph.SERCOM_SPI.INTFLAG.RXC loop
            null;
         end loop;

         Elt := UInt8 (This.Periph.SERCOM_SPI.DATA and 16#FF#);
      end loop;

      Status := Ok;
   end Receive;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out SPI_Device;
      Data    :    out SPI_Data_16b;
      Status  :    out SPI_Status;
      Timeout :        Natural := 1_000)
   is
   begin
      raise Program_Error with "Unimplemented procedure Receive 16bit";
   end Receive;

end SAM.SERCOM.SPI;
