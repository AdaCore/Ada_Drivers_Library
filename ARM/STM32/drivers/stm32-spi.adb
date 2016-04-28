------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_spi.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   SPI HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System;

with STM32_SVD.SPI; use STM32_SVD.SPI;

package body STM32.SPI is

   use type HAL.SPI.SPI_Data_Size;

   Baud_Rate_Value : constant array (SPI_Baud_Rate_Prescaler) of UInt3 :=
     (BRP_2   => 2#000#,
      BRP_4   => 2#001#,
      BRP_8   => 2#010#,
      BRP_16  => 2#011#,
      BRP_32  => 2#100#,
      BRP_64  => 2#101#,
      BRP_128 => 2#110#,
      BRP_256 => 2#111#);

   type Half_Word_Pointer is access all Short
     with Storage_Size => 0;

   function As_Half_Word_Pointer is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Half_Word_Pointer);
   --  So that we can treat the address of a byte as a pointer to a two-byte
   --  sequence representing a Half_Word quantity

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Port : in out SPI_Port; Conf : SPI_Configuration) is
   begin
      case Conf.Mode is
         when Master =>
            Port.Periph.CR1.MSTR := True;
            Port.Periph.CR1.SSI  := True;
         when Slave =>
            Port.Periph.CR1.MSTR := False;
            Port.Periph.CR1.SSI  := False;
      end case;

      case Conf.Direction is
         when D2Lines_FullDuplex =>
            Port.Periph.CR1.BIDIMODE := False;
            Port.Periph.CR1.BIDIOE   := False;
            Port.Periph.CR1.RXONLY   := False;
         when D2Lines_RxOnly =>
            Port.Periph.CR1.BIDIMODE := False;
            Port.Periph.CR1.BIDIOE   := False;
            Port.Periph.CR1.RXONLY   := True;
         when D1Line_Rx =>
            Port.Periph.CR1.BIDIMODE := True;
            Port.Periph.CR1.BIDIOE   := False;
            Port.Periph.CR1.RXONLY   := False;
         when D1Line_Tx =>
            Port.Periph.CR1.BIDIMODE := True;
            Port.Periph.CR1.BIDIOE   := True;
            Port.Periph.CR1.RXONLY   := False;
      end case;

      Port.Periph.CR1.DFF      := Conf.Data_Size = HAL.SPI.Data_Size_16b;
      Port.Periph.CR1.CPOL     := Conf.Clock_Polarity = High;
      Port.Periph.CR1.CPHA     := Conf.Clock_Phase = P2Edge;
      Port.Periph.CR1.SSM      := Conf.Slave_Management = Software_Managed;
      Port.Periph.CR1.BR       := Baud_Rate_Value (Conf.Baud_Rate_Prescaler);
      Port.Periph.CR1.LSBFIRST := Conf.First_Bit = LSB;

      --  Activate the SPI mode (Reset I2SMOD bit in I2SCFGR register)
      Port.Periph.I2SCFGR.I2SMOD := False;

      Port.Periph.CRCPR.CRCPOLY := Conf.CRC_Poly;
   end Configure;

   ------------
   -- Enable --
   ------------

   procedure Enable (Port : in out SPI_Port) is
   begin
      Port.Periph.CR1.SPE := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Port : in out SPI_Port) is
   begin
      Port.Periph.CR1.SPE := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (Port : SPI_Port) return Boolean is
   begin
      return Port.Periph.CR1.SPE;
   end Enabled;

   ----------
   -- Send --
   ----------

   procedure Send (Port : in out SPI_Port; Data : Short) is
   begin
      Port.Periph.DR.DR := Data;
   end Send;

   ----------
   -- Data --
   ----------

   function Data (Port : SPI_Port) return Short is
   begin
      return Port.Periph.DR.DR;
   end Data;

   ----------
   -- Send --
   ----------

   procedure Send (Port : in out SPI_Port; Data : Byte) is
   begin
      Send (Port, Short (Data));
   end Send;

   ----------
   -- Data --
   ----------

   function Data (Port : SPI_Port) return Byte is
   begin
      return Byte (Short'(Data (Port)));
   end Data;

   -------------
   -- Is_Busy --
   -------------

   function Is_Busy (Port : SPI_Port) return Boolean is
   begin
      return (Rx_Is_Empty (Port)
              and then not Tx_Is_Empty (Port))
        or else Busy (Port);
   end Is_Busy;

   -----------------
   -- Tx_Is_Empty --
   -----------------

   function Tx_Is_Empty (Port : SPI_Port) return Boolean is
   begin
      return Port.Periph.SR.TXE;
   end Tx_Is_Empty;

   -----------------
   -- Rx_Is_Empty --
   -----------------

   function Rx_Is_Empty (Port : SPI_Port) return Boolean is
   begin
      return not Port.Periph.SR.RXNE;
   end Rx_Is_Empty;

   ----------
   -- Busy --
   ----------

   function Busy (Port : SPI_Port) return Boolean is
   begin
      return Port.Periph.SR.BSY;
   end Busy;

   ------------------
   -- Current_Mode --
   ------------------

   function Current_Mode (Port : SPI_Port) return SPI_Mode is
   begin
      if Port.Periph.CR1.MSTR and Port.Periph.CR1.SSI then
         return Master;
      else
         return Slave;
      end if;
   end Current_Mode;

   ----------------------------
   -- Current_Data_Direction --
   ----------------------------

   function Current_Data_Direction (Port : SPI_Port) return SPI_Data_Direction
   is
   begin
      if not Port.Periph.CR1.BIDIMODE then
         if not Port.Periph.CR1.RXONLY then
            return D2Lines_FullDuplex;
         else
            return D2Lines_RxOnly;
         end if;
      else
         if not Port.Periph.CR1.BIDIOE then
            return D1Line_Rx;
         else
            return D1Line_Tx;
         end if;
      end if;
   end Current_Data_Direction;

   -----------------
   -- CRC_Enabled --
   -----------------

   function CRC_Enabled (Port : SPI_Port) return Boolean is
      (Port.Periph.CR1.CRCEN);

   ----------------------------
   -- Channel_Side_Indicated --
   ----------------------------

   function Channel_Side_Indicated (Port : SPI_Port) return Boolean is
     (Port.Periph.SR.CHSIDE);

   ------------------------
   -- Underrun_Indicated --
   ------------------------

   function Underrun_Indicated (Port : SPI_Port) return Boolean is
     (Port.Periph.SR.UDR);

   -------------------------
   -- CRC_Error_Indicated --
   -------------------------

   function CRC_Error_Indicated (Port : SPI_Port) return Boolean is
      (Port.Periph.SR.CRCERR);

   --------------------------
   -- Mode_Fault_Indicated --
   --------------------------

   function Mode_Fault_Indicated (Port : SPI_Port) return Boolean is
     (Port.Periph.SR.MODF);

   -----------------------
   -- Overrun_Indicated --
   -----------------------

   function Overrun_Indicated (Port : SPI_Port) return Boolean is
      (Port.Periph.SR.OVR);

   -------------------------------
   -- Frame_Fmt_Error_Indicated --
   -------------------------------

   function Frame_Fmt_Error_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Periph.SR.TIFRFE;
   end Frame_Fmt_Error_Indicated;

   -------------------
   -- Clear_Overrun --
   -------------------

   procedure Clear_Overrun (Port : SPI_Port) is
      Dummy1 : Short;
      Dummy2 : SR_Register;
   begin
      Dummy1 := Port.Periph.DR.DR;
      Dummy2 := Port.Periph.SR;
   end Clear_Overrun;

   ---------------
   -- Reset_CRC --
   ---------------

   procedure Reset_CRC (Port : in out SPI_Port) is
   begin
      Port.Periph.CR1.CRCEN := False;
      Port.Periph.CR1.CRCEN := True;
   end Reset_CRC;

   -------------------------
   -- Is_Data_Frame_16bit --
   -------------------------

   function Is_Data_Frame_16bit (Port : SPI_Port) return Boolean is
      (Port.Periph.CR1.DFF);

   ---------------
   -- Data_Size --
   ---------------

   overriding
   function Data_Size (Port : SPI_Port) return HAL.SPI.SPI_Data_Size is
   begin
      if Port.Periph.CR1.DFF then
         return HAL.SPI.Data_Size_16b;
      else
         return HAL.SPI.Data_Size_8b;
      end if;
   end Data_Size;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (Port    : in out SPI_Port;
      Data    : HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
   begin
      if CRC_Enabled (Port) then
         Reset_CRC (Port);
      end if;

      --  ??? right value to compare???
      if Current_Data_Direction (Port) = D1Line_Tx  then
         Port.Periph.CR1.BIDIOE := True;
      end if;

      Clear_Overrun (Port);

      if not Enabled (Port) then
         Enable (Port);
      end if;

      Send_8bit_Mode (Port, Data);

      --  Wait until TXE flag is set to send data
      while not Tx_Is_Empty (Port) loop
         null;
      end loop;

      --  Wait until Busy flag is reset before disabling SPI
      while Busy (Port) loop
         null;
      end loop;

      --  Clear OVERUN flag in 2-Line communication mode because received byte
      --  is not read.
      if Current_Data_Direction (Port) in D2Lines_RxOnly | D2Lines_FullDuplex
      then  -- right comparison ???
         Clear_Overrun (Port);
      end if;
      Status := HAL.SPI.Ok;
   end Transmit;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (Port    : in out SPI_Port;
      Data    : HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
   begin
      if CRC_Enabled (Port) then
         Reset_CRC (Port);
      end if;

      --  ??? right value to compare???
      if Current_Data_Direction (Port) = D1Line_Tx then
         Port.Periph.CR1.BIDIOE := True;
      end if;

      Clear_Overrun (Port);

      if not Enabled (Port) then
         Enable (Port);
      end if;

      Send_16bit_Mode (Port, Data);

      --  Wait until TXE flag is set to send data
      while not Tx_Is_Empty (Port) loop
         null;
      end loop;

      --  Wait until Busy flag is reset before disabling SPI
      while Busy (Port) loop
         null;
      end loop;

      --  Clear OVERUN flag in 2-Line communication mode because received byte
      --  is not read.
      if Current_Data_Direction (Port) in D2Lines_RxOnly | D2Lines_FullDuplex
      then  -- right comparison ???
         Clear_Overrun (Port);
         Status := HAL.SPI.Err_Error;
      end if;
      Status := HAL.SPI.Ok;
   end Transmit;

   --------------
   -- Transmit --
   --------------

   procedure Transmit
     (Port     : in out SPI_Port;
      Outgoing : Byte)
   is
   begin
      if CRC_Enabled (Port) then
         Reset_CRC (Port);
      end if;

      --  ??? right value to compare???
      if Current_Data_Direction (Port) = D1Line_Tx  then
         Port.Periph.CR1.BIDIOE := True;
      end if;

      if not Enabled (Port) then
         Enable (Port);
      end if;

      Port.Periph.DR.DR := Short (Outgoing);

      while not Tx_Is_Empty (Port) loop
         null;
      end loop;

      while Busy (Port) loop
         null;
      end loop;

      --  Clear OVERUN flag in 2-Line communication mode because received byte
      --  is not read.
      if Current_Data_Direction (Port) in D2Lines_RxOnly | D2Lines_FullDuplex
      then  -- right comparison ???
         Clear_Overrun (Port);
      end if;
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (Port    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
   begin
      if CRC_Enabled (Port) then
         Reset_CRC (Port);
      end if;

      if not Enabled (Port) then
         Enable (Port);
      end if;

      Receive_8bit_Mode (Port, Data);

      while Busy (Port) loop
         null;
      end loop;

      if CRC_Enabled (Port) and CRC_Error_Indicated (Port) then
         Reset_CRC (Port);
         Status := HAL.SPI.Err_Error;
      end if;
      Status := HAL.SPI.Ok;
   end Receive;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (Port    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
   begin
      if CRC_Enabled (Port) then
         Reset_CRC (Port);
      end if;

      if not Enabled (Port) then
         Enable (Port);
      end if;

      Receive_16bit_Mode (Port, Data);

      while Busy (Port) loop
         null;
      end loop;

      if CRC_Enabled (Port) and CRC_Error_Indicated (Port) then
         Reset_CRC (Port);
         Status := HAL.SPI.Err_Error;
      end if;
      Status := HAL.SPI.Ok;
   end Receive;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Port     : in out SPI_Port;
      Incoming : out Byte)
   is
   begin
      if CRC_Enabled (Port) then
         Reset_CRC (Port);
      end if;

      if not Enabled (Port) then
         Enable (Port);
      end if;

      Port.Periph.DR.DR := 0; -- generate clock

      while Rx_Is_Empty (Port) loop
         null;
      end loop;

      Incoming := Byte (Port.Periph.DR.DR);

      if CRC_Enabled (Port) then
         while Rx_Is_Empty (Port) loop
            null;
         end loop;
         Read_CRC : declare
            Dummy : Short;
         begin
            Dummy := Port.Periph.DR.DR;
         end Read_CRC;
      end if;

      while Busy (Port) loop
         null;
      end loop;

      if CRC_Enabled (Port) and CRC_Error_Indicated (Port) then
         Reset_CRC (Port);
      end if;
   end Receive;

   ----------------------
   -- Transmit_Receive --
   ----------------------

   procedure Transmit_Receive
     (Port     : in out SPI_Port;
      Outgoing : Byte_Buffer;
      Incoming : out Byte_Buffer;
      Size     : Positive)
   is
   begin
      if CRC_Enabled (Port) then
         Reset_CRC (Port);
      end if;

      if not Enabled (Port) then
         Enable (Port);
      end if;

      if Is_Data_Frame_16bit (Port) then
         Send_Receive_16bit_Mode (Port, Outgoing, Incoming, Size);
      else
         Send_Receive_8bit_Mode (Port, Outgoing, Incoming, Size);
      end if;

      --  Read CRC to close CRC calculation process
      if CRC_Enabled (Port) then
         --  wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;
         Read_CRC : declare
            Dummy : Short;
         begin
            Dummy := Port.Periph.DR.DR;
         end Read_CRC;
      end if;

      while Busy (Port) loop
         null;
      end loop;

      if CRC_Enabled (Port) and CRC_Error_Indicated (Port) then
         Reset_CRC (Port);
      end if;
   end Transmit_Receive;

   ----------------------
   -- Transmit_Receive --
   ----------------------

   procedure Transmit_Receive
     (Port     : in out SPI_Port;
      Outgoing : Byte;
      Incoming : out Byte)
   is
   begin
      if CRC_Enabled (Port) then
         Reset_CRC (Port);
      end if;

      if not Enabled (Port) then
         Enable (Port);
      end if;

      if Is_Data_Frame_16bit (Port) then
         raise Program_Error;
      end if;

      Port.Periph.DR.DR := Short (Outgoing);

      --  enable CRC transmission
      if CRC_Enabled (Port) then
         Port.Periph.CR1.CRCNEXT := True;
      end if;

      --  wait until data is received
      while Rx_Is_Empty (Port) loop
         null;
      end loop;

      Incoming := Byte (Port.Periph.DR.DR);

      --  Read CRC byte to close CRC calculation
      if CRC_Enabled (Port) then
         --  wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;
         Read_CRC : declare
            Dummy : Short;
         begin
            Dummy := Port.Periph.DR.DR;
         end Read_CRC;
      end if;

      while Busy (Port) loop
         null;
      end loop;

      if CRC_Enabled (Port) and CRC_Error_Indicated (Port) then
         Reset_CRC (Port);
      end if;
   end Transmit_Receive;

   -----------------------------
   -- Send_Receive_16bit_Mode --
   -----------------------------

   procedure Send_Receive_16bit_Mode
     (Port     : in out SPI_Port;
      Outgoing : Byte_Buffer;
      Incoming : out Byte_Buffer;
      Size     : Positive)
   is
      Tx_Count : Natural := Size;
      Outgoing_Index : Natural := Outgoing'First;
      Incoming_Index : Natural := Incoming'First;
   begin
      if Current_Mode (Port) = Slave or else Tx_Count = 1 then
         Port.Periph.DR.DR :=
           As_Half_Word_Pointer (Outgoing (Outgoing_Index)'Address).all;
         Outgoing_Index := Outgoing_Index + 2;
         Tx_Count := Tx_Count - 1;
      end if;

      if Tx_Count = 0 then

         --  enable CRC transmission
         if CRC_Enabled (Port) then
            Port.Periph.CR1.CRCNEXT := True;
         end if;

         --  wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         As_Half_Word_Pointer (Incoming (Incoming_Index)'Address).all :=
           Port.Periph.DR.DR;
         Incoming_Index := Incoming_Index + 2;

         return;
      end if;

      while Tx_Count > 0 loop
         --  wait until we can send data
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;

         Port.Periph.DR.DR :=
           As_Half_Word_Pointer (Outgoing (Outgoing_Index)'Address).all;
         Outgoing_Index := Outgoing_Index + 2;
         Tx_Count := Tx_Count - 1;

         --  enable CRC transmission
         if Tx_Count = 0 and CRC_Enabled (Port) then
            Port.Periph.CR1.CRCNEXT := True;
         end if;

         --  wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         As_Half_Word_Pointer (Incoming (Incoming_Index)'Address).all :=
           Port.Periph.DR.DR;
         Incoming_Index := Incoming_Index + 2;
      end loop;

      --  receive the last byte
      if Current_Mode (Port) = Slave then
         --  wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         As_Half_Word_Pointer (Incoming (Incoming_Index)'Address).all :=
           Port.Periph.DR.DR;
         Incoming_Index := Incoming_Index + 2;
      end if;
   end Send_Receive_16bit_Mode;

   ----------------------------
   -- Send_Receive_8bit_Mode --
   ----------------------------

   procedure Send_Receive_8bit_Mode
     (Port     : in out SPI_Port;
      Outgoing : Byte_Buffer;
      Incoming : out Byte_Buffer;
      Size     : Positive)
   is
      Tx_Count : Natural := Size;
      Outgoing_Index : Natural := Outgoing'First;
      Incoming_Index : Natural := Incoming'First;
   begin
      if Current_Mode (Port) = Slave or else Tx_Count = 1 then
         Port.Periph.DR.DR := Short (Outgoing (Outgoing_Index));
         Outgoing_Index := Outgoing_Index + 1;
         Tx_Count := Tx_Count - 1;
      end if;

      if Tx_Count = 0 then

         --  enable CRC transmission
         if CRC_Enabled (Port) then
            Port.Periph.CR1.CRCNEXT := True;
         end if;

         --  wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         Incoming (Incoming_Index) := Byte (Port.Periph.DR.DR);

         return;

      end if;

      while Tx_Count > 0 loop
         --  wait until we can send data
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;

         Port.Periph.DR.DR := Short (Outgoing (Outgoing_Index));
         Outgoing_Index := Outgoing_Index + 1;
         Tx_Count := Tx_Count - 1;

         --  enable CRC transmission
         if Tx_Count = 0 and CRC_Enabled (Port) then
            Port.Periph.CR1.CRCNEXT := True;
         end if;

         --  wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         Incoming (Incoming_Index) := Byte (Port.Periph.DR.DR);
         Incoming_Index := Incoming_Index + 1;
      end loop;

      if Current_Mode (Port) = Slave then
         --  wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         Incoming (Incoming_Index) := Data (Port);
         Incoming_Index := Incoming_Index + 1;
      end if;
   end Send_Receive_8bit_Mode;

   ---------------------
   -- Send_16bit_Mode --
   ---------------------

   procedure Send_16bit_Mode
     (Port     : in out SPI_Port;
      Outgoing : HAL.SPI.SPI_Data_16b)
   is
      Tx_Count : Natural := Outgoing'Length;
      Index    : Natural := Outgoing'First;
   begin
      if Current_Mode (Port) = Slave or else Tx_Count = 1 then
         Port.Periph.DR.DR :=
           As_Half_Word_Pointer (Outgoing (Index)'Address).all;
         Index := Index + 2;
         Tx_Count := Tx_Count - 1;
      end if;

      while Tx_Count > 0 loop
         --  wait until we can send data
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;

         Port.Periph.DR.DR :=
           As_Half_Word_Pointer (Outgoing (Index)'Address).all;
         Index := Index + 2;
         Tx_Count := Tx_Count - 1;
      end loop;

      if CRC_Enabled (Port) then
         Port.Periph.CR1.CRCNEXT := True;
      end if;
   end Send_16bit_Mode;

   --------------------
   -- Send_8bit_Mode --
   --------------------

   procedure Send_8bit_Mode
     (Port     : in out SPI_Port;
      Outgoing : HAL.SPI.SPI_Data_8b)
   is
      Tx_Count : Natural := Outgoing'Length;
      Index    : Natural := Outgoing'First;
   begin
      if Current_Mode (Port) = Slave or else Tx_Count = 1 then
         Port.Periph.DR.DR := Short (Outgoing (Index));
         Index := Index + 1;
         Tx_Count := Tx_Count - 1;
      end if;

      while Tx_Count > 0 loop
         --  wait until we can send data
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;

         Port.Periph.DR.DR := Short (Outgoing (Index));
         Index := Index + 1;
         Tx_Count := Tx_Count - 1;
      end loop;

      if CRC_Enabled (Port) then
         Port.Periph.CR1.CRCNEXT := True;
      end if;
   end Send_8bit_Mode;

   ------------------------
   -- Receive_16bit_Mode --
   ------------------------

   procedure Receive_16bit_Mode
     (Port     : in out SPI_Port;
      Incoming : out HAL.SPI.SPI_Data_16b)
   is
      Generate_Clock : constant Boolean := Current_Mode (Port) = Master;
   begin
      for K of Incoming loop
         if Generate_Clock then
            Port.Periph.DR.DR := 0;
         end if;
         while Rx_Is_Empty (Port) loop
            null;
         end loop;
         K := Port.Periph.DR.DR;
      end loop;
   end Receive_16bit_Mode;

   -----------------------
   -- Receive_8bit_Mode --
   -----------------------

   procedure Receive_8bit_Mode
     (Port     : in out SPI_Port;
      Incoming : out HAL.SPI.SPI_Data_8b)
   is
      Generate_Clock : constant Boolean := Current_Mode (Port) = Master;
   begin
      for K of Incoming loop
         if Generate_Clock then
            Port.Periph.DR.DR := 0;
         end if;
         while Rx_Is_Empty (Port) loop
            null;
         end loop;
         K := Byte (Port.Periph.DR.DR);
      end loop;
   end Receive_8bit_Mode;

end STM32.SPI;
