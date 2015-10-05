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

with STM32F4.RCC; use STM32F4.RCC;
with Ada.Unchecked_Conversion;
with System;

package body STM32F4.SPI is

   Baud_Rate_Value : constant array (SPI_Baud_Rate_Prescaler) of Bits_3 :=
     (BRP_2   => 2#000#,
      BRP_4   => 2#001#,
      BRP_8   => 2#010#,
      BRP_16  => 2#011#,
      BRP_32  => 2#100#,
      BRP_64  => 2#101#,
      BRP_128 => 2#110#,
      BRP_256 => 2#111#);

   type Half_Word_Pointer is access all Half_Word
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
            Port.CTRL1.Master_Select := 1;
            Port.CTRL1.Slave_Select  := 1;
         when Slave =>
            Port.CTRL1.Master_Select := 0;
            Port.CTRL1.Slave_Select  := 0;
      end case;

      case Conf.Direction is
         when D2Lines_FullDuplex =>
            Port.CTRL1.BiDir_Mode   := 0;
            Port.CTRL1.Output_BiDir := 0;
            Port.CTRL1.RXOnly       := 0;
         when D2Lines_RxOnly =>
            Port.CTRL1.BiDir_Mode   := 0;
            Port.CTRL1.Output_BiDir := 0;
            Port.CTRL1.RXOnly       := 1;
         when D1Line_Rx =>
            Port.CTRL1.BiDir_Mode   := 1;
            Port.CTRL1.Output_BiDir := 0;
            Port.CTRL1.RXOnly       := 0;
         when D1Line_Tx =>
            Port.CTRL1.BiDir_Mode   := 1;
            Port.CTRL1.Output_BiDir := 1;
            Port.CTRL1.RXOnly       := 0;
      end case;

      Port.CTRL1.Data_Frame_Fmt := (if Conf.Data_Size = Data_16 then 1 else 0);
      Port.CTRL1.Clock_Polarity := (if Conf.Clock_Polarity = High then 1 else 0);
      Port.CTRL1.Clock_Phase    := (if Conf.Clock_Phase = P2Edge then 1 else 0);
      Port.CTRL1.Soft_Slave_Mgt := (if Conf.Slave_Management = Software_Managed then 1 else 0);
      Port.CTRL1.Baud_Rate_Ctrl := Baud_Rate_Value (Conf.Baud_Rate_Prescaler);
      Port.CTRL1.LSB_First      := (if Conf.First_Bit = LSB then 1 else 0);

      --  Activate the SPI mode (Reset I2SMOD bit in I2SCFGR register)
      Port.I2S_Conf.Mode_Select := 0;

      Port.CRC_Poly := Conf.CRC_Poly;
   end Configure;

   ------------
   -- Enable --
   ------------

   procedure Enable (Port : in out SPI_Port) is
   begin
      Port.CTRL1.SPI_Enable := 1;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Port : in out SPI_Port) is
   begin
      Port.CTRL1.SPI_Enable := 0;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (Port : SPI_Port) return Boolean is
   begin
      return Port.CTRL1.SPI_Enable = 1;
   end Enabled;

   ----------
   -- Send --
   ----------

   procedure Send (Port : in out SPI_Port; Data : Half_Word) is
   begin
      Port.Data := Data;
   end Send;

   ----------
   -- Data --
   ----------

   function Data (Port : SPI_Port) return Half_Word is
   begin
      return Port.Data;
   end Data;

   ----------
   -- Send --
   ----------

   procedure Send (Port : in out SPI_Port; Data : Byte) is
   begin
      Send (Port, Half_Word (Data));
   end Send;

   ----------
   -- Data --
   ----------

   function Data (Port : SPI_Port) return Byte is
   begin
      return Byte (Half_Word'(Data (Port)));
   end Data;

   -----------------
   -- Tx_Is_Empty --
   -----------------

   function Tx_Is_Empty (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.TX_Buffer_Empty;
   end Tx_Is_Empty;

   -----------------
   -- Rx_Is_Empty --
   -----------------

   function Rx_Is_Empty (Port : SPI_Port) return Boolean is
   begin
      return not Port.Status.RX_Buffer_Not_Empty;
   end Rx_Is_Empty;

   ----------
   -- Busy --
   ----------

   function Busy (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Busy_Flag;
   end Busy;

   ------------------
   -- Current_Mode --
   ------------------

   function Current_Mode (Port : SPI_Port) return SPI_Mode is
   begin
      if Port.CTRL1.Master_Select = 1 and Port.CTRL1.Slave_Select = 1 then
         return Master;
      else
         return Slave;
      end if;
   end Current_Mode;

   ----------------------------
   -- Current_Data_Direction --
   ----------------------------

   function Current_Data_Direction (Port : SPI_Port) return SPI_Data_Direction is

      Direction_Mask : constant := 2#1100_0100_0000_0000#;

      function As_Half_Word is new Ada.Unchecked_Conversion
        (Source => SPI_Control_Register, Target => Half_Word);

   begin
      case As_Half_Word (Port.CTRL1) and Direction_Mask is
         when 0 => return D2Lines_FullDuplex;
         when 1 => return D2Lines_RxOnly;
         when 4 => return D1Line_Rx;
         when 6 => return D1Line_Tx;
         when others =>
            raise Program_Error;
      end case;
   end Current_Data_Direction;

   -----------------
   -- CRC_Enabled --
   -----------------

   function CRC_Enabled (Port : SPI_Port) return Boolean is
      (Port.CTRL1.CRC_Enable = 1);

   ----------------------------
   -- Channel_Side_Indicated --
   ----------------------------

   function Channel_Side_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Channel_Side;
   end Channel_Side_Indicated;

   ------------------------
   -- Underrun_Indicated --
   ------------------------

   function Underrun_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Underrun_Flag;
   end Underrun_Indicated;

   -------------------------
   -- CRC_Error_Indicated --
   -------------------------

   function CRC_Error_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.CRC_Error_Flag;
   end CRC_Error_Indicated;

   --------------------------
   -- Mode_Fault_Indicated --
   --------------------------

   function Mode_Fault_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Mode_Fault;
   end Mode_Fault_Indicated;

   -----------------------
   -- Overrun_Indicated --
   -----------------------

   function Overrun_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Overrun_Flag;
   end Overrun_Indicated;

   -------------------------------
   -- Frame_Fmt_Error_Indicated --
   -------------------------------

   function Frame_Fmt_Error_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Frame_Fmt_Error;
   end Frame_Fmt_Error_Indicated;

   -------------------
   -- Clear_Overrun --
   -------------------

   procedure Clear_Overrun (Port : SPI_Port) is
      Dummy1 : Half_Word;
      Dummy2 : SPI_Status_Register;
   begin
      Dummy1 := Port.Data;
      Dummy2 := Port.Status;
   end Clear_Overrun;

   ---------------
   -- Reset_CRC --
   ---------------

   procedure Reset_CRC (Port : in out SPI_Port) is
   begin
      Port.CTRL1.CRC_Enable := 0;
      Port.CTRL1.CRC_Enable := 1;
   end Reset_CRC;

   -------------------------
   -- Is_Data_Frame_16bit --
   -------------------------

   function Is_Data_Frame_16bit (Port : SPI_Port) return Boolean is
   begin
      return Port.Ctrl1.Data_Frame_Fmt = 1;
   end Is_Data_Frame_16bit;


   procedure Transmit
     (Port     : in out SPI_Port;
      Outgoing : Byte_Buffer;
      Size     : Positive)
   is
   begin
      if CRC_Enabled (Port) then
         Reset_CRC (Port);
      end if;

--      if(hspi->Init.Direction == SPI_DIRECTION_1LINE)
      if Current_Data_Direction (Port) = D1Line_Tx  then  --  ??? right value to compare???
         Port.CTRL1.Output_BiDir := 1;
      end if;
         Clear_Overrun (Port);

      if not Enabled (Port) then
         Enable (Port);
      end if;

      if Is_Data_Frame_16bit (Port) then
         Send_16bit_Mode (Port, Outgoing, Size);
      else
         Send_8bit_Mode (Port, Outgoing, Size);
      end if;

      --  Wait until TXE flag is set to send data
      while not Tx_Is_Empty (Port) loop
         null;
      end loop;

      --  Wait until Busy flag is reset before disabling SPI
      while Busy (Port) loop
         null;
      end loop;

      --  clear OVERUN flag in 2-Line communication mode because received byte is not read
      if Current_Data_Direction (Port) in D2Lines_RxOnly | D2Lines_FullDuplex then  -- right comparison ???
         Clear_Overrun (Port);
      end if;
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

--      if(hspi->Init.Direction == SPI_DIRECTION_1LINE)
      if Current_Data_Direction (Port) = D1Line_Tx  then  --  ??? right value to compare???
         Port.CTRL1.Output_BiDir := 1;
      end if;

      if not Enabled (Port) then
         Enable (Port);
      end if;

      Port.Data := Half_Word (Outgoing);

      while not Tx_Is_Empty (Port) loop
         null;
      end loop;

      while Busy (Port) loop
         null;
      end loop;

      --  clear OVERUN flag in 2-Line communication mode because received byte is not read
      if Current_Data_Direction (Port) in D2Lines_RxOnly | D2Lines_FullDuplex then  -- right comparison ???
         Clear_Overrun (Port);
      end if;
   end Transmit;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Port     : in out SPI_Port;
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
         Receive_16bit_Mode (Port, Incoming, Size);
      else
         Receive_8bit_Mode (Port, Incoming, Size);
      end if;

      while Busy (Port) loop
         null;
      end loop;

      if CRC_Enabled (Port) and CRC_Error_Indicated (Port) then
         Reset_CRC (Port);
      end if;
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

      Port.Data := 0; -- generate clock

      while Rx_Is_Empty (Port) loop
         null;
      end loop;

      Incoming := Byte (Port.Data);

      if CRC_Enabled (Port) then
         while Rx_Is_Empty (Port) loop
            null;
         end loop;
         Read_CRC: declare
            Dummy : Half_Word;
         begin
            Dummy := Port.Data;
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

      -- Read CRC to close CRC calculation process
      if CRC_Enabled (Port) then
         -- wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;
         Read_CRC: declare
            Dummy : Half_Word;
         begin
            Dummy := Port.Data;
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

      Port.Data := Half_Word (Outgoing);

      -- enable CRC transmission
      if CRC_Enabled (Port) then
         Port.Ctrl1.CRC_Next := 1;
      end if;

      -- wait until data is received
      while Rx_Is_Empty (Port) loop
         null;
      end loop;

      Incoming := Byte (Port.Data);

      -- Read CRC byte to close CRC calculation
      if CRC_Enabled (Port) then
         -- wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;
         Read_CRC: declare
            Dummy : Half_Word;
         begin
            Dummy := Port.Data;
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
         Port.Data := As_Half_Word_Pointer (Outgoing (Outgoing_Index)'Address).all;
         Outgoing_Index := Outgoing_Index + 2;
         Tx_Count := Tx_Count - 1;
      end if;

      if Tx_Count = 0 then

         -- enable CRC transmission
         if CRC_Enabled (Port) then
            Port.Ctrl1.CRC_Next := 1;
         end if;

         -- wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         As_Half_Word_Pointer (Incoming (Incoming_Index)'Address).all := Port.Data;
         Incoming_Index := Incoming_Index + 2;

         return;
      end if;

      while Tx_Count > 0 loop
         --  wait until we can send data
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;

         Port.Data := As_Half_Word_Pointer (Outgoing (Outgoing_Index)'Address).all;
         Outgoing_Index := Outgoing_Index + 2;
         Tx_Count := Tx_Count - 1;

         -- enable CRC transmission
         if Tx_Count = 0 and CRC_Enabled (Port) then
            Port.Ctrl1.CRC_Next := 1;
         end if;

         -- wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         As_Half_Word_Pointer (Incoming (Incoming_Index)'Address).all := Port.Data;
         Incoming_Index := Incoming_Index + 2;
      end loop;

      --  receive the last byte
      if Current_Mode (Port) = Slave then
         -- wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         As_Half_Word_Pointer (Incoming (Incoming_Index)'Address).all := Port.Data;
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
         Port.Data := Half_Word (Outgoing (Outgoing_Index));
         Outgoing_Index := Outgoing_Index + 1;
         Tx_Count := Tx_Count - 1;
      end if;

      if Tx_Count = 0 then

         -- enable CRC transmission
         if CRC_Enabled (Port) then
            Port.Ctrl1.CRC_Next := 1;
         end if;

         -- wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         Incoming (Incoming_Index) := Byte (Port.Data);

         return;

      end if;

      while Tx_Count > 0 loop
         --  wait until we can send data
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;

         Port.Data := Half_Word (Outgoing (Outgoing_Index));
         Outgoing_Index := Outgoing_Index + 1;
         Tx_Count := Tx_Count - 1;

         -- enable CRC transmission
         if Tx_Count = 0 and CRC_Enabled (Port) then
            Port.Ctrl1.CRC_Next := 1;
         end if;

         -- wait until data is received
         while Rx_Is_Empty (Port) loop
            null;
         end loop;

         Incoming (Incoming_Index) := Byte (Port.Data);
         Incoming_Index := Incoming_Index + 1;
      end loop;

      if Current_Mode (Port) = Slave then
         -- wait until data is received
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
      Outgoing : Byte_Buffer;
      Size     : Positive)
   is
      Tx_Count : Natural := Size;
      Index    : Natural := Outgoing'First;
   begin
      if Current_Mode (Port) = Slave or else Tx_Count = 1 then
         Port.Data := As_Half_Word_Pointer (Outgoing (Index)'Address).all;
         Index := Index + 2;
         Tx_Count := Tx_Count - 1;
      end if;

      while Tx_Count > 0 loop
         --  wait until we can send data
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;

         Port.Data := As_Half_Word_Pointer (Outgoing (Index)'Address).all;
         Index := Index + 2;
         Tx_Count := Tx_Count - 1;
      end loop;

      if CRC_Enabled (Port) then
         Port.Ctrl1.CRC_Next := 1;
      end if;
   end Send_16bit_Mode;

   --------------------
   -- Send_8bit_Mode --
   --------------------

   procedure Send_8bit_Mode
     (Port     : in out SPI_Port;
      Outgoing : Byte_Buffer;
      Size     : Positive)
   is
      Tx_Count : Natural := Size;
      Index    : Natural := Outgoing'First;
   begin
      if Current_Mode (Port) = Slave or else Tx_Count = 1 then
         Port.Data := Half_Word (Outgoing (Index));
         Index := Index + 1;
         Tx_Count := Tx_Count - 1;
      end if;

      while Tx_Count > 0 loop
         --  wait until we can send data
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;

         Port.Data := Half_Word (Outgoing (Index));
         Index := Index + 1;
         Tx_Count := Tx_Count - 1;
      end loop;

      if CRC_Enabled (Port) then
         Port.Ctrl1.CRC_Next := 1;
      end if;
   end Send_8bit_Mode;

   ------------------------
   -- Receive_16bit_Mode --
   ------------------------

   procedure Receive_16bit_Mode
     (Port     : in out SPI_Port;
      Incoming : out Byte_Buffer;
      Count    : Positive)
   is
      Index : Natural := Incoming'First;
      Generate_Clock : constant Boolean := Current_Mode (Port) = Master;
   begin
      for K in 1 .. Count loop
         if Generate_Clock then
            Port.Data := 0;
         end if;
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;
         As_Half_Word_Pointer (Incoming (Index)'Address).all := Port.Data;
         Index := Index + 2;
      end loop;
   end Receive_16bit_Mode;

   -----------------------
   -- Receive_8bit_Mode --
   -----------------------

   procedure Receive_8bit_Mode
     (Port     : in out SPI_Port;
      Incoming : out Byte_Buffer;
      Count    : Positive)
   is
      Index : Natural := Incoming'First;
      Generate_Clock : constant Boolean := Current_Mode (Port) = Master;
   begin
      for K in 1 .. Count loop
         if Generate_Clock then
            Port.Data := 0;
         end if;
         while not Tx_Is_Empty (Port) loop
            null;
         end loop;
         Incoming (Index) := Byte (Port.Data);
         Index := Index + 1;
      end loop;
   end Receive_8bit_Mode;

end STM32F4.SPI;
