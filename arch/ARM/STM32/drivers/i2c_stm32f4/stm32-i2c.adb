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
--   @file    stm32f4xx_hal_i2c.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   I2C HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;

with STM32_SVD.I2C; use STM32_SVD.I2C;
with STM32.Device; use STM32.Device;

package body STM32.I2C is

   use type HAL.I2C.I2C_Status;

   subtype Dispatch is I2C_Port'Class;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This : in out I2C_Port;
      Conf : I2C_Configuration)
   is
      CR1        : CR1_Register;
      CCR        : CCR_Register;
      OAR1       : OAR1_Register;
      PCLK1      : constant UInt32 := System_Clock_Frequencies.PCLK1;
      Freq_Range : constant UInt16 := UInt16 (PCLK1 / 1_000_000);

   begin
      if This.State /= Reset then
         return;
      end if;

      This.Config := Conf;

      --  Disable the I2C port
      if Freq_Range < 2 or else Freq_Range > 45 then
         raise Program_Error with
           "PCLK1 too high or too low: expected 2-45 MHz, current" &
           Freq_Range'Img & " MHz";
      end if;

      Set_State (This, False);

      --  Load CR2 and clear FREQ
      This.Periph.CR2 :=
        (LAST    => False,
         DMAEN   => False,
         ITBUFEN => False,
         ITEVTEN => False,
         ITERREN => False,
         FREQ    => UInt6 (Freq_Range),
         others  => <>);

      --  Set the port timing
      if Conf.Clock_Speed <= 100_000 then
         --  Mode selection to Standard Mode
         CCR.F_S := False;
         CCR.CCR := UInt12 (PCLK1 / (Conf.Clock_Speed * 2));

         if CCR.CCR < 4 then
            CCR.CCR := 4;
         end if;

         This.Periph.TRISE.TRISE := UInt6 (Freq_Range + 1);

      else
         --  Fast mode
         CCR.F_S := True;

         if Conf.Duty_Cycle = DutyCycle_2 then
            CCR.CCR := UInt12 (PCLK1 / (Conf.Clock_Speed * 3));
         else
            CCR.CCR := UInt12 (PCLK1 / (Conf.Clock_Speed * 25));
            CCR.DUTY := True;
         end if;

         if CCR.CCR = 0 then
            CCR.CCR := 1;
         end if;

         This.Periph.TRISE.TRISE :=
           UInt6 ((UInt32 (Freq_Range) * 300) / 1000 + 1);
      end if;

      This.Periph.CCR := CCR;

      --  CR1 configuration
      case Conf.Mode is
         when I2C_Mode =>
            CR1.SMBUS := False;
            CR1.SMBTYPE := False;
         when SMBusDevice_Mode =>
            CR1.SMBUS := True;
            CR1.SMBTYPE := False;
         when SMBusHost_Mode =>
            CR1.SMBUS := True;
            CR1.SMBTYPE := True;
      end case;
      CR1.ENGC      := Conf.General_Call_Enabled;
      CR1.NOSTRETCH := not Conf.Clock_Stretching_Enabled;
      This.Periph.CR1 := CR1;

      --  Address mode (slave mode) configuration
      OAR1.ADDMODE := Conf.Addressing_Mode = Addressing_Mode_10bit;
      case Conf.Addressing_Mode is
         when Addressing_Mode_7bit =>
            OAR1.ADD0  := False;
            OAR1.ADD7  := UInt7 (Conf.Own_Address / 2);
            OAR1.ADD10 := 0;
         when Addressing_Mode_10bit =>
            OAR1.ADD0  := (Conf.Own_Address and 2#1#) /= 0;
            OAR1.ADD7  := UInt7 ((Conf.Own_Address / 2) and 2#1111111#);
            OAR1.ADD10 := UInt2 (Conf.Own_Address / 2 ** 8);
      end case;

      This.Periph.OAR1 := OAR1;

      This.DMA_Enabled := Conf.Enable_DMA;

      Set_State (This, True);
      This.State := Ready;
   end Configure;

   -----------------
   -- Flag_Status --
   -----------------

   function Flag_Status
     (This : I2C_Port; Flag : I2C_Status_Flag) return Boolean
   is
   begin
      case Flag is
         when Start_Bit =>
            return This.Periph.SR1.SB;
         when Address_Sent =>
            return This.Periph.SR1.ADDR;
         when Byte_Transfer_Finished =>
            return This.Periph.SR1.BTF;
         when Address_Sent_10bit =>
            return This.Periph.SR1.ADD10;
         when Stop_Detection =>
            return This.Periph.SR1.STOPF;
         when Rx_Data_Register_Not_Empty =>
            return This.Periph.SR1.RxNE;
         when Tx_Data_Register_Empty =>
            return This.Periph.SR1.TxE;
         when Bus_Error =>
            return This.Periph.SR1.BERR;
         when Arbitration_Lost =>
            return This.Periph.SR1.ARLO;
         when Ack_Failure =>
            return This.Periph.SR1.AF;
         when UnderOverrun =>
            return This.Periph.SR1.OVR;
         when Packet_Error =>
            return This.Periph.SR1.PECERR;
         when Timeout =>
            return This.Periph.SR1.TIMEOUT;
         when SMB_Alert =>
            return This.Periph.SR1.SMBALERT;
         when Master_Slave_Mode =>
            return This.Periph.SR2.MSL;
         when Busy =>
            return This.Periph.SR2.BUSY;
         when Transmitter_Receiver_Mode =>
            return This.Periph.SR2.TRA;
         when General_Call =>
            return This.Periph.SR2.GENCALL;
         when SMB_Default =>
            return This.Periph.SR2.SMBDEFAULT;
         when SMB_Host =>
            return This.Periph.SR2.SMBHOST;
         when Dual_Flag =>
            return This.Periph.SR2.DUALF;
      end case;
   end Flag_Status;

--     ----------------
--     -- Clear_Flag --
--     ----------------
--
--     procedure Clear_Flag
--       (Port   : in out I2C_Port;
--        Target : Clearable_I2C_Status_Flag)
--     is
--        Unref  : Bit with Unreferenced;
--     begin
--        case Target is
--           when Bus_Error =>
--              Port.SR1.BERR := 0;
--           when Arbitration_Lost =>
--              Port.SR1.ARLO := 0;
--           when Ack_Failure =>
--              Port.SR1.AF := 0;
--           when UnderOverrun =>
--              Port.SR1.OVR := 0;
--           when Packet_Error =>
--              Port.SR1.PECERR := 0;
--           when Timeout =>
--              Port.SR1.TIMEOUT := 0;
--           when SMB_Alert =>
--              Port.SR1.SMBALERT := 0;
--        end case;
--     end Clear_Flag;

   -------------------------------
   -- Clear_Address_Sent_Status --
   -------------------------------

   procedure Clear_Address_Sent_Status (This : I2C_Port)
   is
      Unref  : Boolean with Volatile, Unreferenced;
   begin
      --  ADDR is cleared after reading both SR1 and SR2
      Unref := This.Periph.SR1.ADDR;
      Unref := This.Periph.SR2.MSL;
   end Clear_Address_Sent_Status;

--     ---------------------------------
--     -- Clear_Stop_Detection_Status --
--     ---------------------------------
--
--     procedure Clear_Stop_Detection_Status (Port : in out I2C_Port) is
--        Unref  : Bit with Volatile, Unreferenced;
--     begin
--        Unref := Port.SR1.STOPF;
--        Port.CR1.PE := True;
--     end Clear_Stop_Detection_Status;

   ---------------
   -- Wait_Flag --
   ---------------

   procedure Wait_Flag
     (This    : in out I2C_Port;
      Flag    :        I2C_Status_Flag;
      F_State :        Boolean;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      while Flag_Status (This, Flag) = F_State loop
         if Clock - Start > Milliseconds (Timeout) then
            This.State := Ready;
            Status       := HAL.I2C.Err_Timeout;

            return;
         end if;
      end loop;

      Status := HAL.I2C.Ok;
   end Wait_Flag;

   ----------------------
   -- Wait_Master_Flag --
   ----------------------

   procedure Wait_Master_Flag
     (This    : in out I2C_Port;
      Flag    :        I2C_Status_Flag;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      while not Flag_Status (This, Flag) loop
         if This.Periph.SR1.AF then
            --  Generate STOP
            This.Periph.CR1.STOP := True;

            --  Clear the AF flag
            This.Periph.SR1.AF := False;
            This.State := Ready;
            Status       := HAL.I2C.Err_Error;

            return;
         end if;

         if Clock - Start > Milliseconds (Timeout) then
            This.State := Ready;
            Status       := HAL.I2C.Err_Timeout;

            return;
         end if;
      end loop;

      Status := HAL.I2C.Ok;
   end Wait_Master_Flag;

   --------------------------
   -- Master_Request_Write --
   --------------------------

   procedure Master_Request_Write
     (This       : in out I2C_Port;
      Addr       :        HAL.I2C.I2C_Address;
      Timeout    :        Natural;
      Status     :    out HAL.I2C.I2C_Status)
   is
   begin
      This.Periph.CR1.START := True;

      Wait_Flag (This, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if This.DMA_Enabled then
         This.Periph.CR2.DMAEN := True;
      end if;

      if This.Config.Addressing_Mode = Addressing_Mode_7bit then
         This.Periph.DR.DR := UInt8 (Addr) and not 2#1#;
      else
         declare
            MSB : constant UInt8 :=
                    UInt8 (Shift_Right (UInt16 (Addr) and 16#300#, 7));
            LSB : constant UInt8 :=
                    UInt8 (Addr and 16#FF#);
         begin
            --  We need to send 2#1111_MSB0# when MSB are the 3 most
            --  significant bits of the address
            This.Periph.DR.DR := MSB or 16#F0#;

            Wait_Master_Flag (This, Address_Sent_10bit, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            This.Periph.DR.DR := LSB;
         end;
      end if;

      Wait_Master_Flag (This, Address_Sent, Timeout, Status);
   end Master_Request_Write;

   --------------------------
   -- Master_Request_Write --
   --------------------------

   procedure Master_Request_Read
     (This    : in out I2C_Port;
      Addr    :        HAL.I2C.I2C_Address;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status)
   is
   begin
      This.Periph.CR1.ACK := True;
      This.Periph.CR1.START := True;

      Wait_Flag (This, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if This.DMA_Enabled then
         This.Periph.CR2.DMAEN := True;
      end if;

      if This.Config.Addressing_Mode = Addressing_Mode_7bit then
         This.Periph.DR.DR := UInt8 (Addr) or 2#1#;
      else
         declare
            MSB : constant UInt8 :=
                    UInt8 (Shift_Right (UInt16 (Addr) and 16#300#, 7));
            LSB : constant UInt8 :=
                    UInt8 (Addr and 16#FF#);
         begin
            --  We need to write the address bit. So let's start with a
            --  write header
            --  We need to send 2#1111_MSB0# when MSB are the 3 most
            --  significant bits of the address
            This.Periph.DR.DR := MSB or 16#F0#;

            Wait_Master_Flag (This, Address_Sent_10bit, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            This.Periph.DR.DR := LSB;

            Wait_Master_Flag (This, Address_Sent, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Clear_Address_Sent_Status (This);

            --  Generate a re-start
            This.Periph.CR1.START := True;

            Wait_Flag (This, Start_Bit, False, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            --  resend the MSB with the read bit set.
            This.Periph.DR.DR := MSB or 16#F1#;
         end;
      end if;

      Wait_Master_Flag (This, Address_Sent, Timeout, Status);
   end Master_Request_Read;

   -----------------------
   -- Mem_Request_Write --
   -----------------------

   procedure Mem_Request_Write
     (This          : in out I2C_Port;
      Addr          :        HAL.I2C.I2C_Address;
      Mem_Addr      :        UInt16;
      Mem_Addr_Size :        HAL.I2C.I2C_Memory_Address_Size;
      Timeout       :        Natural;
      Status        :    out HAL.I2C.I2C_Status)
   is
   begin
      This.Periph.CR1.START := True;

      Wait_Flag (This, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if This.DMA_Enabled then
         This.Periph.CR2.DMAEN := True;
      end if;

      --  Send slave address
      This.Periph.DR.DR := UInt8 (Addr) and not 2#1#;
      Wait_Master_Flag (This, Address_Sent, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      Clear_Address_Sent_Status (This);

      --  Wait until TXE flag is set
      Wait_Flag (This, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      case Mem_Addr_Size is
         when HAL.I2C.Memory_Size_8b =>
            This.Periph.DR.DR := UInt8 (Mem_Addr);
         when HAL.I2C.Memory_Size_16b =>
            This.Periph.DR.DR := UInt8 (Shift_Right (Mem_Addr, 8));

            Wait_Flag (This, Tx_Data_Register_Empty, False, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            This.Periph.DR.DR := UInt8 (Mem_Addr and 16#FF#);
      end case;

      --  Wait until TXE flag is set
      Wait_Flag (This, Tx_Data_Register_Empty, False, Timeout, Status);

   end Mem_Request_Write;

   ----------------------
   -- Mem_Request_Read --
   ----------------------

   procedure Mem_Request_Read
     (This          : in out I2C_Port;
      Addr          :        HAL.I2C.I2C_Address;
      Mem_Addr      :        UInt16;
      Mem_Addr_Size :        HAL.I2C.I2C_Memory_Address_Size;
      Timeout       :        Natural;
      Status        :    out HAL.I2C.I2C_Status)
   is
   begin
      This.Periph.CR1.ACK := True;
      This.Periph.CR1.START := True;

      Wait_Flag (This, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if This.DMA_Enabled then
         This.Periph.CR2.DMAEN := True;
      end if;

      --  Send slave address in write mode
      This.Periph.DR.DR := UInt8 (Addr) and not 16#1#;

      Wait_Master_Flag (This, Address_Sent, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      Clear_Address_Sent_Status (This);

      --  Wait until TXE flag is set
      Wait_Flag (This, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      case Mem_Addr_Size is
         when HAL.I2C.Memory_Size_8b =>
            This.Periph.DR.DR := UInt8 (Mem_Addr);
         when HAL.I2C.Memory_Size_16b =>
            This.Periph.DR.DR := UInt8 (Shift_Right (Mem_Addr, 8));

            Wait_Flag (This, Tx_Data_Register_Empty, False, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            This.Periph.DR.DR := UInt8 (Mem_Addr and 16#FF#);
      end case;

      --  Wait until TXE flag is set
      Wait_Flag (This, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      --  We now need to reset and send the slave address in read mode
      This.Periph.CR1.START := True;

      Wait_Flag (This, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      --  Send slave address in read mode
      This.Periph.DR.DR := UInt8 (Addr) or 16#1#;

      Wait_Master_Flag (This, Address_Sent, Timeout, Status);
   end Mem_Request_Read;

   ---------------------
   -- Master_Transmit --
   ---------------------

   overriding
   procedure Master_Transmit
     (This    : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000)
   is
   begin
      if This.State = Reset then
         Status := HAL.I2C.Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := HAL.I2C.Err_Error;
         return;
      end if;

      Wait_Flag (This, Busy, True, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         Status := HAL.I2C.Busy;
         return;
      end if;

      if This.State /= Ready then
         Status := HAL.I2C.Busy;
         return;
      end if;

      This.State := Master_Busy_Tx;

      This.Periph.CR1.POS := False;

      Master_Request_Write (This, Addr, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      Clear_Address_Sent_Status (This);

      --  Use a dispatching call in case Data_Send is overridden for DMA
      --  transfert.
      Dispatch (This).Data_Send (Data    => Data,
                                 Timeout => Timeout,
                                 Status  => Status);

      --  Generate STOP
      This.Periph.CR1.STOP := True;
      This.State := Ready;
   end Master_Transmit;

   --------------------
   -- Master_Receive --
   --------------------

   overriding
   procedure Master_Receive
     (This    : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : out HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000)
   is
   begin
      if This.State = Reset then
         Status := HAL.I2C.Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := HAL.I2C.Err_Error;
         return;
      end if;

      Wait_Flag (This, Busy, True, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         Status := HAL.I2C.Busy;
         return;
      end if;

      if This.State /= Ready then
         Status := HAL.I2C.Busy;
         return;
      end if;

      This.State := Master_Busy_Rx;

      This.Periph.CR1.POS := False;

      Master_Request_Read (This, Addr, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if Data'Length = 1 then
         --  Disable acknowledge
         This.Periph.CR1.ACK := False;
         Clear_Address_Sent_Status (This);
         This.Periph.CR1.STOP := True;

      elsif Data'Length = 2 then
         --  Disable acknowledge
         This.Periph.CR1.ACK := False;
         This.Periph.CR1.POS := True;
         Clear_Address_Sent_Status (This);

      else
         --  Automatic acknowledge
         This.Periph.CR1.ACK := True;
         Clear_Address_Sent_Status (This);
      end if;

      --  Use a dispatching call in case Data_Receive is overridden for DMA
      --  transfert.
      Dispatch (This).Data_Receive (Data, Timeout, Status);

      This.State := Ready;
   end Master_Receive;

   ---------------
   -- Mem_Write --
   ---------------

   overriding
   procedure Mem_Write
     (This          : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin
      if This.State = Reset then
         Status := HAL.I2C.Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := HAL.I2C.Err_Error;
         return;
      end if;

      Wait_Flag (This, Busy, True, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         Status := HAL.I2C.Busy;
         return;
      end if;

      if This.State /= Ready then
         Status := HAL.I2C.Busy;
         return;
      end if;

      This.State := Mem_Busy_Tx;
      This.Periph.CR1.POS := False;

      Mem_Request_Write
        (This, Addr, Mem_Addr, Mem_Addr_Size, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      --  Use a dispatching call in case Data_Send is overridden for DMA
      --  transfert.
      Dispatch (This).Data_Send (Data    => Data,
                                 Timeout => Timeout,
                                 Status  => Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      --  Generate STOP
      This.Periph.CR1.STOP := True;
      This.State := Ready;
   end Mem_Write;

   --------------
   -- Mem_Read --
   --------------

   overriding
   procedure Mem_Read
     (This          : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : out HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin
      if This.State = Reset then
         Status := HAL.I2C.Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := HAL.I2C.Err_Error;
         return;
      end if;

      Wait_Flag (This, Busy, True, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         Status := HAL.I2C.Busy;
         return;
      end if;

      if This.State /= Ready then
         Status := HAL.I2C.Busy;
         return;
      end if;

      This.State := Mem_Busy_Rx;

      This.Periph.CR1.POS := False;

      Mem_Request_Read
        (This, Addr, Mem_Addr, Mem_Addr_Size, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if Data'Length = 1 then
         --  Disable acknowledge
         This.Periph.CR1.ACK := False;
         Clear_Address_Sent_Status (This);
         This.Periph.CR1.STOP := True;

      elsif Data'Length = 2 then
         --  Disable acknowledge
         This.Periph.CR1.ACK := False;
         This.Periph.CR1.POS := True;
         Clear_Address_Sent_Status (This);

      else
         --  Automatic acknowledge
         This.Periph.CR1.ACK := True;
         Clear_Address_Sent_Status (This);
      end if;

      --  Use a dispatching call in case Data_Receive is overridden for DMA
      --  transfert.
      Dispatch (This).Data_Receive (Data, Timeout, Status);

      This.State := Ready;
   end Mem_Read;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State (This : in out I2C_Port; Enabled : Boolean)
   is
   begin
      This.Periph.CR1.PE := Enabled;
   end Set_State;

   ------------------
   -- Port_Enabled --
   ------------------

   function Port_Enabled (This : I2C_Port) return Boolean
   is
   begin
      return This.Periph.CR1.PE;
   end Port_Enabled;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This   : in out I2C_Port;
      Source : I2C_Interrupt)
   is
   begin
      case Source is
         when Error_Interrupt =>
            This.Periph.CR2.ITERREN := True;
         when Event_Interrupt =>
            This.Periph.CR2.ITEVTEN := True;
         when Buffer_Interrupt =>
            This.Periph.CR2.ITBUFEN := True;
      end case;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (This   : in out I2C_Port;
      Source : I2C_Interrupt)
   is
   begin
      case Source is
         when Error_Interrupt =>
            This.Periph.CR2.ITERREN := False;
         when Event_Interrupt =>
            This.Periph.CR2.ITEVTEN := False;
         when Buffer_Interrupt =>
            This.Periph.CR2.ITBUFEN := False;
      end case;
   end Disable_Interrupt;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (This   : I2C_Port;
      Source : I2C_Interrupt)
      return Boolean
   is
   begin
      case Source is
         when Error_Interrupt =>
            return This.Periph.CR2.ITERREN;
         when Event_Interrupt =>
            return This.Periph.CR2.ITEVTEN;
         when Buffer_Interrupt =>
            return This.Periph.CR2.ITBUFEN;
      end case;
   end Enabled;

   ---------------------------
   -- Data_Register_Address --
   ---------------------------

   function Data_Register_Address
     (This : I2C_Port)
      return System.Address
   is (This.Periph.DR'Address);

   -------------------
   -- Data_Transfer --
   -------------------

   procedure Data_Send
     (This    : in out I2C_Port;
      Data    :        HAL.I2C.I2C_Data;
      Timeout :        Natural;
      Status  : out    HAL.I2C.I2C_Status)
   is
      Idx : Natural := Data'First;
   begin
      while Idx <= Data'Last loop
         Wait_Flag (This,
                    Tx_Data_Register_Empty,
                    False,
                    Timeout,
                    Status);

         if Status /= HAL.I2C.Ok then
            return;
         end if;

         This.Periph.DR.DR := Data (Idx);
         Idx := Idx + 1;

         if Flag_Status (This, Byte_Transfer_Finished)
           and then
            Idx <= Data'Last
           and then
            Status = HAL.I2C.Ok
         then
            This.Periph.DR.DR := Data (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      Wait_Flag (This,
                 Tx_Data_Register_Empty,
                 False,
                 Timeout,
                 Status);

   end Data_Send;

   ------------------
   -- Data_Receive --
   ------------------

   procedure Data_Receive
     (This    : in out I2C_Port;
      Data    :    out HAL.I2C.I2C_Data;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status)
   is
      Idx : Natural := Data'First;
   begin
      while Idx <= Data'Last loop
         if Idx = Data'Last then
            --  One UInt8 to read
            Wait_Flag
              (This,
               Rx_Data_Register_Not_Empty,
               False,
               Timeout,
               Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Data (Idx) := This.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 1 = Data'Last then
            --  Two bytes to read
            This.Periph.CR1.ACK := False;

            Wait_Flag (This,
                       Byte_Transfer_Finished,
                       False,
                       Timeout,
                       Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            This.Periph.CR1.STOP := True;

            --  read the data from DR
            Data (Idx) := This.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := This.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 2 = Data'Last then
            --  Three bytes to read
            Wait_Flag (This,
                       Byte_Transfer_Finished,
                       False,
                       Timeout,
                       Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            This.Periph.CR1.ACK := False;

            --  read the data from DR
            Data (Idx) := This.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (This,
                       Byte_Transfer_Finished,
                       False,
                       Timeout,
                       Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            This.Periph.CR1.STOP := True;

            --  read the data from DR
            Data (Idx) := This.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := This.Periph.DR.DR;
            Idx := Idx + 1;

         else
            --  One byte to read
            Wait_Flag
              (This,
               Rx_Data_Register_Not_Empty,
               False,
               Timeout,
               Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Data (Idx) := This.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (This,
                       Byte_Transfer_Finished,
                       False,
                       Timeout,
                       Status);

            if Status = HAL.I2C.Ok then
               Data (Idx) := This.Periph.DR.DR;
               Idx := Idx + 1;
            end if;

         end if;
      end loop;
   end Data_Receive;

end STM32.I2C;
