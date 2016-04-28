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

   type I2C_Status_Flag is
     (Start_Bit,
      Address_Sent,
      Byte_Transfer_Finished,
      Address_Sent_10bit,
      Stop_Detection,
      Rx_Data_Register_Not_Empty,
      Tx_Data_Register_Empty,
      Bus_Error,
      Arbitration_Lost,
      Ack_Failure,
      UnderOverrun,
      Packet_Error,
      Timeout,
      SMB_Alert,
      Master_Slave_Mode,
      Busy,
      Transmitter_Receiver_Mode,
      General_Call,
      SMB_Default,
      SMB_Host,
      Dual_Flag);

   --  Low level flag handling

   function Flag_Status (Port : I2C_Port;
                         Flag : I2C_Status_Flag)
                         return Boolean;
   procedure Clear_Address_Sent_Status (Port : I2C_Port);

   --  Higher level flag handling

   procedure Wait_Flag
     (Handle  : in out I2C_Port;
      Flag    :        I2C_Status_Flag;
      F_State :        Boolean;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status);

   procedure Wait_Master_Flag
     (Handle  : in out I2C_Port;
      Flag    :        I2C_Status_Flag;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status);

   procedure Master_Request_Write
     (Handle  : in out I2C_Port;
      Addr    :        HAL.I2C.I2C_Address;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status);

   procedure Master_Request_Read
     (Handle  : in out I2C_Port;
      Addr    :        HAL.I2C.I2C_Address;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status);

   procedure Mem_Request_Write
     (Handle        : in out I2C_Port;
      Addr          :        HAL.I2C.I2C_Address;
      Mem_Addr      :        Short;
      Mem_Addr_Size :        HAL.I2C.I2C_Memory_Address_Size;
      Timeout       :        Natural;
      Status        :    out HAL.I2C.I2C_Status);

   procedure Mem_Request_Read
     (Handle        : in out I2C_Port;
      Addr          :        HAL.I2C.I2C_Address;
      Mem_Addr      :        Short;
      Mem_Addr_Size :        HAL.I2C.I2C_Memory_Address_Size;
      Timeout       :        Natural;
      Status        :    out HAL.I2C.I2C_Status);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Handle : in out I2C_Port;
      Conf : I2C_Configuration)
   is
      CR1        : CR1_Register;
      CCR        : CCR_Register;
      OAR1       : OAR1_Register;
      PCLK1      : constant Word := System_Clock_Frequencies.PCLK1;
      Freq_Range : constant Short := Short (PCLK1 / 1_000_000);

   begin
      if Handle.State /= Reset then
         return;
      end if;

      Handle.Config := Conf;

--        Enable_Clock (Handle.Periph.all);

      --  Disable the I2C port
      if Freq_Range < 2 or else Freq_Range > 45 then
         raise Program_Error with
           "PCLK1 too high or too low: expected 2-45 MHz, current" &
           Freq_Range'Img & " MHz";
      end if;

      Set_State (Handle, False);

      --  Load CR2 and clear FREQ
      Handle.Periph.CR2 :=
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

         Handle.Periph.TRISE.TRISE := UInt6 (Freq_Range + 1);

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

         CCR.CCR := CCR.CCR or 16#80#;

         Handle.Periph.TRISE.TRISE :=
           UInt6 ((Word (Freq_Range) * 300) / 1000 + 1);
      end if;

      Handle.Periph.CCR := CCR;

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
      Handle.Periph.CR1 := CR1;

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

      Handle.Periph.OAR1 := OAR1;

      Set_State (Handle, True);
      Handle.State := Ready;
   end Configure;

   -----------------
   -- Flag_Status --
   -----------------

   function Flag_Status
     (Port : I2C_Port; Flag : I2C_Status_Flag) return Boolean
   is
   begin
      case Flag is
         when Start_Bit =>
            return Port.Periph.SR1.SB;
         when Address_Sent =>
            return Port.Periph.SR1.ADDR;
         when Byte_Transfer_Finished =>
            return Port.Periph.SR1.BTF;
         when Address_Sent_10bit =>
            return Port.Periph.SR1.ADD10;
         when Stop_Detection =>
            return Port.Periph.SR1.STOPF;
         when Rx_Data_Register_Not_Empty =>
            return Port.Periph.SR1.RxNE;
         when Tx_Data_Register_Empty =>
            return Port.Periph.SR1.TxE;
         when Bus_Error =>
            return Port.Periph.SR1.BERR;
         when Arbitration_Lost =>
            return Port.Periph.SR1.ARLO;
         when Ack_Failure =>
            return Port.Periph.SR1.AF;
         when UnderOverrun =>
            return Port.Periph.SR1.OVR;
         when Packet_Error =>
            return Port.Periph.SR1.PECERR;
         when Timeout =>
            return Port.Periph.SR1.TIMEOUT;
         when SMB_Alert =>
            return Port.Periph.SR1.SMBALERT;
         when Master_Slave_Mode =>
            return Port.Periph.SR2.MSL;
         when Busy =>
            return Port.Periph.SR2.BUSY;
         when Transmitter_Receiver_Mode =>
            return Port.Periph.SR2.TRA;
         when General_Call =>
            return Port.Periph.SR2.GENCALL;
         when SMB_Default =>
            return Port.Periph.SR2.SMBDEFAULT;
         when SMB_Host =>
            return Port.Periph.SR2.SMBHOST;
         when Dual_Flag =>
            return Port.Periph.SR2.DUALF;
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

   procedure Clear_Address_Sent_Status (Port : I2C_Port)
   is
      Unref  : Boolean with Volatile, Unreferenced;
   begin
      --  ADDR is cleared after reading both SR1 and SR2
      Unref := Port.Periph.SR1.ADDR;
      Unref := Port.Periph.SR2.MSL;
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
     (Handle  : in out I2C_Port;
      Flag    :        I2C_Status_Flag;
      F_State :        Boolean;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      while Flag_Status (Handle, Flag) = F_State loop
         if Clock - Start > Milliseconds (Timeout) then
            Handle.State := Ready;
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
     (Handle  : in out I2C_Port;
      Flag    :        I2C_Status_Flag;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      while not Flag_Status (Handle, Flag) loop
         if Handle.Periph.SR1.AF then
            --  Generate STOP
            Handle.Periph.CR1.STOP := True;

            --  Clear the AF flag
            Handle.Periph.SR1.AF := False;
            Handle.State := Ready;
            Status       := HAL.I2C.Err_Error;

            return;
         end if;

         if Clock - Start > Milliseconds (Timeout) then
            Handle.State := Ready;
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
     (Handle  : in out I2C_Port;
      Addr    :        HAL.I2C.I2C_Address;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status)
   is
   begin
      Handle.Periph.CR1.START := True;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if Handle.Config.Addressing_Mode = Addressing_Mode_7bit then
         Handle.Periph.DR.DR := Byte (Addr) and not 2#1#;
      else
         declare
            MSB : constant Byte :=
                    Byte (Shift_Right (Short (Addr) and 16#300#, 7));
            LSB : constant Byte :=
                    Byte (Addr and 16#FF#);
         begin
            --  We need to send 2#1111_MSB0# when MSB are the 3 most
            --  significant bits of the address
            Handle.Periph.DR.DR := MSB or 16#F0#;

            Wait_Master_Flag (Handle, Address_Sent_10bit, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.DR.DR := LSB;
         end;
      end if;

      Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);
   end Master_Request_Write;

   --------------------------
   -- Master_Request_Write --
   --------------------------

   procedure Master_Request_Read
     (Handle  : in out I2C_Port;
      Addr    :        HAL.I2C.I2C_Address;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status)
   is
   begin
      Handle.Periph.CR1.ACK := True;
      Handle.Periph.CR1.START := True;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if Handle.Config.Addressing_Mode = Addressing_Mode_7bit then
         Handle.Periph.DR.DR := Byte (Addr) or 2#1#;
      else
         declare
            MSB : constant Byte :=
                    Byte (Shift_Right (Short (Addr) and 16#300#, 7));
            LSB : constant Byte :=
                    Byte (Addr and 16#FF#);
         begin
            --  We need to write the address bit. So let's start with a
            --  write header
            --  We need to send 2#1111_MSB0# when MSB are the 3 most
            --  significant bits of the address
            Handle.Periph.DR.DR := MSB or 16#F0#;

            Wait_Master_Flag (Handle, Address_Sent_10bit, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.DR.DR := LSB;

            Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Clear_Address_Sent_Status (Handle);

            --  Generate a re-start
            Handle.Periph.CR1.START := True;

            Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            --  resend the MSB with the read bit set.
            Handle.Periph.DR.DR := MSB or 16#F1#;
         end;
      end if;

      Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);
   end Master_Request_Read;

   -----------------------
   -- Mem_Request_Write --
   -----------------------

   procedure Mem_Request_Write
     (Handle        : in out I2C_Port;
      Addr          :        HAL.I2C.I2C_Address;
      Mem_Addr      :        Short;
      Mem_Addr_Size :        HAL.I2C.I2C_Memory_Address_Size;
      Timeout       :        Natural;
      Status        :    out HAL.I2C.I2C_Status)
   is
   begin
      Handle.Periph.CR1.START := True;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      --  Send slave address
      Handle.Periph.DR.DR := Byte (Addr) and not 2#1#;
      Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      Clear_Address_Sent_Status (Handle);

      --  Wait until TXE flag is set
      Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      case Mem_Addr_Size is
         when HAL.I2C.Memory_Size_8b =>
            Handle.Periph.DR.DR := Byte (Mem_Addr);
         when HAL.I2C.Memory_Size_16b =>
            Handle.Periph.DR.DR := Byte (Shift_Right (Mem_Addr, 8));

            Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.DR.DR := Byte (Mem_Addr and 16#FF#);
      end case;
   end Mem_Request_Write;

   ----------------------
   -- Mem_Request_Read --
   ----------------------

   procedure Mem_Request_Read
     (Handle        : in out I2C_Port;
      Addr          :        HAL.I2C.I2C_Address;
      Mem_Addr      :        Short;
      Mem_Addr_Size :        HAL.I2C.I2C_Memory_Address_Size;
      Timeout       :        Natural;
      Status        :    out HAL.I2C.I2C_Status)
   is
   begin
      Handle.Periph.CR1.ACK := True;
      Handle.Periph.CR1.START := True;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      --  Send slave address in write mode
      Handle.Periph.DR.DR := Byte (Addr) and not 16#1#;

      Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      Clear_Address_Sent_Status (Handle);

      --  Wait until TXE flag is set
      Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      case Mem_Addr_Size is
         when HAL.I2C.Memory_Size_8b =>
            Handle.Periph.DR.DR := Byte (Mem_Addr);
         when HAL.I2C.Memory_Size_16b =>
            Handle.Periph.DR.DR := Byte (Shift_Right (Mem_Addr, 8));

            Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.DR.DR := Byte (Mem_Addr and 16#FF#);
      end case;

      --  We now need to reset and send the slave address in read mode
      Handle.Periph.CR1.START := True;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      --  Send slave address in read mode
      Handle.Periph.DR.DR := Byte (Addr) or 16#1#;

      Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);
   end Mem_Request_Read;

   ---------------------
   -- Master_Transmit --
   ---------------------

   overriding
   procedure Master_Transmit
     (Handle  : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000)
   is
      Idx : Natural := Data'First;

   begin
      if Handle.State = Reset then
         Status := HAL.I2C.Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := HAL.I2C.Err_Error;
         return;
      end if;

      Wait_Flag (Handle, Busy, True, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         Status := HAL.I2C.Busy;
         return;
      end if;

      if Handle.State /= Ready then
         Status := HAL.I2C.Busy;
         return;
      end if;

      Handle.State := Master_Busy_Tx;

      Handle.Periph.CR1.POS := False;

      Master_Request_Write (Handle, Addr, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      Clear_Address_Sent_Status (Handle);

      while Idx <= Data'Last loop
         Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

         if Status /= HAL.I2C.Ok then
            return;
         end if;

         Handle.Periph.DR.DR := Data (Idx);
         Idx := Idx + 1;

         if Idx <= Data'Last then
            Wait_Flag (Handle, Byte_Transfer_Finished, True, Timeout, Status);

            if Status = HAL.I2C.Ok then
               Handle.Periph.DR.DR := Data (Idx);
               Idx := Idx + 1;
            end if;
         end if;
      end loop;

      Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      --  Generate STOP
      Handle.Periph.CR1.STOP := True;
      Handle.State := Ready;
   end Master_Transmit;

   --------------------
   -- Master_Receive --
   --------------------

   overriding
   procedure Master_Receive
     (Handle  : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : out HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000)
   is
      Idx : Natural := Data'First;

   begin
      if Handle.State = Reset then
         Status := HAL.I2C.Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := HAL.I2C.Err_Error;
         return;
      end if;

      Wait_Flag (Handle, Busy, True, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         Status := HAL.I2C.Busy;
         return;
      end if;

      if Handle.State /= Ready then
         Status := HAL.I2C.Busy;
         return;
      end if;

      Handle.State := Master_Busy_Rx;

      Handle.Periph.CR1.POS := False;

      Master_Request_Read (Handle, Addr, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if Data'Length = 1 then
         Handle.Periph.CR1.ACK := False;
         Clear_Address_Sent_Status (Handle);
         Handle.Periph.CR1.STOP := True;

      elsif Data'Length = 2 then
         Handle.Periph.CR1.ACK := False;
         Handle.Periph.CR1.POS := True;
         Clear_Address_Sent_Status (Handle);

      else
         --  Automatic acknowledge
         Handle.Periph.CR1.ACK := True;
         Clear_Address_Sent_Status (Handle);
      end if;

      while Idx <= Data'Last loop
         if Idx = Data'Last then
            --  One byte to read
            Wait_Flag
              (Handle,
               Rx_Data_Register_Not_Empty,
               False,
               Timeout,
               Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 1 = Data'Last then
            --  Two bytes to read
            Wait_Flag (Handle,
                       Byte_Transfer_Finished,
                       False,
                       Timeout,
                       Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.CR1.STOP := True;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 2 = Data'Last then
            --  Three bytes to read
            Wait_Flag (Handle,
                       Byte_Transfer_Finished,
                       False,
                       Timeout,
                       Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.CR1.ACK := False;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (Handle,
                       Byte_Transfer_Finished,
                       False,
                       Timeout,
                       Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.CR1.STOP := True;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         else
            --  One byte to read
            Wait_Flag
              (Handle,
               Rx_Data_Register_Not_Empty,
               False,
               Timeout,
               Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (Handle,
                       Byte_Transfer_Finished,
                       False,
                       Timeout,
                       Status);

            if Status = HAL.I2C.Ok then
               Data (Idx) := Handle.Periph.DR.DR;
               Idx := Idx + 1;
            end if;

         end if;
      end loop;

      Handle.State := Ready;
   end Master_Receive;

   ---------------
   -- Mem_Write --
   ---------------

   overriding
   procedure Mem_Write
     (Handle        : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000)
   is
      Idx : Natural := Data'First;

   begin
      if Handle.State = Reset then
         Status := HAL.I2C.Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := HAL.I2C.Err_Error;
         return;
      end if;

      Wait_Flag (Handle, Busy, True, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         Status := HAL.I2C.Busy;
         return;
      end if;

      if Handle.State /= Ready then
         Status := HAL.I2C.Busy;
         return;
      end if;

      Handle.State := Mem_Busy_Tx;
      Handle.Periph.CR1.POS := False;

      Mem_Request_Write
        (Handle, Addr, Mem_Addr, Mem_Addr_Size, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      while Idx <= Data'Last loop
         Wait_Flag (Handle,
                    Tx_Data_Register_Empty,
                    False,
                    Timeout,
                    Status);

         if Status /= HAL.I2C.Ok then
            return;
         end if;

         Handle.Periph.DR.DR := Data (Idx);
         Idx := Idx + 1;

         if Idx <= Data'Last then
            Wait_Flag (Handle,
                       Byte_Transfer_Finished,
                       True,
                       Timeout,
                       Status);

            if Status = HAL.I2C.Ok then
               Handle.Periph.DR.DR := Data (Idx);
               Idx := Idx + 1;
            end if;
         end if;
      end loop;

      Wait_Flag (Handle,
                 Tx_Data_Register_Empty,
                 False,
                 Timeout,
                 Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      --  Generate STOP
      Handle.Periph.CR1.STOP := True;
      Handle.State := Ready;
   end Mem_Write;

   --------------
   -- Mem_Read --
   --------------

   overriding
   procedure Mem_Read
     (Handle        : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : out HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000)
   is
      Idx : Natural := Data'First;

   begin
      if Handle.State = Reset then
         Status := HAL.I2C.Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := HAL.I2C.Err_Error;
         return;
      end if;

      Wait_Flag (Handle, Busy, True, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         Status := HAL.I2C.Busy;
         return;
      end if;

      if Handle.State /= Ready then
         Status := HAL.I2C.Busy;
         return;
      end if;

      Handle.State := Mem_Busy_Rx;

      Handle.Periph.CR1.POS := False;

      Mem_Request_Read
        (Handle, Addr, Mem_Addr, Mem_Addr_Size, Timeout, Status);

      if Status /= HAL.I2C.Ok then
         return;
      end if;

      if Data'Length = 1 then
         Handle.Periph.CR1.ACK := False;
         Clear_Address_Sent_Status (Handle);
         Handle.Periph.CR1.STOP := True;

      elsif Data'Length = 2 then
         Handle.Periph.CR1.ACK := False;
         Handle.Periph.CR1.POS := True;
         Clear_Address_Sent_Status (Handle);

      else
         --  Automatic acknowledge
         Handle.Periph.CR1.ACK := True;
         Clear_Address_Sent_Status (Handle);
      end if;

      while Idx <= Data'Last loop
         if Idx = Data'Last then
            --  One byte to read
            Wait_Flag
              (Handle, Rx_Data_Register_Not_Empty, False, Timeout, Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 1 = Data'Last then
            --  Two bytes to read
            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.CR1.STOP := True;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 2 = Data'Last then
            --  Three bytes to read
            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.CR1.ACK := False;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Handle.Periph.CR1.STOP := True;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         else
            --  One byte to read
            Wait_Flag
              (Handle, Rx_Data_Register_Not_Empty, False, Timeout, Status);
            if Status /= HAL.I2C.Ok then
               return;
            end if;

            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);

            if Status = HAL.I2C.Ok then
               Data (Idx) := Handle.Periph.DR.DR;
               Idx := Idx + 1;
            end if;

         end if;
      end loop;

      Handle.State := Ready;
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

end STM32.I2C;
