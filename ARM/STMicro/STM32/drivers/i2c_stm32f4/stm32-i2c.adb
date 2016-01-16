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

package body STM32.I2C is

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

--     subtype Clearable_I2C_Status_Flag is
--       I2C_Status_Flag range Bus_Error .. SMB_Alert;

   type I2C_State is
     (Reset,
      Ready,
      Master_Busy_Tx,
      Master_Busy_Rx,
      Mem_Busy_Tx,
      Mem_Busy_Rx);

   type I2C_Port_Handle is record
      Config   : I2C_Configuration;
      State    : I2C_State := Reset;
      Periph   : access I2C_Port := null;
   end record;

   I2C_Handles : array (I2C_Port_Id) of I2C_Port_Handle;

   procedure Configure
     (Handle : in out I2C_Port_Handle;
      Port   : I2C_Port_Id;
      Conf   : I2C_Configuration);

   --  Low level flag handling

   function Flag_Status (Port : I2C_Port; Flag : I2C_Status_Flag) return Boolean;
--     procedure Clear_Flag
--       (Port   : in out I2C_Port;
--        Target : Clearable_I2C_Status_Flag);
   procedure Clear_Address_Sent_Status (Port : in out I2C_Port);
--     procedure Clear_Stop_Detection_Status (Port : in out I2C_Port);

   --  Higher level flag handling

   procedure Wait_Flag
     (Handle  : in out I2C_Port_Handle;
      Flag    :        I2C_Status_Flag;
      F_State :        Boolean;
      Timeout :        Duration;
      Status  :    out I2C_Status);

   procedure Wait_Master_Flag
     (Handle  : in out I2C_Port_Handle;
      Flag    :        I2C_Status_Flag;
      Timeout :        Duration;
      Status  :    out I2C_Status);

   procedure Master_Request_Write
     (Handle  : in out I2C_Port_Handle;
      Addr    :        UInt10;
      Timeout :        Duration;
      Status  :    out I2C_Status);

   procedure Master_Request_Read
     (Handle  : in out I2C_Port_Handle;
      Addr    :        UInt10;
      Timeout :        Duration;
      Status  :    out I2C_Status);

   procedure Mem_Request_Write
     (Handle        : in out I2C_Port_Handle;
      Addr          :        UInt10;
      Mem_Addr      :        Short;
      Mem_Addr_Size :        I2C_Memory_Address_Size;
      Timeout       :        Duration;
      Status        :    out I2C_Status);

   procedure Mem_Request_Read
     (Handle        : in out I2C_Port_Handle;
      Addr          :        UInt10;
      Mem_Addr      :        Short;
      Mem_Addr_Size :        I2C_Memory_Address_Size;
      Timeout       :        Duration;
      Status        :    out I2C_Status);

   procedure Master_Transmit
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Duration);

   procedure Master_Receive
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Duration);

   procedure Mem_Write
     (Handle        : in out I2C_Port_Handle;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Duration);

   procedure Mem_Read
     (Handle        : in out I2C_Port_Handle;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Duration);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Handle : in out I2C_Port_Handle;
      Port   : I2C_Port_Id;
      Conf   : I2C_Configuration)
   is
      CR1        : CR1_Register;
      CCR        : CCR_Register;
      OAR1       : OAR1_Register;
      PCLK1      : constant Word := System_Clock_Frequencies.PCLK1;
      Freq_Range : constant Half_Word := Half_Word (PCLK1 / 1_000_000);

   begin
      if Handle.State /= Reset then
         return;
      end if;

      Handle.Periph := As_Port (Port);
      Handle.Config := Conf;

      Enable_Clock (Handle.Periph.all);

      --  Disable the I2C port
      if Freq_Range < 2 or else Freq_Range > 45 then
         raise Program_Error with
           "PCLK1 too high or too low: expected 2-45 MHz, current" &
           Freq_Range'Img & " MHz";
      end if;

      Set_State (Port, False);

      --  Load CR2 and clear FREQ
      Handle.Periph.CR2 :=
        (LAST    => 0,
         DMAEN   => 0,
         ITBUFEN => 0,
         ITEVTEN => 0,
         ITERREN => 0,
         FREQ    => UInt6 (Freq_Range),
         others  => <>);

      --  Set the port timing
      if Conf.Clock_Speed <= 100_000 then
        --  Mode selection to Standard Mode
         CCR.F_S := 0;
         CCR.CCR := UInt12 (PCLK1 / (Conf.Clock_Speed * 2));

         if CCR.CCR < 4 then
            CCR.CCR := 4;
         end if;

         Handle.Periph.TRISE.TRISE := UInt6 (Freq_Range + 1);

      else
         --  Fast mode
         CCR.F_S := 1;

         if Conf.Duty_Cycle = DutyCycle_2 then
            CCR.CCR := UInt12 (Pclk1 / (Conf.Clock_Speed * 3));
         else
            CCR.CCR := UInt12 (Pclk1 / (Conf.Clock_Speed * 25));
            CCR.DUTY := 1;
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
            CR1.SMBUS := 0;
            CR1.SMBTYPE := 0;
         when SMBusDevice_Mode =>
            CR1.SMBUS := 1;
            CR1.SMBTYPE := 0;
         when SMBusHost_Mode =>
            CR1.SMBUS := 1;
            CR1.SMBTYPE := 1;
      end case;
      CR1.ENGC      := (if Conf.General_Call_Enabled then 1 else 0);
      CR1.NOSTRETCH := (if Conf.Clock_Stretching_Enabled then 0 else 1);
      Handle.Periph.CR1 := CR1;

      --  Address mode (slave mode) configuration
      OAR1.ADDMODE := (if Conf.Addressing_Mode = Addressing_Mode_10bit
                       then 1 else 0);
      case Conf.Addressing_Mode is
         when Addressing_Mode_7bit =>
            OAR1.ADD0  := 0;
            OAR1.ADD7  := UInt7 (Conf.Own_Address / 2);
            OAR1.ADD10 := 0;
         when Addressing_Mode_10bit =>
            OAR1.Add0  := Bit (Conf.Own_Address and 2#1#);
            OAR1.ADD7  := UInt7 ((Conf.Own_Address / 2) and 2#1111111#);
            OAR1.Add10 := UInt2 (Conf.Own_Address / 2 ** 8);
      end case;

      Handle.Periph.OAR1 := OAR1;

      Set_State (Port, True);
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
            return Port.SR1.SB = 1;
         when Address_Sent =>
            return Port.SR1.ADDR = 1;
         when Byte_Transfer_Finished =>
            return Port.SR1.BTF = 1;
         when Address_Sent_10bit =>
            return Port.SR1.ADD10 = 1;
         when Stop_Detection =>
            return Port.SR1.STOPF = 1;
         when Rx_Data_Register_Not_Empty =>
            return Port.SR1.RxNE = 1;
         when Tx_Data_Register_Empty =>
            return Port.SR1.TxE = 1;
         when Bus_Error =>
            return Port.SR1.BERR = 1;
         when Arbitration_Lost =>
            return Port.SR1.ARLO = 1;
         when Ack_Failure =>
            return Port.SR1.AF = 1;
         when UnderOverrun =>
            return Port.SR1.OVR = 1;
         when Packet_Error =>
            return Port.SR1.PECERR = 1;
         when Timeout =>
            return Port.SR1.TIMEOUT = 1;
         when SMB_Alert =>
            return Port.SR1.SMBALERT = 1;
         when Master_Slave_Mode =>
            return Port.SR2.MSL = 1;
         when Busy =>
            return Port.SR2.BUSY = 1;
         when Transmitter_Receiver_Mode =>
            return Port.SR2.TRA = 1;
         when General_Call =>
            return Port.SR2.GENCALL = 1;
         when SMB_Default =>
            return Port.SR2.SMBDEFAULT = 1;
         when SMB_Host =>
            return Port.SR2.SMBHOST = 1;
         when Dual_Flag =>
            return Port.SR2.DUALF = 1;
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

   procedure Clear_Address_Sent_Status (Port : in out I2C_Port)
   is
      Unref  : Bit with Volatile, Unreferenced;
   begin
      --  ADDR is cleared after reading both SR1 and SR2
      Unref := Port.SR1.ADDR;
      Unref := Port.SR2.MSL;
   end Clear_Address_Sent_Status;

--     ---------------------------------
--     -- Clear_Stop_Detection_Status --
--     ---------------------------------
--
--     procedure Clear_Stop_Detection_Status (Port : in out I2C_Port) is
--        Unref  : Bit with Volatile, Unreferenced;
--     begin
--        Unref := Port.SR1.STOPF;
--        Port.CR1.PE := 1;
--     end Clear_Stop_Detection_Status;

   ---------------
   -- Wait_Flag --
   ---------------

   procedure Wait_Flag
     (Handle  : in out I2C_Port_Handle;
      Flag    :        I2C_Status_Flag;
      F_State :        Boolean;
      Timeout :        Duration;
      Status  :    out I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      while Flag_Status (Handle.Periph.all, Flag) = F_State loop
         if To_Duration (Clock - Start) > Timeout then
            Handle.State := Ready;
            Status       := Err_Timeout;

            return;
         end if;
      end loop;

      Status := Ok;
   end Wait_Flag;

   ----------------------
   -- Wait_Master_Flag --
   ----------------------

   procedure Wait_Master_Flag
     (Handle  : in out I2C_Port_Handle;
      Flag    :        I2C_Status_Flag;
      Timeout :        Duration;
      Status  :    out I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      while not Flag_Status (Handle.Periph.all, Flag) loop
         if Handle.Periph.SR1.AF = 1 then
            --  Generate STOP
            Handle.Periph.CR1.STOP := 1;

            --  Clear the AF flag
            Handle.Periph.SR1.Af := 0;
            Handle.State := Ready;
            Status       := Err_Error;

            return;
         end if;

         if To_Duration (Clock - Start) > Timeout then
            Handle.State := Ready;
            Status       := Err_Timeout;

            return;
         end if;
      end loop;

      Status := Ok;
   end Wait_Master_Flag;

   --------------------------
   -- Master_Request_Write --
   --------------------------

   procedure Master_Request_Write
     (Handle  : in out I2C_Port_Handle;
      Addr    :        UInt10;
      Timeout :        Duration;
      Status  :    out I2C_Status)
   is
   begin
      Handle.Periph.CR1.START := 1;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= Ok then
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

            if Status /= OK then
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
     (Handle  : in out I2C_Port_Handle;
      Addr    :        UInt10;
      Timeout :        Duration;
      Status  :    out I2C_Status)
   is
   begin
      Handle.Periph.CR1.ACK := 1;
      Handle.Periph.CR1.START := 1;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= Ok then
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

            if Status /= OK then
               return;
            end if;

            Handle.Periph.DR.DR := LSB;

            Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);

            if Status /= OK then
               return;
            end if;

            Clear_Address_Sent_Status (Handle.Periph.all);

            --  Generate a re-start
            Handle.Periph.CR1.START := 1;

            Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

            if Status /= OK then
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
     (Handle        : in out I2C_Port_Handle;
      Addr          :        UInt10;
      Mem_Addr      :        Short;
      Mem_Addr_Size :        I2C_Memory_Address_Size;
      Timeout       :        Duration;
      Status        :    out I2C_Status)
   is
   begin
      Handle.Periph.CR1.START := 1;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      --  Send slave address
      Handle.Periph.DR.DR := Byte (Addr) and not 2#1#;
      Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      Clear_Address_Sent_Status (Handle.Periph.all);

      --  Wait until TXE flag is set
      Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      case Mem_Addr_Size is
         when Memory_Size_8b =>
            Handle.Periph.DR.DR := Byte (Mem_Addr);
         when Memory_Size_16b =>
            Handle.Periph.DR.DR := Byte (Shift_Right (Mem_Addr, 8));

            Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

            if Status /= Ok then
               return;
            end if;

            Handle.Periph.DR.DR := Byte (Mem_Addr and 16#FF#);
      end case;
   end Mem_Request_Write;

   ----------------------
   -- Mem_Request_Read --
   ----------------------

   procedure Mem_Request_Read
     (Handle        : in out I2C_Port_Handle;
      Addr          :        UInt10;
      Mem_Addr      :        Short;
      Mem_Addr_Size :        I2C_Memory_Address_Size;
      Timeout       :        Duration;
      Status        :    out I2C_Status)
   is
   begin
      Handle.Periph.CR1.ACK := 1;
      Handle.Periph.CR1.START := 1;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      --  Send slave address in write mode
      Handle.Periph.DR.DR := Byte (Addr) and not 16#1#;

      Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      Clear_Address_Sent_Status (Handle.Periph.all);

      --  Wait until TXE flag is set
      Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      case Mem_Addr_Size is
         when Memory_Size_8b =>
            Handle.Periph.DR.DR := Byte (Mem_Addr);
         when Memory_Size_16b =>
            Handle.Periph.DR.DR := Byte (Shift_Right (Mem_Addr, 8));

            Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

            if Status /= Ok then
               return;
            end if;

            Handle.Periph.DR.DR := Byte (Mem_Addr and 16#FF#);
      end case;

      --  We now need to reset and send the slave address in read mode
      Handle.Periph.CR1.START := 1;

      Wait_Flag (Handle, Start_Bit, False, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      --  Send slave address in read mode
      Handle.Periph.DR.DR := Byte (Addr) or 16#1#;

      Wait_Master_Flag (Handle, Address_Sent, Timeout, Status);
   end Mem_Request_Read;

   ---------------------
   -- Master_Transmit --
   ---------------------

   procedure Master_Transmit
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Duration)
   is
      Idx : Natural := Data'First;

   begin
      if Handle.State = Reset then
         Status := Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      Wait_Flag (Handle, Busy, True, Timeout, Status);

      if Status /= Ok then
         Status := Busy;
         return;
      end if;

      if Handle.State /= Ready then
         Status := Busy;
         return;
      end if;

      Handle.State := Master_Busy_Tx;

      Handle.Periph.CR1.POS := 0;

      Master_Request_Write (Handle, Addr, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      Clear_Address_Sent_Status (Handle.Periph.all);

      while Idx <= Data'Last loop
         Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

         if Status /= OK then
            return;
         end if;

         Handle.Periph.DR.DR := Data (Idx);
         Idx := Idx + 1;

         if Idx <= Data'Last then
            Wait_Flag (Handle, Byte_Transfer_Finished, True, Timeout, Status);

            if Status = OK then
               Handle.Periph.DR.DR := Data (Idx);
               Idx := Idx + 1;
            end if;
         end if;
      end loop;

      Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= OK then
         return;
      end if;

      --  Generate STOP
      Handle.Periph.CR1.STOP := 1;
      Handle.State := Ready;
   end Master_Transmit;

   --------------------
   -- Master_Receive --
   --------------------

   procedure Master_Receive
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Duration)
   is
      Idx : Natural := Data'First;

   begin
      if Handle.State = Reset then
         Status := Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      Wait_Flag (Handle, Busy, True, Timeout, Status);

      if Status /= Ok then
         Status := Busy;
         return;
      end if;

      if Handle.State /= Ready then
         Status := Busy;
         return;
      end if;

      Handle.State := Master_Busy_Rx;

      Handle.Periph.CR1.POS := 0;

      Master_Request_Read (Handle, Addr, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      if Data'Length = 1 then
         Handle.Periph.CR1.ACK := 0;
         Clear_Address_Sent_Status (Handle.Periph.all);
         Handle.Periph.CR1.STOP := 1;

      elsif Data'Length = 2 then
         Handle.Periph.CR1.ACK := 0;
         Handle.Periph.CR1.POS := 1;
         Clear_Address_Sent_Status (Handle.Periph.all);

      else
         --  Automatic acknowledge
         Handle.Periph.CR1.ACK := 1;
         Clear_Address_Sent_Status (Handle.Periph.all);
      end if;

      while Idx <= Data'Last loop
         if Idx = Data'Last then
            --  One byte to read
            Wait_Flag
              (Handle, Rx_Data_Register_Not_Empty, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 1 = Data'Last then
            --  Two bytes to read
            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Handle.Periph.CR1.STOP := 1;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 2 = Data'Last then
            --  Three bytes to read
            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Handle.Periph.CR1.ACK := 0;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Handle.Periph.CR1.STOP := 1;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         else
            --  One byte to read
            Wait_Flag
              (Handle, Rx_Data_Register_Not_Empty, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);

            if Status = Ok then
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

   procedure Mem_Write
     (Handle        : in out I2C_Port_Handle;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Duration)
   is
      Idx : Natural := Data'First;

   begin
      if Handle.State = Reset then
         Status := Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      Wait_Flag (Handle, Busy, True, Timeout, Status);

      if Status /= Ok then
         Status := Busy;
         return;
      end if;

      if Handle.State /= Ready then
         Status := Busy;
         return;
      end if;

      Handle.State := Mem_Busy_Tx;
      Handle.Periph.CR1.POS := 0;

      Mem_Request_Write
        (Handle, Addr, Mem_Addr, Mem_Addr_Size, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      while Idx <= Data'Last loop
         Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

         if Status /= OK then
            return;
         end if;

         Handle.Periph.DR.DR := Data (Idx);
         Idx := Idx + 1;

         if Idx <= Data'Last then
            Wait_Flag (Handle, Byte_Transfer_Finished, True, Timeout, Status);

            if Status = OK then
               Handle.Periph.DR.DR := Data (Idx);
               Idx := Idx + 1;
            end if;
         end if;
      end loop;

      Wait_Flag (Handle, Tx_Data_Register_Empty, False, Timeout, Status);

      if Status /= OK then
         return;
      end if;

      --  Generate STOP
      Handle.Periph.CR1.STOP := 1;
      Handle.State := Ready;
   end Mem_Write;

   --------------
   -- Mem_Read --
   --------------

   procedure Mem_Read
     (Handle        : in out I2C_Port_Handle;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Duration)
   is
      Idx : Natural := Data'First;

   begin
      if Handle.State = Reset then
         Status := Err_Error;
         return;

      elsif Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      Wait_Flag (Handle, Busy, True, Timeout, Status);

      if Status /= Ok then
         Status := Busy;
         return;
      end if;

      if Handle.State /= Ready then
         Status := Busy;
         return;
      end if;

      Handle.State := Mem_Busy_Rx;

      Handle.Periph.CR1.POS := 0;

      Mem_Request_Read
        (Handle, Addr, Mem_Addr, Mem_Addr_Size, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      if Data'Length = 1 then
         Handle.Periph.CR1.ACK := 0;
         Clear_Address_Sent_Status (Handle.Periph.all);
         Handle.Periph.CR1.STOP := 1;

      elsif Data'Length = 2 then
         Handle.Periph.CR1.ACK := 0;
         Handle.Periph.CR1.POS := 1;
         Clear_Address_Sent_Status (Handle.Periph.all);

      else
         --  Automatic acknowledge
         Handle.Periph.CR1.ACK := 1;
         Clear_Address_Sent_Status (Handle.Periph.all);
      end if;

      while Idx <= Data'Last loop
         if Idx = Data'Last then
            --  One byte to read
            Wait_Flag
              (Handle, Rx_Data_Register_Not_Empty, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 1 = Data'Last then
            --  Two bytes to read
            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Handle.Periph.CR1.STOP := 1;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         elsif Idx + 2 = Data'Last then
            --  Three bytes to read
            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Handle.Periph.CR1.ACK := 0;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Handle.Periph.CR1.STOP := 1;

            --  read the data from DR
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;
            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

         else
            --  One byte to read
            Wait_Flag
              (Handle, Rx_Data_Register_Not_Empty, False, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            Data (Idx) := Handle.Periph.DR.DR;
            Idx := Idx + 1;

            Wait_Flag (Handle, Byte_Transfer_Finished, False, Timeout, Status);

            if Status = Ok then
               Data (Idx) := Handle.Periph.DR.DR;
               Idx := Idx + 1;
            end if;

         end if;
      end loop;

      Handle.State := Ready;
   end Mem_Read;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Port        : I2C_Port_Id; Conf : I2C_Configuration)
   is
   begin
      Configure (I2C_Handles (Port), Port, Conf);
   end Configure;

   ---------------------
   -- Master_Transmit --
   ---------------------

   procedure Master_Transmit
     (Port    : I2C_Port_Id;
      Addr    : UInt10;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
   is
   begin
      Master_Transmit
        (I2C_Handles (Port),
         Addr,
         Data,
         Status,
         Duration (Timeout) / 1000.0);
   end Master_Transmit;

   --------------------
   -- Master_Receive --
   --------------------

   procedure Master_Receive
     (Port    : I2C_Port_Id;
      Addr    : UInt10;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
   is
   begin
      Master_Receive
        (I2C_Handles (Port),
         Addr,
         Data,
         Status,
         Duration (Timeout) / 1000.0);
   end Master_Receive;

   ---------------
   -- Mem_Write --
   ---------------

   procedure Mem_Write
     (Port          : I2C_Port_Id;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin
      Mem_Write
        (I2C_Handles (Port),
         Addr,
         Mem_Addr,
         Mem_Addr_Size,
         Data, Status,
         Duration (Timeout) / 1000.0);
   end Mem_Write;

   --------------
   -- Mem_Read --
   --------------

   procedure Mem_Read
     (Port          : I2C_Port_Id;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin
      Mem_Read
        (I2C_Handles (Port),
         Addr,
         Mem_Addr,
         Mem_Addr_Size,
         Data, Status,
         Duration (Timeout) / 1000.0);
   end Mem_Read;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State (Port : I2C_Port_Id; Enabled : Boolean)
   is
      Periph : constant access I2C_Port := As_Port (Port);
   begin
      Periph.CR1.PE := (if Enabled then 1 else 0);
   end Set_State;

   ------------------
   -- Port_Enabled --
   ------------------

   function Port_Enabled (Port : I2C_Port_Id) return Boolean
   is
      Periph : constant access I2C_Port := As_Port (Port);
   begin
      return Periph.CR1.PE = 1;
   end Port_Enabled;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (Port   : in out I2C_Port;
      Source : I2C_Interrupt)
   is
   begin
      case Source is
         when Error_Interrupt =>
            Port.CR2.ITERREN := 1;
         when Event_Interrupt =>
            Port.CR2.ITEVTEN := 1;
         when Buffer_Interrupt =>
            Port.CR2.ITBUFEN := 1;
      end case;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (Port   : in out I2C_Port;
      Source : I2C_Interrupt)
   is
   begin
      case Source is
         when Error_Interrupt =>
            Port.CR2.ITERREN := 0;
         when Event_Interrupt =>
            Port.CR2.ITEVTEN := 0;
         when Buffer_Interrupt =>
            Port.CR2.ITBUFEN := 0;
      end case;
   end Disable_Interrupt;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (Port   : in out I2C_Port;
      Source : I2C_Interrupt)
      return Boolean
   is
   begin
      case Source is
         when Error_Interrupt =>
            return Port.CR2.ITERREN = 1;
         when Event_Interrupt =>
            return Port.CR2.ITEVTEN = 1;
         when Buffer_Interrupt =>
            return Port.CR2.ITBUFEN = 1;
      end case;
   end Enabled;

end STM32.I2C;
