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

with STM32.RCC;     use STM32.RCC;

with STM32_SVD.I2C; use STM32_SVD.I2C;

package body STM32.I2C is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Port        : in out I2C_Port;
      Clock_Speed : Word;
      Mode        : I2C_Device_Mode;
      Duty_Cycle  : I2C_Duty_Cycle;
      Own_Address : Half_Word;
      Ack         : I2C_Acknowledgement;
      Ack_Address : I2C_Acknowledge_Address)
   is
      CR1        : CR1_Register;
      CCR        : CCR_Register;
      OAR1       : OAR1_Register;
      PCLK1      : constant Word := System_Clock_Frequencies.PCLK1;
      Freq_Range : constant Half_Word := Half_Word (PCLK1 / 1_000_000);
   begin
      --  Load CR2 and clear FREQ
      Port.CR2 :=
        (LAST    => 0,
         DMAEN   => 0,
         ITBUFEN => 0,
         ITEVTEN => 0,
         ITERREN => 0,
         FREQ    => UInt6 (Freq_Range),
         others  => <>);

      Set_State (Port, Disabled);

      if Clock_Speed <= 100_000 then
        --  Mode selection to Standard Mode
         CCR.F_S := 0;
         CCR.CCR := UInt12 (PCLK1 / (Clock_Speed * 2));

         if CCR.CCR < 4 then
            CCR.CCR := 4;
         end if;

         Port.TRISE.TRISE := UInt6 (Freq_Range + 1);

      else
         --  Fast mode
         CCR.F_S := 1;

         if Duty_Cycle = DutyCycle_2 then
            CCR.CCR := UInt12 (Pclk1 / (Clock_Speed * 3));
         else
            CCR.CCR := UInt12 (Pclk1 / (Clock_Speed * 25));
            CCR.DUTY := 1;
         end if;

         if CCR.CCR = 0 then
            CCR.CCR := 1;
         end if;

         CCR.CCR := CCR.CCR or 16#80#;

         Port.TRISE.TRISE := UInt6 ((Word (Freq_Range) * 300) / 1000 + 1);
      end if;

      Port.CCR := CCR;

      Set_State (Port, Enabled);

      CR1 := Port.CR1;
      case Mode is
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
      CR1.ACK := I2C_Acknowledgement'Enum_Rep (Ack);
      Port.CR1 := CR1;

      OAR1.ADDMODE := (if Ack_Address = AcknowledgedAddress_10bit
                       then 1 else 0);
      case Ack_Address is
         when AcknowledgedAddress_7bit =>
            OAR1.ADD0  := 0;
            OAR1.ADD7  := UInt7 (Own_Address / 2);
            OAR1.ADD10 := 0;
         when AcknowledgedAddress_10bit =>
            OAR1.Add0  := Bit (Own_Address and 2#1#);
            OAR1.ADD7  := UInt7 ((Own_Address / 2) and 2#1111111#);
            OAR1.Add10 := UInt2 (Own_Address / 2 ** 8);
      end case;

      Port.OAR1 := OAR1;
   end Configure;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State (Port : in out I2C_Port; State : I2C_State) is
   begin
      Port.CR1.PE := (if State = Enabled then 1 else 0);
   end Set_State;

   ------------------
   -- Port_Enabled --
   ------------------

   function Port_Enabled (Port : I2C_Port) return Boolean is
   begin
      return Port.CR1.PE = 1;
   end Port_Enabled;

   --------------------
   -- Generate_Start --
   --------------------

   procedure Generate_Start (Port : in out I2C_Port; State : I2C_State) is
   begin
      Port.CR1.START := (if State = Enabled then 1 else 0);
   end Generate_Start;

   -------------------
   -- Generate_Stop --
   -------------------

   procedure Generate_Stop (Port : in out I2C_Port; State : I2C_State) is
   begin
      Port.CR1.STOP := (if State = Enabled then 1 else 0);
   end Generate_Stop;

   --------------------
   -- Send_7Bit_Addr --
   --------------------

   procedure Send_7Bit_Address
     (Port      : in out I2C_Port;
      Address   : Byte;
      Direction : I2C_Direction)
   is
      Destination : Byte;
   begin
      if Direction = Receiver then
         Destination := Address or 2#1#;
      else
         Destination := Address and (not 2#1#);
      end if;

      Port.DR.DR := Destination;
   end Send_7Bit_Address;

   --------------
   -- Get_Flag --
   --------------

   function Status (Port : I2C_Port; Flag : I2C_Status_Flag) return Boolean is
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
   end Status;

   ----------------
   -- Clear_Flag --
   ----------------

   procedure Clear_Status
     (Port   : in out I2C_Port;
      Target : Clearable_I2C_Status_Flag)
   is
      Unref  : Bit with Unreferenced;
   begin
      case Target is
         when Bus_Error =>
            Port.SR1.BERR := 0;
         when Arbitration_Lost =>
            Port.SR1.ARLO := 0;
         when Ack_Failure =>
            Port.SR1.AF := 0;
         when UnderOverrun =>
            Port.SR1.OVR := 0;
         when Packet_Error =>
            Port.SR1.PECERR := 0;
         when Timeout =>
            Port.SR1.TIMEOUT := 0;
         when SMB_Alert =>
            Port.SR1.SMBALERT := 0;
      end case;
   end Clear_Status;

   -------------------------------
   -- Clear_Address_Sent_Status --
   -------------------------------

   procedure Clear_Address_Sent_Status (Port : in out I2C_Port)
   is
      Unref  : Bit with Volatile, Unreferenced;
   begin
      --  To clear the ADDR flag we have to read SR2 after reading SR1.
      --  However, per the RM, section 27.6.7, page 850, we should only read
      --  SR2 if the Address_Sent flag is set in SR1, or if the Stop_Detection
      --  flag is cleared, because the Address_Sent flag could be set in
      --  the middle of reading SR1 and SR2 but will be cleared by the
      --  read sequence nonetheless.
      if Status (Port, Start_Bit) or else Status (Port, Stop_Detection) then
         Unref := Port.SR2.MSL;
      end if;
   end Clear_Address_Sent_Status;

   ---------------------------------
   -- Clear_Stop_Detection_Status --
   ---------------------------------

   procedure Clear_Stop_Detection_Status (Port : in out I2C_Port) is
      Unref  : Bit with Volatile, Unreferenced;
   begin
      Unref := Port.SR1.STOPF;
      Port.CR1.PE := 1;
   end Clear_Stop_Detection_Status;

   -------------------
   -- Wait_For_Flag --
   -------------------

   procedure Wait_For_State
     (Port     : I2C_Port;
      Queried  : I2C_Status_Flag;
      State    : I2C_State;
      Time_Out : Natural := 1_000)
   is
      Expected : constant Boolean := State = Enabled;
      Deadline : constant Time := Clock + Milliseconds (Time_Out);
   begin
      while Status (Port, Queried) /= Expected loop
         if Clock >= Deadline then
            raise I2C_Timeout;
         end if;
      end loop;
   end Wait_For_State;

   ---------------
   -- Send_Data --
   ---------------

   procedure Send_Data (Port : in out I2C_Port; Data : Byte) is
   begin
      Port.DR.DR := Data;
   end Send_Data;

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (Port : I2C_Port) return Byte is
   begin
      return Port.DR.DR;
   end Read_Data;

   --------------------
   -- Set_Ack_Config --
   --------------------

   procedure Set_Ack_Config (Port : in out I2C_Port; State : I2C_State) is
   begin
      Port.CR1.ACK := (if State = Enabled then 1 else 0);
   end Set_Ack_Config;

   ---------------------
   -- Set_Nack_Config --
   ---------------------

   procedure Set_Nack_Config
     (Port : in out I2C_Port;
      Pos  : I2C_Nack_Position)
   is
   begin
      Port.CR1.POS := (if Pos = Next then 1 else 0);
   end Set_Nack_Config;

   -----------
   -- Start --
   -----------

   procedure Start
     (Port      : in out I2C_Port;
      Address   : Byte;
      Direction : I2C_Direction)
   is
   begin
      Generate_Start (Port, Enabled);
      Wait_For_State (Port, Start_Bit, Enabled);

      Set_Ack_Config (Port, Enabled);

      Send_7Bit_Address (Port, Address, Direction);

      while not Status (Port, Address_Sent) loop
         if Status (Port, Ack_Failure) then
            raise Program_Error;
         end if;
      end loop;
      Clear_Address_Sent_Status (Port);
   end Start;

   --------------
   -- Read_Ack --
   --------------

   function Read_Ack (Port : in out I2C_Port) return Byte is
   begin
      Set_Ack_Config (Port, Enabled);
      Wait_For_State (Port, Rx_Data_Register_Not_Empty, Enabled);
      return Read_Data (Port);
   end Read_Ack;

   ---------------
   -- Read_Nack --
   ---------------

   function Read_Nack (Port : in out I2C_Port) return Byte is
   begin
      Set_Ack_Config (Port, Disabled);
      Generate_Stop (Port, Enabled);
      Wait_For_State (Port, Rx_Data_Register_Not_Empty, Enabled);
      return Read_Data (Port);
   end Read_Nack;

   -----------
   -- Write --
   -----------

   procedure Write (Port : in out I2C_Port; Data : Byte) is
   begin
      Wait_For_State (Port, Tx_Data_Register_Empty, Enabled);
      Send_Data (Port, Data);

      while
        not Status (Port, Tx_Data_Register_Empty) or else
        not Status (Port, Byte_Transfer_Finished)
      loop
         null;
      end loop;
   end Write;

   ----------
   -- Stop --
   ----------

   procedure Stop (Port : in out I2C_Port) is
   begin
      Generate_Stop (Port, Enabled);
   end Stop;

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
