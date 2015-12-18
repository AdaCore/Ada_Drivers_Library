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
--   @file    stm32f4xx_hal_i2c.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of I2C HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides definitions for the STM32F4 (ARM Cortex M4F
--  from ST Microelectronics) Inter-Integrated Circuit (I2C) facility.

private with STM32_SVD.I2C;

package STM32.I2C is

   type I2C_Port is limited private;

   type I2C_Device_Mode is
     (I2C_Mode,
      SMBusDevice_Mode,
      SMBusHost_Mode);

   type I2C_Duty_Cycle is
     (DutyCycle_16_9,
      DutyCycle_2);

   type I2C_Acknowledgement is (Ack_Disable, Ack_Enable);

   type I2C_Direction is (Transmitter, Receiver);

   type I2C_Acknowledge_Address is
     (AcknowledgedAddress_7bit,
      AcknowledgedAddress_10bit);

   procedure Configure
     (Port        : in out I2C_Port;
      Clock_Speed : Word;
      Mode        : I2C_Device_Mode;
      Duty_Cycle  : I2C_Duty_Cycle;
      Own_Address : Half_Word;
      Ack         : I2C_Acknowledgement;
      Ack_Address : I2C_Acknowledge_Address)
     with Post => Port_Enabled (Port);

   type I2C_State is (Enabled, Disabled);

   procedure Set_State (Port : in out I2C_Port; State : I2C_State);

   function Port_Enabled (Port : I2C_Port) return Boolean;

   procedure Generate_Start (Port : in out I2C_Port; State : I2C_State);

   procedure Generate_Stop (Port : in out I2C_Port; State : I2C_State);

   procedure Send_7Bit_Address
     (Port      : in out I2C_Port;
      Address   : Byte;
      Direction : I2C_Direction);

   procedure Send_Data (Port : in out I2C_Port; Data : Byte);

   function Read_Data (Port : I2C_Port) return Byte;

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

   function Status (Port : I2C_Port; Flag : I2C_Status_Flag) return Boolean;

   subtype Clearable_I2C_Status_Flag is
     I2C_Status_Flag range Bus_Error .. SMB_Alert;

   procedure Clear_Status
     (Port   : in out I2C_Port;
      Target : Clearable_I2C_Status_Flag);

   procedure Clear_Address_Sent_Status (Port : in out I2C_Port);

   procedure Clear_Stop_Detection_Status (Port : in out I2C_Port);

   procedure Wait_For_State
     (Port     : I2C_Port;
      Queried  : I2C_Status_Flag;
      State    : I2C_State;
      Time_Out : Natural := 1_000);  -- milliseconds

   I2C_Timeout : exception;
   --  Raised by Wait_For_Flag

   procedure Set_Ack_Config (Port : in out I2C_Port; State : I2C_State);

   type I2C_Nack_Position is (Next, Current);

   procedure Set_Nack_Config (Port : in out I2C_Port; Pos : I2C_Nack_Position);

   procedure Start
     (Port      : in out I2C_Port;
      Address   : Byte;
      Direction : I2C_Direction);

   function Read_Ack (Port : in out I2C_Port) return Byte;

   function Read_Nack (Port : in out I2C_Port) return Byte;

   procedure Write (Port : in out I2C_Port; Data : Byte);

   procedure Stop (Port : in out I2C_Port);

   type I2C_Interrupt is
     (Error_Interrupt,
      Event_Interrupt,
      Buffer_Interrupt);

   procedure Enable_Interrupt
     (Port   : in out I2C_Port;
      Source : I2C_Interrupt)
     with Post => Enabled (Port, Source);

   procedure Disable_Interrupt
     (Port   : in out I2C_Port;
      Source : I2C_Interrupt)
     with Post => not Enabled (Port, Source);

   function Enabled
     (Port   : in out I2C_Port;
      Source : I2C_Interrupt)
     return Boolean;

private

   type I2C_Port is new STM32_SVD.I2C.I2C_Peripheral;

end STM32.I2C;
