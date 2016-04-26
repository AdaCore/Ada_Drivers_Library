------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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
with HAL.I2C;

package STM32.I2C is

   type I2C_Device_Mode is
     (I2C_Mode,
      SMBusDevice_Mode,
      SMBusHost_Mode);

   type I2C_Duty_Cycle is
     (DutyCycle_16_9,
      DutyCycle_2);

   type I2C_Acknowledgement is (Ack_Disable, Ack_Enable);

   type I2C_Direction is (Transmitter, Receiver);

   type I2C_Addressing_Mode is
     (Addressing_Mode_7bit,
      Addressing_Mode_10bit);

   type I2C_Configuration is record
      Clock_Speed              : Word;
      Mode                     : I2C_Device_Mode := I2C_Mode;
      Duty_Cycle               : I2C_Duty_Cycle  := DutyCycle_2;

      Addressing_Mode          : I2C_Addressing_Mode;
      Own_Address              : UInt10;

      --  an I2C general call dispatches the same data to all connected
      --  devices.
      General_Call_Enabled     : Boolean := False;

      --  Clock stretching is a mean for a slave device to slow down the
      --  i2c clock in order to process the communication.
      Clock_Stretching_Enabled : Boolean := True;
   end record;

   type Internal_I2C_Port is private;
   type I2C_Port (Periph : not null access Internal_I2C_Port) is
      limited new HAL.I2C.I2C_Port with private;

   procedure Configure
     (Handle : in out I2C_Port;
      Conf : I2C_Configuration)
     with Post => Port_Enabled (Handle);

   procedure Set_State (This : in out I2C_Port; Enabled : Boolean);
   function Port_Enabled (This : I2C_Port) return Boolean;

   overriding
   procedure Master_Transmit
     (Handle  : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Master_Receive
     (Handle  : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : out HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Mem_Write
     (Handle        : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000);

   overriding
   procedure Mem_Read
     (Handle        : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : out HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000);

   type I2C_Interrupt is
     (Error_Interrupt,
      Event_Interrupt,
      Buffer_Interrupt);

   procedure Enable_Interrupt
     (This   : in out I2C_Port;
      Source : I2C_Interrupt)
     with Post => Enabled (This, Source);

   procedure Disable_Interrupt
     (This   : in out I2C_Port;
      Source : I2C_Interrupt)
     with Post => not Enabled (This, Source);

   function Enabled
     (This   : I2C_Port;
      Source : I2C_Interrupt)
      return Boolean;
private
   type I2C_State is
     (Reset,
      Ready,
      Master_Busy_Tx,
      Master_Busy_Rx,
      Mem_Busy_Tx,
      Mem_Busy_Rx);

   type Internal_I2C_Port is new STM32_SVD.I2C.I2C_Peripheral;

   type I2C_Port (Periph : not null access Internal_I2C_Port) is
      limited new HAL.I2C.I2C_Port with record
         Config   : I2C_Configuration;
         State    : I2C_State := Reset;
      end record;

end STM32.I2C;
