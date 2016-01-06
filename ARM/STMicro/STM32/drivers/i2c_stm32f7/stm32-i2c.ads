------------------------------------------------------------------------------
--                                                                          --
--               Standard Peripheral Library for STM32 Targets              --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This file provides definitions for the STM32F7 (ARM Cortex M7F
--  from ST Microelectronics) Inter-Integrated Circuit (I2C) facility.

with STM32.Device; use STM32.Device;

package STM32.I2C is

   type I2C_Status is
     (Ok,
      Err_Error,
      Err_Timeout,
      Busy);

   type I2C_Direction is (Transmitter, Receiver);

   type I2C_Addressing_Mode is
     (Addressing_Mode_7bit,
      Addressing_Mode_10bit);

   type I2C_Memory_Address_Size is
     (Memory_Size_8b,
      Memory_Size_16b);

   type I2C_Configuration is record
      Clock_Speed              : Word;
      Addressing_Mode          : I2C_Addressing_Mode;
      Own_Address              : STM32_SVD.UInt10;

      --  an I2C general call dispatches the same data to all connected
      --  devices.
      General_Call_Enabled     : Boolean := False;

      --  Clock stretching is a mean for a slave device to slow down the
      --  i2c clock in order to process the communication.
      Clock_Stretching_Enabled : Boolean := True;
   end record;

   type I2C_Data is array (Natural range <>) of Byte;

   I2C_Timeout : exception;
   I2C_Error   : exception;

   function Port_Enabled (Port : I2C_Port_Id) return Boolean
     with Inline;

   procedure Configure (Port : I2C_Port_Id; Conf : I2C_Configuration)
     with Post => Port_Enabled (Port);

   procedure Master_Transmit
     (Port    : I2C_Port_Id;
      Addr    : UInt10;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000);

   procedure Master_Receive
     (Port    : I2C_Port_Id;
      Addr    : UInt10;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000);

   procedure Mem_Write
     (Port          : I2C_Port_Id;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000);

   procedure Mem_Read
     (Port          : I2C_Port_Id;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000);

end STM32.I2C;
