
--  This file provides definitions for the STM32F7 (ARM Cortex M7F
--  from ST Microelectronics) Inter-Integrated Circuit (I2C) facility.

private with STM32_SVD.I2C;
with HAL.I2C;

package STM32.I2C is

   type I2C_Direction is (Transmitter, Receiver);

   type I2C_Addressing_Mode is
     (Addressing_Mode_7bit,
      Addressing_Mode_10bit);

   type I2C_Configuration is record
      Clock_Speed              : UInt32;
      Addressing_Mode          : I2C_Addressing_Mode;
      Own_Address              : UInt10;

      --  an I2C general call dispatches the same data to all connected
      --  devices.
      General_Call_Enabled     : Boolean := False;

      --  Clock stretching is a mean for a slave device to slow down the
      --  i2c clock in order to process the communication.
      Clock_Stretching_Enabled : Boolean := True;
   end record;

   subtype I2C_Address is UInt10;

   I2C_Timeout : exception;
   I2C_Error   : exception;

   type Internal_I2C_Port is private;

   type I2C_Port (Periph : not null access Internal_I2C_Port) is
      new HAL.I2C.I2C_Port with private;

   function Port_Enabled (This : I2C_Port) return Boolean
     with Inline;

   procedure Configure
     (This          : in out I2C_Port;
      Configuration : I2C_Configuration)
     with Pre  => not Is_Configured (This),
          Post => Is_Configured (This);

   function Is_Configured (Port : I2C_Port) return Boolean;

   overriding
   procedure Master_Transmit
     (This    : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000)
     with Pre => Is_Configured (This);

   overriding
   procedure Master_Receive
     (This    : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : out HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000)
     with Pre => Is_Configured (This);

   overriding
   procedure Mem_Write
     (This          : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000)
     with Pre => Is_Configured (This);

   overriding
   procedure Mem_Read
     (This          : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : out HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000)
     with Pre => Is_Configured (This);

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
      new HAL.I2C.I2C_Port with record
         Config   : I2C_Configuration;
         State    : I2C_State := Reset;
      end record;

end STM32.I2C;
