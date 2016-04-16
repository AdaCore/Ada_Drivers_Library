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

with Ada.Real_Time; use Ada.Real_Time;

with STM32_SVD.I2C; use STM32_SVD.I2C;
with STM32_SVD.RCC; use STM32_SVD.RCC;

with STM32.RCC;

with HAL.I2C; use HAL.I2C;

package body STM32.I2C is

   type I2C_Transfer_Mode is
     (Reload_Mode,   --  Enable reload mode
      Autoend_Mode,  -- Enable automatic end mode
      Softend_Mode); --  Enable software end mode

   type I2C_Request is
     (No_Start_Stop,         --  Don't generate start or stop
      Generate_Stop,         --  Generate a stop condition
      Generate_Start_Read,   --  Generate a start read request
      Generate_Start_Write); --  Generate a start write request

   procedure Config_Transfer
     (Port    : in out I2C_Port;
      Addr    : I2C_Address;
      Size    : Byte;
      Mode    : I2C_Transfer_Mode;
      Request : I2C_Request);

   procedure Reset_Config (Port : in out I2C_Port);

   procedure Check_Nack
     (Port    : in out I2C_Port;
      Timeout : Natural;
      Status  : out HAL.I2C.I2C_Status);

   procedure Wait_Tx_Interrupt_Status
     (Port    : in out I2C_Port;
      Timeout : Natural;
      Status  : out I2C_Status);

   procedure Wait_Transfer_Complete_Reset_Flag
     (Port    : in out I2C_Port;
      Timeout : Natural;
      Status  : out I2C_Status);

   procedure Wait_Stop_Flag
     (Port    : in out I2C_Port;
      Timeout : Natural;
      Status  : out I2C_Status);

   ------------------
   -- Port_Enabled --
   ------------------

   function Port_Enabled (Port : I2C_Port) return Boolean
   is
   begin
      return Port.Periph.CR1.PE;
   end Port_Enabled;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Port          : in out I2C_Port;
      Configuration : I2C_Configuration)
   is
   begin
      if Port.State /= Reset then
         return;
      end if;

      Port.Config := Configuration;

      STM32.RCC.Enable_Clock (STM32_SVD.I2C.I2C_Peripheral (Port.Periph.all));
      STM32.RCC.Reset (STM32_SVD.I2C.I2C_Peripheral (Port.Periph.all));

      --  Disable the I2C port
      Port.Periph.CR1.PE := False;

      --  Reset the timing register to 100_000 Hz
      Port.Periph.TIMINGR :=
        (SCLL   => 50,
         SCLH   => 39,
         SDADEL => 1,
         SCLDEL => 9,
         PRESC  => 4,
         others => <>);

      --  I2C Own Address Register configuration
      if Configuration.Own_Address /= 0 then
         Port.Periph.OAR1 :=
           (OA1     => Configuration.Own_Address,
            OA1EN   => True,
            OA1MODE => Configuration.Addressing_Mode = Addressing_Mode_10bit,
            others  => <>);
      end if;

      --  CR2 configuration
      --  Enable AUTOEND by default, set NACK (should be disabled only in
      --  slave mode
      Port.Periph.CR2 :=
        (AUTOEND => True,
         NACK    => True,
         ADD10   => Configuration.Addressing_Mode = Addressing_Mode_10bit,
         others  => <>);

      --  OAR2 configuration
      --  ??? Add support for dual addressing
      Port.Periph.OAR2 := (others => <>);

      --  CR1 configuration
      Port.Periph.CR1 :=
        (GCEN      => Configuration.General_Call_Enabled,
         NOSTRETCH => Configuration.Clock_Stretching_Enabled,
         others    => <>);

      Port.State := Ready;
      --  Enable the port
      Port.Periph.CR1.PE := True;
   end Configure;

   -------------------
   -- Is_Configured --
      -------------------

   function Is_Configured (Port : I2C_Port) return Boolean
   is
   begin
      return Port.State /= Reset;
   end Is_Configured;

   ---------------------
   -- Config_Transfer --
   ---------------------

   procedure Config_Transfer
     (Port    : in out I2C_Port;
      Addr    : I2C_Address;
      Size    : Byte;
      Mode    : I2C_Transfer_Mode;
      Request : I2C_Request)
   is
      CR2 : CR2_Register := Port.Periph.CR2;
   begin
      CR2.SADD := UInt10 (Addr);
      CR2.NBYTES := Size;
      CR2.RELOAD := Mode = Reload_Mode;
      CR2.AUTOEND := Mode = Autoend_Mode;

      CR2.RD_WRN := False;
      CR2.START  := False;
      CR2.STOP   := False;

      case Request is
         when No_Start_Stop =>
            null;

         when Generate_Stop =>
            CR2.STOP := True;

         when Generate_Start_Read =>
            CR2.RD_WRN := True;
            CR2.START  := True;

         when Generate_Start_Write =>
            CR2.START := True;
      end case;

      Port.Periph.CR2 := CR2;
   end Config_Transfer;

   ------------------
   -- Reset_Config --
   ------------------

   procedure Reset_Config (Port  : in out I2C_Port)
   is
      CR2 : CR2_Register := Port.Periph.CR2;
   begin
      CR2.SADD    := 0;
      CR2.HEAD10R := False;
      CR2.NBYTES  := 0;
      CR2.RELOAD  := False;
      CR2.RD_WRN  := False;
      Port.Periph.CR2 := CR2;
   end Reset_Config;

   ----------------
   -- Check_Nack --
   ----------------

   procedure Check_Nack
     (Port    : in out I2C_Port;
      Timeout : Natural;
      Status  : out I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      if Port.Periph.ISR.NACKF then
         if Port.State = Master_Busy_Tx
           or else Port.State = Mem_Busy_Tx
           or else Port.State = Mem_Busy_Rx
         then
            --  We generate a STOP condition if SOFTEND mode is enabled
            if not Port.Periph.CR2.AUTOEND then
               Port.Periph.CR2.STOP := True;
            end if;
         end if;

         while not Port.Periph.ISR.STOPF loop
            if Timeout > 0
              and then Start + Milliseconds (Timeout) < Clock
            then
               Port.State := Ready;
               Status       := Err_Timeout;
               return;
            end if;
         end loop;

         --  Clear the MACL amd STOP flags
         Port.Periph.ICR.NACKCF := True;
         Port.Periph.ICR.STOPCF := True;

         --  Clear CR2
         Reset_Config (Port);

         Port.State := Ready;
         Status       := Err_Error;

      else
         Status := Ok;
      end if;
   end Check_Nack;

   ------------------------------
   -- Wait_Tx_Interrupt_Status --
   ------------------------------

   procedure Wait_Tx_Interrupt_Status
     (Port    : in out I2C_Port;
      Timeout : Natural;
      Status  : out I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      while not Port.Periph.ISR.TXIS loop
         Check_Nack (Port, Timeout, Status);

         if Status /= Ok then
            Port.State := Ready;
            Status       := Err_Error;

            return;
         end if;

         if Timeout > 0
           and then Start + Milliseconds (Timeout) < Clock
         then
            Reset_Config (Port);
            Port.State := Ready;
            Status       := Err_Timeout;

            return;
         end if;
      end loop;

      Status := Ok;
   end Wait_Tx_Interrupt_Status;

   ---------------------------------------
   -- Wait_Transfer_Complete_Reset_Flag --
   ---------------------------------------

   procedure Wait_Transfer_Complete_Reset_Flag
     (Port    : in out I2C_Port;
      Timeout : Natural;
      Status  : out I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      while not Port.Periph.ISR.TCR loop
         if Timeout > 0
           and then Start + Milliseconds (Timeout) < Clock
         then
            Reset_Config (Port);
            Status       := Err_Timeout;
            Port.State := Ready;

            return;
         end if;
      end loop;

      Status := Ok;
   end Wait_Transfer_Complete_Reset_Flag;

   --------------------
   -- Wait_Stop_Flag --
   --------------------

   procedure Wait_Stop_Flag
     (Port    : in out I2C_Port;
      Timeout : Natural;
      Status  : out I2C_Status)
   is
      Start : constant Time := Clock;
   begin
      while not Port.Periph.ISR.STOPF loop
         Check_Nack (Port, Timeout, Status);

         if Status /= Ok then
            Port.State := Ready;
            Status       := Err_Error;

            return;
         end if;

         if Timeout > 0
           and then Start + Milliseconds (Timeout) < Clock
         then
            Reset_Config (Port);
            Status       := Err_Timeout;
            Port.State := Ready;

            return;
         end if;
      end loop;

      --  Clear the stop flag
      Port.Periph.ICR.STOPCF := True;

      Status := Ok;
   end Wait_Stop_Flag;

   ---------------------
   -- Master_Transmit --
   ---------------------

   overriding
   procedure Master_Transmit
     (Port    : in out I2C_Port;
      Addr    : I2C_Address;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
   is
      Size_Temp   : Natural := 0;
      Transmitted : Natural := 0;

   begin
      if Port.Periph.ISR.BUSY then
         Status := Busy;
         return;
      end if;

      if Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      if Port.State /= Ready then
         Status := Busy;
         return;
      end if;

      Port.State := Master_Busy_Tx;

      --  Initiate the transfer
      if Data'Length > 255 then
         Config_Transfer
           (Port, Addr, 255, Reload_Mode, Generate_Start_Write);
         Size_Temp := 255;
      else
         Config_Transfer
           (Port, Addr, Data'Length, Autoend_Mode, Generate_Start_Write);
         Size_Temp := Data'Length;
      end if;

      --  Transfer the data
      while Transmitted <= Data'Length loop
         Wait_Tx_Interrupt_Status (Port, Timeout, Status);

         if Status /= Ok then
            return;
         end if;

         Port.Periph.TXDR.TXDATA := Data (Data'First + Transmitted);
         Transmitted := Transmitted + 1;

         if Transmitted = Size_Temp
           and then Transmitted < Data'Length
         then
            --  Wait for the Transfer complete reload flag
            Wait_Transfer_Complete_Reset_Flag (Port, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            if Data'Length - Transmitted > 255 then
               Config_Transfer
                 (Port, Addr, 255, Reload_Mode, No_Start_Stop);
               Size_Temp := 255;
            else
               Config_Transfer
                 (Port, Addr, Byte (Data'Length - Transmitted), Autoend_Mode,
                  No_Start_Stop);
               Size_Temp := Data'Length - Transmitted;
            end if;
         end if;
      end loop;

      Wait_Stop_Flag (Port, Timeout, Status);
      if Status /= Ok then
         return;
      end if;

      --  Reset CR2
      Reset_Config (Port);
      Port.State := Ready;
      Status       := Ok;
   end Master_Transmit;

   --------------------
   -- Master_Receive --
   --------------------

   overriding
   procedure Master_Receive
     (Port    : in out I2C_Port;
      Addr    : I2C_Address;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
   is
      Size_Temp   : Natural := 0;
      Transmitted : Natural := 0;
   begin
      if Port.Periph.ISR.BUSY then
         Status := Busy;
         return;
      end if;

      if Port.State /= Ready then
         Status := Busy;
         return;
      end if;

      Port.State := Master_Busy_Rx;

      if Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      --  Initiate the transfer
      if Data'Length > 255 then
         Config_Transfer
           (Port, Addr, 255, Reload_Mode, Generate_Start_Read);
         Size_Temp := 255;
      else
         Config_Transfer
           (Port, Addr, Data'Length, Autoend_Mode, Generate_Start_Read);
         Size_Temp := Data'Length;
      end if;

      --  Transfer the data
      while Transmitted < Data'Length loop
         while not Port.Periph.ISR.RXNE loop
            null;
         end loop;

         Data (Data'First + Transmitted) := Port.Periph.RXDR.RXDATA;
         Transmitted := Transmitted + 1;
         Size_Temp   := Size_Temp - 1;

         if Size_Temp = 0
           and then Transmitted < Data'Length
         then
            --  Wait for the Transfer complete reload flag
            while Port.Periph.ISR.TCR loop
               null;
            end loop;

            if Data'Length - Transmitted > 255 then
               Config_Transfer
                 (Port, Addr, 255, Reload_Mode, No_Start_Stop);
               Size_Temp := 255;
            else
               Config_Transfer
                 (Port, Addr, Byte (Data'Length - Transmitted), Autoend_Mode,
                  No_Start_Stop);
               Size_Temp := Data'Length - Transmitted;
            end if;
         end if;
      end loop;

      Wait_Stop_Flag (Port, Timeout, Status);
      if Status /= Ok then
         return;
      end if;

      --  Reset CR2
      Reset_Config (Port);
      Port.State := Ready;
      Status       := Ok;
   end Master_Receive;

   ---------------
   -- Mem_Write --
   ---------------

   overriding
   procedure Mem_Write
     (Port          : in out I2C_Port;
      Addr          : I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
   is
      Size_Temp   : Natural := 0;
      Transmitted : Natural := 0;

   begin
      if Port.Periph.ISR.BUSY then
         Status := Busy;
         return;
      end if;

      if Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      if Port.State /= Ready then
         Status := Busy;
         return;
      end if;

      Port.State := Mem_Busy_Tx;

      --  Configure the memory transfer
      Config_Transfer
        (Port,
         Addr,
         (case Mem_Addr_Size is
             when Memory_Size_8b  => 1,
             when Memory_Size_16b => 2),
         Reload_Mode,
         Generate_Start_Write);

      Wait_Tx_Interrupt_Status (Port, Timeout, Status);

      if Status /= Ok then
         Port.State := Ready;

         return;
      end if;

      case Mem_Addr_Size is
         when Memory_Size_8b =>
            Port.Periph.TXDR.TXDATA := Byte (Mem_Addr);

         when Memory_Size_16b =>
            declare
               MSB : constant Byte := Byte (Shift_Right (Mem_Addr, 8));
               LSB : constant Byte := Byte (Mem_Addr and 16#FF#);
            begin
               Port.Periph.TXDR.TXDATA := MSB;

               Wait_Tx_Interrupt_Status (Port, Timeout, Status);
               if Status /= Ok then
                  return;
               end if;

               Port.Periph.TXDR.TXDATA := LSB;
            end;
      end case;

      Wait_Transfer_Complete_Reset_Flag (Port, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      --  Initiate the transfer
      if Data'Length > 255 then
         Config_Transfer
           (Port, Addr, 255, Reload_Mode, No_Start_Stop);
         Size_Temp := 255;
      else
         Config_Transfer
           (Port, Addr, Data'Length, Autoend_Mode, No_Start_Stop);
         Size_Temp := Data'Length;
      end if;

      --  Transfer the data
      while Transmitted < Data'Length loop
         Wait_Tx_Interrupt_Status (Port, Timeout, Status);

         if Status /= Ok then
            return;
         end if;

         Port.Periph.TXDR.TXDATA := Data (Data'First + Transmitted);
         Transmitted := Transmitted + 1;

         if Transmitted = Size_Temp
           and then Transmitted < Data'Length
         then
            --  Wait for the Transfer complete reload flag
            Wait_Transfer_Complete_Reset_Flag (Port, Timeout, Status);

            if Status /= Ok then
               return;
            end if;

            if Data'Length - Transmitted > 255 then
               Config_Transfer
                 (Port, Addr, 255, Reload_Mode, No_Start_Stop);
               Size_Temp := 255;
            else
               Config_Transfer
                 (Port, Addr,
                  Byte (Data'Length - Transmitted),
                  Autoend_Mode,
                  No_Start_Stop);
               Size_Temp := Data'Length - Transmitted;
            end if;
         end if;
      end loop;

      Wait_Stop_Flag (Port, Timeout, Status);
      if Status /= Ok then
         return;
      end if;

      --  Reset CR2
      Reset_Config (Port);
      Port.State := Ready;
      Status       := Ok;
   end Mem_Write;

   --------------
   -- Mem_Read --
   --------------

   overriding
   procedure Mem_Read
     (Port          : in out I2C_Port;
      Addr          : I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
   is
      Size_Temp   : Natural := 0;
      Transmitted : Natural := 0;
   begin
      if Port.Periph.ISR.BUSY then
         Status := Busy;
         return;
      end if;

      if Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      if Port.State /= Ready then
         Status := Busy;
         return;
      end if;

      Port.State := Mem_Busy_Rx;

      --  Configure the memory transfer
      Config_Transfer
        (Port,
         Addr,
         (case Mem_Addr_Size is
             when Memory_Size_8b  => 1,
             when Memory_Size_16b => 2),
         Softend_Mode,
         Generate_Start_Write);

      Wait_Tx_Interrupt_Status (Port, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      case Mem_Addr_Size is
         when Memory_Size_8b =>
            Port.Periph.TXDR.TXDATA := Byte (Mem_Addr);

         when Memory_Size_16b =>
            declare
               MSB : constant Byte := Byte (Shift_Right (Mem_Addr, 8));
               LSB : constant Byte := Byte (Mem_Addr and 16#FF#);
            begin
               Port.Periph.TXDR.TXDATA := MSB;

               Wait_Tx_Interrupt_Status (Port, Timeout, Status);

               if Status /= Ok then
                  return;
               end if;

               Port.Periph.TXDR.TXDATA := LSB;
            end;
      end case;

      --  Wait for transfer complete
      while not Port.Periph.ISR.TC loop
         null;
      end loop;

      --  Initiate the transfer
      if Data'Length > 255 then
         Config_Transfer
           (Port, Addr, 255, Reload_Mode, Generate_Start_Read);
         Size_Temp := 255;
      else
         Config_Transfer
           (Port, Addr, Data'Length, Autoend_Mode, Generate_Start_Read);
         Size_Temp := Data'Length;
      end if;

      --  Transfer the data
      while Transmitted < Data'Length loop
         while not Port.Periph.ISR.RXNE loop
            null;
         end loop;

         Data (Data'First + Transmitted) := Port.Periph.RXDR.RXDATA;
         Transmitted := Transmitted + 1;
         Size_Temp   := Size_Temp - 1;

         if Size_Temp = 0
           and then Transmitted < Data'Length
         then
            --  Wait for the Transfer complete reload flag
            while not Port.Periph.ISR.TCR loop
               null;
            end loop;

            if Data'Length - Transmitted > 255 then
               Config_Transfer
                 (Port, Addr, 255, Reload_Mode, No_Start_Stop);
               Size_Temp := 255;
            else
               Config_Transfer
                 (Port, Addr,
                  Byte (Data'Length - Transmitted),
                  Autoend_Mode,
                  No_Start_Stop);
               Size_Temp := Data'Length - Transmitted;
            end if;
         end if;
      end loop;

      Wait_Stop_Flag (Port, Timeout, Status);
      if Status /= Ok then
         return;
      end if;

      --  Reset CR2
      Reset_Config (Port);
      Port.State := Ready;
      Status       := Ok;
   end Mem_Read;

end STM32.I2C;
