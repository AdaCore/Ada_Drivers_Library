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

package body STM32.I2C is

   type I2C_State is
     (Reset,
      Ready,
      Master_Busy_Tx,
      Master_Busy_Rx,
      Mem_Busy_Tx,
      Mem_Busy_Rx);

   type I2C_Transfer_Mode is
     (Reload_Mode,   --  Enable reload mode
      Autoend_Mode,  -- Enable automatic end mode
      Softend_Mode); --  Enable software end mode

   type I2C_Request is
     (No_Start_Stop,         --  Don't generate start or stop
      Generate_Stop,         --  Generate a stop condition
      Generate_Start_Read,   --  Generate a start read request
      Generate_Start_Write); --  Generate a start write request

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

   function Is_Configured (Handle : in out I2C_Port_Handle) return Boolean;

   procedure Master_Transmit
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Duration := 0.0);

   procedure Master_Receive
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Duration := 0.0);

   procedure Mem_Write
     (Handle        : in out I2C_Port_Handle;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Duration := 0.0);

   procedure Mem_Read
     (Handle        : in out I2C_Port_Handle;
      Addr          : UInt10;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Duration := 0.0);

   procedure Config_Transfer
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Size    : Byte;
      Mode    : I2C_Transfer_Mode;
      Request : I2C_Request);

   procedure Reset_Config (Handle : in out I2C_Port_Handle);

   procedure Check_Nack
     (Handle  : in out I2C_Port_Handle;
      Timeout : Duration;
      Status  : out I2C_Status);
   procedure Wait_Tx_Interrupt_Status
     (Handle  : in out I2C_Port_Handle;
      Timeout : Duration;
      Status  : out I2C_Status);
   procedure Wait_Stop_Flag
     (Handle  : in out I2C_Port_Handle;
      Timeout : Duration;
      Status  : out I2C_Status);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Handle : in out I2C_Port_Handle;
      Port   : I2C_Port_Id;
      Conf   : I2C_Configuration)
   is
   begin
      if Handle.State /= Reset then
         return;
      end if;

      Handle.Periph := As_Port (Port);
      Handle.Config := Conf;

      Enable_Clock (Handle.Periph.all);
      Reset (Handle.Periph.all);

      --  Disable the I2C port
      Handle.Periph.CR1.PE := 0;

      --  Reset the timing register to 100_000 Hz
      Handle.Periph.TIMINGR :=
        (SCLL   => 50,
         SCLH   => 39,
         SDADEL => 1,
         SCLDEL => 9,
         PRESC  => 4,
         others => <>);

      --  I2C Own Address Register configuration
      if Conf.Own_Address /= 0 then
         Handle.Periph.OAR1 :=
           (OA1     => Conf.Own_Address,
            OA1EN   => 1,
            OA1MODE => (case Conf.Addressing_Mode is
                           when Addressing_Mode_7bit  => 0,
                           when Addressing_Mode_10bit => 1),
            others  => <>);
      end if;

      --  CR2 configuration
      --  Enable AUTOEND by default, set NACK (should be disabled only in
      --  slave mode
      Handle.Periph.CR2 :=
        (AUTOEND => 1,
         NACK    => 1,
         ADD10   => (if Conf.Addressing_Mode = Addressing_Mode_10bit
                     then 1 else 0),
         others  => <>);

      --  OAR2 configuration
      --  ??? Add support for dual addressing
      Handle.Periph.OAR2 := (others => <>);

      --  CR1 configuration
      Handle.Periph.CR1 :=
        (GCEN      => (if Conf.General_Call_Enabled then 1 else 0),
         NOSTRETCH => (if Conf.Clock_Stretching_Enabled then 0 else 1),
         others    => <>);

      Handle.State := Ready;
      --  Enable the port
      Handle.Periph.CR1.PE := 1;
   end Configure;

   -------------------
   -- Is_Configured --
      -------------------

   function Is_Configured (Handle : in out I2C_Port_Handle) return Boolean
   is
   begin
      return Handle.State /= Reset;
   end Is_Configured;

   ---------------------
   -- Config_Transfer --
   ---------------------

   procedure Config_Transfer
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Size    : Byte;
      Mode    : I2C_Transfer_Mode;
      Request : I2C_Request)
   is
      CR2 : CR2_Register := Handle.Periph.CR2;
   begin
      CR2.SADD := Addr;
      CR2.NBYTES := Size;
      CR2.RELOAD := (if Mode = Reload_Mode then 1 else 0);
      CR2.AUTOEND := (if Mode = Autoend_Mode then 1 else 0);

      CR2.RD_WRN := 0;
      CR2.START  := 0;
      CR2.STOP   := 0;

      case Request is
         when No_Start_Stop =>
            null;

         when Generate_Stop =>
            CR2.STOP := 1;

         when Generate_Start_Read =>
            CR2.RD_WRN := 1;
            CR2.START := 1;

         when Generate_Start_Write =>
            CR2.START := 1;
      end case;

      Handle.Periph.CR2 := CR2;
   end Config_Transfer;

   ------------------
   -- Reset_Config --
   ------------------

   procedure Reset_Config (Handle  : in out I2C_Port_Handle)
   is
      CR2 : CR2_Register := Handle.Periph.CR2;
   begin
      CR2.SADD    := 0;
      CR2.HEAD10R := 0;
      CR2.NBYTES  := 0;
      CR2.RELOAD  := 0;
      CR2.RD_WRN  := 0;
      Handle.Periph.CR2 := CR2;
   end Reset_Config;

   ----------------
   -- Check_Nack --
   ----------------

   procedure Check_Nack
     (Handle  : in out I2C_Port_Handle;
      Timeout : Duration;
      Status  : out I2C_Status)
   is
      Now : constant Time := Clock;
   begin
      if Handle.Periph.ISR.NACKF = 1 then
         if Handle.State = Master_Busy_Tx
           or else Handle.State = Mem_Busy_Tx
           or else Handle.State = Mem_Busy_Rx
         then
            --  We generate a STOP condition if SOFTEND mode is enabled
            if Handle.Periph.CR2.AUTOEND = 0 then
               Handle.Periph.CR2.STOP := 1;
            end if;
         end if;

         while Handle.Periph.ISR.STOPF = 0 loop
            if Timeout > 0.0
              and then To_Duration (Clock - Now) > Timeout
            then
               Handle.State := Ready;
               Status       := Err_Timeout;
               return;
            end if;
         end loop;

         --  Clear the MACL amd STOP flags
         Handle.Periph.ICR.NACKCF := 1;
         Handle.Periph.ICR.STOPCF := 1;

         --  Clear CR2
         Reset_Config (Handle);

         Handle.State := Ready;
         Status       := Err_Error;

      else
         Status := Ok;
      end if;
   end Check_Nack;

   ------------------------------
   -- Wait_Tx_Interrupt_Status --
   ------------------------------

   procedure Wait_Tx_Interrupt_Status
     (Handle  : in out I2C_Port_Handle;
      Timeout : Duration;
      Status  : out I2C_Status)
   is
      Now    : constant Time := Clock;
   begin
      while Handle.Periph.ISR.TXIS = 0 loop
         Check_Nack (Handle, Timeout, Status);

         if Status /= Ok then
            Handle.State := Ready;
            Status       := Err_Error;

            return;
         end if;

         if Timeout > 0.0
           and then To_Duration (Clock - Now) > Timeout
         then
            Reset_Config (Handle);
            Handle.State := Ready;
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
     (Handle  : in out I2C_Port_Handle;
      Timeout : Duration;
      Status  : out I2C_Status)
   is
      Now    : constant Time := Clock;
   begin
      while Handle.Periph.ISR.TCR = 0 loop
         if Timeout > 0.0
           and then To_Duration (Clock - Now) > Timeout
         then
            Reset_Config (Handle);
            Status       := Err_Timeout;
            Handle.State := Ready;

            return;
         end if;
      end loop;

      Status := Ok;
   end Wait_Transfer_Complete_Reset_Flag;

   --------------------
   -- Wait_Stop_Flag --
   --------------------

   procedure Wait_Stop_Flag
     (Handle  : in out I2C_Port_Handle;
      Timeout : Duration;
      Status  : out I2C_Status)
   is
      Now    : constant Time := Clock;
   begin
      while Handle.Periph.ISR.STOPF = 0 loop
         Check_Nack (Handle, Timeout, Status);

         if Status /= Ok then
            Handle.State := Ready;
            Status       := Err_Error;

            return;
         end if;

         if Timeout > 0.0
           and then To_Duration (Clock - Now) > Timeout
         then
            Reset_Config (Handle);
            Status       := Err_Timeout;
            Handle.State := Ready;

            return;
         end if;
      end loop;

      --  Clear the stop flag
      Handle.Periph.ICR.STOPCF := 1;

      Status := Ok;
   end Wait_Stop_Flag;

      ---------------------
      -- Master_Transmit --
      ---------------------

   procedure Master_Transmit
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Duration := 0.0)
   is
      Size_Temp   : Natural := 0;
      Transmitted : Natural := 0;

   begin
      if Handle.Periph.ISR.BUSY = 1 then
         Status := Busy;
         return;
      end if;

      if Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      if Handle.State /= Ready then
         Status := Busy;
         return;
      end if;

      Handle.State := Master_Busy_Tx;

      --  Initiate the transfer
      if Data'Length > 255 then
         Config_Transfer
           (Handle, Addr, 255, Reload_Mode, Generate_Start_Write);
         Size_Temp := 255;
      else
         Config_Transfer
           (Handle, Addr, Data'Length, Autoend_Mode, Generate_Start_Write);
         Size_Temp := Data'Length;
      end if;

      --  Transfer the data
      while Transmitted <= Data'Length loop
         Wait_Tx_Interrupt_Status (Handle, Timeout, Status);

         if Status /= Ok then
            return;
         end if;

         Handle.Periph.TXDR.TXDATA := Data (Data'First + Transmitted);
         Transmitted := Transmitted + 1;

         if Transmitted = Size_Temp
           and then Transmitted < Data'Length
         then
            --  Wait for the Transfer complete reload flag
            Wait_Transfer_Complete_Reset_Flag (Handle, Timeout, Status);
            if Status /= Ok then
               return;
            end if;

            if Data'Length - Transmitted > 255 then
               Config_Transfer
                 (Handle, Addr, 255, Reload_Mode, No_Start_Stop);
               Size_Temp := 255;
            else
               Config_Transfer
                 (Handle, Addr, Byte (Data'Length - Transmitted), Autoend_Mode,
                  No_Start_Stop);
               Size_Temp := Data'Length - Transmitted;
            end if;
         end if;
      end loop;

      Wait_Stop_Flag (Handle, Timeout, Status);
      if Status /= Ok then
         return;
      end if;

      --  Reset CR2
      Reset_Config (Handle);
      Handle.State := Ready;
      Status       := Ok;
   end Master_Transmit;

   --------------------
   -- Master_Receive --
   --------------------

   procedure Master_Receive
     (Handle  : in out I2C_Port_Handle;
      Addr    : UInt10;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Duration := 0.0)
   is
      Size_Temp   : Natural := 0;
      Transmitted : Natural := 0;
   begin
      if Handle.Periph.ISR.BUSY = 1 then
         Status := Busy;
         return;
      end if;

      if Handle.State /= Ready then
         Status := Busy;
         return;
      end if;

      Handle.State := Master_Busy_Rx;

      if Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      --  Initiate the transfer
      if Data'Length > 255 then
         Config_Transfer
           (Handle, Addr, 255, Reload_Mode, Generate_Start_Read);
         Size_Temp := 255;
      else
         Config_Transfer
           (Handle, Addr, Data'Length, Autoend_Mode, Generate_Start_Read);
         Size_Temp := Data'Length;
      end if;

      --  Transfer the data
      while Transmitted < Data'Length loop
         while Handle.Periph.ISR.RXNE = 0 loop
            null;
         end loop;

         Data (Data'First + Transmitted) := Handle.Periph.RXDR.RXDATA;
         Transmitted := Transmitted + 1;
         Size_Temp   := Size_Temp - 1;

         if Size_Temp = 0
           and then Transmitted < Data'Length
         then
            --  Wait for the Transfer complete reload flag
            while Handle.Periph.ISR.TCR /= 0 loop
               null;
            end loop;

            if Data'Length - Transmitted > 255 then
               Config_Transfer
                 (Handle, Addr, 255, Reload_Mode, No_Start_Stop);
               Size_Temp := 255;
            else
               Config_Transfer
                 (Handle, Addr, Byte (Data'Length - Transmitted), Autoend_Mode,
                  No_Start_Stop);
               Size_Temp := Data'Length - Transmitted;
            end if;
         end if;
      end loop;

      Wait_Stop_Flag (Handle, Timeout, Status);
      if Status /= Ok then
         return;
      end if;

      --  Reset CR2
      Reset_Config (Handle);
      Handle.State := Ready;
      Status       := Ok;
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
      Timeout       : Duration := 0.0)
   is
      Size_Temp   : Natural := 0;
      Transmitted : Natural := 0;

   begin
      if Handle.Periph.ISR.BUSY = 1 then
         Status := Busy;
         return;
      end if;

      if Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      if Handle.State /= Ready then
         Status := Busy;
         return;
      end if;

      Handle.State := Mem_Busy_Tx;

      --  Configure the memory transfer
      Config_Transfer
        (Handle,
         Addr,
         (case Mem_Addr_Size is
             when Memory_Size_8b  => 1,
             when Memory_Size_16b => 2),
         Reload_Mode,
         Generate_Start_Write);

      Wait_Tx_Interrupt_Status (Handle, Timeout, Status);

      if Status /= Ok then
         Handle.State := Ready;

         return;
      end if;

      case Mem_Addr_Size is
         when Memory_Size_8b =>
            Handle.Periph.TXDR.TXDATA := Byte (Mem_Addr);

         when Memory_Size_16b =>
            declare
               MSB : constant Byte := Byte (Shift_Right (Mem_Addr, 8));
               LSB : constant Byte := Byte (Mem_Addr and 16#FF#);
            begin
               Handle.Periph.TXDR.TXDATA := MSB;

               Wait_Tx_Interrupt_Status (Handle, Timeout, Status);
               if Status /= Ok then
                  return;
               end if;

               Handle.Periph.TXDR.TXDATA := LSB;
            end;
      end case;

      Wait_Transfer_Complete_Reset_Flag (Handle, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      --  Initiate the transfer
      if Data'Length > 255 then
         Config_Transfer
           (Handle, Addr, 255, Reload_Mode, No_Start_Stop);
         Size_Temp := 255;
      else
         Config_Transfer
           (Handle, Addr, Data'Length, Autoend_Mode, No_Start_Stop);
         Size_Temp := Data'Length;
      end if;

      --  Transfer the data
      while Transmitted < Data'Length loop
         Wait_Tx_Interrupt_Status (Handle, Timeout, Status);

         if Status /= Ok then
            return;
         end if;

         Handle.Periph.TXDR.TXDATA := Data (Data'First + Transmitted);
         Transmitted := Transmitted + 1;

         if Transmitted = Size_Temp
           and then Transmitted < Data'Length
         then
            --  Wait for the Transfer complete reload flag
            Wait_Transfer_Complete_Reset_Flag (Handle, Timeout, Status);

            if Status /= Ok then
               return;
            end if;

            if Data'Length - Transmitted > 255 then
               Config_Transfer
                 (Handle, Addr, 255, Reload_Mode, No_Start_Stop);
               Size_Temp := 255;
            else
               Config_Transfer
                 (Handle, Addr,
                  Byte (Data'Length - Transmitted),
                  Autoend_Mode,
                  No_Start_Stop);
               Size_Temp := Data'Length - Transmitted;
            end if;
         end if;
      end loop;

      Wait_Stop_Flag (Handle, Timeout, Status);
      if Status /= Ok then
         return;
      end if;

      --  Reset CR2
      Reset_Config (Handle);
      Handle.State := Ready;
      Status       := Ok;
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
      Timeout       : Duration := 0.0)
   is
      Size_Temp   : Natural := 0;
      Transmitted : Natural := 0;
   begin
      if Handle.Periph.ISR.BUSY = 1 then
         Status := Busy;
         return;
      end if;

      if Data'Length = 0 then
         Status := Err_Error;
         return;
      end if;

      if Handle.State /= Ready then
         Status := Busy;
         return;
      end if;

      Handle.State := Mem_Busy_Rx;

      --  Configure the memory transfer
      Config_Transfer
        (Handle,
         Addr,
         (case Mem_Addr_Size is
             when Memory_Size_8b  => 1,
             when Memory_Size_16b => 2),
         Softend_Mode,
         Generate_Start_Write);

      Wait_Tx_Interrupt_Status (Handle, Timeout, Status);

      if Status /= Ok then
         return;
      end if;

      case Mem_Addr_Size is
         when Memory_Size_8b =>
            Handle.Periph.TXDR.TXDATA := Byte (Mem_Addr);
         when Memory_Size_16b =>
            declare
               MSB : constant Byte := Byte (Shift_Right (Mem_Addr, 8));
               LSB : constant Byte := Byte (Mem_Addr and 16#FF#);
            begin
               Handle.Periph.TXDR.TXDATA := MSB;

               Wait_Tx_Interrupt_Status (Handle, Timeout, Status);

               if Status /= Ok then
                  return;
               end if;

               Handle.Periph.TXDR.TXDATA := LSB;
            end;
      end case;

      --  Wait for transfer complete
      while Handle.Periph.ISR.TC = 0 loop
         null;
      end loop;

      --  Initiate the transfer
      if Data'Length > 255 then
         Config_Transfer
           (Handle, Addr, 255, Reload_Mode, Generate_Start_Read);
         Size_Temp := 255;
      else
         Config_Transfer
           (Handle, Addr, Data'Length, Autoend_Mode, Generate_Start_Read);
         Size_Temp := Data'Length;
      end if;

      --  Transfer the data
      while Transmitted < Data'Length loop
         while Handle.Periph.ISR.RXNE = 0 loop
            null;
         end loop;

         Data (Data'First + Transmitted) := Handle.Periph.RXDR.RXDATA;
         Transmitted := Transmitted + 1;
         Size_Temp   := Size_Temp - 1;

         if Size_Temp = 0
           and then Transmitted < Data'Length
         then
            --  Wait for the Transfer complete reload flag
            while Handle.Periph.ISR.TCR /= 0 loop
               null;
            end loop;

            if Data'Length - Transmitted > 255 then
               Config_Transfer
                 (Handle, Addr, 255, Reload_Mode, No_Start_Stop);
               Size_Temp := 255;
            else
               Config_Transfer
                 (Handle, Addr,
                  Byte (Data'Length - Transmitted),
                  Autoend_Mode,
                  No_Start_Stop);
               Size_Temp := Data'Length - Transmitted;
            end if;
         end if;
      end loop;

      Wait_Stop_Flag (Handle, Timeout, Status);
      if Status /= Ok then
         return;
      end if;

      --  Reset CR2
      Reset_Config (Handle);
      Handle.State := Ready;
      Status       := Ok;
   end Mem_Read;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Port : I2C_Port_Id; Conf : I2C_Configuration)
   is
   begin
      Configure (I2C_Handles (Port), Port, Conf);
   end Configure;

   ------------------
   -- Port_Enabled --
   ------------------

   function Port_Enabled (Port : I2C_Port_Id) return Boolean is
      Periph : constant access I2C_Port := As_Port (Port);
   begin
      return Periph.CR1.PE = 1;
   end Port_Enabled;

   function Is_Configured (Port : I2C_Port_Id) return Boolean
   is
      Handle : I2C_Port_Handle renames I2C_Handles (Port);
   begin
      return Is_Configured (Handle);
   end Is_Configured;

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
      Handle : I2C_Port_Handle renames I2C_Handles (Port);
   begin
      if not Is_Configured (Handle) then
         raise I2C_Error with "I2C port not configured";
      end if;

      Master_Transmit
        (Handle, Addr, Data, Status,
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
      Handle : I2C_Port_Handle renames I2C_Handles (Port);
   begin
      if not Is_Configured (Handle) then
         raise I2C_Error with "I2C port not configured";
      end if;

      Master_Receive
        (Handle, Addr, Data, Status,
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
      Handle : I2C_Port_Handle renames I2C_Handles (Port);
   begin
      if not Is_Configured (Handle) then
         raise I2C_Error with "I2C port not configured";
      end if;

      Mem_Write
        (Handle, Addr, Mem_Addr, Mem_Addr_Size, Data, Status,
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
      Handle : I2C_Port_Handle renames I2C_Handles (Port);
   begin
      if not Is_Configured (Handle) then
         raise I2C_Error with "I2C port not configured";
      end if;

      Mem_Read
        (Handle, Addr, Mem_Addr, Mem_Addr_Size, Data, Status,
         Duration (Timeout) / 1000.0);
   end Mem_Read;

end STM32.I2C;
