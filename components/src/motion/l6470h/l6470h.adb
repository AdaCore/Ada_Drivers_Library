pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Real_Time;
with System;

with L6470h_Constants;
with HAL.SPI;
with STM32.Timers;
with STM32.Device;

package body L6470h is

   Period : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Nanoseconds(800);
   Next_Release : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   type Cmd_Type is
      record
         Code  : Hal.UInt3;
         Param : L6470h_Constants.Register_Address;
      end record
     with
       Size => 8;
   for Cmd_Type use record
      Param  at 0 range 0 .. 4;
      Code   at 0 range 5 .. 7;
   end record;

   type Uint8_Pointer is access all Hal.UInt8
     with Storage_Size => 0;
   function As_Pointer is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Uint8_Pointer);

   procedure Wait With
     Inline;
   procedure Wait is
      use type Ada.Real_Time.Time_Span;
   begin
      Next_Release := Next_Release + Period;
      delay until Next_Release;
--      Daley_Handler.Reset;
--      Daley_Handler.Wait;
   end Wait;

   procedure Initialize is
      procedure Init_SPI is
         use STM32.SPI;
         Config : SPI_Configuration;
      begin
         STM32.Device.Enable_Clock (SPI);

         Config.Mode := Master;
         Config.Baud_Rate_Prescaler := BRP_16;
         Config.Clock_Polarity := Low;
         Config.Clock_Phase := P1Edge;
         Config.First_Bit := MSB;
         Config.CRC_Poly := 7;
         Config.Slave_Management := Software_Managed;  --  essential!!
         Config.Direction := D2Lines_FullDuplex;
         Config.Data_Size := HAL.SPI.Data_Size_8b;

         Disable (SPI);
         Configure (SPI, Config);
         Enable (SPI);
      end Init_SPI;

      procedure Init_GPIO is
         use STM32.GPIO;
         Config : GPIO_Port_Configuration;
         SPI_Points : constant GPIO_Points := SPI_MOSI_Pin &
                        SPI_MISO_Pin & SPI_SCK_Pin;
      begin
         STM32.Device.Enable_Clock (SPI_Points);

         Config.Output_Type := Push_Pull;
         Config.Resistors   := Floating;
         Config.Speed       := Speed_25MHz;
         Config.Mode        := Mode_AF;

         Configure_IO (SPI_Points, Config);
         Configure_Alternate_Function (SPI_Points, SPI_AF);

         STM32.Device.Enable_Clock (Chip_Select_Pin);

         Config.Mode        := Mode_Out;
         Config.Output_Type := Push_Pull;
         Config.Resistors   := Pull_Up;
         Config.Speed       := Speed_25MHz;

         Chip_Select_Pin.Configure_IO (Config);
         Chip_Select_Pin.Set;

         STM32.Device.Enable_Clock (Reset_Pin);

         Config.Mode        := Mode_Out;
         Config.Output_Type := Push_Pull;
         Config.Resistors   := Pull_Up;
         Config.Speed       := Speed_2MHz;

         Reset_Pin.Configure_IO (Config);
         Reset_Pin.Set;

      end Init_GPIO;

      procedure Init_Timer is
      begin
         STM32.Device.Enable_Clock (This => STM32.Device.Timer_3);
         STM32.Device.Reset (This => STM32.Device.Timer_3);
         STM32.Timers.Configure (This      => STM32.Device.Timer_3,
                                 Prescaler => 21,
                                 Period    => 7);
         STM32.Timers.Enable_Interrupt (This   => STM32.Device.Timer_3,
                                        Source => STM32.Timers.Timer_Update_Interrupt);
         STM32.Timers.Enable (This => STM32.Device.Timer_3);

      end Init_Timer;
   begin
      Init_SPI;
      Init_GPIO;
      Init_Timer;

      Initialized := True;
   end Initialize;

   function Is_Initialized return Boolean is (Initialized);

   function Get_KVAL_RUN return KVAL_RUN_Val_Type is
      Data : Data_Type (1 .. 1);
      Raw_Val : Hal.UInt8 renames As_Pointer (Data (1 .. 1)'Address).all;
      function To_Val_Type is new Ada.Unchecked_Conversion
        (Source => Hal.UInt8, Target => L6470h_Constants.KVAL_RUN_Val_Type);

   begin
      Get_Param (Reg  => KVAL_RUN, Value => Data);
      return Res : KVAL_RUN_Val_Type do
         Res := To_Val_Type (Raw_Val);
      end return;
   end Get_KVAL_RUN;

   procedure Set_KVAL_RUN (Ratio : L6470h_Constants.KVAL_RUN_Val_Type) is
      Tmp : L6470h_Constants.KVAL_RUN_Val_Type := Ratio;
      Raw_Val : Hal.UInt8 renames As_Pointer (Tmp'Address).all;
   begin
      Set_Param (Reg  => KVAL_RUN,
                 Value => (1 => Raw_Val));
   end Set_KVAL_RUN;

   procedure Set_Step (Ratio : L6470h_Constants.STEP_MODE_Record) is
      Tmp : L6470h_Constants.STEP_MODE_Record := Ratio;
      Raw_Val : Hal.UInt8 renames As_Pointer (Tmp'Address).all;
  begin
      Set_Param (Reg  => STEP_MODE,
                 Value => (1 => Raw_Val));
   end Set_Step;

   procedure Set_Param (Reg : in L6470h_Constants.Register'Class;
                        Value : in Data_Type) is
      use type Ada.Real_Time.Time_Span;

      Cmd : constant Cmd_Type := (Code => 0, Param => Reg.Addr);

      Raw_Cmd : Hal.UInt8 renames As_Pointer (Cmd'Address).all;

      Set_Cmd : constant Hal.Spi.SPI_Data_8b (1 .. 1) := (1 => Hal.UInt8 (Raw_Cmd));
      Data              : Hal.SPI.SPI_Data_8b (1 .. 1) := (1 => 0);

      Status     : Hal.Spi.SPI_Status;
      use type Hal.SPI.SPI_Status;
   begin
      Chip_Select_Pin.Clear;
      Spi.Transmit (Set_Cmd, Status);
      Chip_Select_Pin.Set;
      Wait;
      for I of Value loop
         declare
            Raw_Val : Hal.UInt8 renames As_Pointer (I'Address).all;
         begin
            Chip_Select_Pin.Clear;
            Data := (1 => Hal.UInt8 (Raw_Val));
            Spi.Transmit (Data, Status);
            Chip_Select_Pin.Set;
            Wait;
         end;
      end loop;
   end Set_Param;

   procedure Get_Param (Reg : in L6470h_Constants.Register'Class;
                        Value : out Data_Type) is
      use type Ada.Real_Time.Time_Span;

      Cmd : constant Cmd_Type := (Code => 1, Param => Reg.Addr);

      Raw_Cmd : Hal.UInt8 renames As_Pointer (Cmd'Address).all;

      Get_Cmd : constant Hal.Spi.SPI_Data_8b (1 .. 1) := (1 => Hal.UInt8 (Raw_Cmd));
--      Data              : Hal.SPI.SPI_Data_8b (1 .. 1) := (1 => 0);

      Status     : Hal.Spi.SPI_Status;
      use type Hal.SPI.SPI_Status;
begin
      Chip_Select_Pin.Clear;
      Spi.Transmit (Get_Cmd, Status);
      Chip_Select_Pin.Set;
      Wait;
      for I of reverse Value loop
         Chip_Select_Pin.Clear;
         SPI.Receive (I);
         Chip_Select_Pin.Set;
         Wait;
      end loop;
   end Get_Param;

   procedure Reset is
      use type Ada.Real_Time.Time_Span;
   begin
      Reset_Pin.Clear;
      Next_Release := Next_Release + Ada.Real_Time.Milliseconds (100);
      delay until Next_Release;
      Reset_Pin.Set;
   end Reset;

   function Cmd_Ok return Boolean is
      use type Hal.Bit;
      use L6470h_Constants;
      Status : STATUS_Record;
   begin
      return Res : Boolean do
         Status := Get_Status;
         if STATUS.WRONG_CMD = 0 and STATUS.NOTPERF_CMD = 0 then
            Res := True;
         else
            Res := False;
         end if;
      end return;
   end Cmd_Ok;

   function Get_Status return L6470h_Constants.STATUS_Record is
      Status_Cmd : constant Hal.Spi.SPI_Data_8b (1 .. 1) := (1 => 2#11010000#);
      subtype Data_Type is HAL.Spi.SPI_Data_8b (1 .. 2);
      Data       : Data_Type := (others => 0);
      Status     : Hal.Spi.SPI_Status;
      use type Hal.SPI.SPI_Status;
      function To_Res is new Ada.Unchecked_Conversion
        (Source => Hal.UInt16, Target => L6470h_Constants.STATUS_Record);

      type Integer16_Pointer is access all Hal.UInt16
        with Storage_Size => 0;

      function As_Pointer is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Integer16_Pointer);
      Raw : Hal.UInt16 renames As_Pointer (Data (1)'Address).all;
      use type Ada.Real_Time.Time_Span;
   begin
      Chip_Select_Pin.Clear;
      Spi.Transmit (Status_Cmd, Status);

      if Status /= Hal.SPI.Ok then
         --  No error handling...
         raise Program_Error;
      end if;

      Chip_Select_Pin.Set;
      Wait;
      for I of reverse Data loop
         Chip_Select_Pin.Clear;
         SPI.Receive (I);
         Chip_Select_Pin.Set;
         Wait;
      end loop;
      return Res : L6470h_Constants.STATUS_Record do
         Res := To_Res (Raw);
      end return;
   end Get_Status;

   procedure Run (Spd : L6470h_Constants.SPEED_Val_Type) is
      Run_Cmd : constant Hal.Spi.SPI_Data_8b (1 .. 1) := (1 => 2#01010000#);
      subtype Data_Type is HAL.Spi.SPI_Data_8b (1 .. 3);
      Status     : Hal.Spi.SPI_Status;
      use type Hal.SPI.SPI_Status;
      pragma Warnings (Off);
      function To_Data is new Ada.Unchecked_Conversion
        (Source => L6470h_Constants.SPEED_Val_Type, Target => Data_Type);
      pragma Warnings (On);
      Data       : Data_Type := To_Data (Spd);
      use type Ada.Real_Time.Time_Span;
   begin
      Chip_Select_Pin.Clear;
      Spi.Transmit (Run_Cmd, Status);

      if Status /= Hal.SPI.Ok then
         --  No error handling...
         raise Program_Error;
      end if;

      Chip_Select_Pin.Set;
      Wait;
      for I Of reverse Data loop
         Chip_Select_Pin.Clear;
         Spi.Transmit (I);
         Chip_Select_Pin.Set;
         Wait;
      end loop;
   end Run;

   procedure Reset_Pos is
      Reset_Pos_Cmd : constant Hal.Spi.SPI_Data_8b (1 .. 1) := (1 => 2#11011000#);
      Status     : Hal.Spi.SPI_Status;
      use type Hal.SPI.SPI_Status;
   begin
      Chip_Select_Pin.Clear;
      Spi.Transmit (Reset_Pos_Cmd, Status);

      if Status /= Hal.SPI.Ok then
         --  No error handling...
         raise Program_Error;
      end if;

      Chip_Select_Pin.Set;
   end Reset_Pos;

   procedure Set_STALL_TH (Ratio : L6470h_Constants.STALL_TH_Val_Type) is
      Tmp : L6470h_Constants.STALL_TH_Val_Type := Ratio;
      Raw_Val : Hal.UInt8 renames As_Pointer (Tmp'Address).all;
   begin
   Set_Param (Reg  => STALL_TH,
              Value => (1 => Raw_Val));
   end;

begin
   Initialize;
end L6470h;
