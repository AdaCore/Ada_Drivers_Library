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
--   @file    lis3dsh.c                                                     --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file provides a set of functions needed to manage the    --
--            LIS3DSH MEMS Accelerometer.                                   --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System;
with STM32F4.NVIC;

package body STM32F4.LIS3DSH is

   Full_Scale_Selection_Mask : constant Byte := 2#0011_1000#;
   --  bits 3..5 of CTRL5

   Data_Rate_Selection_Mask : constant Byte := 2#0111_0000#;
   --  bits 4..6 of CTRL4

   ----------------
   -- Configured --
   ----------------

   function Configured (This : Three_Axis_Accelerometer) return Boolean is
     (This.Device_Configured);

   -----------------
   -- Initialized --
   -----------------

   function Initialized (This : Three_Axis_Accelerometer) return Boolean is
     (This.Device_Initialized);

   -----------------------
   -- Get_Accelerations --
   -----------------------

   procedure Get_Accelerations
     (This : in out Three_Axis_Accelerometer;
      Axes : out Axes_Accelerations)
   is

      Buffer : array (0 .. 5) of Byte with Alignment => 2, Size => 48;
      Scaled : Float;

      use Interfaces;

      type Integer16_Pointer is access all Integer_16
        with Storage_Size => 0;

      function As_Pointer is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Integer16_Pointer);
      --  So that we can treat the address of a byte as a pointer to a two-byte
      --  sequence representing a signed Integer_16 quantity

   begin
      Read (This, Buffer (0), OUT_X_L);
      Read (This, Buffer (1), OUT_X_H);
      Read (This, Buffer (2), OUT_Y_L);
      Read (This, Buffer (3), OUT_Y_H);
      Read (This, Buffer (4), OUT_Z_L);
      Read (This, Buffer (5), OUT_Z_H);

      Get_X : declare
         Raw : Integer_16 renames As_Pointer (Buffer(0)'Address).all;
      begin
         Scaled := Float (Raw) * This.Sensitivity;
         Axes.X := Axis_Acceleration (Scaled);
      end Get_X;

      Get_Y : declare
         Raw : Integer_16 renames As_Pointer (Buffer(2)'Address).all;
      begin
         Scaled := Float (Raw) * This.Sensitivity;
         Axes.Y := Axis_Acceleration (Scaled);
      end Get_Y;

      Get_Z : declare
         Raw : Integer_16 renames As_Pointer (Buffer(4)'Address).all;
      begin
         Scaled := Float (Raw) * This.Sensitivity;
         Axes.Z := Axis_Acceleration (Scaled);
      end Get_Z;
   end Get_Accelerations;

   -----------------------------
   -- Configure_Accelerometer --
   -----------------------------

   procedure Configure_Accelerometer
     (This            : out Three_Axis_Accelerometer;
      Output_DataRate : Data_Rate_Power_Mode_Selection;
      Axes_Enable     : Direction_XYZ_Selection;
      SPI_Wire        : SPI_Serial_Interface_Mode_Selection;
      Self_Test       : Self_Test_Selection;
      Full_Scale      : Full_Scale_Selection;
      Filter_BW       : Anti_Aliasing_Filter_Bandwidth)
   is
      Temp  : Half_Word;
      Value : Byte;
   begin
      Temp := Output_DataRate'Enum_Rep or
              Axes_Enable'Enum_Rep     or
              SPI_Wire'Enum_Rep        or
              Self_Test'Enum_Rep       or
              Full_Scale'Enum_Rep      or
              Filter_BW'Enum_Rep;

      Value := Byte (Temp); -- the low byte of the half-word
      Write (This, Value, CTRL_REG4);

      Value := Byte (Shift_Right (Temp, 8)); -- the high byte
      Write (This, Value, CTRL_REG5);

      case Full_Scale is
         when Fullscale_2g =>
            This.Sensitivity := Sensitivity_0_06mg;
         when Fullscale_4g =>
            This.Sensitivity := Sensitivity_0_12mg;
         when Fullscale_6g =>
            This.Sensitivity := Sensitivity_0_18mg;
         when Fullscale_8g =>
            This.Sensitivity := Sensitivity_0_24mg;
         when Fullscale_16g =>
            This.Sensitivity := Sensitivity_0_73mg;
      end case;

      This.Device_Configured := True;
   end Configure_Accelerometer;

   ---------------
   -- Device_Id --
   ---------------

   function Device_Id (This : in out Three_Axis_Accelerometer) return Byte is
      Response : Byte;
   begin
      Read (This, Response, Who_Am_I);
      return Response;
   end Device_Id;

   ------------
   -- Reboot --
   ------------

   procedure Reboot (This : in out Three_Axis_Accelerometer) is
      Value : Byte;
      Force_Reboot : constant Byte := 2#1000_0000#;
   begin
      Read (This, Value, CTRL_REG6);
      Value := Value or Force_Reboot;
      Write (This, Value, CTRL_REG6);
   end Reboot;

   --------------------------
   -- Configure_Interrupts --
   --------------------------

   procedure Configure_Interrupts
     (This                       : in out Three_Axis_Accelerometer;
      Interrupt_Request          : Interrupt_Request_Selection;
      Interrupt_Selection_Enable : Interrupt_Selection_Enablers;
      Interrupt_Signal           : Interrupt_Signal_Active_Selection;
      State_Machine1_Enable      : Boolean;
      State_Machine1_Interrupt   : State_Machine_Routed_Interrupt;
      State_Machine2_Enable      : Boolean;
      State_Machine2_Interrupt   : State_Machine_Routed_Interrupt)
   is
      CTRL : Byte;
   begin
      CTRL := Interrupt_Selection_Enable'Enum_Rep or
              Interrupt_Request'Enum_Rep          or
              Interrupt_Signal'Enum_Rep;

      Write (This, CTRL, CTRL_REG3);

      --  configure State Machine 1
      CTRL := State_Machine1_Enable'Enum_Rep or
              State_Machine1_Interrupt'Enum_Rep;

      Write (This, CTRL, CTRL_REG1);

      --  configure State Machine 2
      CTRL := State_Machine2_Enable'Enum_Rep or
              State_Machine2_Interrupt'Enum_Rep;

      Write (This, CTRL, CTRL_REG2);
   end Configure_Interrupts;

   -------------------------------
   -- Configure_Click_Interrupt --
   -------------------------------

   procedure Configure_Click_Interrupt (This : in out Three_Axis_Accelerometer) is
   begin
      -- NB: Configure_External_Interrupt must be called first!

      Configure_Interrupts
        (This,
         Interrupt_Request          => Interrupt_Request_Latched,
         Interrupt_Selection_Enable => Interrupt_2_Enable,
         Interrupt_Signal           => Interrupt_Signal_High,
         State_Machine1_Enable      => False,
         State_Machine1_Interrupt   => SM_Int1, -- Ignored
         State_Machine2_Enable      => True,
         State_Machine2_Interrupt   => SM_Int1);

      --  configure state machines
      Write (This, 3, TIM2_1_L);
      Write (This, 16#C8#, TIM1_1_L);
      Write (This, 16#45#, THRS2_1);
      Write (This, 16#FC#, MASK1_A);
      Write (This, 16#A1#, SETT1);
      Write (This, 16#1#, PR1);

      Write (This, 16#1#, SETT2);

      --  configure State Machine 2 to detect single click
      Write (This, 16#1#, ST2_1);
      Write (This, 16#6#, ST2_2);
      Write (This, 16#28#, ST2_3);
      Write (This, 16#11#, ST2_4);
   end Configure_Click_Interrupt;

   -------------------
   -- Set_Low_Power --
   -------------------

   procedure Set_Low_Power
     (This : in out Three_Axis_Accelerometer;
      Mode : Data_Rate_Power_Mode_Selection)
   is
      Value : Byte;
   begin
      Read (This, Value, CTRL_REG4);
      Value := Value and (not Data_Rate_Selection_Mask); -- clear bits
      Value := Value or Mode'Enum_Rep;
      Write (This, Value, CTRL_REG4);
   end Set_Low_Power;

   -------------------
   -- Set_Data_Rate --
   -------------------

   procedure Set_Data_Rate
     (This     : in out Three_Axis_Accelerometer;
      DataRate : Data_Rate_Power_Mode_Selection)
   is
      Value : Byte;
   begin
      Read (This, Value, CTRL_REG4);
      Value := Value and (not Data_Rate_Selection_Mask); -- clear bits
      Value := Value or DataRate'Enum_Rep;
      Write (This, Value, CTRL_REG4);
   end Set_Data_Rate;

   --------------------
   -- Set_Full_Scale --
   --------------------

   procedure Set_Full_Scale
     (This : in out Three_Axis_Accelerometer;
      Scale : Full_Scale_Selection)
   is
      Value : Byte;
   begin
      Read (This, Value, CTRL_REG5);
      Value := Value and (not Full_Scale_Selection_Mask); -- clear bits
      Value := Value or Scale'Enum_Rep;
      Write (This, Value, CTRL_REG5);
   end Set_Full_Scale;

   -------------------
   -- As_Full_Scale --
   -------------------

   function As_Full_Scale is new Ada.Unchecked_Conversion
     (Source => Byte, Target => Full_Scale_Selection);

   --------------------------
   -- Selected_Sensitivity --
   --------------------------

   function Selected_Sensitivity (This : in out Three_Axis_Accelerometer) return Float is
      CTRL5 : Byte;
   begin
      Read (This, CTRL5, CTRL_REG5);
      case As_Full_Scale (CTRL5 and Full_Scale_Selection_Mask) is
         when Fullscale_2g =>
            return Sensitivity_0_06mg;
         when Fullscale_4g =>
            return Sensitivity_0_12mg;
         when Fullscale_6g =>
            return Sensitivity_0_18mg;
         when Fullscale_8g =>
            return Sensitivity_0_24mg;
         when Fullscale_16g =>
            return Sensitivity_0_73mg;
      end case;
   end Selected_Sensitivity;

   -----------------
   -- Temperature --
   -----------------

   function Temperature (This : in out Three_Axis_Accelerometer) return Byte is
      Result : Byte;
   begin
      Read (This, Result, Out_T);
      return Result;
   end Temperature;

   ReadWrite_Cmd_Bit : constant := 16#80#;

   ----------------------
   -- Chip_Select_High --
   ----------------------

   procedure Chip_Select_High (This : in out Three_Axis_Accelerometer) is
   begin
      GPIO.Set (This.Chip_Select);
   end Chip_Select_High;

   ---------------------
   -- Chip_Select_Low --
   ---------------------

   procedure Chip_Select_Low (This : in out Three_Axis_Accelerometer) is
   begin
      GPIO.Clear (This.Chip_Select);
   end Chip_Select_Low;

   -------------------
   -- Init_SPI_GPIO --
   -------------------

   procedure Init_SPI_GPIO
     (Port : access GPIO_Port;
      Pins : GPIO_Pins;
      AF   : GPIO_Alternate_Function)
   is
      Config : GPIO_Port_Configuration;
   begin
      Config.Output_Type := Push_Pull;
      Config.Resistors := Floating;
      Config.Speed := Speed_50MHz;
      Config.Mode := Mode_AF;

      Configure_IO (Port.all, Pins, Config);
      Configure_Alternate_Function (Port.all, Pins, AF);
   end Init_SPI_GPIO;

   --------------
   -- Init_SPI --
   --------------

   procedure Init_SPI (SPIx : access SPI_Port) is
      Config : SPI_Configuration;
   begin
      Config.Mode := Master;
      Config.Baud_Rate_Prescaler := BRP_32;
      Config.Clock_Polarity := Low;
      Config.Clock_Phase := P1Edge;
      Config.First_Bit := MSB;
      Config.CRC_Poly := 7;
      Config.Slave_Management := Software_Managed;  --  essential!!
      Config.Direction := D2Lines_FullDuplex;
      Config.Data_Size := Data_8;

      Disable (SPIx.all);
      Configure (SPIx.all, Config);
      Enable (SPIx.all);
   end Init_SPI;

   ------------------------------
   -- Init_LIS3DSH_Chip_Select --
   ------------------------------

   procedure Init_LIS3DSH_Chip_Select (Chip_Select : GPIO_Point) is
      Config : GPIO_Port_Configuration;
   begin
      Config.Mode := Mode_Out;
      Config.Output_Type := Push_Pull;
      Config.Resistors := Pull_Up;
      Config.Speed := Speed_25MHz;
      Configure_IO (Chip_Select.Port.all, Chip_Select.Pin, Config);

      --  Chip_Select_High;
   end Init_LIS3DSH_Chip_Select;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_Accelerometer
     (This                          : out Three_Axis_Accelerometer;
      Chip_Select                   : GPIO_Point;
      Enable_SPIx_Chip_Select_Clock : access procedure;
      SPIx                          : access SPI_Port;
      SPIx_AF                       : GPIO_Alternate_Function;
      Enable_SPIx_Clock             : access procedure;
      SPIx_GPIO_Port                : access GPIO_Port;
      SPIx_SCK_Pin                  : GPIO_Pin;
      SPIx_MISO_Pin                 : GPIO_Pin;
      SPIx_MOSI_Pin                 : GPIO_Pin;
      Enable_SPIx_GPIO_Clock        : access procedure)
   is
   begin
      if not This.Device_Initialized then
         Enable_SPIx_Chip_Select_Clock.all;
         Enable_SPIx_Clock.all;
         Enable_SPIx_GPIO_Clock.all;

         This.Chip_Select := Chip_Select;
         This.SPIx := SPIx;

         Init_SPI_GPIO
           (SPIx_GPIO_Port,
            SPIx_SCK_Pin & SPIx_MISO_Pin & SPIx_MOSI_Pin,
            SPIx_AF);

         Init_SPI (SPIx);
         Init_LIS3DSH_Chip_Select (Chip_Select);

         Chip_Select_High (This);

         This.Device_Initialized := True;
      end if;
   end Initialize_Accelerometer;

   -----------
   -- Write --
   -----------

   procedure Write
     (This      : in out Three_Axis_Accelerometer;
      Value     : Byte;
      WriteAddr : Register_Address)
   is
   begin
      Chip_Select_Low (This);
      SPI.Transmit (This.SPIx.all, Byte (WriteAddr));
      SPI.Transmit (This.SPIx.all, Value);
      Chip_Select_High (This);
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (This     : in out Three_Axis_Accelerometer;
      Value    : out Byte;
      ReadAddr : Register_Address)
   is
      Source : constant Register_Address := ReadAddr or ReadWrite_Cmd_Bit;
   begin
      Chip_Select_Low (This);
      SPI.Transmit (This.SPIx.all, Byte (Source));
      SPI.Receive (This.SPIx.all, Value);
      Chip_Select_High (This);
   end Read;

   ----------------------------------
   -- Configure_External_Interrupt --
   ----------------------------------

   procedure Configure_External_Interrupt
     (This              : in out Three_Axis_Accelerometer;
      Device_Interrupt  : GPIO_Point;
      IRQ               : Interrupt_Id;
      Edge              : EXTI.External_Triggers;
      Enable_GPIO_Clock : access procedure)
   is
      pragma Unreferenced (This);
      Config : GPIO_Port_Configuration;
   begin
      Enable_GPIO_Clock.all;

      Config.Mode := Mode_In;
      Config.Speed := Speed_50MHz;
      Config.Resistors := Floating;

      Configure_IO (Device_Interrupt.Port.all, Device_Interrupt.Pin, Config);

      Configure_Trigger (Device_Interrupt.Port.all,
                         Device_Interrupt.Pin,
                         Edge);

      STM32F4.NVIC.Set_Priority (IRQ,
                                 Preempt_Priority => 16#F#,
                                 Subpriority      => 0);

      STM32F4.NVIC.Enable_Interrupt (IRQ);
   end Configure_External_Interrupt;

end STM32F4.LIS3DSH;
