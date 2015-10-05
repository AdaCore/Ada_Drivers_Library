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
with STM32F4.LIS3DSH.IO;

package body STM32F4.LIS3DSH is

   Full_Scale_Selection_Mask : constant Byte := 2#0011_1000#;
   --  bits 3..5 of CTRL5

   Data_Rate_Selection_Mask : constant Byte := 2#0111_0000#;
   --  bits 4..6 of CTRL4

   ---------
   -- AIO --
   ---------

   package AIO renames STM32F4.LIS3DSH.IO;
   --  A convenient name for the accelerometer I/O package.

   ----------------
   -- Configured --
   ----------------

   function Configured (This : Three_Axis_Accelerometer) return Boolean is
     (This.Device_Configured);

   -----------------------
   -- Get_Accelerations --
   -----------------------

   procedure Get_Accelerations
     (This : Three_Axis_Accelerometer;
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
      AIO.Read (Buffer (0), OUT_X_L);
      AIO.Read (Buffer (1), OUT_X_H);
      AIO.Read (Buffer (2), OUT_Y_L);
      AIO.Read (Buffer (3), OUT_Y_H);
      AIO.Read (Buffer (4), OUT_Z_L);
      AIO.Read (Buffer (5), OUT_Z_H);

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
      AIO.Initialize;

      Temp := Output_DataRate'Enum_Rep or
              Axes_Enable'Enum_Rep     or
              SPI_Wire'Enum_Rep        or
              Self_Test'Enum_Rep       or
              Full_Scale'Enum_Rep      or
              Filter_BW'Enum_Rep;

      Value := Byte (Temp); -- the low byte of the half-word
      AIO.Write (Value, CTRL_REG4);

      Value := Byte (Shift_Right (Temp, 8)); -- the high byte
      AIO.Write (Value, CTRL_REG5);

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

   function Device_Id (This : Three_Axis_Accelerometer) return Byte is
      pragma Unreferenced (This);
      Response : Byte;
   begin
      AIO.Initialize;
      -- In addition to the configuration routine, we also initialize here
      -- because clients may call this function before doing any configuration
      -- setup...

      AIO.Read (Response, Who_Am_I);
      return Response;
   end Device_Id;

   ------------
   -- Reboot --
   ------------

   procedure Reboot (This : Three_Axis_Accelerometer) is
      pragma Unreferenced (This);
      Value : Byte;
      Force_Reboot : constant Byte := 2#1000_0000#;
   begin
      AIO.Initialize;
      -- In addition to the configuration routine, we also initialize here
      -- because clients may call this function before doing any configuration
      -- setup...

      AIO.Read (Value, CTRL_REG6);
      Value := Value or Force_Reboot;
      AIO.Write (Value, CTRL_REG6);
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
      pragma Unreferenced (This);
      CTRL : Byte;
   begin
      CTRL := Interrupt_Selection_Enable'Enum_Rep or
              Interrupt_Request'Enum_Rep          or
              Interrupt_Signal'Enum_Rep;

      AIO.Write (CTRL, CTRL_REG3);

      --  configure State Machine 1
      CTRL := State_Machine1_Enable'Enum_Rep or
              State_Machine1_Interrupt'Enum_Rep;

      AIO.Write (CTRL, CTRL_REG1);

      --  configure State Machine 2
      CTRL := State_Machine2_Enable'Enum_Rep or
              State_Machine2_Interrupt'Enum_Rep;

      AIO.Write (CTRL, CTRL_REG2);
   end Configure_Interrupts;

   -------------------------------
   -- Configure_Click_Interrupt --
   -------------------------------

   procedure Configure_Click_Interrupt (This : in out Three_Axis_Accelerometer) is
   begin
      AIO.Configure_Interrupt;

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
      AIO.Write (3, TIM2_1_L);
      AIO.Write (16#C8#, TIM1_1_L);
      AIO.Write (16#45#, THRS2_1);
      AIO.Write (16#FC#, MASK1_A);
      AIO.Write (16#A1#, SETT1);
      AIO.Write (16#1#, PR1);

      AIO.Write (16#1#, SETT2);

      --  configure State Machine 2 to detect single click
      AIO.Write (16#1#, ST2_1);
      AIO.Write (16#6#, ST2_2);
      AIO.Write (16#28#, ST2_3);
      AIO.Write (16#11#, ST2_4);
   end Configure_Click_Interrupt;

   -------------------
   -- Set_Low_Power --
   -------------------

   procedure Set_Low_Power
     (This : in out Three_Axis_Accelerometer;
      Mode : Data_Rate_Power_Mode_Selection)
   is
      pragma Unreferenced (This);
      Value : Byte;
   begin
      AIO.Read (Value, CTRL_REG4);
      Value := Value and (not Data_Rate_Selection_Mask); -- clear bits
      Value := Value or Mode'Enum_Rep;
      AIO.Write (Value, CTRL_REG4);
   end Set_Low_Power;

   -------------------
   -- Set_Data_Rate --
   -------------------

   procedure Set_Data_Rate
     (This     : in out Three_Axis_Accelerometer;
      DataRate : Data_Rate_Power_Mode_Selection)
   is
      pragma Unreferenced (This);
      Value : Byte;
   begin
      AIO.Read (Value, CTRL_REG4);
      Value := Value and (not Data_Rate_Selection_Mask); -- clear bits
      Value := Value or DataRate'Enum_Rep;
      AIO.Write (Value, CTRL_REG4);
   end Set_Data_Rate;

   --------------------
   -- Set_Full_Scale --
   --------------------

   procedure Set_Full_Scale
     (This : in out Three_Axis_Accelerometer;
      Scale : Full_Scale_Selection)
   is
      pragma Unreferenced (This);
      Value : Byte;
   begin
      AIO.Read (Value, CTRL_REG5);
      Value := Value and (not Full_Scale_Selection_Mask); -- clear bits
      Value := Value or Scale'Enum_Rep;
      AIO.Write (Value, CTRL_REG5);
   end Set_Full_Scale;

   -------------------
   -- As_Full_Scale --
   -------------------

   function As_Full_Scale is new Ada.Unchecked_Conversion
     (Source => Byte, Target => Full_Scale_Selection);

   --------------------------
   -- Selected_Sensitivity --
   --------------------------

   function Selected_Sensitivity (This : Three_Axis_Accelerometer) return Float is
      pragma Unreferenced (This);
      CTRL5 : Byte;
   begin
      AIO.Read (CTRL5, CTRL_REG5);
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

   function Temperature (This : Three_Axis_Accelerometer) return Byte is
      pragma Unreferenced (This);
      Result : Byte;
   begin
      AIO.Initialize;
      -- In addition to the configuration routine, we also initialize here
      -- because clients may call this function before doing any configuration
      -- setup...

      AIO.Read (Result, Out_T);
      return Result;
   end Temperature;

end STM32F4.LIS3DSH;
