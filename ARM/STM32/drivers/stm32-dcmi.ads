------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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
--  @file    stm32f4xx_hal_dcmi.h                                           --
--  @author  MCD Application Team                                           --
--  @version V1.0.0                                                         --
--  @date    18-February-2014                                               --
--  @brief   Header file of DCMI HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32_SVD.DCMI; use STM32_SVD.DCMI;
with System;

package STM32.DCMI is

   function DCMI_Enabled return Boolean;
   procedure Enable_DCMI
     with Post => DCMI_Enabled;
   procedure Disable_DCMI
     with Post => not DCMI_Enabled;

   type DCMI_Capture_Mode is (Continous, Snapshot);

   procedure Start_Capture (Mode : DCMI_Capture_Mode)
     with Pre => DCMI_Enabled;
   procedure Stop_Capture;
   function Capture_In_Progess return Boolean;

   type DCMI_Data_Mode is (DCMI_8bit,
                           DCMI_10bit,
                           DCMI_12bit,
                           DCMI_14bit);

   type DCMI_Capture_Rate is (Capture_All,
                              Capture_Half,
                              Capture_Quarter);

   type DCMI_Polarity is (Active_Low, Active_High);

   procedure Configure (Data_Mode            : DCMI_Data_Mode;
                        Capture_Rate         : DCMI_Capture_Rate;
                        Vertical_Polarity    : DCMI_Polarity;
                        Horizontal_Polarity  : DCMI_Polarity;
                        Pixel_Clock_Polarity : DCMI_Polarity;
                        Hardware_Sync        : Boolean;
                        JPEG                 : Boolean)
     with Pre => (if JPEG then not Hardware_Sync);

   procedure Set_Software_Synchronization_Codes
     (Frame_Start : Byte;
      Frame_End   : Byte;
      Line_Start  : Byte;
      Line_End    : Byte);

   procedure Set_Software_Synchronization_Masks
     (Frame_Start : Byte;
      Frame_End   : Byte;
      Line_Start  : Byte;
      Line_End    : Byte);

   procedure Set_Crop_Window
     (X      : UInt13;
      Y      : UInt14;
      Width  : UInt14;
      Height : UInt14);

   procedure Enable_Crop;
   procedure Disable_Crop;

   function Data return Word;
   function Data return DR_Byte_Field_Array;

   function Data_Register_Address return System.Address;
   --  For DMA transfer

   function FIFO_Not_Empty return Boolean;
   function FIFO_Empty return Boolean is (not FIFO_Not_Empty);
   function VSYNC_Active_Frame return Boolean;
   function HSYNC_Active_Line return Boolean;

   type DCMI_Interrupts is (Line_Interrupt,
                            VSYNC_Interrupt,
                            SYNC_Error_Interrupt,
                            Overrun_Interrupt,
                            Frame_Interrupt);

   procedure Enable_Interrupt (Int : DCMI_Interrupts);
   procedure Disable_Interrupt (Int : DCMI_Interrupts);
   procedure Clear_Interrupt (Int : DCMI_Interrupts);
   function Raw_Interrupt_Status (Int : DCMI_Interrupts) return Boolean;
   function Masked_Interrupt_Status (Int : DCMI_Interrupts) return Boolean;

end STM32.DCMI;
