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
--  @file    stm32f4xx_hal_dcmi.c                                           --
--  @author  MCD Application Team                                           --
--  @version V1.0.0                                                         --
--  @date    18-February-2014                                               --
--  @brief   Header file of DCMI HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

package body STM32.DCMI is

   procedure Set_Interrupt (Int : DCMI_Interrupts;
                            Set : Boolean);

   ------------------
   -- DCMI_Enabled --
   ------------------

   function DCMI_Enabled return Boolean is (DCMI_Periph.CR.ENABLE);

   -----------------
   -- Enable_DCMI --
   -----------------

   procedure Enable_DCMI is
   begin
      DCMI_Periph.CR.ENABLE := True;
   end Enable_DCMI;

   ------------------
   -- Disable_DCMI --
   ------------------

   procedure Disable_DCMI is
   begin
      DCMI_Periph.CR.ENABLE := False;
   end Disable_DCMI;

   -------------------
   -- Start_Capture --
   -------------------

   procedure Start_Capture (Mode : DCMI_Capture_Mode) is
   begin
      case Mode is
         when Continous =>
            DCMI_Periph.CR.CM := False;
         when Snapshot =>
            DCMI_Periph.CR.CM := True;
      end case;

      DCMI_Periph.CR.CAPTURE := True;
   end Start_Capture;

   ------------------
   -- Stop_Capture --
   ------------------

   procedure Stop_Capture is
   begin
      DCMI_Periph.CR.CAPTURE := False;
   end Stop_Capture;

   ------------------------
   -- Capture_In_Progess --
   ------------------------

   function Capture_In_Progess return Boolean is (DCMI_Periph.CR.CAPTURE);

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Data_Mode            : DCMI_Data_Mode;
                        Capture_Rate         : DCMI_Capture_Rate;
                        Vertical_Polarity    : DCMI_Polarity;
                        Horizontal_Polarity  : DCMI_Polarity;
                        Pixel_Clock_Polarity : DCMI_Polarity;
                        Hardware_Sync        : Boolean;
                        JPEG                 : Boolean)
   is
   begin
      DCMI_Periph.CR.EDM := (case Data_Mode is
                                when DCMI_8bit  => 2#00#,
                                when DCMI_10bit => 2#01#,
                                when DCMI_12bit => 2#10#,
                                when DCMI_14bit => 2#11#);

      DCMI_Periph.CR.FCRC := (case Capture_Rate is
                                 when Capture_All     => 2#00#,
                                 when Capture_Half    => 2#01#,
                                 when Capture_Quarter => 2#10#);

      DCMI_Periph.CR.VSPOL  := Vertical_Polarity    = Active_High;
      DCMI_Periph.CR.HSPOL  := Horizontal_Polarity  = Active_High;
      DCMI_Periph.CR.PCKPOL := Pixel_Clock_Polarity = Active_High;

      DCMI_Periph.CR.JPEG := JPEG;
      DCMI_Periph.CR.ESS := not Hardware_Sync;
   end Configure;

   ----------------------------------------
   -- Set_Software_Synchronization_Codes --
   ----------------------------------------

   procedure Set_Software_Synchronization_Codes
     (Frame_Start : Byte;
      Frame_End   : Byte;
      Line_Start  : Byte;
      Line_End    : Byte)
   is
   begin
      DCMI_Periph.ESCR.FSC := Frame_Start;
      DCMI_Periph.ESCR.LSC := Line_Start;
      DCMI_Periph.ESCR.FEC := Frame_End;
      DCMI_Periph.ESCR.LEC := Line_End;
   end Set_Software_Synchronization_Codes;

   ----------------------------------------
   -- Set_Software_Synchronization_Masks --
   ----------------------------------------

   procedure Set_Software_Synchronization_Masks
     (Frame_Start : Byte;
      Frame_End   : Byte;
      Line_Start  : Byte;
      Line_End    : Byte)
   is
   begin
      DCMI_Periph.ESUR.FSU := Frame_Start;
      DCMI_Periph.ESUR.LSU := Line_Start;
      DCMI_Periph.ESUR.FEU := Frame_End;
      DCMI_Periph.ESUR.LEU := Line_End;
   end Set_Software_Synchronization_Masks;

   ---------------------
   -- Set_Crop_Window --
   ---------------------

   procedure Set_Crop_Window
     (X      : UInt13;
      Y      : UInt14;
      Width  : UInt14;
      Height : UInt14)
   is
   begin
      DCMI_Periph.CWSTRT.VST := X;
      DCMI_Periph.CWSTRT.HOFFCNT := Y;
      DCMI_Periph.CWSIZE.CAPCNT := Width;
      DCMI_Periph.CWSIZE.VLINE := Height;
   end Set_Crop_Window;

   -----------------
   -- Enable_Crop --
   -----------------

   procedure Enable_Crop is
   begin
      DCMI_Periph.CR.CROP := True;
   end Enable_Crop;

   ------------------
   -- Disable_Crop --
   ------------------

   procedure Disable_Crop is
   begin
      DCMI_Periph.CR.CROP := False;
   end Disable_Crop;

   ----------
   -- Data --
   ----------

   function Data return Word is (DCMI_Periph.DR.Val);

   ----------
   -- Data --
   ----------

   function Data return DR_Byte_Field_Array is (DCMI_Periph.DR.Arr);

   ---------------------------
   -- Data_Register_Address --
   ---------------------------

   function Data_Register_Address return System.Address is
     (DCMI_Periph.DR'Address);

   --------------------
   -- FIFO_Not_Empty --
   --------------------

   function FIFO_Not_Empty return Boolean is
     (DCMI_Periph.SR.FNE);

   ------------------------
   -- VSYNC_Active_Frame --
   ------------------------

   function VSYNC_Active_Frame return Boolean is
     (not DCMI_Periph.SR.VSYNC);

   -----------------------
   -- HSYNC_Active_Line --
   -----------------------

   function HSYNC_Active_Line return Boolean is
     (not DCMI_Periph.SR.HSYNC);

   -------------------
   -- Set_Interrupt --
   -------------------

   procedure Set_Interrupt (Int : DCMI_Interrupts;
                            Set : Boolean)
   is
   begin
      case Int is
         when Line_Interrupt =>
            DCMI_Periph.IER.LINE_IE := Set;
         when VSYNC_Interrupt =>
            DCMI_Periph.IER.VSYNC_IE := Set;
         when SYNC_Error_Interrupt =>
            DCMI_Periph.IER.ERR_IE := Set;
         when Overrun_Interrupt =>
            DCMI_Periph.IER.OVR_IE := Set;
         when Frame_Interrupt =>
            DCMI_Periph.IER.FRAME_IE := Set;
      end case;
   end Set_Interrupt;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt (Int : DCMI_Interrupts) is
   begin
      Set_Interrupt (Int, True);
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt (Int : DCMI_Interrupts) is
   begin
      Set_Interrupt (Int, False);
   end Disable_Interrupt;

   ---------------------
   -- Clear_Interrupt --
   ---------------------

   procedure Clear_Interrupt (Int : DCMI_Interrupts) is
   begin
      case Int is
         when Line_Interrupt =>
            DCMI_Periph.ICR.LINE_ISC := True;
         when VSYNC_Interrupt =>
            DCMI_Periph.ICR.VSYNC_ISC := True;
         when SYNC_Error_Interrupt =>
            DCMI_Periph.ICR.ERR_ISC := True;
         when Overrun_Interrupt =>
            DCMI_Periph.ICR.OVR_ISC := True;
         when Frame_Interrupt =>
            DCMI_Periph.ICR.FRAME_ISC := True;
      end case;
   end Clear_Interrupt;

   --------------------------
   -- Raw_Interrupt_Status --
   --------------------------

   function Raw_Interrupt_Status (Int : DCMI_Interrupts) return Boolean is
   begin
      case Int is
         when Line_Interrupt =>
            return DCMI_Periph.RIS.LINE_RIS;
         when VSYNC_Interrupt =>
            return DCMI_Periph.RIS.VSYNC_RIS;
         when SYNC_Error_Interrupt =>
            return DCMI_Periph.RIS.ERR_RIS;
         when Overrun_Interrupt =>
            return DCMI_Periph.RIS.OVR_RIS;
         when Frame_Interrupt =>
            return DCMI_Periph.RIS.FRAME_RIS;
      end case;
   end Raw_Interrupt_Status;

   -----------------------------
   -- Masked_Interrupt_Status --
   -----------------------------

   function Masked_Interrupt_Status (Int : DCMI_Interrupts) return Boolean is
   begin
      case Int is
         when Line_Interrupt =>
            return DCMI_Periph.MIS.LINE_MIS;
         when VSYNC_Interrupt =>
            return DCMI_Periph.MIS.VSYNC_MIS;
         when SYNC_Error_Interrupt =>
            return DCMI_Periph.MIS.ERR_MIS;
         when Overrun_Interrupt =>
            return DCMI_Periph.MIS.OVR_MIS;
         when Frame_Interrupt =>
            return DCMI_Periph.MIS.FRAME_MIS;
      end case;
   end Masked_Interrupt_Status;

end STM32.DCMI;
