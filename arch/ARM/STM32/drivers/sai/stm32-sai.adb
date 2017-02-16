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
--   @file    stm32f7xx_hal_sai.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.0.2                                                        --
--   @date    21-September-2015                                             --
--   @brief   This file provides firmware functions to manage the following --
--            functionalities of the Serial Audio Interface (SAI)           --
--            peripheral:                                                   --
--             + Initialization/de-initialization functions                 --
--             + I/O operation functions                                    --
--             + Peripheral Control functions                               --
--             + Peripheral State functions                                 --
--                                                                          --
--   COPYRIGHT(c) 2015 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Real_Time;             use Ada.Real_Time;

with STM32.Device;              use STM32.Device;

with STM32_SVD;                 use STM32_SVD;
with STM32_SVD.SAI;             use STM32_SVD.SAI;
with STM32_SVD.RCC;             use STM32_SVD.RCC;

package body STM32.SAI is

   type Block_Registers is record
      --  AConfiguration register 1
      CR1   : ACR1_Register;
      --  AConfiguration register 2
      CR2   : ACR2_Register;
      --  AFRCR
      FRCR  : AFRCR_Register;
      --  ASlot register
      SLOTR : ASLOTR_Register;
      --  AInterrupt mask register2
      IM    : AIM_Register;
      --  AStatus register
      SR    : ASR_Register;
      --  AClear flag register
      CLRFR : ACLRFR_Register;
      --  AData register
      DR    : UInt32;
   end record with Volatile;
   for Block_Registers use record
      CR1   at 0 range 0 .. 31;
      CR2   at 4 range 0 .. 31;
      FRCR  at 8 range 0 .. 31;
      SLOTR at 12 range 0 .. 31;
      IM    at 16 range 0 .. 31;
      SR    at 20 range 0 .. 31;
      CLRFR at 24 range 0 .. 31;
      DR    at 28 range 0 .. 31;
   end record;
   type Block_Registers_Access is access all Block_Registers;

   function Get_Block
     (Periph : SAI_Controller;
      Block  : SAI_Block) return Block_Registers_Access;

   ---------------
   -- Get_Block --
   ---------------

   function Get_Block
     (Periph : SAI_Controller;
      Block  : SAI_Block) return Block_Registers_Access
   is
      BlockA : aliased Block_Registers
        with Volatile, Import, Address => Periph.ACR1'Address;
      BlockB : aliased Block_Registers
        with Volatile, Import, Address => Periph.BCR1'Address;
   begin
      case Block is
         when Block_A =>
            return BlockA'Unchecked_Access;
         when Block_B =>
            return BlockB'Unchecked_Access;
      end case;
   end Get_Block;

   ------------------
   -- Deinitialize --
   ------------------

   procedure Deinitialize
     (This : SAI_Controller;
      Block  : SAI_Block)
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (This, Block);
      Start      : Time;
   begin
      --  Disable SAI
      Block_Regs.CR1.SAIAEN := False;

      Start := Clock;
      while Block_Regs.CR1.SAIAEN loop
         if Start + Seconds (1) < Clock then
            raise Constraint_Error with "Cannot reset the SAI peripheral";
         end if;
      end loop;

      --  Reset the SAI block interrupts
      Block_Regs.IM := (others => <>);
      Block_Regs.CLRFR :=
        (Reserved_3_3 => 0,
         Reserved_7_31 => 0,
         others        => True);
      --  Flush the FIFO
      Block_Regs.CR2.FFLUS := True;
   end Deinitialize;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : SAI_Controller;
                     Block  : SAI_Block) return Boolean
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (This, Block);
   begin
      return Block_Regs.CR1.SAIAEN;
   end Enabled;

   ------------
   -- Enable --
   ------------

   procedure Enable
     (This : SAI_Controller;
      Block  : SAI_Block)
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (This, Block);
   begin
      Block_Regs.CR1.SAIAEN := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (This : SAI_Controller;
      Block  : SAI_Block)
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (This, Block);
   begin
      Block_Regs.CR1.SAIAEN := False;
   end Disable;

   ----------------
   -- Enable_DMA --
   ----------------

   procedure Enable_DMA
     (This : SAI_Controller;
      Block  : SAI_Block)
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (This, Block);
   begin
      Block_Regs.CR1.DMAEN := True;
   end Enable_DMA;

   ---------------
   -- DMA_Pause --
   ---------------

   procedure DMA_Pause
     (This  : SAI_Controller;
      Block : SAI_Block)
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (This, Block);
   begin
      Block_Regs.CR1.DMAEN := False;
   end DMA_Pause;

   ----------------
   -- DMA_Resume --
   ----------------

   procedure DMA_Resume
     (This  : SAI_Controller;
      Block : SAI_Block)
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (This, Block);
   begin
      Block_Regs.CR1.DMAEN := True;

      if not Block_Regs.CR1.SAIAEN then
         Enable (This, Block);
      end if;
   end DMA_Resume;

   --------------
   -- DMA_Stop --
   --------------

   procedure DMA_Stop
     (This  : SAI_Controller;
      Block : SAI_Block)
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (This, Block);
   begin
      Block_Regs.CR1.DMAEN := False;
      Disable (This, Block);
   end DMA_Stop;

   ---------------------------
   -- Configure_Audio_Block --
   ---------------------------

   procedure Configure_Audio_Block
     (This          : SAI_Controller;
      Block           : SAI_Block;
      Frequency       : SAI_Audio_Frequency;
      Stereo_Mode     : SAI_Mono_Stereo_Mode;
      Mode            : SAI_Audio_Mode;
      MCD_Enabled     : Boolean;
      Protocol        : SAI_Protocol_Configuration;
      Data_Size       : SAI_Data_Size;
      Endianness      : SAI_Endianness;
      Clock_Strobing  : SAI_Clock_Strobing_Edge;
      Synchronization : SAI_Synchronization;
      Output_Drive    : SAI_Output_Drive;
      FIFO_Threshold  : SAI_FIFO_Threshold;
      Tristate_Mgt    : SAI_Tristate_Management := SD_Line_Driven;
      Companding_Mode : SAI_Companding_Mode := No_Companding)
   is
      Block_Reg : constant Block_Registers_Access := Get_Block (This, Block);
      Freq      : UInt32;
      Tmp_Clock : UInt32;
      Mckdiv    : UInt32;
   begin
      Deinitialize (This, Block);

      --  Configure Master Clock using the following formula :
      --  MCLK_x = SAI_CK_x / (MCKDIV[3:0] * 2) with MCLK_x = 256 * FS
      --  FS = SAI_CK_x / (MCKDIV[3:0] * 2) * 256
      --  MCKDIV[3:0] = SAI_CK_x / FS * 512
      Freq := Get_Input_Clock (This);

      --  Calculate *10 to keep some precision
      Tmp_Clock := Freq * 10 / (Frequency * 512);
      Mckdiv := Tmp_Clock / 10;

      --  Round the result if needed
      if (Tmp_Clock mod 10) > 8 then
         Mckdiv := Mckdiv + 1;
      end if;

      Block_Reg.CR1 :=
        (MODE     => SAI_Audio_Mode'Enum_Rep (Mode),
         PRTCFG   => SAI_Protocol_Configuration'Enum_Rep (Protocol),
         DS       => SAI_Data_Size'Enum_Rep (Data_Size),
         LSBFIRST => Endianness = Data_LSB_First,
         CKSTR    => Clock_Strobing = Clock_Strobing_Rising_Edge,
         SYNCEN   => SAI_Synchronization'Enum_Rep (Synchronization),
         MONO     => Stereo_Mode = Mono,
         OutDri   => Output_Drive = Drive_Immediate,
         SAIAEN   => False,
         DMAEN    => False,
         NODIV    => not MCD_Enabled,
         MCJDIV   => UInt4 (Mckdiv),
         others   => <>);
      Block_Reg.CR2.FTH   := SAI_FIFO_Threshold'Enum_Rep (FIFO_Threshold);
      Block_Reg.CR2.FFLUS := False;
      Block_Reg.CR2.TRIS  := Tristate_Mgt = SD_Line_Released;
      Block_Reg.CR2.COMP  := SAI_Companding_Mode'Enum_Rep (Companding_Mode);
   end Configure_Audio_Block;

   ---------------------------
   -- Configure_Block_Frame --
   ---------------------------

   procedure Configure_Block_Frame
     (This       : SAI_Controller;
      Block        : SAI_Block;
      Frame_Length : UInt8;
      Frame_Active : UInt7;
      Frame_Sync   : SAI_Frame_Synchronization;
      FS_Polarity  : SAI_Frame_Sync_Polarity;
      FS_Offset    : SAI_Frame_Sync_Offset)
   is
      Block_Reg : constant Block_Registers_Access := Get_Block (This, Block);
   begin
      Block_Reg.FRCR :=
        (FRL   => Frame_Length - 1,
         FSALL => Frame_Active - 1,
         FSDEF => Frame_Sync = FS_Frame_And_Channel_Identification,
         FSPOL => FS_Polarity = FS_Active_High,
         FSOFF => FS_Offset = Before_First_Bit,
         others => <>);
   end Configure_Block_Frame;

   --------------------------
   -- Configure_Block_Slot --
   --------------------------

   procedure Configure_Block_Slot
     (This           : SAI_Controller;
      Block            : SAI_Block;
      First_Bit_Offset : UInt5;
      Slot_Size        : SAI_Slot_Size;
      Number_Of_Slots  : Slots_Number;
      Enabled_Slots    : SAI_Slots)
   is
      Block_Reg : constant Block_Registers_Access := Get_Block (This, Block);
   begin
      Block_Reg.SLOTR :=
        (FBOFF  => First_Bit_Offset,
         SLOTSZ => SAI_Slot_Size'Enum_Rep (Slot_Size),
         NBSLOT => UInt4 (Number_Of_Slots - 1),
         SLOTEN => UInt16 (Enabled_Slots),
         others => <>);
   end Configure_Block_Slot;

end STM32.SAI;
