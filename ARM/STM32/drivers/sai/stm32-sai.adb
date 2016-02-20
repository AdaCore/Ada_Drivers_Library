with Ada.Real_Time; use Ada.Real_Time;

with STM32.Device;  use STM32.Device;

with STM32_SVD.SAI; use STM32_SVD.SAI;
with STM32_SVD.RCC; use STM32_SVD.RCC;

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
      DR    : STM32_SVD.Word;
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

   function Get_Input_Clock (Periph : SAI_Controller) return Word;

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

   ---------------------
   -- Get_Input_Clock --
   ---------------------

   function Get_Input_Clock (Periph : SAI_Controller) return Word
   is
      Input_Selector  : UInt2;
      VCO_Input       : Word;
      SAI_First_Level : Word;
   begin
      if Periph'Address = SAI1_Base then
         Input_Selector := RCC_Periph.DKCFGR1.SAI1SEL;
      else
         Input_Selector := RCC_Periph.DKCFGR1.SAI2SEL;
      end if;

      --  This driver doesn't support external source clock
      if Input_Selector > 1 then
         raise Constraint_Error
           with "External PLL SAI source clock unsupported";
      end if;

      if RCC_Periph.PLLCFGR.PLLSRC = 0 then
         --  PLLSAI SRC is HSI
         VCO_Input := HSI_VALUE / Word (RCC_Periph.PLLCFGR.PLLM);
      else
         --  PLLSAI SRC is HSE
         VCO_Input := HSE_VALUE / Word (RCC_Periph.PLLCFGR.PLLM);
      end if;

      if Input_Selector = 0 then
         --  PLLSAI is the clock source

         --  VCO out = VCO in & PLLSAIN
         --  SAI firstlevel = VCO out / PLLSAIQ
         SAI_First_Level :=
           VCO_Input * Word (RCC_Periph.PLLSAICFGR.PLLSAIN) /
           Word (RCC_Periph.PLLSAICFGR.PLLSAIQ);

         --  SAI frequency is SAI First level / PLLSAIDIVQ
         return SAI_First_Level / Word (RCC_Periph.DKCFGR1.PLLSAIDIVQ);

      else
         --  PLLI2S as clock source
         SAI_First_Level :=
           VCO_Input * Word (RCC_Periph.PLLI2SCFGR.PLLI2SN) /
           Word (RCC_Periph.PLLI2SCFGR.PLLI2SQ);
         --  SAI frequency is SAI First level / PLLI2SDIVQ
         return SAI_First_Level / Word (RCC_Periph.DKCFGR1.PLLI2SDIV + 1);
      end if;
   end Get_Input_Clock;

   ------------------
   -- Deinitialize --
   ------------------

   procedure Deinitialize
     (Periph : SAI_Controller;
      Block  : SAI_Block)
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (Periph, Block);
      Start      : Time;
   begin
      --  Disable SAI
      Block_Regs.CR1.SAIAEN := 0;

      Start := Clock;
      while Block_Regs.CR1.SAIAEN /= 0 loop
         if Start + To_Time_Span (Duration (1.0)) < Clock then
            raise Constraint_Error with "Cannot reset the SAI2 peripheral";
         end if;
      end loop;

      --  Reset the SAI block interrupts
      Block_Regs.IM := (others => <>);
      Block_Regs.CLRFR :=
        (Reserved_3_3 => 0,
         Reserved_7_31 => 0,
         others        => 1);
      --  Flush the FIFO
      Block_Regs.CR2.FFLUS := 1;
   end Deinitialize;

   ------------
   -- Enable --
   ------------

   procedure Enable
     (Periph : SAI_Controller;
      Block  : SAI_Block)
   is
      Block_Regs : constant Block_Registers_Access :=
                     Get_Block (Periph, Block);
   begin
      Block_Regs.CR1.SAIAEN := 1;
   end Enable;

   ---------------------------
   -- Configure_Audio_Block --
   ---------------------------

   procedure Configure_Audio_Block
     (Periph          : SAI_Controller;
      Block           : SAI_Block;
      Frequency       : Audio_Frequency;
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
      Block_Reg : constant Block_Registers_Access := Get_Block (Periph, Block);
      Freq      : Word;
      Tmp_Clock : Word;
      Mckdiv    : Word;
   begin
      Deinitialize (Periph, Block);

      --  Configure Master Clock using the following formula :
      --  MCLK_x = SAI_CK_x / (MCKDIV[3:0] * 2) with MCLK_x = 256 * FS
      --  FS = SAI_CK_x / (MCKDIV[3:0] * 2) * 256
      --  MCKDIV[3:0] = SAI_CK_x / FS * 512
      Freq := Get_Input_Clock (Periph);

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
         LSBFIRST => SAI_Endianness'Enum_Rep (Endianness),
         CKSTR    => SAI_Clock_Strobing_Edge'Enum_Rep (Clock_Strobing),
         SYNCEN   => SAI_Synchronization'Enum_Rep (Synchronization),
         MONO     => SAI_Mono_Stereo_Mode'Enum_Rep (Stereo_Mode),
         OutDri   => SAI_Output_Drive'Enum_Rep (Output_Drive),
         SAIAEN   => 0,
         DMAEN    => 0,
         NODIV    => Boolean'Enum_Rep (not MCD_Enabled),
         MCJDIV   => UInt4 (Mckdiv),
         others   => <>);
      Block_Reg.CR2.FTH  := SAI_FIFO_Threshold'Enum_Rep (FIFO_Threshold);
      Block_Reg.CR2.TRIS := SAI_Tristate_Management'Enum_Rep (Tristate_Mgt);
      Block_Reg.CR2.COMP := SAI_Companding_Mode'Enum_Rep (Companding_Mode);
   end Configure_Audio_Block;

   ---------------------------
   -- Configure_Block_Frame --
   ---------------------------

   procedure Configure_Block_Frame
     (Periph       : SAI_Controller;
      Block        : SAI_Block;
      Frame_Length : Byte;
      Frame_Active : UInt7;
      Frame_Sync   : SAI_Frame_Synchronization;
      FS_Polarity  : SAI_Frame_Sync_Polarity;
      FS_Offset    : SAI_Frame_Sync_Offset)
   is
      Block_Reg : constant Block_Registers_Access := Get_Block (Periph, Block);
   begin
      Block_Reg.FRCR :=
        (FRL   => Frame_Length,
         FSALL => Frame_Active,
         FSDEF => SAI_Frame_Synchronization'Enum_Rep (Frame_Sync),
         FSPOL => SAI_Frame_Sync_Polarity'Enum_Rep (FS_Polarity),
         FSOFF => SAI_Frame_Sync_Offset'Enum_Rep (FS_Offset),
         others => <>);
   end Configure_Block_Frame;

   --------------------------
   -- Configure_Block_Slot --
   --------------------------

   procedure Configure_Block_Slot
     (Periph           : SAI_Controller;
      Block            : SAI_Block;
      First_Bit_Offset : UInt5;
      Slot_Size        : SAI_Slot_Size;
      Number_Of_Slots  : UInt4;
      Enabled_Slots    : SAI_Slots)
   is
      Block_Reg : constant Block_Registers_Access := Get_Block (Periph, Block);
   begin
      Block_Reg.SLOTR :=
        (FBOFF  => First_Bit_Offset,
         SLOTSZ => SAI_Slot_Size'Enum_Rep (Slot_Size),
         NBSLOT => Number_Of_Slots,
         SLOTEN => Short (Enabled_Slots),
         others => <>);
   end Configure_Block_Slot;

end STM32.SAI;
