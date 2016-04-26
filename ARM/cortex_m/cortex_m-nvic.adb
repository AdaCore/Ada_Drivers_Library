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
--   @file    stm32f4xx_hal_cortex.c                                        --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   CORTEX HAL module driver.                                     --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Memory_Barriers;

package body Cortex_M.NVIC is

   -----------------------
   -- Priority_Grouping --
   -----------------------

   function Priority_Grouping return Word is
   begin
      return Shift_Right (SCB.AIRCR and SCB_AIRCR_PRIGROUP_Mask,
                          SCB_AIRCR_PRIGROUP_Pos);
   end Priority_Grouping;

   ---------------------------
   -- Set_Priority_Grouping --
   ---------------------------

   procedure Set_Priority_Grouping (Priority_Group : Word) is
      Reg_Value : Word;
      PriorityGroupTmp : constant Word := Priority_Group and 16#07#;
      Key              : constant := 16#5FA#;
   begin
      Reg_Value := SCB.AIRCR;
      Reg_Value := Reg_Value and
        (not (SCB_AIRCR_VECTKEY_Mask or SCB_AIRCR_PRIGROUP_Mask));
      Reg_Value := Reg_Value or
        Shift_Left (Key, SCB_AIRCR_VECTKEY_Pos) or
        Shift_Left (PriorityGroupTmp, 8);
      SCB.AIRCR := Reg_Value;
   end Set_Priority_Grouping;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (IRQn     : Interrupt_ID;
      Priority : Word)
   is
      Index : constant Natural := Integer (IRQn);
      Value : constant Word :=
        Shift_Left (Priority, 8 - NVIC_PRIO_BITS) and 16#FF#;
   begin
      --  IRQ numbers are never less than 0 in the current definition, hence
      --  the code is different from that in the CMSIS.
      NVIC.IP (Index) := Byte (Value);
   end Set_Priority;

   ----------------------
   -- Encoded_Priority --
   ----------------------

   function Encoded_Priority
     (Priority_Group : Word;  Preempt_Priority : Word;  Subpriority : Word)
      return Word
   is
      PriorityGroupTmp    : constant Word := Priority_Group and 16#07#;
      PreemptPriorityBits : Word;
      SubPriorityBits     : Word;
      Temp1 : Word;
      Temp2 : Word;
      Temp3 : Word;
      Temp4 : Word;
      Temp5 : Word;
   begin
      if (7 - PriorityGroupTmp) > NVIC_PRIO_BITS then
         PreemptPriorityBits := NVIC_PRIO_BITS;
      else
         PreemptPriorityBits := 7 - PriorityGroupTmp;
      end if;

      if (PriorityGroupTmp + NVIC_PRIO_BITS) < 7 then
         SubPriorityBits := 0;
      else
         SubPriorityBits := PriorityGroupTmp - 7 + NVIC_PRIO_BITS;
      end if;

      Temp1 := Shift_Left (1, Integer (PreemptPriorityBits)) - 1;
      Temp2 := Preempt_Priority and Temp1;
      Temp3 := Shift_Left (Temp2, Integer (SubPriorityBits));

      Temp4 := Shift_Left (1, Integer (SubPriorityBits)) - 1;
      Temp5 := Subpriority and Temp4;

      return Temp3 or Temp5;
   end Encoded_Priority;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (IRQn             : Interrupt_ID;
      Preempt_Priority : Word;
      Subpriority      : Word)
   is
      Priority_Group : constant Word := Priority_Grouping;
   begin
      Set_Priority
        (IRQn,
         Encoded_Priority (Priority_Group, Preempt_Priority, Subpriority));
   end Set_Priority;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt (IRQn : Interrupt_ID) is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural :=
        Integer (Shift_Right (IRQn_As_Word, 5));
   begin
      NVIC.ISER (Index) := Shift_Left (IRQn_As_Word and 16#1F#, 1);
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt (IRQn : Interrupt_ID) is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural :=
        Integer (Shift_Right (IRQn_As_Word, 5));
   begin
      --  NVIC->ICER[((uint32_t)(IRQn)>>5)] = (1 << ((uint32_t)(IRQn) & 0x1F));
      NVIC.ICER (Index) := Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   end Disable_Interrupt;

   ------------
   -- Active --
   ------------

   function Active (IRQn : Interrupt_ID) return Boolean is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural :=
        Integer (Shift_Right (IRQn_As_Word, 5));
      Value        : constant Word :=
        Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   begin
      return (NVIC.IABR (Index) and Value) /= 0;
   end Active;

   -------------
   -- Pending --
   -------------

   function Pending (IRQn : Interrupt_ID) return Boolean is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural :=
        Integer (Shift_Right (IRQn_As_Word, 5));
      Value        : constant Word :=
        Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   begin
      return (NVIC.ISPR (Index) and Value) /= 0;
   end Pending;

   -----------------
   -- Set_Pending --
   -----------------

   procedure Set_Pending (IRQn : Interrupt_ID) is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural :=
        Integer (Shift_Right (IRQn_As_Word, 5));
   begin
      NVIC.ISPR (Index) := Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   end Set_Pending;

   -------------------
   -- Clear_Pending --
   -------------------

   procedure Clear_Pending (IRQn : Interrupt_ID) is
      IRQn_As_Word : constant Word := Word (IRQn);
      Index        : constant Natural :=
        Integer (Shift_Right (IRQn_As_Word, 5));
   begin
      NVIC.ICPR (Index) := Shift_Left (1, Integer (IRQn_As_Word and 16#1F#));
   end Clear_Pending;

   ------------------
   -- Reset_System --
   ------------------

   procedure Reset_System is
      Key : constant := 16#05FA#;  --  required for write to be accepted
      use Memory_Barriers;
   begin
      --  Ensure all outstanding memory accesses including
      --  buffered writes are completed
      Data_Synchronization_Barrier;

      SCB.AIRCR := Shift_Left (Key, SCB_AIRCR_VECTKEY_Pos) or
        -- keep priority group unchanged
        (SCB.AIRCR and SCB_AIRCR_PRIGROUP_Mask)  or
        SCB_AIRCR_SYSRESETREQ_Mask;

      --  TODO: why is all code from here onward in the CMSIS???

      Data_Synchronization_Barrier;

      --  wait until reset
      pragma Warnings (Off);
      <<spin>> goto spin;
      pragma Warnings (On);
   end Reset_System;

end Cortex_M.NVIC;

