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

with Cortex_M_SVD.NVIC; use Cortex_M_SVD.NVIC;

package body Cortex_M.NVIC is


   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (IRQn     : Interrupt_ID;
      Priority : Interrupt_Priority)
   is
      type As_Array (As_Arr : Boolean := True) is record
         case As_Arr is
            when True =>
               Arr : UInt8_Array (0 .. 3);
            when False =>
               IPR : UInt32;
         end case;
      end record with Unchecked_Union, Pack, Size => 32;

      IPR_Index : constant Natural := IRQn / 4;
      IP_Index  : constant Natural := IRQn mod 4;
      IPR       : As_Array;
   begin

      IPR.IPR := NVIC_Periph.NVIC_IPR (IPR_Index);

      IPR.Arr (IP_Index) := Priority;

      NVIC_Periph.NVIC_IPR (IPR_Index) := IPR.IPR;
   end Set_Priority;

   -------------
   -- Enable_ --
   -------------

   procedure Enable (IRQn : Interrupt_ID) is
   begin
      NVIC_Periph.NVIC_ISER := Shift_Left (1, IRQn);
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (IRQn : Interrupt_ID) is
   begin
      NVIC_Periph.NVIC_ICER := Shift_Left (1, IRQn);
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (IRQn : Interrupt_ID) return Boolean is
   begin
      return ((NVIC_Periph.NVIC_ISER and Shift_Left (1, IRQn)) /= 0);
   end Enabled;

   -------------
   -- Pending --
   -------------

   function Pending (IRQn : Interrupt_ID) return Boolean is
   begin
      return ((NVIC_Periph.NVIC_ISPR and Shift_Left (1, IRQn)) /= 0);
   end Pending;

   -----------------
   -- Set_Pending --
   -----------------

   procedure Set_Pending (IRQn : Interrupt_ID) is
   begin
      NVIC_Periph.NVIC_ISPR := Shift_Left (1, IRQn);
   end Set_Pending;

   -------------------
   -- Clear_Pending --
   -------------------

   procedure Clear_Pending (IRQn : Interrupt_ID) is
   begin
      NVIC_Periph.NVIC_ICPR := Shift_Left (1, IRQn);
   end Clear_Pending;

end Cortex_M.NVIC;

