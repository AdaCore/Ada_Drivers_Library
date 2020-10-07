------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2020, AdaCore                         --
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
--   @file    stm32f4xx_hal_cortex.h                                        --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of CORTEX HAL module.                             --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides Nested Vector interrupt Controller definitions for the
--  ARM Cortex M0 microcontrollers from ST Microelectronics.

with HAL;                  use HAL;

package Cortex_M.NVIC is  -- the Nested Vectored Interrupt Controller

   NVIC_PRIO_BITS : constant := 2;
   --  All Cortex M0 parts have 2 bit priority mask

   type Interrupt_ID is new Natural range 0 .. 31;
   type Interrupt_Priority is new UInt8 range 0 .. (2**NVIC_PRIO_BITS - 1);

   procedure Set_Priority
     (IRQn     : Interrupt_ID;
      Priority : Interrupt_Priority) with Inline;

   procedure Enable_Interrupt (IRQn : Interrupt_ID) with Inline;

   procedure Disable_Interrupt (IRQn : Interrupt_ID) with Inline;

   function Enabled (IRQn : Interrupt_ID) return Boolean with Inline;

   function Pending (IRQn : Interrupt_ID) return Boolean with Inline;

   procedure Set_Pending (IRQn : Interrupt_ID) with Inline;

   procedure Clear_Pending (IRQn : Interrupt_ID) with Inline;

end Cortex_M.NVIC;
