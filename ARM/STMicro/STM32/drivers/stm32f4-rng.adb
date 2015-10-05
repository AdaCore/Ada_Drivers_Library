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
--   @file    stm32f4xx_hal_rng.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   RNG HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32F4.RCC;

package body STM32F4.RNG is

   ----------------------
   -- Enable_RNG_Clock --
   ----------------------

   procedure Enable_RNG_Clock renames STM32F4.RCC.RNG_Clock_Enable;

   ----------------
   -- Enable_RNG --
   ----------------

   procedure Enable_RNG is
   begin
      RNG.Control.Generator_Enabled := True;
   end Enable_RNG;

   -----------------
   -- Disable_RNG --
   -----------------

   procedure Disable_RNG is
   begin
      RNG.Control.Generator_Enabled := False;
   end Disable_RNG;

   -----------------
   -- RNG_Enabled --
   -----------------

   function RNG_Enabled return Boolean is
      (RNG.Control.Generator_Enabled);

   --------------------------
   -- Enable_RNG_Interrupt --
   --------------------------

   procedure Enable_RNG_Interrupt is
   begin
      RNG.Control.Interrupt_Enabled := True;
   end Enable_RNG_Interrupt;

   ---------------------------
   -- Disable_RNG_Interrupt --
   ---------------------------

   procedure Disable_RNG_Interrupt is
   begin
      RNG.Control.Interrupt_Enabled := False;
   end Disable_RNG_Interrupt;

   ---------------------------
   -- RNG_Interrupt_Enabled --
   ---------------------------

   function RNG_Interrupt_Enabled return Boolean is
      (RNG.Control.Interrupt_Enabled);

   ---------------------------
   -- RNG_Seed_Error_Status --
   ---------------------------

   function RNG_Seed_Error_Status return Boolean is
      (RNG.Status.SECS);

   ----------------------------
   -- RNG_Clock_Error_Status --
   ----------------------------

   function RNG_Clock_Error_Status return Boolean is
      (RNG.Status.CECS);

end STM32F4.RNG;
