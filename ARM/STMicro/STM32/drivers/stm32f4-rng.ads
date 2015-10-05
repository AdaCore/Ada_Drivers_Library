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
--   @file    stm32f4xx_hal_rng.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of RNG HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides the API for the random number generator on the STM32F4
--  (ARM Cortex M4F) microcontrollers from ST Microelectronics.
--
--  See the child packages for routines to initialze the generator and acquire
--  numbers, using either polling or interrupts.

package STM32F4.RNG is

   procedure Enable_RNG;

   procedure Disable_RNG;

   procedure Enable_RNG_Clock;

   function RNG_Enabled return Boolean;

   procedure Enable_RNG_Interrupt;

   procedure Disable_RNG_Interrupt;

   function RNG_Interrupt_Enabled return Boolean;

   function RNG_Seed_Error_Status return Boolean;

   function RNG_Clock_Error_Status return Boolean;

private

   type RNG_Control_Register is record
      Reserved1         : Bits_28;
      Interrupt_Enabled : Boolean;
      Generator_Enabled : Boolean;
      Reserved2         : Bits_2;
   end record
     with Volatile_Full_Access, Size => 32;

   for RNG_Control_Register use record
      Reserved1         at 0 range 4 .. 31;
      Interrupt_Enabled at 0 range 3 .. 3;
      Generator_Enabled at 0 range 2 .. 2;
      Reserved2         at 0 range 0 .. 1;
   end record;

   type RNG_Status_Register is record
      Reserved1   : Bits_25;
      Seed_Error  : Boolean;
      Clock_Error : Boolean;
      Reserved2   : Bits_2;
      SECS        : Boolean;  -- Seed Error Current Status
      CECS        : Boolean;  -- Clock Error Current Status
      Data_Ready  : Boolean;
   end record
     with Volatile_Full_Access, Size => 32;

   for RNG_Status_Register use record
      Reserved1   at 0 range 7 .. 31;
      Seed_Error  at 0 range 6 .. 6;
      Clock_Error at 0 range 5 .. 5;
      Reserved2   at 0 range 3 .. 4;
      SECS        at 0 range 2 .. 2;
      CECS        at 0 range 1 .. 1;
      Data_Ready  at 0 range 0 .. 0;
   end record;

   type RNG_Data_Register is mod 2**32
     with Atomic, Size => 32;

   type RNG_Registers is record
      Control : RNG_Control_Register;
      Status  : RNG_Status_Register;
      Data    : RNG_Data_Register;
   end record
     with Volatile, Size => 96;

   RNG : RNG_Registers
     with Address => RNG_Base;

end STM32F4.RNG;
