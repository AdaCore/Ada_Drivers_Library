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
--  STM32F4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

with System;
with HAL;            use HAL;

package Cortex_M.NVIC is  -- the Nested Vectored Interrupt Controller

   type Interrupt_ID is new Natural range 0 .. 240;
   type Interrupt_Priority is new UInt32;

   --  0 bits for pre-emption priority;  4 bits for subpriority
   Priority_Group_0 : constant UInt32 := 16#00000007#;

   --  1 bits for pre-emption priority;  3 bits for subpriority
   Priority_Group_1 : constant UInt32 := 16#00000006#;

   --  2 bits for pre-emption priority;  2 bits for subpriority
   Priority_Group_2 : constant UInt32 := 16#00000005#;

   --  3 bits for pre-emption priority;  1 bits for subpriority
   Priority_Group_3 : constant UInt32 := 16#00000004#;

   --  4 bits for pre-emption priority;  0 bits for subpriority
   Priority_Group_4 : constant UInt32 := 16#00000003#;


   procedure Set_Priority_Grouping (Priority_Group : Interrupt_Priority)
     with Inline;

   function Priority_Grouping return Interrupt_Priority
     with Inline;

   procedure Set_Priority
     (IRQn     : Interrupt_ID;
      Priority : Interrupt_Priority) with Inline;

   function Encoded_Priority
     (Priority_Group   : Interrupt_Priority;
      Preempt_Priority : Interrupt_Priority;
      Subpriority      : Interrupt_Priority)
      return Interrupt_Priority
     with Inline;

   procedure Set_Priority
     (IRQn             : Interrupt_ID;
      Preempt_Priority : Interrupt_Priority;
      Subpriority      : Interrupt_Priority) with Inline;
   --  A convenience routine that first encodes (Priority_Grouping(),
   --  Preempt_Priority, and Subpriority), and then calls the other
   --  Set_Priority with the resulting encoding for the Priority argument.

   procedure Enable_Interrupt (IRQn : Interrupt_ID) with Inline;

   procedure Disable_Interrupt (IRQn : Interrupt_ID) with Inline;

   function Active (IRQn : Interrupt_ID) return Boolean with Inline;

   function Pending (IRQn : Interrupt_ID) return Boolean with Inline;

   procedure Set_Pending (IRQn : Interrupt_ID) with Inline;

   procedure Clear_Pending (IRQn : Interrupt_ID) with Inline;

   procedure Reset_System;

private

   type Words is array (Natural range <>) of UInt32;
   type UInt8s is array (Natural range <>) of UInt8;


   type Nested_Vectored_Interrupt_Controller is record
      ISER      : Words (0 .. 7);
      --  Interrupt Set Enable Register
      Reserved0 : Words (0 .. 23);
      ICER      : Words (0 .. 7);
      --  Interrupt Clear Enable Register
      Reserved1 : Words (0 .. 23);
      ISPR      : Words (0 .. 7);
      --  Interrupt Set Pending Register
      Reserved2 : Words (0 .. 23);
      ICPR      : Words (0 .. 7);
      --  Interrupt Clear Pending Register
      Reserved3 : Words (0 .. 23);
      IABR      : Words (0 .. 7);
      --  Interrupt Active Bit Register
      Reserved4 : Words (0 .. 55);
      IP        : UInt8s (0 .. 239);
      --  Interrupt Priority Register
      Reserved5 : Words (0 .. 643);
      STIR      : UInt32;
      --  Software Trigger Interrupt Register (write-only)
   end record
     with Volatile;

   for Nested_Vectored_Interrupt_Controller use record
      ISER      at 0    range 0 .. 255;    -- 32 UInt8s
      Reserved0 at 32   range 0 .. 767;    -- 96 UInt8s
      ICER      at 128  range 0 .. 255;    -- 32 UInt8s
      Reserved1 at 160  range 0 .. 767;    -- 96 UInt8s
      ISPR      at 256  range 0 .. 255;    -- 32 UInt8s
      Reserved2 at 288  range 0 .. 767;    -- 96 UInt8s
      ICPR      at 384  range 0 .. 255;    -- 32 UInt8s
      Reserved3 at 416  range 0 .. 767;    -- 96 UInt8s
      IABR      at 512  range 0 .. 255;    -- 32 UInt8s
      Reserved4 at 544  range 0 .. 1791;   -- 220 UInt8s
      IP        at 768  range 0 .. 1919;   -- 240 UInt8s
      Reserved5 at 1008 range 0 .. 20607;  -- 2576 UInt8s
      STIR      at 3584 range 0 .. 31;     -- 4 UInt8s
   end record;


   type System_Control_Block is record
      CPUID     : UInt32;
      --  CPUID Base Register   (read-only)
      ICSR      : UInt32;
      --  Interrupt Control and State Register
      VTOR      : UInt32;
      --  Vector Table Offset Register
      AIRCR     : UInt32;
      --  Application Interrupt and Reset Control Register
      SCR       : UInt32;
      --  System Control Register
      CCR       : UInt32;
      --  Configuration Control Register
      SHP       : UInt8s (0 .. 11);
      --  System Handlers Priority Registers (4-7, 8-11, 12-15)
      SHCSR     : UInt32;
      --  System Handler Control and State Register
      CFSR      : UInt32;
      --  Configurable Fault Status Register
      HFSR      : UInt32;
      --  HardFault Status Register
      DFSR      : UInt32;
      --  Debug Fault Status Register
      MMFAR     : UInt32;
      --  MemManage Fault Address Register
      BFAR      : UInt32;
      --  BusFault Address Register
      AFSR      : UInt32;
      --  Auxiliary Fault Status Register
      PFR       : Words (0 .. 1);
      --  Processor Feature Register           (read-only)
      DFR       : UInt32;
      --  Debug Feature Register               (read-only)
      ADR       : UInt32;
      --  Auxiliary Feature Register           (read-only)
      MMFR      : Words (0 .. 3);
      --  Memory Model Feature Register        (read-only)
      ISAR      : Words (0 .. 4);
      --  Instruction Set Attributes Register  (read-only)
      RESERVED0 : Words (0 .. 4);
      CPACR     : UInt32;
      --  Coprocessor Access Control Register
   end record
     with Volatile;

   for System_Control_Block use record
      CPUID     at 0   range 0 .. 31;              -- Offset: 0x000
      ICSR      at 4   range 0 .. 31;              -- Offset: 0x004
      VTOR      at 8   range 0 .. 31;              -- Offset: 0x008
      AIRCR     at 12  range 0 .. 31;              -- Offset: 0x00C
      SCR       at 16  range 0 .. 31;              -- Offset: 0x010
      CCR       at 20  range 0 .. 31;              -- Offset: 0x014
      SHP       at 24  range 0 .. 95;              -- Offset: 0x018
      SHCSR     at 36  range 0 .. 31;              -- Offset: 0x024
      CFSR      at 40  range 0 .. 31;              -- Offset: 0x028
      HFSR      at 44  range 0 .. 31;              -- Offset: 0x02C
      DFSR      at 48  range 0 .. 31;              -- Offset: 0x030
      MMFAR     at 52  range 0 .. 31;              -- Offset: 0x034
      BFAR      at 56  range 0 .. 31;              -- Offset: 0x038
      AFSR      at 60  range 0 .. 31;              -- Offset: 0x03C
      PFR       at 64  range 0 .. 63;              -- Offset: 0x040
      DFR       at 72  range 0 .. 31;              -- Offset: 0x048
      ADR       at 76  range 0 .. 31;              -- Offset: 0x04C
      MMFR      at 80  range 0 .. 127;             -- Offset: 0x050
      ISAR      at 96  range 0 .. 159;             -- Offset: 0x060
      RESERVED0 at 116 range 0 .. 159;
      CPACR     at 136 range 0 .. 31;              -- Offset: 0x088
   end record;


   SCS_Base  : constant := 16#E000_E000#;
   --  system control space base address
   NVIC_Base : constant := SCS_Base +  16#0100#;
   SCB_Base  : constant := SCS_Base +  16#0D00#;
   --  System Control Block base address

   SCB : System_Control_Block with
     Volatile,
     Address => System'To_Address (SCB_Base);
   pragma Import (Ada, SCB);

   NVIC : Nested_Vectored_Interrupt_Controller with
     Volatile,
     Address => System'To_Address (NVIC_Base);
   pragma Import (Ada, NVIC);


   SCB_AIRCR_PRIGROUP_Pos  : constant := 8;
   SCB_AIRCR_PRIGROUP_Mask : constant UInt32 :=
     Shift_Left (7, SCB_AIRCR_PRIGROUP_Pos);

   SCB_AIRCR_VECTKEY_Pos   : constant := 16;
   SCB_AIRCR_VECTKEY_Mask  : constant UInt32 :=
     Shift_Left (16#FFFF#, SCB_AIRCR_VECTKEY_Pos);

   SCB_AIRCR_SYSRESETREQ_Pos  : constant := 2;
   SCB_AIRCR_SYSRESETREQ_Mask : constant UInt32 :=
     Shift_Left (1, SCB_AIRCR_SYSRESETREQ_Pos);

   NVIC_PRIO_BITS : constant := 4;
   --  STM32F4XX uses 4 bits for the priority levels

end Cortex_M.NVIC;
