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
--   @file    stm32f407xx.h   et al.                                        --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   CMSIS STM32F407xx Device Peripheral Access Layer Header File. --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides register definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.

with STM32.GPIO;  use STM32.GPIO;

private with STM32.Device;
private with STM32_SVD;

package STM32.SYSCFG is

   procedure Connect_External_Interrupt
     (Port : GPIO_Port;  Pin  : GPIO_Pin) with Inline;

   procedure Connect_External_Interrupt
     (Point  : GPIO_Point) with Inline;

   procedure Connect_External_Interrupt
     (Port : GPIO_Port;  Pins : GPIO_Pins) with Inline;

   procedure Set_External_Trigger
     (Pin  : GPIO_Pin;   Trigger : External_Triggers) with Inline;

   procedure Set_External_Trigger
     (Pin  : GPIO_Pin_Index;   Trigger : External_Triggers) with Inline;

   procedure Set_External_Trigger
     (Pins : GPIO_Pins;  Trigger : External_Triggers) with Inline;

   --  TODO: consider merging these two into those of Set_External_Trigger ???
   procedure Select_Trigger_Edge
     (Pin  : GPIO_Pin;   Trigger : External_Triggers) with Inline;

   procedure Select_Trigger_Edge
     (Pin  : GPIO_Pin_Index;   Trigger : External_Triggers) with Inline;

   procedure Select_Trigger_Edge
     (Pins : GPIO_Pins;  Trigger : External_Triggers) with Inline;

   procedure Clear_External_Interrupt (Pin : GPIO_Pin) with Inline;

   procedure Clear_External_Interrupt (Pin : GPIO_Pin_Index) with Inline;

private

   use STM32.Device;

   type Boolean32 is array (0 .. 31) of Boolean;
   for Boolean32'Component_Size use 1;

   type EXTI_Registers is record
      IMR   : Boolean32;   -- Interrupt Mask Register
      EMR   : Boolean32;   -- Event Mask Register
      RTSR  : Boolean32;   -- Rising Trigger Selection Register
      FTSR  : Boolean32;   -- Falling Trigger Selection Register
      SWIER : Bits_32x1;   -- Software Interrupt Event Register
      PR    : Bits_32x1;   -- Pending Register
   end record
     with Volatile;

   for EXTI_Registers use record
      IMR   at 0  range 0 .. 31;
      EMR   at 4  range 0 .. 31;
      RTSR  at 8  range 0 .. 31;
      FTSR  at 12 range 0 .. 31;
      SWIER at 16 range 0 .. 31;
      PR    at 20 range 0 .. 31;
   end record;

   EXTI : EXTI_Registers with
     Volatile,
     Address => STM32_SVD.EXTI_Base;
   pragma Import (Ada, EXTI);

   --  see pg 298 of "RM0090 Reference manual" in file named DM00031020.pdf

   --  There are four EXTICR registers in SYSCFG. Each register logically
   --  contains four EXTI_n values, each of which is 4 bits in length, for
   --  a total of 16 bits used in any one register. The other 16 bits are
   --  reserved per register.

   --  For a given EXTI_n, 'n' corresponds to a GPIO pin in the range
   --  zero through fifteen. Hence EXTI_0 corresponds to pin zero, EXTI_1
   --  corresponds to pin one, and so on. Since there are 16 total pins, and
   --  each EXTRCR register holds four EXTI_n values, four EXTICR registers are
   --  required.

   --  So we have a way of handling 16 pins, one per EXTI_n value. But of
   --  course there are multiple ports, each with 16 distinct pins, yet there
   --  are only a total of 16 EXTI_n values. Therefore the 16 EXTI_n values are
   --  shared among all the ports. EXTI_3 corresponds to pin 3 for any one of
   --  GPIO_A, or GPIO_B, or GPIO_C, and so on. Combinations of ports are not
   --  allowed for any given EXTI_n value. Only one port can be associated with
   --  the same EXTI_n pin. Thus the value held within any EXTI_n indicates
   --  which single GPIO port is intended to be associated with that
   --  corresponding pin.

   --  Therefore, to map a given pin 'n' on a given port 'P' to an external
   --  interrupt or event, all we must do is find the EXTI_n corresponding
   --  to pin 'n' and write the port value 'P' into it.

   --  The hardware dictates the mapping of each EXTI_n within the four EXTICR
   --  registers. EXTICR(1) contains EXTI_0 through EXTI_3, EXTICR(2) holds
   --  EXTI_4 through EXTI_7, and so on.

   --  An EXTI_n value is just a GPIO port name. We need to have four of them
   --  per register, so we define an array of port names. These port names
   --  are numeric values, starting with zero for port A and increasing
   --  monotonically. These values are ensured by the aspect clause on the
   --  type GPIO_Port_Id.

   type EXTI_N_List is array (0 .. 3) of GPIO_Port_Id;
   --  NB: if you change the indexes you need to change the calculations in
   --  procedure Connect_External_Interrupt

   for EXTI_n_List'Component_Size use 4;

   --  Each control register has four EXTI_n values and a reserved 16-bit area.

   type EXTI_Control_Register is record
      EXTI     : EXTI_n_List;
      Reserved : Half_Word;
   end record;

   for EXTI_Control_Register use record
      EXTI     at 0 range 0 .. 15;
      Reserved at 2 range 0 .. 15;
   end record;

   type EXTI_Control_Registers is
     array (0 .. 3) of EXTI_Control_Register;
   --  NB: if you change the indexes you need to change the calculations in
   --  procedure Connect_External_Interrupt

   for EXTI_Control_Registers'Component_Size use 32;
   for EXTI_Control_Registers'Size use 4 * 32;

   type SYSCFG_Registers is record
      MEMRM     : Word;
      PMC       : Word;
      EXTICR    : EXTI_Control_Registers;
      Reserved1 : Word;
      Reserved2 : Word;
      CMPCR     : Word;
   end record
     with Volatile;

   for SYSCFG_Registers use record
      MEMRM     at 0  range 0 .. 31;
      PMC       at 4  range 0 .. 31;
      EXTICR    at 8  range 0 .. 127;
      Reserved1 at 24 range 0 .. 31;
      Reserved2 at 28 range 0 .. 31;
      CMPCR     at 32 range 0 .. 31;
   end record;

   SYSCFG : SYSCFG_Registers with
     Volatile,
     Address => STM32_SVD.SYSCFG_Base;
   pragma Import (Ada, SYSCFG);

end STM32.SYSCFG;
