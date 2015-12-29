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

--  This file provides selected SYSCFG routines for the STM32F4 (ARM Cortex
--  M4F) microcontrollers from ST Microelectronics.

with STM32F4.GPIO;  use STM32F4.GPIO;

package STM32F4.SYSCFG is

   procedure Connect_External_Interrupt
     (Port : GPIO_Port;
      Pin  : GPIO_Pin)
     with Inline;

   procedure Connect_External_Interrupt
     (Port : GPIO_Port;
      Pins : GPIO_Pins)
     with Inline;

   procedure Clear_External_Interrupt (Pin : GPIO_Pin) with Inline;
   --  for convenience
   --  accesses the EXTI registers to clear the line corresponding to the pin

private

   --  see pg 298 of "RM0090 Reference manual" in file named DM00031020.pdf

   --  The register layouts are slightly different for the F40x and F42x families,
   --  but the parts concerning the GPIO pins are identical.

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

   type GPIO_Port_Id is
     (GPIO_Port_A, GPIO_Port_B, GPIO_Port_C, GPIO_Port_D, GPIO_Port_E, GPIO_Port_F,
      GPIO_Port_G, GPIO_Port_H, GPIO_Port_I, GPIO_Port_J, GPIO_Port_K);
      -- TODO: rather ugly to have this board-specific range here, but if we
      -- put it in STM32F4.GPIO it will be just as bad, and if we put it in
      -- STM32F4[29].Discovery that will create a dependence on that package here,
      -- which is not the intent

   for GPIO_Port_Id'Size use 4;

   pragma Compile_Time_Error
     (not (GPIO_Port_Id'First = GPIO_Port_A and GPIO_Port_Id'Last = GPIO_Port_K and
           GPIO_Port_A'Enum_Rep = 0 and
           GPIO_Port_B'Enum_Rep = 1 and
           GPIO_Port_C'Enum_Rep = 2 and
           GPIO_Port_D'Enum_Rep = 3 and
           GPIO_Port_E'Enum_Rep = 4 and
           GPIO_Port_F'Enum_Rep = 5 and
           GPIO_Port_G'Enum_Rep = 6 and
           GPIO_Port_H'Enum_Rep = 7 and
           GPIO_Port_I'Enum_Rep = 8 and
           GPIO_Port_J'Enum_Rep = 9 and
           GPIO_Port_K'Enum_Rep = 10),
      "Invalid representation for type GPIO_Port_Id");
   --  Confirming, but depended upon so we check it.

   function As_GPIO_Port_Id (Port : GPIO_Port) return GPIO_Port_Id with Inline;

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
     Address => SYSCFG_Base;
   pragma Import (Ada, SYSCFG);

end STM32F4.SYSCFG;
