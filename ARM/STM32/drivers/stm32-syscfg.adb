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

with STM32_SVD.EXTI;   use STM32_SVD.EXTI;
with STM32_SVD.SYSCFG; use STM32_SVD.SYSCFG;

with STM32.EXTI;

with STM32.Device;     use STM32.Device;

package body STM32.SYSCFG is
   procedure Connect_External_Interrupt
     (Port : Internal_GPIO_Port;
      Pin  : GPIO_Pin_Index);

   procedure Connect_External_Interrupt
     (Port : Internal_GPIO_Port;
      Pin  : GPIO_Pin_Index)
   is
      Port_Name  : constant GPIO_Port_Id := As_GPIO_Port_Id (Port);
   begin

      --  Finally we assign the port 'number' to the EXTI_n value within the
      --  control register. We depend upon the Port enumerals' underlying
      --  numeric representation values matching what the hardware expects,
      --  that is, the values 0 .. n-1, which we get automatically unless
      --  overridden.
      case Pin is
         when 0 .. 3 =>
            SYSCFG_Periph.EXTICR1.EXTI.Arr (Pin) :=
              GPIO_Port_Id'Enum_Rep (Port_Name);
         when 4 .. 7 =>
            SYSCFG_Periph.EXTICR2.EXTI.Arr (Pin) :=
              GPIO_Port_Id'Enum_Rep (Port_Name);
         when 8 .. 11 =>
            SYSCFG_Periph.EXTICR3.EXTI.Arr (Pin) :=
              GPIO_Port_Id'Enum_Rep (Port_Name);
         when 12 .. 15 =>
            SYSCFG_Periph.EXTICR4.EXTI.Arr (Pin) :=
              GPIO_Port_Id'Enum_Rep (Port_Name);
      end case;
   end Connect_External_Interrupt;

   --------------------------------
   -- Connect_External_Interrupt --
   --------------------------------

   procedure Connect_External_Interrupt
     (Port : Internal_GPIO_Port;
      Pin  : GPIO_Pin)
   is
   begin
      Connect_External_Interrupt (Port, GPIO_Pin'Pos (Pin));
   end Connect_External_Interrupt;

   --------------------------------
   -- Connect_External_Interrupt --
   --------------------------------

   procedure Connect_External_Interrupt
     (Point  : GPIO_Point)
   is
   begin
      Connect_External_Interrupt (Point.Periph.all, Point.Pin);
   end Connect_External_Interrupt;

   --------------------------------
   -- Connect_External_Interrupt --
   --------------------------------

   procedure Connect_External_Interrupt
     (Port : Internal_GPIO_Port;
      Pins : GPIO_Pins)
   is
   begin
      for Pin of Pins loop
         Connect_External_Interrupt (Port, Pin);
      end loop;
   end Connect_External_Interrupt;

   ------------------------------
   -- Clear_External_Interrupt --
   ------------------------------

   procedure Clear_External_Interrupt (Pin : GPIO_Pin) is
      use STM32.EXTI;
   begin
      Clear_External_Interrupt (External_Line_Number'Val (GPIO_Pin'Pos (Pin)));
   end Clear_External_Interrupt;

   ------------------------------
   -- Clear_External_Interrupt --
   ------------------------------

   procedure Clear_External_Interrupt (Pin : GPIO_Pin_Index) is
   begin
      EXTI_Periph.PR.PR.Arr (Pin) := True; --  Set to 1 to clear
   end Clear_External_Interrupt;

end STM32.SYSCFG;
