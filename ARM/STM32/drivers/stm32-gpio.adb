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
--   @file    stm32f4xx_hal_gpio.c                                          --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   GPIO HAL module driver.                                       --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with System;              use System;
with STM32_SVD.GPIO;      use STM32_SVD.GPIO;

with STM32.RCC;
with STM32.SYSCFG;
with System.Machine_Code;

package body STM32.GPIO is

   procedure Lock_The_Pin (Port : in out Internal_GPIO_Port;  Pin : Short);
   --  This is the routine that actually locks the pin for the port. It is an
   --  internal routine and has no preconditions. We use it to avoid redundant
   --  calls to the precondition that checks that the pin is not already
   --  locked. The redundancy would otherwise occur because the routine that
   --  locks an array of pins is implemented by calling the routine that locks
   --  a single pin: both those Lock routines have a precondition that checks
   --  that the pin(s) is not already being locked.

   -------------
   -- Any_Set --
   -------------

   function Any_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if Pin.Set then
            return True;
         end if;
      end loop;

      return False;
   end Any_Set;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : GPIO_Point) return Boolean is
     (This.Periph.IDR.IDR.Arr (This.Pin));

   -------------
   -- All_Set --
   -------------

   function All_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if not Pin.Set then
            return False;
         end if;
      end loop;

      return True;
   end All_Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out GPIO_Point) is
   begin
      This.Periph.BSRR.BS.Arr (This.Pin) := True;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Pins : in out GPIO_Points) is
   begin
      for Pin of Pins loop
         Pin.Set;
      end loop;
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out GPIO_Point) is
   begin
      This.Periph.BSRR.BR.Arr (This.Pin) := True;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Pins : in out GPIO_Points) is
   begin
      for Pin of Pins loop
         Pin.Clear;
      end loop;
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out GPIO_Point) is
   begin
      This.Periph.ODR.ODR.Arr (This.Pin) :=
        not This.Periph.ODR.ODR.Arr (This.Pin);
   end Toggle;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Points : in out GPIO_Points) is
   begin
      for Point of Points loop
         Point.Toggle;
      end loop;
   end Toggle;

   ------------
   -- Locked --
   ------------

   function Locked (Pin : GPIO_Point) return Boolean is
     (Pin.Periph.LCKR.LCK.Arr (Pin.Pin));

   ------------------
   -- Lock_The_Pin --
   ------------------

   procedure Lock_The_Pin (Port : in out Internal_GPIO_Port;  Pin : Short) is
      Temp : Word;
      pragma Volatile (Temp);

      use System.Machine_Code;
      use ASCII;
   begin
      --  As per the Reference Manual (RM0090; Doc ID 018909 Rev 6) pg 282,
      --  a specific sequence is required to set the Lock bit. Throughout the
      --  sequence the same value for the lower 15 bits of the word must be
      --  used (ie the pin number). The lock bit is referred to as LCKK in
      --  the doc.

--        Temp := LCCK or Pin'Enum_Rep;
--
--        --  set the lock bit
--        Port.LCKR := Temp;
--
--        --  clear the lock bit
--        Port.LCKR := Pin'Enum_Rep;
--
--        --  set the lock bit again
--        Port.LCKR := Temp;
--
--        --  read the lock bit
--        Temp := Port.LCKR;
--
--        --  read the lock bit again
--        Temp := Port.LCKR;

      --  We use the following assembly language sequence because the above
      --  high-level version in Ada works only if the optimizer is enabled.
      --  This is not an issue specific to Ada. If you need a specific sequence
      --  of instructions you should really specify those instructions.
      --  We don't want the functionality to depend on the switches, and we
      --  don't want to preclude debugging, hence the following:

      Asm ("orr  r3, %2, #65536"  & LF & HT &
           "str  r3, %0"          & LF & HT &
           "ldr  r3, %0"          & LF & HT &  -- temp <- pin or LCCK
           "str  r3, [%1, #28]"   & LF & HT &  -- temp -> lckr
           "str  %2, [%1, #28]"   & LF & HT &  -- pin -> lckr
           "ldr  r3, %0"          & LF & HT &
           "str  r3, [%1, #28]"   & LF & HT &  -- temp -> lckr
           "ldr  r3, [%1, #28]"   & LF & HT &
           "str  r3, %0"          & LF & HT &  -- temp <- lckr
           "ldr  r3, [%1, #28]"   & LF & HT &
           "str  r3, %0"          & LF & HT,   -- temp <- lckr
           Inputs => (Address'Asm_Input ("r", Port'Address), -- %1
                     (Short'Asm_Input ("r", Pin))),            -- %2
           Outputs => (Word'Asm_Output ("=m", Temp)),  -- %0
           Volatile => True,
           Clobber  => ("r2, r3"));
   end Lock_The_Pin;

   ----------
   -- Lock --
   ----------

   procedure Lock (Point : GPIO_Point) is
   begin
      Lock_The_Pin (Point.Periph.all, Shift_Left (1, Point.Pin));
   end Lock;

   ----------
   -- Lock --
   ----------

   procedure Lock (Points : GPIO_Points) is
   begin
      for Point of Points loop
         Point.Lock;
      end loop;
   end Lock;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Point  : GPIO_Point;
      Config : GPIO_Port_Configuration)
   is
      MODER   : MODER_Register   := Point.Periph.MODER;
      OTYPER  : OTYPER_Register  := Point.Periph.OTYPER;
      OSPEEDR : OSPEEDR_Register := Point.Periph.OSPEEDR;
      PUPDR   : PUPDR_Register   := Point.Periph.PUPDR;

   begin
      MODER.Arr (Point.Pin)     :=
        Pin_IO_Modes'Enum_Rep (Config.Mode);
      OTYPER.OT.Arr (Point.Pin) := Config.Output_Type = Open_Drain;
      OSPEEDR.Arr (Point.Pin) :=
        Pin_Output_Speeds'Enum_Rep (Config.Speed);
      PUPDR.Arr (Point.Pin)     :=
        Internal_Pin_Resistors'Enum_Rep (Config.Resistors);

      Point.Periph.MODER   := MODER;
      Point.Periph.OTYPER  := OTYPER;
      Point.Periph.OSPEEDR := OSPEEDR;
      Point.Periph.PUPDR   := PUPDR;
   end Configure_IO;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Points : GPIO_Points;
      Config : GPIO_Port_Configuration)
   is
   begin
      for Point of Points loop
         Point.Configure_IO (Config);
      end loop;
   end Configure_IO;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Point : GPIO_Point;
      AF    : GPIO_Alternate_Function)
   is
   begin
      if Point.Pin < 8 then
         Point.Periph.AFRL.Arr (Point.Pin) := UInt4 (AF);
      else
         Point.Periph.AFRH.Arr (Point.Pin) := UInt4 (AF);
      end if;
   end Configure_Alternate_Function;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Points : GPIO_Points;
      AF     : GPIO_Alternate_Function)
   is
   begin
      for Point of Points loop
         Point.Configure_Alternate_Function (AF);
      end loop;
   end Configure_Alternate_Function;

   -------------------------------
   -- Get_Interrupt_Line_Number --
   -------------------------------

   function Get_Interrupt_Line_Number
     (Point : GPIO_Point) return EXTI.External_Line_Number
   is
   begin
      return EXTI.External_Line_Number'Val (Point.Pin);
   end Get_Interrupt_Line_Number;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (Point   : GPIO_Point;
      Trigger : EXTI.External_Triggers)
   is
      use STM32.EXTI;
      Line : constant External_Line_Number :=
        External_Line_Number'Val (Point.Pin);
      use STM32.SYSCFG, STM32.RCC;
   begin
      SYSCFG_Clock_Enable;

      Connect_External_Interrupt (Point);
      if Trigger in Interrupt_Triggers then
         Enable_External_Interrupt (Line, Trigger);
      else
         Enable_External_Event (Line, Trigger);
      end if;
   end Configure_Trigger;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (Points  : GPIO_Points;
      Trigger : EXTI.External_Triggers)
   is
   begin
      for Point of Points loop
         Point.Configure_Trigger (Trigger);
      end loop;
   end Configure_Trigger;

end STM32.GPIO;
