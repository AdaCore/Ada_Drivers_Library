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

   procedure Lock_The_Pin (Port : in out GPIO_Port;  Pin : Short);
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

   function Any_Set (Port : GPIO_Port;  Pins : GPIO_Pins) return Boolean is
      These_Pins : Short := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      return (Port.IDR.IDR.Val and These_Pins) /= 0;
   end Any_Set;

   -------------
   -- Any_Set --
   -------------

   function Any_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if Pin.Port.IDR.IDR.Arr (Pin.Pin) then
            return True;
         end if;
      end loop;

      return False;
   end Any_Set;

   ---------
   -- Set --
   ---------

   function Set (Port : GPIO_Port;  Pin : GPIO_Pin) return Boolean is
      Pin_Mask : constant Short := GPIO_Pin'Enum_Rep (Pin);
   begin
      return (Port.IDR.IDR.Val and Pin_Mask) = Pin_Mask;
   end Set;

   ---------
   -- Set --
   ---------

   function Set (This : GPIO_Point) return Boolean is
     (This.Port.IDR.IDR.Arr (This.Pin));

   -------------
   -- All_Set --
   -------------

   function All_Set (Port : GPIO_Port;  Pins : GPIO_Pins) return Boolean is
      These_Pins : Short := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      return (Port.IDR.IDR.Val and These_Pins) = These_Pins;
   end All_Set;

   -------------
   -- All_Set --
   -------------

   function All_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if not Set (Pin) then
            return False;
         end if;
      end loop;

      return True;
   end All_Set;

   ---------
   -- Set --
   ---------

   procedure Set (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
   begin
      Port.BSRR.BS.Val := Pin'Enum_Rep;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
      These_Pins : Short := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      Port.BSRR.BS.Val := These_Pins'Enum_Rep;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This : GPIO_Point) is
   begin
      This.Port.BSRR.BS.Arr (This.Pin) := True;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Pins : GPIO_Points) is
   begin
      for Pin of Pins loop
         Set (Pin);
      end loop;
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
   begin
      Port.BSRR.BR.Val := Pin'Enum_Rep;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
      These_Pins : Short := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      Port.BSRR.BR.Val := These_Pins;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : GPIO_Point) is
   begin
      This.Port.BSRR.BR.Arr (this.Pin) := True;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Pins : GPIO_Points) is
   begin
      for Pin of Pins loop
         Clear (Pin);
      end loop;
   end Clear;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
      pragma Annotate (CodePeer,
                       False_Positive,
                       "subp always fails",
                       "hardware-specific interaction for IDR and ODR");
   begin
      Port.ODR.ODR.Val := Port.ODR.ODR.Val xor Pin'Enum_Rep;
   end Toggle;
   pragma Annotate (CodePeer,
                    False_Positive,
                    "postcondition",
                    "hardware-specific semantics");

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
      These_Pins : Short := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      Port.ODR.ODR.Val := Port.ODR.ODR.Val xor These_Pins;
   end Toggle;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (This : GPIO_Point) is
   begin
      This.Port.ODR.ODR.Arr (This.Pin) := not This.Port.ODR.ODR.Arr (This.Pin);
   end Toggle;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Points : GPIO_Points) is
   begin
      for Point of Points loop
         Toggle (Point);
      end loop;
   end Toggle;

   ------------
   -- Locked --
   ------------

   function Locked (Port : GPIO_Port;  Pin : GPIO_Pin) return Boolean is
   begin
      return (Port.LCKR.LCK.Val and Pin'Enum_Rep) /= 0;
   end Locked;

   ------------
   -- Locked --
   ------------

   function Locked (Pin : GPIO_Point) return Boolean is
     (Pin.Port.LCKR.LCK.Arr (Pin.Pin));

   ------------------
   -- Lock_The_Pin --
   ------------------

   procedure Lock_The_Pin (Port : in out GPIO_Port;  Pin : Short) is
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

   procedure Lock (Port : in out GPIO_Port;  Pin : GPIO_Pin)
   is
   begin
      Lock_The_Pin (Port, GPIO_Pin'Enum_Rep (Pin));
   end Lock;

   ----------
   -- Lock --
   ----------

   procedure Lock (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
   begin
      for Pin of Pins loop
         Lock_The_Pin (Port, GPIO_Pin'Enum_Rep (Pin));
      end loop;
   end Lock;

   ----------
   -- Lock --
   ----------

   procedure Lock (Point : GPIO_Point) is
   begin
      Lock_The_Pin (Point.Port.all, Shift_Left (1, Point.Pin));
   end Lock;

   ----------
   -- Lock --
   ----------

   procedure Lock (Points : GPIO_Points) is
   begin
      for Point of Points loop
         Lock (Point);
      end loop;
   end Lock;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Port   : in out GPIO_Port;
      Pins   : GPIO_Pins;
      Config : GPIO_Port_Configuration)
   is
      MODER   : MODER_Register   := Port.MODER;
      OTYPER  : OTYPER_OT_Field  := Port.OTYPER.OT;
      OSPEEDR : OSPEEDR_Register := Port.OSPEEDR;
      PUPDR   : PUPDR_Register   := Port.PUPDR;
   begin
      for Pin of Pins loop
         declare
            Index : constant Integer := GPIO_Pin'Pos (Pin); -- 0 .. 15
         begin
            MODER.Arr (Index)   := Pin_IO_Modes'Enum_Rep (Config.Mode);
            OTYPER.Arr (Index)  := Config.Output_Type = Open_Drain;
            OSPEEDR.Arr (Index) := Pin_Output_Speeds'Enum_Rep (Config.Speed);
            PUPDR.Arr (Index)   :=
              Internal_Pin_Resistors'Enum_Rep (Config.Resistors);
         end;
      end loop;

      Port.MODER     := MODER;
      Port.OTYPER.OT := OTYPER;
      Port.OSPEEDR   := OSPEEDR;
      Port.PUPDR     := PUPDR;
   end Configure_IO;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Port   : in out GPIO_Port;
      Pin    : GPIO_Pin;
      Config : GPIO_Port_Configuration)
   is
      MODER   : MODER_Register   := Port.MODER;
      OTYPER  : OTYPER_OT_Field  := Port.OTYPER.OT;
      OSPEEDR : OSPEEDR_Register := Port.OSPEEDR;
      PUPDR   : PUPDR_Register   := Port.PUPDR;

      Index : constant Integer := GPIO_Pin'Pos (Pin);
   begin
      MODER.Arr (Index)   := Pin_IO_Modes'Enum_Rep (Config.Mode);
      OTYPER.Arr (Index)  := Config.Output_Type = Open_Drain;
      OSPEEDR.Arr (Index) := Pin_Output_Speeds'Enum_Rep (Config.Speed);
      PUPDR.Arr (Index)  := Internal_Pin_Resistors'Enum_Rep (Config.Resistors);

      Port.MODER     := MODER;
      Port.OTYPER.OT := OTYPER;
      Port.OSPEEDR   := OSPEEDR;
      Port.PUPDR     := PUPDR;
   end Configure_IO;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Point  : GPIO_Point;
      Config : GPIO_Port_Configuration)
   is
      MODER   : MODER_Register   := Point.Port.MODER;
      OTYPER  : OTYPER_Register  := Point.Port.OTYPER;
      OSPEEDR : OSPEEDR_Register := Point.Port.OSPEEDR;
      PUPDR   : PUPDR_Register   := Point.Port.PUPDR;

   begin
      MODER.Arr (Point.Pin)     :=
        Pin_IO_Modes'Enum_Rep (Config.Mode);
      OTYPER.OT.Arr (Point.Pin) := Config.Output_Type = Open_Drain;
      OSPEEDR.Arr (Point.Pin) :=
        Pin_Output_Speeds'Enum_Rep (Config.Speed);
      PUPDR.Arr (Point.Pin)     :=
        Internal_Pin_Resistors'Enum_Rep (Config.Resistors);

      Point.Port.MODER   := MODER;
      Point.Port.OTYPER  := OTYPER;
      Point.Port.OSPEEDR := OSPEEDR;
      Point.Port.PUPDR   := PUPDR;
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
         Configure_IO (Point, Config);
      end loop;
   end Configure_IO;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input (Port : GPIO_Port) return Short is
   begin
      return Port.IDR.IDR.Val;
   end Current_Input;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output (Port : GPIO_Port) return Short is
   begin
      return Port.ODR.ODR.Val;
   end Current_Output;

   ------------------
   -- Write_Output --
   ------------------

   procedure Write_Output (Port : in out GPIO_Port; Data : Short) is
   begin
      Port.ODR.ODR.Val := Data;
   end Write_Output;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Port : in out GPIO_Port;
      Pin  : GPIO_Pin;
      AF   : GPIO_Alternate_Function)
   is
      Index : constant Integer := GPIO_Pin'Pos (Pin);
   begin
      if Index < 8 then
         Port.AFRL.Arr (Index) := UInt4 (AF);
      else
         Port.AFRH.Arr (Index) := UInt4 (AF);
      end if;
   end Configure_Alternate_Function;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Point : GPIO_Point;
      AF    : GPIO_Alternate_Function)
   is
   begin
      if Point.Pin < 8 then
         Point.Port.AFRL.Arr (Point.Pin) := UInt4 (AF);
      else
         Point.Port.AFRH.Arr (Point.Pin) := UInt4 (AF);
      end if;
   end Configure_Alternate_Function;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Port : in out GPIO_Port;
      Pins : GPIO_Pins;
      AF   : GPIO_Alternate_Function)
   is
   begin
      for Pin of Pins loop
         Configure_Alternate_Function (Port, Pin, AF);
      end loop;
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
         Configure_Alternate_Function (Point, AF);
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
     (Port    : in out GPIO_Port;
      Pin     : GPIO_Pin;
      Trigger : EXTI.External_Triggers)
   is
      use STM32.SYSCFG, STM32.RCC;
      use STM32.EXTI;
      Line : constant External_Line_Number := External_Line_Number'Val (GPIO_Pin'Pos (Pin));
   begin
      RCC.SYSCFG_Clock_Enable;
      SYSCFG.Connect_External_Interrupt (Port, Pin);

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
     (Point   : GPIO_Point;
      Trigger : EXTI.External_Triggers)
   is
      use STM32.EXTI;
      Line : constant External_Line_Number := External_Line_Number'Val (Point.Pin);
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
         Configure_Trigger (Point, Trigger);
      end loop;
   end Configure_Trigger;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (Port    : in out GPIO_Port;
      Pins    : GPIO_Pins;
      Trigger : EXTI.External_Triggers)
   is
   begin
      for Pin of Pins loop
         Configure_Trigger (Port, Pin, Trigger);
      end loop;
   end Configure_Trigger;

end STM32.GPIO;
