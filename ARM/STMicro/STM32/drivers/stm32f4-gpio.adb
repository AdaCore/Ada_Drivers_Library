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

with STM32F4.RCC;
with STM32F4.SYSCFG;
with System.Machine_Code;

package body STM32F4.GPIO is

   -------------
   -- Any_Set --
   -------------

   function Any_Set (Port : GPIO_Port;  Pins : GPIO_Pins) return Boolean is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      return (Port.IDR and These_Pins) /= 0;
   end Any_Set;

   ---------
   -- Set --
   ---------

   function Set (Port : GPIO_Port;  Pin : GPIO_Pin) return Boolean is
   begin
      return (Port.IDR and Pin'Enum_Rep) = Pin'Enum_Rep;
   end Set;

   ---------
   -- Set --
   ---------

   function Set (This : GPIO_Point) return Boolean is
   begin
      return Set (This.Port.all, This.Pin);
   end Set;

   -------------
   -- All_Set --
   -------------

   function All_Set (Port : GPIO_Port;  Pins : GPIO_Pins) return Boolean is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      return (Port.IDR and These_Pins) = These_Pins;
   end All_Set;

   ---------
   -- Set --
   ---------

   procedure Set (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
   begin
      Port.BSRR_Set := Pin'Enum_Rep;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      Port.BSRR_Set := These_Pins'Enum_Rep;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out GPIO_Point) is
   begin
      Set (This.Port.all, This.Pin);
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
   begin
      Port.BSRR_Reset := Pin'Enum_Rep;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      Port.BSRR_Reset := These_Pins;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out GPIO_Point) is
   begin
      Clear (This.Port.all, This.Pin);
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
      Port.ODR := Port.ODR xor Pin'Enum_Rep;
   end Toggle;
   pragma Annotate (CodePeer,
                    False_Positive,
                    "postcondition",
                    "hardware-specific semantics");

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
      These_Pins : Half_Word := 0;
   begin
      for Pin of Pins loop
         These_Pins := These_Pins or Pin'Enum_Rep;
      end loop;
      Port.ODR := Port.ODR xor These_Pins;
   end Toggle;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (This : in out GPIO_Point) is
   begin
      Toggle (This.Port.all, This.Pin);
   end Toggle;

   ------------
   -- Locked --
   ------------

   function Locked (Port : GPIO_Port;  Pin : GPIO_Pin) return Boolean is
   begin
      return (Port.LCKR and Pin'Enum_Rep) = Pin'Enum_Rep;
   end Locked;

   ----------
   -- Lock --
   ----------

   procedure Lock (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
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
                     (GPIO_Pin'Asm_Input ("r", Pin))),            -- %2
           Outputs => (Word'Asm_Output ("=m", Temp)),  -- %0
           Volatile => True,
           Clobber  => ("r2, r3"));
   end Lock;

   ----------
   -- Lock --
   ----------

   procedure Lock (Port : in out GPIO_Port;  Pins : GPIO_Pins) is
   begin
      for Pin of Pins loop
         Lock (Port, Pin);
      end loop;
   end Lock;

   ----------
   -- Lock --
   ----------

   procedure Lock (Point : GPIO_Point) is
   begin
      Lock (Point.Port.all, Point.Pin);
   end Lock;

   ------------
   -- Locked --
   ------------

   function Locked (Point : GPIO_Point) return Boolean is
   begin
      return Locked (Point.Port.all, Point.Pin);
   end Locked;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Port   : in out GPIO_Port;
      Pins   : GPIO_Pins;
      Config : GPIO_Port_Configuration)
   is
      MODER   : Pin_Modes_Register := Port.MODER;
      OTYPER  : Output_Types_Register := Port.OTYPER;
      OSPEEDR : Output_Speeds_Register := Port.OSPEEDR;
      PUPDR   : Resistors_Register := Port.PUPDR;
   begin
      for Pin of Pins loop
         declare
            Index : constant Integer := GPIO_Pin'Pos (Pin); -- 0 .. 15
         begin
            MODER (Index)   := Config.Mode;
            OTYPER (Index)  := Config.Output_Type;
            OSPEEDR (Index) := Config.Speed;
            PUPDR (Index)   := Config.Resistors;
         end;
      end loop;
      Port.MODER   := MODER;
      Port.OTYPER  := OTYPER;
      Port.OSPEEDR := OSPEEDR;
      Port.PUPDR   := PUPDR;

      if Config.Locked then
         Lock (Port, Pins);
      end if;
   end Configure_IO;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Port   : in out GPIO_Port;
      Pin    : GPIO_Pin;
      Config : GPIO_Port_Configuration)
   is
      MODER   : Pin_Modes_Register := Port.MODER;
      OTYPER  : Output_Types_Register := Port.OTYPER;
      OSPEEDR : Output_Speeds_Register := Port.OSPEEDR;
      PUPDR   : Resistors_Register := Port.PUPDR;

      Index : constant Integer := GPIO_Pin'Pos (Pin);
   begin
      MODER (Index)   := Config.Mode;
      OTYPER (Index)  := Config.Output_Type;
      OSPEEDR (Index) := Config.Speed;
      PUPDR (Index)   := Config.Resistors;

      Port.MODER   := MODER;
      Port.OTYPER  := OTYPER;
      Port.OSPEEDR := OSPEEDR;
      Port.PUPDR   := PUPDR;

      if Config.Locked then
         Lock (Port, Pin);
      end if;
   end Configure_IO;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Point  : GPIO_Point;
      Config : GPIO_Port_Configuration)
   is
   begin
      Configure_IO (Point.Port.all, Point.Pin, Config);
   end Configure_IO;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input (Port : GPIO_Port) return Half_Word is
   begin
      return Port.IDR;
   end Current_Input;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output (Port : GPIO_Port) return Half_Word is
   begin
      return Port.ODR;
   end Current_Output;

   ------------------
   -- Write_Output --
   ------------------

   procedure Write_Output (Port : in out GPIO_Port; Data : Half_Word) is
   begin
      Port.ODR := Data;
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
      Port.AFR (Index) := Bits_4 (AF);
   end Configure_Alternate_Function;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Point : GPIO_Point;
      AF    : GPIO_Alternate_Function)
   is
   begin
      Configure_Alternate_Function (Point.Port.all, Point.Pin, AF);
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

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (Port    : in out GPIO_Port;
      Pin     : GPIO_Pin;
      Trigger : External_Triggers)
   is
      use STM32F4.SYSCFG, STM32F4.RCC;
   begin
      SYSCFG_Clock_Enable;

      Connect_External_Interrupt (Port, Pin);
      Set_External_Trigger (Pin, Trigger);
      Select_Trigger_Edge (Pin, Trigger);
   end Configure_Trigger;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (Point   : GPIO_Point;
      Trigger : External_Triggers)
   is
   begin
      Configure_Trigger (Point.Port.all, Point.Pin, Trigger);
   end Configure_Trigger;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (Port    : in out GPIO_Port;
      Pins    : GPIO_Pins;
      Trigger : External_Triggers)
   is
   begin
      for Pin of Pins loop
         Configure_Trigger (Port, Pin, Trigger);
      end loop;
   end Configure_Trigger;

end STM32F4.GPIO;
