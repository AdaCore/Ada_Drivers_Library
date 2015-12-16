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
------------------------------------------------------------------------------

--  This package provides an interface to the on-board "independent watchdog"
--  provided by the STM32F4 family.

package STM32.IWDG is -- the Independent Watchdog

   subtype Countdown_Value is Half_Word range 0 .. 4095;

   type Prescalars is
     (Divider_4,
      Divider_8,
      Divider_16,
      Divider_32,
      Divider_64,
      Divider_128,
      Divider_256);
   --  These are the values used to control the rate at which the watchdog
   --  countdown timer counts down to zero. The clock driving the watchdog
   --  downcounter is approximately 32KHz. Thus, for example, dividing by
   --  32 gives about 1 millisecond per count.

   procedure Initialize_Watchdog
     (Prescalar : Prescalars;
      Count     : Countdown_Value);
   --  Sets the watchdog operating parameters. See the ST Micro RM0090
   --  Reference Manual, table 106 (pg 691) for the minimum and maximum timeout
   --  values possible for the range of countdown values, given any specific
   --  prescalar value.
   --
   --  Note that the counter does not begin counting as a result of calling
   --  this routine.

   procedure Start_Watchdog;
   --  Starts the counter counting down to zero. At zero the interrupt is
   --  generated to reset the board.

   procedure Reset_Watchdog;
   --  Reloads the countdown value so that the hardware reset interrupt
   --  is not generated.

private

   type Key_Register is record
      Reserved : Half_Word;
      Value    : Half_Word;
   end record
     with Volatile_Full_Access, Size => 32;

   for Key_Register use record
      Reserved at 0 range 16 .. 31;
      Value    at 0 range  0 .. 15;
   end record;

   type Prescalar_Register is record
      Reserved : Bits_29;
      Value    : Prescalars;
   end record
     with Volatile_Full_Access, Size => 32;

   for Prescalar_Register use record
      Reserved at 0 range 3 .. 31;
      Value    at 0 range 0 .. 2;
   end record;

   type Reload_Register is record
      Reserved : Bits_20;
      Value    : Countdown_Value;
   end record
     with Volatile_Full_Access, Size => 32;

   for Reload_Register use record
      Reserved at 0 range 12 .. 31;
      Value    at 0 range  0 .. 11;
   end record;

   type Status_Register is record
      Reserved       : Bits_30;
      Reload_Busy    : Boolean;
      Prescalar_Busy : Boolean;
   end record
     with Volatile_Full_Access, Size => 32;

   for Status_Register use record
      Reserved       at 0 range 2 .. 31;
      Reload_Busy    at 0 range 1 .. 1;
      Prescalar_Busy at 0 range 0 .. 0;
   end record;

   type Watchdog_Registers is record
      Key       : Key_Register;
      Prescalar : Prescalar_Register;
      Reload    : Reload_Register;
      Status    : Status_Register;
   end record
      with Volatile, Size => 4 * 32;

   for Watchdog_Registers use record
      Key       at 0  range 0 .. 31;
      Prescalar at 4  range 0 .. 31;
      Reload    at 8  range 0 .. 31;
      Status    at 12 range 0 .. 31;
   end record;

   Watchdog : Watchdog_Registers
     with Address => IWDG_Base;

   --  commands to the watchdog hardware

   Reload_Counter : constant Half_Word := 16#AAAA#;
   Enable_Access  : constant Half_Word := 16#5555#;
   Start          : constant Half_Word := 16#CCCC#;

end STM32.IWDG;
