------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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

--  Watchdogs to catch IRQs

with Ada.Interrupts.Names;

with STM32.GPIO;            use STM32.GPIO;
with STM32.Device;
with STM32.EXTI;

package Watchdogs is

   RX_IRQ : GPIO_Point renames STM32.Device.PB4;
   TX_IRQ : GPIO_Point renames STM32.Device.PC3;

   type Watchdog_Callback is access procedure;

   -- RX_Watchdog --

   protected RX_Watchdog is
      pragma Interrupt_Priority;

      procedure Set_Callback (Proc : Watchdog_Callback);

   private
      RX_EXTI_Line : STM32.EXTI.External_Line_Number :=
        RX_IRQ.Interrupt_Line_Number;

      procedure Interrupt;
      pragma Attach_Handler (Interrupt, Ada.Interrupts.Names.EXTI4_Interrupt);

      Callback : Watchdog_Callback := null;
   end RX_Watchdog;

   -- TX_Watchdog --

   protected TX_Watchdog is
      pragma Interrupt_Priority;

      procedure Set_Callback (Proc : Watchdog_Callback);

   private
      TX_EXTI_Line : STM32.EXTI.External_Line_Number :=
        TX_IRQ.Interrupt_Line_Number;

      procedure Interrupt;
      pragma Attach_Handler (Interrupt, Ada.Interrupts.Names.EXTI3_Interrupt);

      Callback : Watchdog_Callback := null;
   end TX_Watchdog;

end Watchdogs;
