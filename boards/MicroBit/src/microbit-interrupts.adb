------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with HAL; use HAL;
with System.Machine_Code; use System.Machine_Code;

package body MicroBit.Interrupts is

   Handlers : array (nRF51.Interrupts.Interrupt_Name) of Handler
     := (others => null);

   procedure GNAT_IRQ_Handler;
   pragma Export (Asm, GNAT_IRQ_Handler, "__gnat_irq_trap");

   --------------
   -- Register --
   --------------

   procedure Register (Id  : nRF51.Interrupts.Interrupt_Name;
                       Hdl : Handler)
   is
   begin
      Handlers (Id) := Hdl;
   end Register;

   ----------------------
   -- GNAT_IRQ_Handler --
   ----------------------

   procedure GNAT_IRQ_Handler is
      Id  : nRF51.Interrupts.Interrupt_Name;
      IPSR : UInt32;
   begin
      Asm ("mrs %0, ipsr",
           UInt32'Asm_Output ("=r", IPSR),
           Volatile => True);

      IPSR := IPSR and 16#FF#;

      Id := nRF51.Interrupts.Interrupt_Name'Val (IPSR - 16);

      if Handlers (Id) /= null then
         Handlers (Id).all;
      end if;
   end GNAT_IRQ_Handler;

end MicroBit.Interrupts;
