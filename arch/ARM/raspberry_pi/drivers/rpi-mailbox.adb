------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with Ada.Unchecked_Conversion;

with RPi.Regs; use RPi.Regs;

package body RPi.Mailbox is

   Mail_Read_Reg : UInt32
     with Address => System'To_Address (Mail_Base + 16#00#),
     Volatile, Import;
   Mail_Status_Reg : UInt32
     with Address => System'To_Address (Mail_Base + 16#18#),
     Volatile, Import;
   Mail_Write_Reg : UInt32
     with Address => System'To_Address (Mail_Base + 16#20#),
     Volatile, Import;

   Mail_Empty : constant UInt32 := 16#4000_0000#;  -- Cannot read
   Mail_Full  : constant UInt32 := 16#8000_0000#;  -- Cannot write

   -------------------
   -- Mailbox_Write --
   -------------------

   procedure Mailbox_Write (Val : UInt32; Channel : Mailbox_Channel)
   is
   begin
      while (Mail_Status_Reg and Mail_Full) /= 0 loop
         null;
      end loop;

      Mail_Write_Reg := Val or UInt32 (Channel);
   end Mailbox_Write;

   -------------------
   -- Mailbox_Write --
   -------------------

   procedure Mailbox_Write (Addr : System.Address; Channel : Mailbox_Channel)
   is
      pragma Warnings (Off);
      --  Addresses are expected to be 32-bit, even on the RPi3
      function To_U32 is new Ada.Unchecked_Conversion (System.Address, UInt32);
      pragma Warnings (On);
      Val : constant UInt32 := To_U32 (Addr);
   begin
      pragma Assert ((Val and 16#F#) = 0, "Address need to be 16-byte aligned");
      Mailbox_Write (Val, Channel);
   end Mailbox_Write;

   ------------------
   -- Mailbox_Read --
   ------------------

   function Mailbox_Read (Channel : Mailbox_Channel) return UInt32
   is
      Res : UInt32;
   begin
      loop
         while (Mail_Status_Reg and Mail_Empty) /= 0 loop
            null;
         end loop;

         Res := Mail_Read_Reg;

         if (Res and 16#0f#) = UInt32 (Channel) then
            return Shift_Right (Res, 4);
         end if;
      end loop;
   end Mailbox_Read;

end RPi.Mailbox;
