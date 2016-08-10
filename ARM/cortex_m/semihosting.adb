------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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

with Cortex_M.Debug;
with System.Machine_Code; use System.Machine_Code;
with HAL; use HAL;

package body Semihosting is

   procedure Write_C (C : Character);
   procedure Write_0 (Str : String);

   ----------
   -- Send --
   ----------

   procedure Write_C (C : Character) is
   begin
      if not Cortex_M.Debug.Halting_Debug_Enabled then
         --  No debugger attached
         return;
      end if;

      Asm ("mov r0, #0x03" & ASCII.LF & ASCII.HT &
           "mov r1, %0" & ASCII.LF & ASCII.HT &
           "bkpt #0xAB",
           Inputs   => (System.Address'Asm_Input ("r", C'Address)),
           Volatile => True,
           Clobber => ("r1, r0"));
   end Write_C;

   ----------
   -- Send --
   ----------

   procedure Write_0 (Str : String) is
      Data : Byte_Array (Str'First .. Str'Last + 1);
   begin
      if not Cortex_M.Debug.Halting_Debug_Enabled then
         --  No debugger attached
         return;
      end if;

      for Index in Str'Range loop
         Data (Index) := Character'Pos (Str (Index));
      end loop;

      --  Add trailing zero
      Data (Str'Last + 1) := 0;

      Asm ("mov r0, #0x04" & ASCII.LF & ASCII.HT &
           "mov r1, %0" & ASCII.LF & ASCII.HT &
           "bkpt #0xAB",
           Inputs   => (System.Address'Asm_Input ("r", Data'Address)),
           Volatile => True,
           Clobber => ("r1, r0"));
   end Write_0;

   ---------
   -- Log --
   ---------

   procedure Log (C : Character) is
   begin
      Write_C (C);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Str : String) is
   begin
      Write_0 (Str);
   end Log;

   --------------
   -- Log_Line --
   --------------

   procedure Log_Line (Str : String) is
   begin
      Log (Str);
      Log_New_Line;
   end Log_Line;

   ------------------
   -- Log_New_Line --
   ------------------

   procedure Log_New_Line is
   begin
      Write_C (ASCII.LF);
   end Log_New_Line;

end Semihosting;
