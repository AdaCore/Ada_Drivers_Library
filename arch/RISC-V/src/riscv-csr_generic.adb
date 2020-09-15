------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with System.Machine_Code; use System.Machine_Code;

package body RISCV.CSR_Generic is

   --------------
   -- Read_CSR --
   --------------

   function Read_CSR return Reg_Type is
      Ret : Reg_Type;
   begin
      Asm ("csrr %0, " & Reg_Name,
           Outputs  => Reg_Type'Asm_Output ("=r", Ret),
           Volatile => True);
      return Ret;
   end Read_CSR;

   ---------------
   -- Write_CSR --
   ---------------

   procedure Write_CSR (Val : Reg_Type) is
   begin
      Asm ("csrw " & Reg_Name & ", %0",
           Inputs   => Reg_Type'Asm_Input ("r", Val),
           Volatile => True);
   end Write_CSR;

   --------------
   -- Swap_CSR --
   --------------

   function Swap_CSR (Val : Reg_Type) return Reg_Type is
      Ret : Reg_Type;
   begin
      Asm ("csrrw %1, " & Reg_Name & ", %0",
           Inputs   => Reg_Type'Asm_Input ("r", Val),
           Outputs  => Reg_Type'Asm_Output ("=r", Ret),
           Volatile => True);
      return Ret;
   end Swap_CSR;

   ------------------
   -- Set_Bits_CSR --
   ------------------

   procedure Set_Bits_CSR (Val : Reg_Type) is
   begin
      Asm ("csrs " & Reg_Name & ", %0",
           Inputs   => Reg_Type'Asm_Input ("r", Val),
           Volatile => True);
   end Set_Bits_CSR;

   ---------------------------
   -- Read_And_Set_Bits_CSR --
   ---------------------------

   function Read_And_Set_Bits_CSR (Val : Reg_Type) return Reg_Type is
      Ret : Reg_Type;
   begin
      Asm ("csrrs %1, " & Reg_Name & ", %0",
           Inputs   => Reg_Type'Asm_Input ("r", Val),
           Outputs  => Reg_Type'Asm_Output ("=r", Ret),
           Volatile => True);
      return Ret;
   end Read_And_Set_Bits_CSR;

   --------------------
   -- Clear_Bits_CSR --
   --------------------

   procedure Clear_Bits_CSR (Val : Reg_Type) is
   begin
      Asm ("csrc " & Reg_Name & ", %0",
           Inputs   => Reg_Type'Asm_Input ("r", Val),
           Volatile => True);
   end Clear_Bits_CSR;

   -----------------------------
   -- Read_And_Clear_Bits_CSR --
   -----------------------------

   function Read_And_Clear_Bits_CSR (Val : Reg_Type) return Reg_Type is
      Ret : Reg_Type;
   begin
      Asm ("csrrc %1, " & Reg_Name & ", %0",
           Inputs   => Reg_Type'Asm_Input ("r", Val),
           Outputs  => Reg_Type'Asm_Output ("=r", Ret),
           Volatile => True);
      return Ret;
   end Read_And_Clear_Bits_CSR;

   function Read_CSR_64 return HAL.UInt64 is separate;

   procedure Write_CSR_64 (Val : HAL.UInt64) is separate;

end RISCV.CSR_Generic;
