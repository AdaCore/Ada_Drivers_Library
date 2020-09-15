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

with HAL;

package RISCV.CSR_Generic is

   generic
      Reg_Name : String;
      type Reg_Type is private;
   function Read_CSR return Reg_Type
     with Inline_Always;

   generic
      Reg_Name : String;
      type Reg_Type is private;
   procedure Write_CSR (Val : Reg_Type)
     with Inline_Always;

   generic
      Reg_Name : String;
      type Reg_Type is private;
   function Swap_CSR (Val : Reg_Type) return Reg_Type
     with Inline_Always;

   generic
      Reg_Name : String;
      type Reg_Type is private;
   procedure Set_Bits_CSR (Val : Reg_Type)
     with Inline_Always;

   generic
      Reg_Name : String;
      type Reg_Type is private;
   function Read_And_Set_Bits_CSR (Val : Reg_Type) return Reg_Type
     with Inline_Always;

   generic
      Reg_Name : String;
      type Reg_Type is private;
   procedure Clear_Bits_CSR (Val : Reg_Type)
     with Inline_Always;

   generic
      Reg_Name : String;
      type Reg_Type is private;
   function Read_And_Clear_Bits_CSR (Val : Reg_Type) return Reg_Type
     with Inline_Always;

   --  Package to access Read/Write CSR
   generic
      Reg_Name : String;
      type Reg_Type is private;
   package CSR_RW_Pck is

      function Read is new Read_CSR (Reg_Name, Reg_Type);
      procedure Write is new Write_CSR (Reg_Name, Reg_Type);
      function Swap is new Swap_CSR (Reg_Name, Reg_Type);

      procedure Set_Bits is new Set_Bits_CSR (Reg_Name, Reg_Type);
      function Read_And_Set_Bits is new Read_And_Set_Bits_CSR (Reg_Name, Reg_Type);

      procedure Clear_Bits is new Clear_Bits_CSR (Reg_Name, Reg_Type);
      function Read_And_Clear_Bits is new Read_And_Clear_Bits_CSR (Reg_Name, Reg_Type);

   end CSR_RW_Pck;

   --  Package to access Read-only CSR
   generic
      Reg_Name : String;
      type Reg_Type is private;
   package CSR_RO_Pck is
      function Read is new Read_CSR (Reg_Name, Reg_Type);
   end CSR_RO_Pck;

   generic
      Reg_Name : String;
   function Read_CSR_64 return HAL.UInt64
     with Inline_Always;
   --  Some CSR always have a 64bit precision on all RV32 and RV64 systems.
   --  This function abstracts the hanlding of low and high CSRs on RV32.

   generic
      Reg_Name : String;
   procedure Write_CSR_64 (Val : HAL.UInt64)
     with Inline_Always;
   --  Some CSR always have a 64bit precision on all RV32 and RV64 systems.
   --  This function abstracts the hanlding of low and high CSRs on RV32.

   generic
      Reg_Name : String;
   package CSR_RW_64_Pck is
      function Read is new Read_CSR_64 (Reg_Name);
      procedure Write is new Write_CSR_64 (Reg_Name);
   end CSR_RW_64_Pck;

   generic
      Reg_Name : String;
   package CSR_RO_64_Pck is
      function Read is new Read_CSR_64 (Reg_Name);
   end CSR_RO_64_Pck;

end RISCV.CSR_Generic;
