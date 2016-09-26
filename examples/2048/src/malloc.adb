------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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



with System.Storage_Elements;  use System.Storage_Elements;
with Ada.Unchecked_Conversion;

with STM32.Board;
with STM32.SDRAM;

package body Malloc is

   Heap_Start : Character;
   for Heap_Start'Alignment use Standard'Maximum_Alignment;
   for Heap_Start'Address use System'To_Address (STM32.Board.SDRAM_Base);
   --  The address of the variable is the start of the heap

   Heap_End : Character;
   for Heap_End'Address use System'To_Address
     (STM32.Board.SDRAM_Base + STM32.Board.SDRAM_Size);
   --  The address of the variable is the end of the heap

   G_Preallocated  : System.Address := System.Null_Address;
   G_Prealloc_Size : Storage_Offset := 0;
   G_Fast_Mode     : Boolean := False;
   G_Current       : System.Address := System.Null_Address;

   -----------------------
   -- Start_Fast_Malloc --
   -----------------------

   procedure Start_Fast_Malloc (Byte_Size : Unsigned_32) is
   begin
      if not G_Fast_Mode then
         G_Prealloc_Size := Storage_Offset (Byte_Size);
         G_Fast_Mode := True;
         G_Preallocated := STM32.SDRAM.Reserve (Byte_Size);
      end if;

      --  We reset the allocation buffer
      G_Current := G_Preallocated;
   end Start_Fast_Malloc;

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return Address is
      Rounded_Size : size_t;
      Ret          : Address;

   begin
      --  Return null address for zero length request

      if Size = 0 then
         return Null_Address;
      end if;

      STM32.SDRAM.Initialize;

      --  Round size up

      if G_Fast_Mode then
         Rounded_Size := (Size + Standard'Maximum_Alignment);
         Rounded_Size :=
           Rounded_Size - Rounded_Size rem Standard'Maximum_Alignment;

         Ret := G_Current;
         G_Current := G_Current + Storage_Offset (Rounded_Size);

         if G_Current > (G_Preallocated + G_Prealloc_Size) then
            raise Constraint_Error with "Not enough memory";
         end if;

         return Ret;
      else
         return STM32.SDRAM.Reserve (Unsigned_32 (Size));
      end if;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : Address) is
      pragma Unreferenced (Ptr);
   begin
      return;
   end Free;

   -------------
   -- Realloc --
   -------------

   function Realloc (Ptr : Address; Size : size_t) return Address is
   begin
      raise Program_Error with "Not implemented";
      return Null_Address;
   end Realloc;

   ---------------
   -- Allocated --
   ---------------

   function Allocated return Unsigned_32
   is
   begin
      if G_Fast_Mode then
         return Unsigned_32 (G_Prealloc_Size) +
           Unsigned_32 (G_Current - G_Preallocated);
      else
         return 0;
      end if;
   end Allocated;

   -------------
   -- Is_Full --
   -------------

   function Is_Full return Boolean is
   begin
      if not G_Fast_Mode then
         return False;
      end if;

      return (G_Current - G_Preallocated) >= G_Prealloc_Size;
   end Is_Full;

end Malloc;
