------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
