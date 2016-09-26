
--  A simple implementation of storage allocation (malloc etc) for ZFP use

pragma Restrictions (No_Elaboration_Code);

with System;       use System;
with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

package Malloc is
   pragma Elaborate_Body;

   function Alloc (Size : size_t) return Address;
   pragma Export (C, Alloc, "malloc");

   procedure Free (Ptr : Address);
   pragma Export (C, Free, "free");

   function Realloc (Ptr : Address; Size : size_t) return Address;
   pragma Export (C, Realloc, "realloc");

   procedure Start_Fast_Malloc (Byte_Size : Unsigned_32);

   function Allocated return Unsigned_32;
   --  Total allocated memory

   function Is_Full return Boolean;
   --  In 'fast malloc' mode, tells if we've allocated the full buffer

end Malloc;
