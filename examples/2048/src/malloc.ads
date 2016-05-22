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
