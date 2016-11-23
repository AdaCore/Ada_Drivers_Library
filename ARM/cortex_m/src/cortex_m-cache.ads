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

--  This file provides subprograms necessary to handle the cache on the
--  Cortex-M7 family of CPU.

with System;

package Cortex_M.Cache is

   procedure Enable_I_Cache;

   procedure Enable_D_Cache;

   procedure Disable_I_Cache;

   procedure Disable_D_Cache;

   function I_Cache_Enabled return Boolean
     with Inline_Always;

   function D_Cache_Enabled return Boolean
     with Inline_Always;

   procedure Clean_DCache
     (Start : System.Address;
      Len   : Natural)
     with Inline_Always;
   --  This ensures that the data cache region do not contain any "dirty"
   --  data, which is data that has been modified by the CPU but has not been
   --  synchronized yet with the main memory.
   --  This make sure that any peripheral accessing the region will see
   --  the updated data.

   procedure Invalidate_DCache
     (Start : System.Address;
      Len   : Natural)
     with Inline_Always;
   --  Invalidates the Data Cache for the specified region.
   --  Note that if the cache is dirty (e.g. contains updated data that has
   --  not been synchronized with the main memory), then such data will be
   --  lost.
   --  Calling this subprogram ensures that the main memory will be read
   --  instead of values from the cache.

   procedure Clean_Invalidate_DCache
     (Start : System.Address;
      Len   : Natural)
     with Inline_Always;
   --  Performs both clean and invalidate operations.

end Cortex_M.Cache;
