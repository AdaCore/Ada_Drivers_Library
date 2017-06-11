------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                        --
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

with HAL;                 use HAL;
with HAL.Managed_Buffers; use HAL.Managed_Buffers;
with System;

generic
   Buffers_Len : UInt64;
   Capacity    : Positive;
   --  Zero means unlimited
package Dynamic_Anonynous_Managed_Buffers is

   function Allocate return Any_Managed_Buffer;

   function Remaining return Natural;

private

   type Data_Block is array (1 .. Buffers_Len) of UInt8 with Pack;

   type Buffer is new Managed_Buffer with record
      Count : Natural := 1;
      Data  : aliased Data_Block;
   end record;

   type Buffer_Access is access all Buffer;

   overriding
   procedure Take (This : in out Buffer);

   overriding
   procedure Release (This : in out Buffer);

   overriding
   function Buffer_Address (This : Buffer) return System.Address;

   overriding
   function Buffer_Length (This : Buffer) return UInt64
   is (Buffers_Len);

   subtype Buffer_Index is Positive range 1 .. Capacity;

   Count         : Natural := 0;
end Dynamic_Anonynous_Managed_Buffers;
