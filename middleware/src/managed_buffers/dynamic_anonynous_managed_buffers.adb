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

with Ada.Unchecked_Deallocation;

package body Dynamic_Anonynous_Managed_Buffers is

   procedure Free is new Ada.Unchecked_Deallocation (Object => Buffer,
                                                     Name   => Buffer_Access);
   --------------
   -- Allocate --
   --------------

   function Allocate
      return Any_Managed_Buffer
   is
      Buf : Buffer_Access;
   begin

      if Count = Capacity then
         return null;
      end if;

      Buf := new Buffer;
      Buf.Count := 1;

      Count := Count + 1;

      return Any_Managed_Buffer (Buf);
   end Allocate;

   ---------------
   -- Remaining --
   ---------------

   function Remaining return Natural is
   begin
      if Capacity = 0 then
         return Natural'Last - Count;
      else
         return Capacity - Count;
      end if;
   end Remaining;

   ----------
   -- Take --
   ----------

   overriding procedure Take
     (This : in out Buffer)
   is
   begin
      This.Count := This.Count + 1;
   end Take;

   -------------
   -- Release --
   -------------

   overriding procedure Release
     (This : in out Buffer)
   is
      Buf : Buffer_Access;
   begin
      This.Count := This.Count - 1;

      if This.Count = 0 then
         Count := Count - 1;
         Buf := This'Unchecked_Access;
         Free (Buf);
      end if;
   end Release;

   --------------------
   -- Buffer_Address --
   --------------------

   overriding
   function Buffer_Address
     (This : Buffer)
      return System.Address
   is
   begin
      return This.Data'Address;
   end Buffer_Address;


end Dynamic_Anonynous_Managed_Buffers;
