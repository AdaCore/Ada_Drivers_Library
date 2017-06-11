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

package body Static_Anonynous_Managed_Buffers is

   Init_Done : Boolean := False;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      for Index in Buffers_Array'Range loop
         Buffers_Array (Index).Count := 0;
         Buffers_Array (Index).Addr := Data (Index)'Address;
         Buffers_Accesses (Index) := Buffers_Array (Index)'Unchecked_Access;
      end loop;
      Init_Done := True;
   end Initialize;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean
   is (Init_Done);

   --------------
   -- Allocate --
   --------------

   function Allocate
      return Any_Managed_Buffer
   is
      Ret : Any_Managed_Buffer;
   begin

      if Count = Capacity then
         return null;
      end if;

      Ret := Buffers_Accesses (Next_Out);

      Next_Out := (Next_Out mod Capacity) + 1;
      Count := Count + 1;

      Ret.Take;
      return Ret;
   end Allocate;

   ---------------
   -- Remaining --
   ---------------

   function Remaining return Natural
   is (Capacity - Count);

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
   begin
      This.Count := This.Count - 1;

      if This.Count = 0 then
         Buffers_Accesses (Next_In) := This'Unchecked_Access;
         Next_In := (Next_In mod Capacity) + 1;
         Count := Count - 1;
      end if;
   end Release;

end Static_Anonynous_Managed_Buffers;
