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

package body Synchronized_Static_Anonynous_Managed_Buffers is

   Init_Done : Boolean := False;
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      for Index in Buffers_Array'Range loop
         Buffers_Array (Index).Set_Count (0);
         Buffers_Array (Index).Set_Address (Data (Index)'Address);
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
      Prot.Allocate (Ret);
      return Ret;
   end Allocate;

   ---------------
   -- Remaining --
   ---------------

   function Remaining return Natural
   is (Prot.Remaining);


   ------------
   -- Buffer --
   ------------

   protected body Buffer is

      ----------
      -- Take --
      ----------

      procedure Take is
      begin
         Count := Count + 1;
      end Take;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Count := Count - 1;
         if Count = 0 then
            Prot.Deallocate (Buffer'Unchecked_Access);
         end if;
      end Release;

      --------------------
      -- Buffer_Address --
      --------------------

      function Buffer_Address return System.Address is
      begin
         return Addr;
      end Buffer_Address;

      -------------------
      -- Buffer_Length --
      -------------------

      function Buffer_Length return UInt64 is
      begin
         return Buffers_Len;
      end Buffer_Length;

      ---------------
      -- Set_Count --
      ---------------

      procedure Set_Count (Cnt : Natural) is
      begin
         Count := Cnt;
      end Set_Count;

      -----------------
      -- Set_Address --
      -----------------

      procedure Set_Address (Addr : System.Address) is
      begin
         Buffer.Addr := Addr;
      end Set_Address;

   end Buffer;

   ----------
   -- Prot --
   ----------

   protected body Prot is

      --------------
      -- Allocate --
      --------------

      procedure Allocate (Buf : in out Any_Managed_Buffer) is
      begin

         if Count = Capacity then
            Buf := null;
            return;
         end if;

         Buf := Buffers_Accesses (Next_Out);

         Next_Out := (Next_Out mod Capacity) + 1;
         Count := Count + 1;

         Buf.Take;
      end Allocate;

      ----------------
      -- Deallocate --
      ----------------

      procedure Deallocate (Buf : Any_Managed_Buffer) is
      begin
         Buffers_Accesses (Next_In) := Buf;
         Next_In := (Next_In mod Capacity) + 1;
         Count := Count - 1;
      end Deallocate;

      ---------------
      -- Remaining --
      ---------------

      function Remaining return Natural
      is (Capacity - Count);
   end Prot;

end Synchronized_Static_Anonynous_Managed_Buffers;
