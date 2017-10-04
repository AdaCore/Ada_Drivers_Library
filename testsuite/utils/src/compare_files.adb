------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with Ada.Streams;
with Ada.Streams.Stream_IO;
with File_IO;  use File_IO;
with GNAT.MD5; use GNAT.MD5;

package body Compare_Files is

   package Hash renames GNAT.MD5;

   ------------------
   -- Compute_Hash --
   ------------------

   function Compute_Hash (FD : in out File_IO.File_Descriptor)
                          return String
   is
      Context : aliased GNAT.MD5.Context := GNAT.MD5.Initial_Context;

      Buffer : Ada.Streams.Stream_Element_Array (1 .. 512);
      Last   : Ada.Streams.Stream_Element_Offset;
      Size   : File_Size;
      Status : Status_Code;
      pragma Unreferenced (Status);
      use type Ada.Streams.Stream_Element_Offset;
   begin
      loop
         Size := Buffer'Length;
         Size := Read (FD,
                       Addr   => Buffer'Address,
                       Length => Size);
         Last := Ada.Streams.Stream_Element_Offset (Size);
         Hash.Update (Context, Buffer (1 .. Last));
         exit when Last < Buffer'Last;
      end loop;
      return Hash.Digest (Context);
   end Compute_Hash;

   -------------------
   -- Binnary_Equal --
   -------------------

   function Binnary_Equal (A_Path, B_Path : String) return Boolean is

      function Compute_Hash (Path : String) return Message_Digest;

      function Compute_Hash (Path : String) return Message_Digest
      is
         Context : aliased GNAT.MD5.Context := GNAT.MD5.Initial_Context;

         File   : Ada.Streams.Stream_IO.File_Type;
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
         Last   : Ada.Streams.Stream_Element_Offset;
         use type Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Open (File,
                                     Mode => Ada.Streams.Stream_IO.In_File,
                                     Name => Path);
         loop
            Ada.Streams.Stream_IO.Read (File, Item => Buffer, Last => Last);
            Hash.Update (Context, Buffer (1 .. Last));
            exit when Last < Buffer'Last;
         end loop;
         Ada.Streams.Stream_IO.Close (File);
         return Hash.Digest (Context);
      end Compute_Hash;
   begin
      return Compute_Hash (A_Path) = Compute_Hash (B_Path);
   end Binnary_Equal;

end Compare_Files;
