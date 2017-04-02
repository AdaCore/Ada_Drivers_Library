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

with Pathname_Manipulation; use Pathname_Manipulation;
with Virtual_File_System;

package body Filesystem is

   VFS : Virtual_File_System.VFS;

   -----------
   -- Valid --
   -----------

   function Valid (FD : File_Descriptor) return Boolean
   is (FD.Handle /= null);

   ----------
   -- Read --
   ----------

   function Read
     (FD   : in out File_Descriptor;
      Data : out UInt8_Array)
      return Status_Kind
   is
   begin
      if FD.Handle /= null then
         return FD.Handle.Read (Data);
      else
         return Invalid_FD;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   function Write
     (FD   : in out File_Descriptor;
      Data : UInt8_Array)
      return Status_Kind
   is
   begin
      if FD.Handle /= null then
         return FD.Handle.Write (Data);
      else
         return Invalid_FD;
      end if;
   end Write;

   -----------------
   -- Create_File --
   -----------------

   function Create_File (Path : Pathname)
                         return Status_Kind
   is
   begin
      return VFS.Create_Node (Path       => Path,
                              Delimiters => Parse (Path),
                              Kind       => Regular_File);
   end Create_File;

   ----------
   -- Open --
   ----------

   function Open
     (Path    : String;
      Mode    : File_Mode;
      FD      : out File_Descriptor)
      return Status_Kind
   is
      Delimiters : constant Path_Delimiters := Parse (Path);
   begin
      return VFS.Open (Path       => Path,
                       Delimiters => Delimiters,
                       Mode       => Mode,
                       Handle     => FD.Handle);
   end Open;

   -----------
   -- Close --
   -----------

   function Close (FD : in out File_Descriptor) return Status_Kind is
      Status : Status_Kind;
   begin
      if not Valid (FD) then
         return Invalid_FD;
      else
         Status := FD.Handle.Close;
         FD.Handle := null;
         return Status;
      end if;
   end Close;

   -----------
   -- Mount --
   -----------

   function Mount
     (Path : String;
      FS   : not null Any_FS_Driver)
      return Status_Kind
   is
   begin
      return VFS.Mount (Path       => Path,
                 Filesystem => FS);
   end Mount;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out File_Descriptor;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      pragma Unreferenced (Stream, Last, Item);
   begin
      --  Generated stub: replace with real body!
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out File_Descriptor;
      Item   : Stream_Element_Array)
   is
      pragma Unreferenced (Stream, Item);
   begin
      --  Generated stub: replace with real body!
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

end Filesystem;
