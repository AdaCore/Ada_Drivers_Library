------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

--  This package provides a low level driver to access the native file system.
--  It is recommended to _not_ use this interface directly but to access the
--  file system using the File_IO package. For more info, see the file system
--  chapter of the documentation.

with Ada.Directories;
with Ada.Unchecked_Deallocation;

package body Filesystem.Native is

   --  ??? There are a bunch of 'Unrestricted_Access here because the
   --  HAL.Filesystem API embeds implicit references to filesystems. It
   --  woudl be good to make these references explicit at some point to
   --  avoid kludges.

   function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;
   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Destroy is new Ada.Unchecked_Deallocation
     (File_Handle, File_Handle_Access);
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Directory_Handle, Directory_Handle_Access);
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Node_Handle, Node_Handle_Access);

   function "<" (Left, Right : Node_Handle) return Boolean is
     (Ada.Strings.Unbounded."<" (Left.Name, Right.Name));

   package Node_Sorting is
     new Node_Vectors.Generic_Sorting;

   --  Most of the time, standard operations give us no reliable way to
   --  determine specifically what triggered a failure, so use the following
   --  error code as a "generic" one.

   Generic_Error : constant Status_Code := Input_Output_Error;

   function Absolute_Path
     (This          : Native_FS_Driver;
      Relative_Path : String)
      return String
   is (if Relative_Path = ""
       then +This.Root_Dir
       else Join (+This.Root_Dir, Relative_Path, True));

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle
     (FS : in out Native_FS_Driver)
      return File_Handle_Access
   is
      Result : File_Handle_Access := FS.Free_File_Handles;
   begin
      if Result = null then
         Result := new File_Handle'
           (FS     => FS'Unrestricted_Access,
            others => <>);
      else
         FS.Free_File_Handles := Result.Next;
      end if;
      return Result;
   end Get_Handle;

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle
     (FS : in out Native_FS_Driver)
      return Directory_Handle_Access
   is
      Result : Directory_Handle_Access := FS.Free_Dir_Handles;
   begin
      if Result = null then
         Result := new Directory_Handle'
           (FS     => FS'Unrestricted_Access,
            others => <>);
      else
         FS.Free_Dir_Handles := Result.Next;
      end if;
      return Result;
   end Get_Handle;

   ---------------------
   -- Add_Free_Handle --
   ---------------------

   procedure Add_Free_Handle
     (FS     : in out Native_FS_Driver;
      Handle : in out File_Handle_Access)
   is
   begin
      Handle.Next := FS.Free_File_Handles;
      FS.Free_File_Handles := Handle;
      Handle := null;
   end Add_Free_Handle;

   ---------------------
   -- Add_Free_Handle --
   ---------------------

   procedure Add_Free_Handle
     (FS     : in out Native_FS_Driver;
      Handle : in out Directory_Handle_Access)
   is
   begin
      Handle.Next := FS.Free_Dir_Handles;
      FS.Free_Dir_Handles := Handle;
      Handle := null;
   end Add_Free_Handle;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (This : in out Native_FS_Driver_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Native_FS_Driver, Native_FS_Driver_Access);
   begin
      --  Free all handles

      while This.Free_File_Handles /= null loop
         declare
            H : constant File_Handle_Access := This.Free_File_Handles.Next;
         begin
            Destroy (This.Free_File_Handles);
            This.Free_File_Handles := H;
         end;
      end loop;
      while This.Free_Dir_Handles /= null loop
         declare
            H : constant Directory_Handle_Access :=
              This.Free_Dir_Handles.Next;
         begin
            Destroy (This.Free_Dir_Handles);
            This.Free_Dir_Handles := H;
         end;
      end loop;

      Destroy (This);
   end Destroy;

   ------------
   -- Create --
   ------------

   function Create
     (FS       : out Native_FS_Driver;
      Root_Dir : String)
      return Status_Code
   is
   begin
      declare
         use type Ada.Directories.File_Kind;
      begin
         if Ada.Directories.Kind (Root_Dir) /= Ada.Directories.Directory then
            return Invalid_Parameter;
         end if;
      exception
         when Ada.Directories.Name_Error =>
            return Invalid_Parameter;
      end;

      FS.Root_Dir := Ada.Strings.Unbounded.To_Unbounded_String (Root_Dir);
      return OK;
   end Create;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (This : in out Native_FS_Driver;
      Path : String;
      Kind : File_Kind)
      return Status_Code
   is
      Abs_Path : constant String := Absolute_Path (This, Path);
   begin
      case Kind is
         when Regular_File =>
            declare
               File : Byte_IO.File_Type;
            begin
               begin
                  Byte_IO.Create (File, Byte_IO.Out_File, Abs_Path);
               exception
                  when Byte_IO.Status_Error
                     | Byte_IO.Name_Error
                     | Byte_IO.Use_Error
                     =>
                     return Generic_Error;
               end;
               Byte_IO.Close (File);
            end;

         when Directory =>
            begin
               Ada.Directories.Create_Directory (Abs_Path);
            exception
               when Ada.Directories.Name_Error
                  | Ada.Directories.Use_Error
                  =>
                  return Generic_Error;
            end;
      end case;

      return OK;
   end Create_Node;

   ----------------------
   -- Create_Directory --
   ----------------------

   function Create_Directory
     (This : in out Native_FS_Driver;
      Path : String)
      return Status_Code
   is
   begin
      return This.Create_Node (Absolute_Path (This, Path), Directory);
   end Create_Directory;

   -----------------
   -- Create_File --
   -----------------

   overriding
   function Create_File (This : in out Native_FS_Driver;
                         Path : String)
                         return Status_Code
   is
   begin
      return This.Create_Node (Path, Regular_File);
   end Create_File;

   ------------
   -- Unlink --
   ------------

   overriding
   function Unlink
     (This : in out Native_FS_Driver;
      Path : String)
      return Status_Code
   is
   begin
      Ada.Directories.Delete_File (Absolute_Path (This, Path));
      return OK;
   exception
      when Ada.Directories.Name_Error
         | Ada.Directories.Use_Error
         =>
         return Generic_Error;
   end Unlink;

   ----------------------
   -- Remove_Directory --
   ----------------------

   overriding function Remove_Directory
     (This : in out Native_FS_Driver;
      Path : String)
      return Status_Code
   is
   begin
      Ada.Directories.Delete_Directory (Absolute_Path (This, Path));
      return OK;
   exception
      when Ada.Directories.Name_Error
         | Ada.Directories.Use_Error
         =>
         return Generic_Error;
   end Remove_Directory;

   ------------
   -- Get_FS --
   ------------

   overriding
   function Get_FS
     (This : Directory_Handle) return Any_Filesystem_Driver
   is (Any_Filesystem_Driver (This.FS));

   ---------------
   -- Root_Node --
   ---------------

   overriding
   function Root_Node
     (This   : in out Native_FS_Driver;
      As     : String;
      Handle : out Any_Node_Handle)
      return Status_Code
   is
      Ret : constant Node_Handle_Access := new Node_Handle;
   begin
      Ret.FS := This'Unchecked_Access;
      Ret.Kind := Directory;
      Ret.Name := +As;
      Ret.Read_Only := False;
      Ret.Hidden := False;
      Ret.Symlink := False;
      Ret.Size := 0;

      Handle := Any_Node_Handle (Ret);
      return OK;
   end Root_Node;

   ----------
   -- Read --
   ----------

   overriding
   function Read
     (This   : in out Directory_Handle;
      Handle : out Any_Node_Handle) return Status_Code
   is
      Ret : Node_Handle_Access;
   begin
      if This.Index > This.Data.Last_Index then
         Handle := null;
         return No_More_Entries;
      else
         Ret := new Node_Handle;
         Ret.all := This.Data.Element (This.Index);
         Handle := Any_Node_Handle (Ret);
         This.Index := This.Index + 1;
         return OK;
      end if;
   end Read;

   ------------
   -- Rename --
   ------------

   function Rename
     (This     : in out Native_FS_Driver;
      Old_Path : String;
      New_Path : String)
      return Status_Code
   is
      Old_Abs_Path : constant String := Absolute_Path (This, Old_Path);
      New_Abs_Path : constant String := Absolute_Path (This, New_Path);
   begin
      Ada.Directories.Rename (Old_Abs_Path, New_Abs_Path);
      return OK;
   exception
      when Ada.Directories.Name_Error
         | Ada.Directories.Use_Error
         =>
         return Generic_Error;
   end Rename;

   -------------------
   -- Truncate_File --
   -------------------

   function Truncate_File
     (This   : in out Native_FS_Driver;
      Path   : String;
      Length : File_Size)
      return Status_Code
   is
      pragma Unreferenced (This, Path, Length);
   begin
      --  ??? Implement this. This is not done at the moment as there seems to
      --  be no other way using standard Ada packages than do delete the file
      --  and re-create it.

      return Generic_Error;
   end Truncate_File;

   ----------
   -- Open --
   ----------

   overriding function Open
     (This   : in out Native_FS_Driver;
      Path   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code
   is
      Result : File_Handle_Access := This.Get_Handle;
   begin
      begin
         Byte_IO.Open
           (File => Result.File,
            Mode => (case Mode is
                     when Read_Only  => Byte_IO.In_File,
                     when Write_Only => Byte_IO.Out_File,
                     when Read_Write => Byte_IO.Inout_File),
            Name => Absolute_Path (This, Path));
      exception
         when Byte_IO.Status_Error
            | Byte_IO.Name_Error
            | Byte_IO.Use_Error =>
            Destroy (Result);
            return Generic_Error;
      end;
      Result.Mode := Mode;
      Handle := Result.all'Access;
      return OK;
   end Open;

   ----------
   -- Open --
   ----------

   overriding
   function Open
     (This   : Node_Handle;
      Name   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle) return Status_Code
   is
   begin
      raise Program_Error with "Not implemented...";
      return Input_Output_Error;
   end Open;

   ----------
   -- Open --
   ----------

   overriding function Open
     (This   : in out Native_FS_Driver;
      Path   : String;
      Handle : out Any_Directory_Handle)
      return Status_Code
   is
      use Ada.Strings.Unbounded;

      Result : Directory_Handle_Access := This.Get_Handle;
      Search : Ada.Directories.Search_Type;
   begin
      begin
         Ada.Directories.Start_Search (Search, Absolute_Path (This, Path), "");
      exception
         when Ada.Directories.Name_Error
            | Ada.Directories.Use_Error
            =>
            This.Add_Free_Handle (Result);
            return Generic_Error;
      end;

      Result.Full_Name := +Path;
      while Ada.Directories.More_Entries (Search) loop
         declare
            use type Ada.Directories.File_Kind;

            E    : Ada.Directories.Directory_Entry_Type;
            Kind : Ada.Directories.File_Kind;
            Name : Unbounded_String;
         begin
            Ada.Directories.Get_Next_Entry (Search, E);
            Kind := Ada.Directories.Kind (E);
            Name := +Ada.Directories.Simple_Name (E);

            --  HAL.Filesystem does not support special files, so just ignores
            --  them. As for current and parent directories entries, skip them
            --  for now as this makes tree traversal cumbersome to write.

            if Kind /= Ada.Directories.Special_File
              and then +Name /= "."
              and then +Name /= ".."
            then
               Result.Data.Append
                 ((FS        => This'Unchecked_Access,
                   Kind => (case Kind is
                            when Ada.Directories.Ordinary_File => Regular_File,
                            when Ada.Directories.Directory     => Directory,
                            when others => raise Program_Error),
                   Name      => Name,
                   Read_Only => False,
                   Hidden    => False,
                   Symlink   => False,
                   Size      => File_Size (Ada.Directories.Size (E))));
            end if;
         end;
      end loop;
      Ada.Directories.End_Search (Search);

      --  Make sure entries are sorted so that we get determinism. This is
      --  convenient for testing.

      Node_Sorting.Sort (Result.Data);
      Result.Index := Result.Data.First_Index;

      Handle := Result.all'Access;
      return OK;
   end Open;

   ------------
   -- Get_FS --
   ------------

   overriding
   function Get_FS
     (This : in out File_Handle) return Any_Filesystem_Driver
   is (Any_Filesystem_Driver (This.FS));

   ----------
   -- Size --
   ----------

   overriding
   function Size
     (This : File_Handle) return File_Size
   is (File_Size (Byte_IO.Size (This.File)));

   ----------
   -- Mode --
   ----------

   overriding
   function Mode
     (This : File_Handle) return File_Mode
   is (This.Mode);

   ----------
   -- Read --
   ----------

   overriding function Read
     (This   : in out File_Handle;
      Addr   : System.Address;
      Length : in out File_Size)
      return Status_Code
   is
      Data : UInt8_Array (1 .. Natural (Length)) with Address => Addr;
      Ret : File_Size := 0;
   begin
      for B of Data loop
         Byte_IO.Read (This.File, B);
         Ret := Ret + 1;
      end loop;
      Length := Ret;
      return OK;
   exception
      when Byte_IO.Mode_Error
         | Byte_IO.End_Error
         | Byte_IO.Data_Error
         =>
         Length := Ret;
         return Generic_Error;
   end Read;

   -----------
   -- Write --
   -----------

   overriding function Write
     (This   : in out File_Handle;
      Addr   : System.Address;
      Length : File_Size)
      return Status_Code
   is
      Data : UInt8_Array (1 .. Natural (Length)) with Address => Addr;
   begin
      for B of Data loop
         Byte_IO.Write (This.File, B);
      end loop;
      return OK;
   exception
      when Byte_IO.Mode_Error
         | Byte_IO.Use_Error
         =>
         return Generic_Error;
   end Write;

   ------------
   -- Offset --
   ------------

   overriding
   function Offset
     (This : File_Handle) return File_Size
   is
   begin
      return File_Size (Byte_IO.Index (This.File));
   end Offset;

   -----------
   -- Flush --
   -----------

   overriding
   function Flush
     (This : in out File_Handle) return Status_Code
   is
   begin
      Byte_IO.Flush (This.File);
      return OK;
   end Flush;

   ----------
   -- Seek --
   ----------

   overriding function Seek
     (This   : in out File_Handle;
      Origin : Seek_Mode;
      Amount : in out File_Size)
      return Status_Code
   is
      use type Byte_IO.Positive_Count;

      Index : Byte_IO.Positive_Count;
   begin
      case Origin is
         when From_Start =>
            Index := Byte_IO.Positive_Count (Amount + 1);
         when From_End =>
            Index := Byte_IO.Size (This.File) - Byte_IO.Count (Amount);
         when Forward =>
            Index := Byte_IO.Index (This.File) + Byte_IO.Positive_Count (Amount);
         when Backward =>
            Index := Byte_IO.Index (This.File) - Byte_IO.Positive_Count (Amount);
      end case;

      Byte_IO.Set_Index (This.File, Index);
      return OK;
   end Seek;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (This : in out File_Handle)
   is
      This_Access : File_Handle_Access := This'Unrestricted_Access;
   begin
      begin
         Byte_IO.Close (This.File);
      exception
         when Byte_IO.Status_Error =>
            null;
      end;

      This_Access.FS.Add_Free_Handle (This_Access);
   end Close;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (This : in out Native_FS_Driver) is
      Tmp : Native_FS_Driver_Access := This'Unchecked_Access;
   begin
      Destroy (Tmp);
   end Close;

   -----------
   -- Reset --
   -----------

   overriding
   procedure Reset (This : in out Directory_Handle) is
   begin
      This.Index := This.Data.First_Index;
   end Reset;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (This : in out Directory_Handle)
   is
      This_Access : Directory_Handle_Access := This'Unrestricted_Access;
   begin
      This.Data.Clear;
      This_Access.FS.Add_Free_Handle (This_Access);
   end Close;

   ---------------------
   -- Node operations --
   ---------------------

   ------------
   -- Get_FS --
   ------------

   overriding
   function Get_FS (This : Node_Handle) return Any_Filesystem_Driver
   is (Any_Filesystem_Driver (This.FS));

   --------------
   -- Basename --
   --------------

   overriding
   function Basename (This : Node_Handle) return String
   is (+This.Name);

   ------------------
   -- Is_Read_Only --
   ------------------

   overriding
   function Is_Read_Only (This : Node_Handle) return Boolean
   is (This.Read_Only);

      ---------------
   -- Is_Hidden --
   ---------------

   overriding
   function Is_Hidden (This : Node_Handle) return Boolean
   is (This.Hidden);

   ---------------------
   -- Is_Subdirectory --
   ---------------------

   overriding
   function Is_Subdirectory (This : Node_Handle) return Boolean
   is (This.Kind = Directory);

   ----------------
   -- Is_Symlink --
   ----------------

   overriding
   function Is_Symlink (This : Node_Handle) return Boolean
   is (This.Symlink);

   ----------
   -- Size --
   ----------

   overriding
   function Size (This : Node_Handle) return File_Size
   is (This.Size);

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (This : in out Node_Handle) is
      Hack : Node_Handle_Access := This'Unchecked_Access;
   begin
      Destroy (Hack);
   end Close;

   ----------
   -- Join --
   ----------

   function Join
     (Prefix, Suffix           : String;
      Ignore_Absolute_Suffixes : Boolean)
      return String
   is
      use Ada.Strings.Unbounded;

      package String_Vectors is new Ada.Containers.Vectors
        (Positive, Unbounded_String);

      Result : Unbounded_String := +Prefix;
      Names  : String_Vectors.Vector;

   begin
      --  First, decompose Suffix into individual names: the following pushes
      --  the most local directory names first, then the most global ones. For
      --  instance, "a/b/c" will gives: Name => ("c", "b", "a").

      declare
         Suffix_Acc : Unbounded_String := +Suffix;
      begin
         while Length (Suffix_Acc) > 0 loop
            begin
               declare
                  Suffix      : constant String := +Suffix_Acc;
                  Next_Suffix : constant String :=
                    Ada.Directories.Containing_Directory (Suffix);
                  Name        : constant String :=
                    Ada.Directories.Simple_Name (Suffix);
               begin
                  --  The following happens when Suffix is "." (the current
                  --  directory).

                  exit when Suffix = Next_Suffix;
                  Names.Append (+Name);
                  Suffix_Acc := +Next_Suffix;
               end;

            exception
               when Ada.Directories.Use_Error =>

                  --  Suffix is actually an absolute path. Forget about it and
                  --  process it as a relative one.

                  exit when Ignore_Absolute_Suffixes;
                  return Suffix;
            end;
         end loop;
      end;

      --  Then, compose them using Ada.Directories.Compose so we have our
      --  result.

      for Name of reverse Names loop
         Result := +Ada.Directories.Compose (+Result, +Name);
      end loop;
      return +Result;
   end Join;

end Filesystem.Native;
