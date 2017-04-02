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

with Ada.Directories;
with Ada.Unchecked_Deallocation;

package body Native.Filesystem is

   --  ??? There are a bunch of 'Unrestricted_Access here because the
   --  HAL.Filesystem API embeds implicit references to filesystems. It
   --  woudl be good to make these references explicit at some point to
   --  avoid kludges.

   function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;
   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Native_File_Handle, Native_File_Handle_Ref);
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Native_Directory_Handle, Native_Directory_Handle_Ref);

   function "<" (Left, Right : Directory_Data_Entry) return Boolean is
     (Ada.Strings.Unbounded."<" (Left.Name, Right.Name));

   package Directory_Data_Sorting is
     new Directory_Data_Vectors.Generic_Sorting;

   --  Most of the time, standard operations give us no reliable way to
   --  determine specifically what triggered a failure, so use the following
   --  error code as a "generic" one.

   Generic_Error : constant Status_Kind := Input_Output_Error;

   function Absolute_Path
     (This          : Native_FS_Driver;
      Relative_Path : Pathname)
      return Pathname
   is (if Relative_Path = ""
       then +This.Root_Dir
       else Join (+This.Root_Dir, Relative_Path, True));

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle
     (FS : in out Native_FS_Driver)
      return Native_File_Handle_Ref
   is
      Result : Native_File_Handle_Ref := FS.Free_File_Handles;
   begin
      if Result = null then
         Result := new Native_File_Handle'
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
      return Native_Directory_Handle_Ref
   is
      Result : Native_Directory_Handle_Ref := FS.Free_Dir_Handles;
   begin
      if Result = null then
         Result := new Native_Directory_Handle'
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
      Handle : in out Native_File_Handle_Ref)
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
      Handle : in out Native_Directory_Handle_Ref)
   is
   begin
      Handle.Next := FS.Free_Dir_Handles;
      FS.Free_Dir_Handles := Handle;
      Handle := null;
   end Add_Free_Handle;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (This : in out Native_FS_Driver_Ref) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Native_FS_Driver, Native_FS_Driver_Ref);
   begin
      --  Free all handles

      while This.Free_File_Handles /= null loop
         declare
            H : constant Native_File_Handle_Ref := This.Free_File_Handles.Next;
         begin
            Destroy (This.Free_File_Handles);
            This.Free_File_Handles := H;
         end;
      end loop;
      while This.Free_Dir_Handles /= null loop
         declare
            H : constant Native_Directory_Handle_Ref :=
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
      Root_Dir : Pathname)
      return Status_Kind
   is
   begin
      declare
         use type Ada.Directories.File_Kind;
      begin
         if Ada.Directories.Kind (Root_Dir) /= Ada.Directories.Directory then
            return Invalid_Argument;
         end if;
      exception
         when Ada.Directories.Name_Error =>
            return Invalid_Argument;
      end;

      FS.Root_Dir := Ada.Strings.Unbounded.To_Unbounded_String (Root_Dir);
      return Status_Ok;
   end Create;

   -----------------
   -- Create_Node --
   -----------------

   overriding function Create_Node
     (This       : in out Native_FS_Driver;
      Path       : Pathname;
      Delimiters : Path_Delimiters;
      Kind       : File_Kind)
      return Status_Kind
   is
      pragma Unreferenced (Delimiters);
      Abs_Path : constant Pathname := Absolute_Path (This, Path);
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

      return Status_Ok;
   end Create_Node;

   ----------------------
   -- Create_Directory --
   ----------------------

   overriding function Create_Directory
     (This       : in out Native_FS_Driver;
      Path       : Pathname;
      Delimiters : Path_Delimiters)
      return Status_Kind
   is
   begin
      return This.Create_Node (Path, Delimiters, Directory);
   end Create_Directory;

   ------------
   -- Unlink --
   ------------

   overriding function Unlink
     (This       : in out Native_FS_Driver;
      Path       : Pathname;
      Delimiters : Path_Delimiters)
      return Status_Kind
   is
      pragma Unreferenced (Delimiters);
   begin
      Ada.Directories.Delete_File (Absolute_Path (This, Path));
      return Status_Ok;
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
     (This       : in out Native_FS_Driver;
      Path       : Pathname;
      Delimiters : Path_Delimiters)
      return Status_Kind
   is
      pragma Unreferenced (Delimiters);
   begin
      Ada.Directories.Delete_Directory (Absolute_Path (This, Path));
      return Status_Ok;
   exception
      when Ada.Directories.Name_Error
         | Ada.Directories.Use_Error
         =>
         return Generic_Error;
   end Remove_Directory;

   ------------
   -- Rename --
   ------------

   overriding function Rename
     (This     : in out Native_FS_Driver;
      Old_Path : Pathname;
      New_Path : Pathname)
      return Status_Kind
   is
      Old_Abs_Path : constant Pathname := Absolute_Path (This, Old_Path);
      New_Abs_Path : constant Pathname := Absolute_Path (This, New_Path);
   begin
      Ada.Directories.Rename (Old_Abs_Path, New_Abs_Path);
      return Status_Ok;
   exception
      when Ada.Directories.Name_Error
         | Ada.Directories.Use_Error
         =>
         return Generic_Error;
   end Rename;

   -------------------
   -- Truncate_File --
   -------------------

   overriding function Truncate_File
     (This       : in out Native_FS_Driver;
      Path       : Pathname;
      Delimiters : Path_Delimiters;
      Length     : IO_Count)
      return Status_Kind
   is
      pragma Unreferenced (This, Path, Length, Delimiters);
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
     (This       : in out Native_FS_Driver;
      Path       : Pathname;
      Delimiters : Path_Delimiters;
      Mode       : File_Mode;
      Handle     : out Any_File_Handle)
      return Status_Kind
   is
      pragma Unreferenced (Delimiters);
      Result     : Native_File_Handle_Ref := This.Get_Handle;
   begin
      begin
         Byte_IO.Open
           (File => Result.File,
            Mode => (case Mode is
                     when Read_Only => Byte_IO.In_File,
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
      Handle := Result.all'Access;
      return Status_Ok;
   end Open;

   --------------------
   -- Open_Directory --
   --------------------

   overriding function Open_Directory
     (This       : in out Native_FS_Driver;
      Path       : Pathname;
      Delimiters : Path_Delimiters;
      Handle     : out Any_Directory_Handle)
      return Status_Kind
   is
      pragma Unreferenced (Delimiters);
      use Ada.Strings.Unbounded;

      Result : Native_Directory_Handle_Ref := This.Get_Handle;
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
                 ((Kind => (case Kind is
                            when Ada.Directories.Ordinary_File => Regular_File,
                            when Ada.Directories.Directory     => Directory,
                            when others => raise Program_Error),
                   Name => Name));
            end if;
         end;
      end loop;
      Ada.Directories.End_Search (Search);

      --  Make sure entries are sorted so that we get determinism. This is
      --  convenient for testing.

      Directory_Data_Sorting.Sort (Result.Data);

      Handle := Result.all'Access;
      return Status_Ok;
   end Open_Directory;

   ----------
   -- Read --
   ----------

   overriding function Read
     (This : in out Native_File_Handle;
      Data : out UInt8_Array)
      return Status_Kind
   is
   begin
      for B of Data loop
         Byte_IO.Read (This.File, B);
      end loop;
      return Status_Ok;
   exception
      when Byte_IO.Mode_Error
         | Byte_IO.End_Error
         | Byte_IO.Data_Error
         =>
         return Generic_Error;
   end Read;

   -----------
   -- Write --
   -----------

   overriding function Write
     (This : in out Native_File_Handle;
      Data : UInt8_Array)
      return Status_Kind
   is
   begin
      for B of Data loop
         Byte_IO.Write (This.File, B);
      end loop;
      return Status_Ok;
   exception
      when Byte_IO.Mode_Error
         | Byte_IO.Use_Error
         =>
         return Generic_Error;
   end Write;

   ----------
   -- Seek --
   ----------

   overriding function Seek
     (This   : in out Native_File_Handle;
      Offset : IO_Count)
      return Status_Kind
   is
   begin
      Byte_IO.Set_Index (This.File, Byte_IO.Positive_Count (Offset + 1));
      return Status_Ok;
   end Seek;

   -----------
   -- Close --
   -----------

   overriding function Close
     (This : in out Native_File_Handle)
      return Status_Kind
   is
      This_Ref : Native_File_Handle_Ref := This'Unrestricted_Access;
   begin
      begin
         Byte_IO.Close (This.File);
      exception
         when Byte_IO.Status_Error =>
            return Generic_Error;
      end;

      This_Ref.FS.Add_Free_Handle (This_Ref);
      return Status_Ok;
   end Close;

   ----------------
   -- Read_Entry --
   ----------------

   overriding function Read_Entry
     (This         : in out Native_Directory_Handle;
      Entry_Number : Positive;
      Dir_Entry    : out Directory_Entry)
      return Status_Kind
   is
   begin
      if Entry_Number > This.Data.Last_Index then
         return No_Such_File_Or_Directory;
      end if;

      Dir_Entry := (Entry_Type => This.Data.Element (Entry_Number).Kind);
      return Status_Ok;
   end Read_Entry;

   ----------------
   -- Entry_Name --
   ----------------

   overriding function Entry_Name
     (This         : in out Native_Directory_Handle;
      Entry_Number : Positive)
      return Pathname
   is
   begin
      if Entry_Number > This.Data.Last_Index then
         return "";
      end if;

      return +This.Data.Element (Entry_Number).Name;
   end Entry_Name;

   -----------
   -- Close --
   -----------

   overriding function Close
     (This : in out Native_Directory_Handle)
      return Status_Kind
   is
      This_Ref : Native_Directory_Handle_Ref := This'Unrestricted_Access;
   begin
      This.Data.Clear;
      This_Ref.FS.Add_Free_Handle (This_Ref);
      return Status_Ok;
   end Close;

   ----------
   -- Join --
   ----------

   function Join
     (Prefix, Suffix           : Pathname;
      Ignore_Absolute_Suffixes : Boolean)
      return Pathname
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

end Native.Filesystem;
