
package body Semihosting.Filesystem is

   -----------------
   -- Create_Node --
   -----------------

   overriding function Create_Node
     (This : in out SHFS;
      Path : Pathname;
      Kind : File_Kind)
      return Status_Kind
   is
      pragma Unreferenced (This, Path, Kind);
   begin
      return Read_Only_File_System;
   end Create_Node;

   ----------------------
   -- Create_Directory --
   ----------------------

   overriding function Create_Directory
     (This : in out SHFS;
      Path : Pathname)
      return Status_Kind
   is
      pragma Unreferenced (This, Path);
   begin
      return Read_Only_File_System;
   end Create_Directory;

   ------------
   -- Unlink --
   ------------

   overriding function Unlink
     (This : in out SHFS;
      Path : Pathname)
      return Status_Kind
   is
      pragma Unreferenced (This, Path);
   begin
      return Read_Only_File_System;
   end Unlink;

   ----------------------
   -- Remove_Directory --
   ----------------------

   overriding function Remove_Directory
     (This : in out SHFS;
      Path : Pathname)
      return Status_Kind
   is
      pragma Unreferenced (This, Path);
   begin
      return Read_Only_File_System;
   end Remove_Directory;

   ------------
   -- Rename --
   ------------

   overriding function Rename
     (This : in out SHFS;
      Old_Path : Pathname;
      New_Path : Pathname)
      return Status_Kind
   is
      pragma Unreferenced (This, Old_Path, New_Path);
   begin
      return Read_Only_File_System;
   end Rename;

   ------------------------
   -- Change_Permissions --
   ------------------------

   overriding function Change_Permissions
     (This        : in out SHFS;
      Path        : Pathname;
      Permissions : Permission_Set)
      return Status_Kind
   is
      pragma Unreferenced (This, Path, Permissions);
   begin
      return Read_Only_File_System;
   end Change_Permissions;

   ----------------------------
   -- Change_Owner_And_Group --
   ----------------------------

   overriding function Change_Owner_And_Group
     (This  : in out SHFS;
      Path  : Pathname;
      Owner : User_ID;
      Group : Group_ID)
      return Status_Kind
   is
      pragma Unreferenced (This, Path, Owner, Group);
   begin
      return Read_Only_File_System;
   end Change_Owner_And_Group;

   ------------------------
   -- Change_Permissions --
   ------------------------

   overriding function Truncate_File
     (This   : in out SHFS;
      Path   : Pathname;
      Lenght : IO_Count)
      return Status_Kind
   is
      pragma Unreferenced (Path, Lenght, This);
   begin
      return Read_Only_File_System;
   end Truncate_File;

   ----------
   -- Open --
   ----------

   overriding function Open
     (This    : in out SHFS;
      Path    : Pathname;
      Mode    : File_Mode;
      Handler : out File_Handle_Ref)
      return Status_Kind
   is
      FH : SHFS_File_Handle_Access;
      FD : SH_Word;
   begin
      if Path'Length = 0 then
         return No_Such_File_Or_Directory;
      end if;

      if Mode /= Read_Only then
         return Permission_Denied;
      end if;

      FD := Semihosting.Open (Filename => Path,
                              Mode     => OPEN_FLAG_RB);

      if FD = SH_Word'Last then
         return No_Such_File_Or_Directory;
      else
         FH := This.Get_File_Handle;
         FH.FD := FD;
         FH.Is_Open := True;
         Handler := File_Handle_Ref (FH);
         return Status_Ok;
      end if;
   end Open;

   --------------------
   -- Open_Directory --
   --------------------

   overriding function Open_Directory
     (This   : in out SHFS;
      Path   : Pathname;
      Handle : out Directory_Handle_Ref)
      return Status_Kind
   is
      pragma Unreferenced (This, Path, Handle);
   begin
      return Operation_Not_Permitted;
   end Open_Directory;

   ---------------------
   -- Get_File_Handle --
   ---------------------

   function Get_File_Handle (This : in out SHFS)
                             return not null SHFS_File_Handle_Access
   is
      Ret : SHFS_File_Handle_Access := This.File_Handles;
   begin

      --  Try to find a free handle
      while Ret /= null and then Ret.Is_Open loop
         Ret := Ret.Next;
      end loop;

      --  Allocate a new handle
      if Ret = null then
         Ret := new SHFS_File_Handle;
         Ret.Is_Open := False;
         Ret.Next := This.File_Handles;
         This.File_Handles := Ret;
      end if;

      return Ret;
   end Get_File_Handle;

   ----------
   -- Read --
   ----------

   overriding function Read
     (This : in out SHFS_File_Handle;
      Data : out Byte_Array)
      return Status_Kind
   is
   begin
      if not This.Is_Open then
         return Invalid_Argument;
      end if;

      if Semihosting.Read (File_Handle    => This.FD,
                           Buffer_Address => Data'Address,
                           Buffer_Size    => Data'Length) /= 0
      then
         return Input_Output_Error;
      else
         return Status_Ok;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   overriding function Write
     (This : in out SHFS_File_Handle;
      Data : Byte_Array)
      return Status_Kind
   is
      pragma Unreferenced (Data);
   begin
      if not This.Is_Open then
         return Invalid_Argument;
      end if;

      return Permission_Denied;
   end Write;

   ----------
   -- Seek --
   ----------

   overriding function Seek
     (This   : in out SHFS_File_Handle;
      Offset : IO_Count)
      return Status_Kind
   is
   begin
      if not This.Is_Open then
         return Invalid_Argument;
      end if;

      if Semihosting.Seek (File_Handle    => This.FD,
                           Absolute_Position => SH_Word (Offset)) /= 0
      then
         return Input_Output_Error;
      else
         return Status_Ok;
      end if;
   end Seek;

   -----------
   -- Close --
   -----------

   overriding function Close
     (This   : in out SHFS_File_Handle)
      return Status_Kind
   is
   begin
      if not This.Is_Open then
         return Invalid_Argument;
      end if;

      if Semihosting.Close (This.FD) /= 0 then
         return Invalid_Argument;
      else
         This.Is_Open := False;
         return Status_Ok;
      end if;
   end Close;

end Semihosting.Filesystem;
