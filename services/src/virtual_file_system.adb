with Pathname_Manipulation; use Pathname_Manipulation;

package body Virtual_File_System is

   -------------
   -- Find_FS --
   -------------

   function Find_FS (This : in out VFS;
                     Path : Pathname)
                     return FS_Driver_Ref
   is
      Root : constant String := Root_Dir (Path);
      Elt  : Mount_Point_Access := This.Mount_points;
   begin
      if Path'Length < 1 and then Path (Path'First) /= '/' then
         return null;
      end if;

      while Elt /= null and then Elt.Directory.all /= Root loop
         Elt := Elt.Next;
      end loop;

      return (if Elt /= null then Elt.FS else null);
   end Find_FS;

   -----------
   -- Mount --
   -----------

   function Mount (This       : in out VFS;
                   Path       : Pathname;
                   Filesystem : not null FS_Driver_Ref)
                   return Status_Kind
   is
      MP : constant Mount_Point_Access :=
        new Mount_Point'(Directory => new String'(Path),
                         FS        => Filesystem,
                         Next      => This.Mount_points);
   begin
      This.Mount_points := MP;
      return Status_Ok;
   end Mount;

   ------------
   -- Umount --
   ------------

   function Umount (This       : in out VFS;
                    Path       : Pathname)
                    return Status_Kind
   is
      pragma Unreferenced (This, Path);
   begin
      return Permission_Denied;
   end Umount;


   -----------------
   -- Create_Node --
   -----------------

   overriding
   function Create_Node (This : in out VFS;
                         Path : Pathname;
                         Kind : File_Kind)
                         return Status_Kind
   is
      FS : constant FS_Driver_Ref := This.Find_FS (Path);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Create_Node (Path, Kind);
      end if;
   end Create_Node;

   ----------------------
   -- Create_Directory --
   ----------------------

   overriding
   function Create_Directory (This : in out VFS;
                              Path : Pathname)
                              return Status_Kind
   is
      FS : constant FS_Driver_Ref := This.Find_FS (Path);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Create_Directory (Path);
      end if;
   end Create_Directory;

   ------------
   -- Unlink --
   ------------

   overriding
   function Unlink (This : in out VFS;
                    Path : Pathname)
                    return Status_Kind
   is
      FS : constant FS_Driver_Ref := This.Find_FS (Path);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Unlink (Path);
      end if;
   end Unlink;

   ----------------------
   -- Remove_Directory --
   ----------------------

   overriding
   function Remove_Directory (This : in out VFS;
                              Path : Pathname)
                              return Status_Kind
   is
      FS : constant FS_Driver_Ref := This.Find_FS (Path);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Remove_Directory (Path);
      end if;
   end Remove_Directory;

   ------------
   -- Rename --
   ------------

   overriding
   function Rename (This : in out VFS;
                    Old_Path : Pathname;
                    New_Path : Pathname)
                    return Status_Kind
   is
      pragma Unreferenced (This, Old_Path, New_Path);
   begin
      return Permission_Denied;
   end Rename;

   ------------------------
   -- Change_Permissions --
   ------------------------

   overriding
   function Change_Permissions (This        : in out VFS;
                                Path        : Pathname;
                                Permissions : Permission_Set)
                                return Status_Kind
   is
      FS : constant FS_Driver_Ref := This.Find_FS (Path);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Change_Permissions (Path, Permissions);
      end if;
   end Change_Permissions;

   ----------------------------
   -- Change_Owner_And_Group --
   ----------------------------

   overriding
   function Change_Owner_And_Group (This  : in out VFS;
                                    Path  : Pathname;
                                    Owner : User_ID;
                                    Group : Group_ID)
                                    return Status_Kind
   is
      FS : constant FS_Driver_Ref := This.Find_FS (Path);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Change_Owner_And_Group (Path, Owner, Group);
      end if;
   end Change_Owner_And_Group;

   ------------------------
   -- Change_Permissions --
   ------------------------

   overriding
   function Change_Permissions (This   : in out VFS;
                                Path   : Pathname;
                                Lenght : IO_Count)
                                return Status_Kind
   is
      FS : constant FS_Driver_Ref := This.Find_FS (Path);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Change_Permissions (Path, Lenght);
      end if;
   end Change_Permissions;

   ----------
   -- Open --
   ----------

   overriding
   function Open (This    : in out VFS;
                  Path    : Pathname;
                  Handler : out File_Handle_Ref)
                  return Status_Kind
   is
      FS : constant FS_Driver_Ref := This.Find_FS (Path);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Open (Path, Handler);
      end if;
   end Open;

   -----------
   -- Close --
   -----------

   overriding
   function Close (This    : in out VFS;
                   Handler : out File_Handle_Ref)
                   return Status_Kind
   is
      pragma Unreferenced (Handler, This);
   begin
      return Permission_Denied;
   end Close;

   --------------------
   -- Open_Directory --
   --------------------

   overriding
   function Open_Directory (This   : in out VFS;
                            Path   : Pathname;
                            Handle : out Directory_Handle_Ref)
                            return Status_Kind
   is
      FS : constant FS_Driver_Ref := This.Find_FS (Path);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Open_Directory (Path, Handle);
      end if;
   end Open_Directory;

   ---------------------
   -- Close_Directory --
   ---------------------

   overriding
   function Close_Directory (This   : in out VFS;
                             Handle : out Directory_Handle_Ref)
                             return Status_Kind
   is
      pragma Unreferenced (This, Handle);
   begin
      return Permission_Denied;
   end Close_Directory;


end Virtual_File_System;
