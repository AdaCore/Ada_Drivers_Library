with Pathname_Manipulation; use Pathname_Manipulation;

package body Virtual_File_System is

   -------------
   -- Find_FS --
   -------------

   function Find_FS (This                : in out VFS;
                     Path                : Pathname;
                     Path_Reminder_Start : out Integer)
                     return FS_Driver_Ref
   is
      Start, Stop : Integer;
      Elt  : Mount_Point_Access := This.Mount_points;
   begin
      Root_Dir (Path, Start, Stop);
      declare
         Root_Dir_Name : constant String := Path (Start .. Stop);
      begin

         while Elt /= null and then Elt.Directory.all /= Root_Dir_Name loop
            Elt := Elt.Next;
         end loop;

         Path_Reminder_Start := Stop + 1;
         return (if Elt /= null then Elt.FS else null);
      end;
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
      Path_Reminder_Start : Integer;
      FS : constant FS_Driver_Ref := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Create_Node (Path (Path_Reminder_Start .. Path'Last), Kind);
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
      Path_Reminder_Start : Integer;
      FS : constant FS_Driver_Ref := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Create_Directory (Path (Path_Reminder_Start .. Path'Last));
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
      Path_Reminder_Start : Integer;
      FS : constant FS_Driver_Ref := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Unlink (Path (Path_Reminder_Start .. Path'Last));
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
      Path_Reminder_Start : Integer;
      FS : constant FS_Driver_Ref := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Remove_Directory (Path (Path_Reminder_Start .. Path'Last));
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
      Path_Reminder_Start : Integer;
      FS : constant FS_Driver_Ref := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Change_Permissions (Path (Path_Reminder_Start .. Path'Last),
                                       Permissions);
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
      Path_Reminder_Start : Integer;
      FS : constant FS_Driver_Ref := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Change_Owner_And_Group (Path (Path_Reminder_Start .. Path'Last),
                                           Owner, Group);
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
      Path_Reminder_Start : Integer;
      FS : constant FS_Driver_Ref := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Change_Permissions (Path (Path_Reminder_Start .. Path'Last),
                                       Lenght);
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
      Path_Reminder_Start : Integer;
      FS : constant FS_Driver_Ref := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Open (Path (Path_Reminder_Start .. Path'Last), Handler);
      end if;
   end Open;

   --------------------
   -- Open_Directory --
   --------------------

   overriding
   function Open_Directory (This   : in out VFS;
                            Path   : Pathname;
                            Handle : out Directory_Handle_Ref)
                            return Status_Kind
   is
      Path_Reminder_Start : Integer;
      FS : constant FS_Driver_Ref := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Open_Directory (Path (Path_Reminder_Start .. Path'Last),
                                   Handle);
      end if;
   end Open_Directory;

end Virtual_File_System;
