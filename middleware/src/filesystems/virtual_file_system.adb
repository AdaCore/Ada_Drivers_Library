------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package body Virtual_File_System is

   -------------
   -- Find_FS --
   -------------

   function Find_FS (This                : in out VFS;
                     Path                : Pathname;
                     Path_Reminder_Start : out Integer)
                     return Any_FS_Driver
   is
      Start, Stop : Integer;
      Elt  : Mount_Point_Access := This.Mount_points;
   begin
      Root_Dir (Path, Start, Stop);

      if Start not in Path'Range or else Stop not in Path'Range then
         Path_Reminder_Start := Path'Last + 1;
         return null;
      end if;

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
                   Filesystem : not null Any_FS_Driver)
                   return Status_Kind
   is
      MP : constant Mount_Point_Access :=
        new Mount_Point'(Directory  => new String'(Path),
                         FS         => Filesystem,
                         Next       => This.Mount_points);
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
      FS : constant Any_FS_Driver := This.Find_FS (Path, Path_Reminder_Start);
      Sub_Path : constant String := (if Path_Reminder_Start not in Path'Range
                                     then ""
                                     else Path (Path_Reminder_Start .. Path'Last));
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Create_Node (Sub_Path, Kind);
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
      FS : constant Any_FS_Driver := This.Find_FS (Path, Path_Reminder_Start);
      Sub_Path : constant String := (if Path_Reminder_Start not in Path'Range
                                     then ""
                                     else Path (Path_Reminder_Start .. Path'Last));
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Create_Directory (Sub_Path);
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
      FS : constant Any_FS_Driver := This.Find_FS (Path, Path_Reminder_Start);
      Sub_Path : constant String := (if Path_Reminder_Start not in Path'Range
                                     then ""
                                     else Path (Path_Reminder_Start .. Path'Last));
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Unlink (Sub_Path);
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
      FS : constant Any_FS_Driver := This.Find_FS (Path, Path_Reminder_Start);
      Sub_Path : constant String := (if Path_Reminder_Start not in Path'Range
                                     then ""
                                     else Path (Path_Reminder_Start .. Path'Last));
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Remove_Directory (Sub_Path);
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
   function Truncate_File (This   : in out VFS;
                           Path   : Pathname;
                           Length : IO_Count)
                           return Status_Kind
   is
      Path_Reminder_Start : Integer;
      FS : constant Any_FS_Driver := This.Find_FS (Path, Path_Reminder_Start);
      Sub_Path : constant String := (if Path_Reminder_Start not in Path'Range
                                     then ""
                                     else Path (Path_Reminder_Start .. Path'Last));
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Truncate_File (Sub_Path, Length);
      end if;
   end Truncate_File;

   ----------
   -- Open --
   ----------

   overriding
   function Open (This    : in out VFS;
                  Path    : Pathname;
                  Mode    : File_Mode;
                  Handler : out Any_File_Handle)
                  return Status_Kind
   is
      Path_Reminder_Start : Integer;
      FS : constant Any_FS_Driver := This.Find_FS (Path, Path_Reminder_Start);
   begin
      if FS = null then
         return No_Such_File_Or_Directory;
      else
         return FS.Open (Path (Path_Reminder_Start .. Path'Last), Mode, Handler);
      end if;
   end Open;

   --------------------
   -- Open_Directory --
   --------------------

   overriding
   function Open_Directory (This   : in out VFS;
                            Path   : Pathname;
                            Handle : out Any_Directory_Handle)
                            return Status_Kind
   is
      Path_Reminder_Start : Integer;
      FS : constant Any_FS_Driver := This.Find_FS (Path, Path_Reminder_Start);
      Sub_Path : constant String := (if Path_Reminder_Start not in Path'Range
                                     then ""
                                     else Path (Path_Reminder_Start .. Path'Last));
   begin
      if Path = "/" or else Path = "" then
         This.Dir_Handle.FS := This'Unchecked_Access;
         Handle := This.Dir_Handle'Unchecked_Access;
         return Status_Ok;
      else
         if FS = null then
            return No_Such_File_Or_Directory;
         else
            return FS.Open_Directory (Sub_Path,
                                      Handle);
         end if;
      end if;
   end Open_Directory;

   ----------------
   -- Read_Entry --
   ----------------

   overriding
   function Read_Entry (This         : in out VFS_Directory_Handle;
                        Entry_Number : Positive;
                        Dir_Entry    : out Directory_Entry)
                        return Status_Kind
   is
      Pt  : Mount_Point_Access := This.FS.Mount_points;
      Nbr : Positive := Positive'First;
   begin
      while Pt /= null and then Nbr /= Entry_Number loop
         Pt := Pt.Next;
         Nbr := Nbr + 1;
      end loop;

      if Pt /= null then
         Dir_Entry.Entry_Type := Directory;
         return Status_Ok;
      else
         return No_Such_File_Or_Directory;
      end if;

   end Read_Entry;

   ----------------
   -- Entry_Name --
   ----------------

   overriding
   function Entry_Name (This         : in out VFS_Directory_Handle;
                        Entry_Number : Positive)
                        return Pathname
   is
      Pt  : Mount_Point_Access := This.FS.Mount_points;
      Nbr : Positive := Positive'First;
   begin
      while Pt /= null and then Nbr /= Entry_Number loop
         Pt := Pt.Next;
         Nbr := Nbr + 1;
      end loop;

      if Pt /= null then
         return Pt.Directory.all;
      else
         return "";
      end if;
   end Entry_Name;

   -----------
   -- Close --
   -----------
   overriding
   function Close (This : in out VFS_Directory_Handle)
                   return Status_Kind
   is
      pragma Unreferenced (This);
   begin
      return Status_Ok;
   end Close;

end Virtual_File_System;
