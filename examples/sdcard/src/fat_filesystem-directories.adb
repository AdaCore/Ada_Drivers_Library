--  Project: StratoX
--  System:  Stratosphere Balloon Flight Controller
--  Author:  Martin Becker (becker@rcs.ei.tum.de)
--  based on AdaCore's Ada_Driver_Library
--  XXX! Nothing here is thread-safe!

with Ada.Unchecked_Conversion;

--  @summary Directory (end directory entries) handling for FAT FS
package body FAT_Filesystem.Directories with SPARK_Mode => Off is

   -------------------------
   -- Open_Root_Directory --
   -------------------------

   function Open_Root_Directory
     (FS  : FAT_Filesystem_Access;
      Dir : out Directory_Handle) return Status_Code
   is

   begin
      if FS.Version = FAT16 then
         Dir.Dir_Begin := (Cluster => 0, Index => 0, Block =>
                             Unsigned_32 (FS.Reserved_Blocks) +
                             FS.FAT_Table_Size_In_Blocks *
                               Unsigned_32 (FS.Number_Of_FATs));
      else
         Dir.Dir_Begin := (Cluster => FS.Root_Dir_Cluster,
                           Block => FS.Cluster_To_Block (FS.Root_Dir_Cluster),
                           Index => 0);
      end if;
      Dir.FS := FS;
      Rewind (Dir);
      return OK;
   end Open_Root_Directory;

   -------------------------
   --  Make_Directory
   -------------------------

   function Make_Directory
     (Parent  : in out Directory_Handle;
      newname : String;
      D_Entry : out Directory_Entry) return Status_Code
   is
      subtype Entry_Data is Block (1 .. 32);
      function From_Entry is new Ada.Unchecked_Conversion
        (FAT_Directory_Entry, Entry_Data);

      procedure Fill_Entry_Subdirectory (Sub_Entry     : in out Directory_Entry;
                                         Sub_Name      : String;
                                         Start_Cluster : Unsigned_32;
                                         Ent_Addr      : FAT_Address);
      procedure Fill_Entry_Subdirectory (Sub_Entry     : in out Directory_Entry;
                                         Sub_Name      : String;
                                         Start_Cluster : Unsigned_32;
                                         Ent_Addr      : FAT_Address) is
      begin
         Sub_Entry.FS := Parent.FS;
         Sub_Entry.Attributes.Read_Only := False;
         Sub_Entry.Attributes.Hidden := False;
         Sub_Entry.Attributes.Archive := False;
         Sub_Entry.Attributes.System_File := False;
         Sub_Entry.Attributes.Volume_Label := False;
         Sub_Entry.Attributes.Subdirectory := True;
         Sub_Entry.Start_Cluster := Start_Cluster;
         Sub_Entry.Size := 0; -- directories always carry zero
         Sub_Entry.Entry_Address := Ent_Addr;
         Set_Name (Sub_Name, Sub_Entry); -- FIXME: something isn't right here. The implicit dot (8+3) is shown on my Linux machine
      end Fill_Entry_Subdirectory;

      function Entry_To_Window (Sub_Entry : Directory_Entry) return Status_Code;
      function Entry_To_Window (Sub_Entry : Directory_Entry) return Status_Code is
         F_Entry : FAT_Directory_Entry;
         Status  : Status_Code;
      begin
         Status := Directory_To_FAT_Entry (Sub_Entry, F_Entry);
         if Status /= OK then
            return Status;
         end if;

         --  now write the directory entry in the parent
         Status := Parent.FS.Ensure_Block (Sub_Entry.Entry_Address.Block_LBA);
         if Status /= OK then
            return Status;
         end if;
         Parent.FS.Window (Sub_Entry.Entry_Address.Block_Off .. Sub_Entry.Entry_Address.Block_Off
                           + ENTRY_SIZE - 1) := From_Entry (F_Entry);
         return OK;
      end Entry_To_Window;

      NewDir_Addr    : FAT_Address;
      Status         : Status_Code;
      NewDir_Cluster : Unsigned_32 := INVALID_CLUSTER;
   begin
      if Parent.FS.Version /= FAT32 then
         --  we only support FAT32 for now.
         return Internal_Error;
      end if;

      --  find a place for another entry and return it
      Status := Allocate_Entry (Parent, newname, NewDir_Addr);
      if Status = Already_Exists then
         --  FIXME: this is a bit inefficient.
         if Get_Entry (Parent, newname, D_Entry) then
            return Already_Exists;
         else
            return Internal_Error;
         end if;
      elsif Status /= OK then
         return Status;
      end if;

      --  if we are here, then a new entry was created

      --  first, allocate a new cluster for the directory contents
      NewDir_Cluster := Parent.FS.Get_Free_Cluster;
      if NewDir_Cluster = INVALID_CLUSTER then
         return Device_Full;
      end if;
      if not Parent.FS.Allocate_Cluster (NewDir_Cluster)
      then
         return Allocation_Error;
      end if;

      --  fill entry attrs to make it a directory
      Fill_Entry_Subdirectory (Sub_Entry => D_Entry, Sub_Name => newname,
                               Start_Cluster => NewDir_Cluster, Ent_Addr => NewDir_Addr);

      --  write back to disk
      Status := Entry_To_Window (D_Entry);
      if Status /= OK then
         return Status;
      end if;
      Status := Parent.FS.Write_Window (D_Entry.Entry_Address.Block_LBA);
      if Status /= OK then
         return Status;
      end if;

      -------------------------------------------------------------
      --  create obligatory entries "." and ".." in new directory
      -------------------------------------------------------------
      --  we cannot use Allocate_Entry for this, because the directory
      --  doesn't have a terminator, yet. And it's not required, because
      --  block size is > 3 entries anyway.
      pragma Assert (Parent.FS.Block_Size_In_Bytes >= 3 * ENTRY_SIZE);

      declare
         Dot_Addr  : FAT_Address := (Block_LBA => Parent.FS.Cluster_To_Block (NewDir_Cluster),
                                     Block_Off => 0);
         Dot_Entry : Directory_Entry;
      begin
         --  entry "."
         Fill_Entry_Subdirectory (Sub_Entry => Dot_Entry,
                                  Sub_Name => ".",
                                  Start_Cluster => NewDir_Cluster,  -- "." points to same cluster
                                  Ent_Addr => Dot_Addr);
         Status := Entry_To_Window (Dot_Entry);
         if Status /= OK then
            return Status;
         end if;

         --  entry ".."
         Dot_Addr.Block_Off := Dot_Addr.Block_Off + ENTRY_SIZE;
         Fill_Entry_Subdirectory (Sub_Entry => Dot_Entry,
                                  Sub_Name => "..",
                                  Start_Cluster => Parent.Dir_Begin.Cluster, -- FIXME: Microsoft wants zero if Parent = Root
                                  Ent_Addr => Dot_Addr);
         Status := Entry_To_Window (Dot_Entry);
         if Status /= OK then
            return Status;
         end if;

         --  terminator
         Dot_Addr.Block_Off := Dot_Addr.Block_Off + ENTRY_SIZE;
         Parent.FS.Window (Dot_Addr.Block_Off)  := 0;

         --  finally .. write back the block with the new directory contents
         Status := Parent.FS.Write_Window (Parent.FS.Cluster_To_Block (NewDir_Cluster));
      end;

      return Status;
   end Make_Directory;

   -------------------------
   --  Invalidate_Handle_Pointer
   -------------------------

   procedure Invalidate_Handle_Pointer (h : in out Directory_Handle_Pointer) is
   begin
      h.Cluster := INVALID_CLUSTER;
   end Invalidate_Handle_Pointer;

   -------------------------
   --  Valid_Handle_Pointer
   -------------------------

   function Valid_Handle_Pointer (h : Directory_Handle_Pointer) return Boolean
   is (h.Cluster /= INVALID_CLUSTER);

   -------------------------
   --  Rewind
   -------------------------

   procedure Rewind (Dir : in out Directory_Handle) is
   begin
      Dir.Dir_Current := Dir.Dir_Begin;
      Invalidate_Handle_Pointer (Dir.Dir_End);
   end Rewind;

   -------------------------
   --  Open
   -------------------------

   function Open
     (E   : Directory_Entry;
      Dir : out Directory_Handle) return Status_Code
   is
   begin
      Dir.FS := E.FS;
      Dir.Dir_Begin := (Cluster => E.Start_Cluster,
                        Index => 0,
                        Block => E.FS.Cluster_To_Block (E.Start_Cluster));
      Rewind (Dir);
      return OK;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Dir : in out Directory_Handle)
   is
   begin
      Dir.FS              := null;
      Dir.Dir_Begin   := (Cluster => INVALID_CLUSTER, Block => 0, Index => 0);
      Rewind (Dir);
   end Close;

   --  @summary encode Directory_Entry into FAT_Directory_Entry
   function Directory_To_FAT_Entry
     (D_Entry : in Directory_Entry;
      F_Entry : out FAT_Directory_Entry) return Status_Code
   is
   begin
      F_Entry.Date := 0; -- FIXME: set date/time
      F_Entry.Time := 0;
      F_Entry.Size := D_Entry.Size;
      F_Entry.Attributes := D_Entry.Attributes;
      Set_Name (Get_Name (D_Entry), F_Entry);
      F_Entry.Cluster_L := Unsigned_16 (D_Entry.Start_Cluster and 16#FFFF#);
      if D_Entry.FS.Version = FAT16 then
         F_Entry.Cluster_H := 0;
      else
         F_Entry.Cluster_H := Unsigned_16 (Shift_Right
              (D_Entry.Start_Cluster and 16#FFFF_0000#, 16));
      end if;

      return OK;
   end Directory_To_FAT_Entry;

   ----------------------------
   -- FAT_To_Directory_Entry --
   ----------------------------

   function FAT_To_Directory_Entry
     (FS : FAT_Filesystem_Access;
      F_Entry : in FAT_Directory_Entry;
      D_Entry : in out Directory_Entry;
      Last_Seq : in out VFAT_Sequence_Number) return Status_Code
   is
      procedure Prepend
        (VName : Wide_String;
         Full : in out String;
         Idx  : in out Natural);

      procedure Prepend
        (VName : Wide_String;
         Full : in out String;
         Idx  : in out Natural)
      is
         Val : Unsigned_16;
      begin
         for J in reverse VName'Range loop
            Val := Wide_Character'Pos (VName (J));
            if Val /= 16#FFFF#
              and then Val /= 0
            then
               Idx := Idx - 1;

               exit when Idx not in Full'Range;

               if Val < 255 then
                  Full (Idx) := Character'Val (Val);
               else
                  Full (Idx) := '?';
               end if;
            end if;
         end loop;
      end Prepend;

      function To_VFAT_Entry is new Ada.Unchecked_Conversion
        (FAT_Directory_Entry, VFAT_Directory_Entry);
      V_Entry     : VFAT_Directory_Entry;
      Is_VFAT_Ent : Boolean;
   begin

      if F_Entry.Attributes = VFAT_Directory_Entry_Attribute then
         Is_VFAT_Ent := True;
         V_Entry := To_VFAT_Entry (F_Entry);

         if V_Entry.VFAT_Attr.Stop_Bit then
            D_Entry.Name_First := D_Entry.Name'Last + 1;  --  starts here. invalidate long name.
            Last_Seq := 0;
         else
            if Last_Seq = 0
              or else Last_Seq - 1 /= V_Entry.VFAT_Attr.Sequence
            then
               D_Entry.Name_First := D_Entry.Name'Last + 1;
            end if;

            Last_Seq := V_Entry.VFAT_Attr.Sequence;

            Prepend (V_Entry.Name_3, D_Entry.Name, D_Entry.Name_First);
            Prepend (V_Entry.Name_2, D_Entry.Name, D_Entry.Name_First);
            Prepend (V_Entry.Name_1, D_Entry.Name, D_Entry.Name_First);

            if V_Entry.VFAT_Attr.Sequence = 1 then
               D_Entry.CRC := V_Entry.Checksum;
            end if;
         end if;
         --  VFAT needs more data...cannot return "OK", yet
      elsif not F_Entry.Attributes.Volume_Label --  Ignore Volumes
        and then (not Is_Deleted (F_Entry))
      then
         --  either the "tail" of a VFAT entry, or a regular entry
         if D_Entry.Name_First not in D_Entry.Name'Range then
            --  regular entry
            Is_VFAT_Ent := False;
         else
            --  VFAT tail. must check crc
            Is_VFAT_Ent := True;
            declare
               Current_CRC : Unsigned_8 := 0;
               C           : Unsigned_8 := 0;
               shortname   : constant String := F_Entry.Filename & F_Entry.Extension;
            begin
               Last_Seq := 0;
               for Ch of shortname loop
                  C := Character'Enum_Rep (Ch);
                  Current_CRC := Shift_Right (Current_CRC and 16#FE#, 1)
                    or Shift_Left (Current_CRC and 16#01#, 7);
                  --  Modulo addition
                  Current_CRC := Current_CRC + C;
               end loop;
               Is_VFAT_Ent := Current_CRC = D_Entry.CRC; -- if mismatch, drop VFAT entry
            end;
         end if;

         if Is_VFAT_Ent then
            --  terminate string
            D_Entry.Name_Last := D_Entry.Name'Last;
         else
            --  set short name for regular entries
            if not F_Entry.Attributes.Subdirectory then
               declare
                  Base   : String renames RTrim (F_Entry.Filename);
                  Ext    : String renames RTrim (F_Entry.Extension);
                  Ent_Name : constant String :=
                    Base & (if Ext'Length > 0 then "." & Ext else "");
               begin
                  Set_Name (Ent_Name, D_Entry);
               end;
            else
               declare
                  Cat_Name : constant String := F_Entry.Filename & F_Entry.Extension;
                  Ent_Name : String renames RTrim (Cat_Name);
               begin
                  Set_Name (Ent_Name, D_Entry);
               end;
            end if;
         end if;

         D_Entry.Attributes    := F_Entry.Attributes;
         D_Entry.Start_Cluster := Unsigned_32 (F_Entry.Cluster_L);
         D_Entry.Size          := F_Entry.Size;
         D_Entry.FS            := FS;
         --  FIXME: add date and time

         if FS.Version = FAT32 then
            D_Entry.Start_Cluster :=
              D_Entry.Start_Cluster or
              Shift_Left (Unsigned_32 (F_Entry.Cluster_H), 16);
         end if;

         return OK; -- finished fetching next entry
      end if;
      return Invalid_Name;
   end FAT_To_Directory_Entry;

   ---------------
   --  Is_Deleted
   ---------------

   function Is_Deleted (F_Entry : FAT_Directory_Entry) return Boolean
   is (Character'Pos (F_Entry.Filename (1)) = 16#E5#);

   ---------------
   --  Read Dir
   ---------------

   function Read (Dir    : in out Directory_Handle;
                  DEntry : out Directory_Entry;
                  Deleted : Boolean := False) return Status_Code
   is
      subtype Entry_Data is Block (1 .. 32);
      function To_Entry is new Ada.Unchecked_Conversion (Entry_Data, FAT_Directory_Entry);
      Ret         : Status_Code;
      F_Entry     : FAT_Directory_Entry;
      Last_Seq    : VFAT_Sequence_Number := 0;
      Block_Off   : Unsigned_16;
   begin
      if Dir.Dir_Begin.Cluster = 0
        and then Dir.Dir_Current.Index >= Dir.FS.Number_Of_Entries_In_Root_Dir
      then
         return Invalid_Object_Entry;
      elsif Dir.Dir_Current.Index = 16#FFFF# then -- we are at the end
         return Invalid_Object_Entry;
      end if;

      Ret := Dir.FS.Ensure_Block (Dir.Dir_Current.Block);
      if Ret /= OK then
         return Ret;
      end if;

      --  invalidate name
      DEntry.Name_First := DEntry.Name'Last + 1;
      DEntry.Name_Last := DEntry.Name'First - 1;

      --  Dir_Current.index has been incremented before (in the previous call)
      Fetch_Next : loop
         Block_Off :=
           Unsigned_16
             (Unsigned_32 (Dir.Dir_Current.Index) * ENTRY_SIZE
              mod Dir.FS.Block_Size_In_Bytes);
         --  offset of entry within current block

         --  do we need to fetch the next block?
         if Dir.Dir_Current.Index > 0
           and then Block_Off = 0
         then
            Dir.Dir_Current.Block := Dir.Dir_Current.Block + 1;
            --  do we need to fetch the next cluster?
            if Dir.Dir_Current.Block -
              Dir.FS.Cluster_To_Block (Dir.Dir_Current.Cluster) =
              Unsigned_32 (Dir.FS.Number_Of_Blocks_Per_Cluster)
            then
               Dir.Dir_Current.Cluster := Dir.FS.Get_FAT (Dir.Dir_Current.Cluster);

               if Dir.Dir_Current.Cluster = INVALID_CLUSTER
                 or else Dir.FS.Is_Last_Cluster (Dir.Dir_Current.Cluster)
               then
                  return Internal_Error;
               end if;

               Dir.Dir_Current.Block :=
                 Dir.FS.Cluster_To_Block (Dir.Dir_Current.Cluster);
            end if;

            --  read next block
            Ret := Dir.FS.Ensure_Block (Dir.Dir_Current.Block);

            if Ret /= OK then
               return Ret;
            end if;
         end if;

         --  are we at the end of the entries?
         if Dir.FS.Window (Block_Off) = 0 then
            Dir.Dir_Current.Index := 16#FFFF#;
            --  invalidates the handle...next call to this will return.
            --  because Dir_current.block is already invalid now
            return Invalid_Object_Entry;
         end if;

         --  okay, there are more entries. each entry is 32bytes:
         Dir.Dir_End := Dir.Dir_Current; -- remember last valid
         F_Entry := To_Entry (Dir.FS.Window (Block_Off .. Block_Off + ENTRY_SIZE - 1));

         --  move forward for next call of Read()
         Dir.Dir_Current.Index := Dir.Dir_Current.Index + 1;

         --  continue until we have an entry...because VFAT has multiple parts
         if Deleted and then Is_Deleted (F_Entry) then
            --  caller also wants deleted...we found one
            DEntry.FS := null;
            exit Fetch_Next;
         else
            if FAT_To_Directory_Entry
              (Dir.FS, F_Entry, DEntry, Last_Seq) = OK
            then
               exit Fetch_Next;
            end if;
         end if;
      end loop Fetch_Next;
      --  if we are here, then we have an entry
      DEntry.Entry_Address := (Block_LBA => Dir.Dir_Current.Block, Block_Off => Block_Off);
      return OK;
   end Read;

   ---------------
   --  Set_Name
   ---------------

   procedure Set_Name (newname : String; D : in out Directory_Entry) is
      trimname : String renames LTrim (newname); -- leading spaces not allowed in FAT
      maxlen   : constant Integer := trimname'Length;
      newlast  : constant Integer := D.Name'First + maxlen - 1;
   begin
      D.Name (D.Name'First .. newlast) := trimname;
      D.Name_First := D.Name'First;
      D.Name_Last := newlast;
   end Set_Name;

   ---------------
   --  Set_Name
   ---------------

   procedure Set_Name (newname : String; E : in out FAT_Directory_Entry) is
   begin
      if E.Attributes.Subdirectory then
         StrCpySpace (outstring => E.Filename, instring => newname);
         if newname'Length > E.Filename'Length then
            StrCpySpace (E.Extension, newname (newname'First + E.Filename'Length .. newname'Last));
         else
            StrCpySpace (E.Extension, "");
         end if;
      else
         --  file: remove '.' and limit to 8+3...
         declare
            base  : String (1 .. 8);
            ext   : String (1 .. 3);
            dotpos : constant Integer := StrChr (newname, '.');
         begin
            if dotpos in newname'Range then
               if dotpos = newname'First then
                  StrCpySpace (base, "");
               else
                  StrCpySpace (base, newname (newname'First .. dotpos - 1));
               end if;
               if dotpos = newname'Last then
                  StrCpySpace (ext, "");
               else
                  StrCpySpace (ext, newname (dotpos + 1 .. newname'Last));
               end if;
            else
               StrCpySpace (base, newname);
               StrCpySpace (ext, "");
            end if;
            E.Filename := base;
            E.Extension := ext;
         end;
      end if;
   end Set_Name;

   ---------------
   --  Get_Name
   ---------------

   function Get_Name (E : Directory_Entry) return String is
   begin
      if E.Name_First in E.Name'Range and then E.Name_Last in E.Name'Range then
         return E.Name (E.Name_First .. E.Name_Last);
      else
         return "";
      end if;
   end Get_Name;

   -------------------------
   --  Get_Entry_Or_Deleted
   --------------------------

   function Get_Entry_Or_Deleted
     (Parent  : in out Directory_Handle;
      E_Name  : String;
      Ent     : out Directory_Entry;
      Deleted : out Boolean) return Boolean
   is
      FAT_Approx_Name : String renames Trim (E_Name);
      --  FIXME: this isn't completely right, since ent_name
      --  went through FAT name compression, but E_Name not.
   begin
      Rewind (Parent); Deleted := False;
      while Read (Parent, Ent, True) = OK loop
         if Ent.FS = null then
            Deleted := True;
            return True;
         else
            declare
               Ent_Name : constant String := Get_Name (Ent);
            begin
               if Ent_Name = FAT_Approx_Name then
                  return True;
               end if;
            end;
         end if;
      end loop;

      return False;
   end Get_Entry_Or_Deleted;

   ---------------
   --  Get_Entry
   ---------------

   function Get_Entry
     (Parent  : in out Directory_Handle;
      E_Name  : String;
      Ent     : out Directory_Entry) return Boolean
   is
   begin
      Rewind (Parent);
      while Read (Parent, Ent, False) = OK loop
         declare
            ent_name : constant String := Get_Name (Ent);
         begin
            --  FIXME: this isn't completely right, since ent_name
            --  went through FAT name compression, but New_Name not.
            if ent_name = E_Name then
               return True;
            end if;
         end;
      end loop;

      return False;
   end Get_Entry;

   ---------------------
   --  Goto_Last_Entry
   ---------------------

   procedure Goto_Last_Entry (Parent   : in out Directory_Handle) is
      Tmp : Directory_Entry;
   begin
      while (Read (Parent, Tmp)) = OK loop
         null;
      end loop;
   end Goto_Last_Entry;

   --------------------
   --  Allocate_Entry
   --------------------

   function Allocate_Entry
     (Parent   : in out Directory_Handle;
      New_Name : String;
      Ent_Addr : out FAT_Address) return Status_Code
   is
      Block_Off : Unsigned_16;
   begin
      if Parent.FS.Version /= FAT32 then
         --  we only support FAT32 for now.
         return Internal_Error;
      end if;

      declare
         Ent : Directory_Entry;
         Deleted : Boolean;
      begin
         if Get_Entry_Or_Deleted (Parent => Parent, E_Name => New_Name,
                                  Ent => Ent, Deleted => Deleted)
         then
            if Deleted then
               --  silently re-use the deleted entry
               Ent_Addr := Ent.Entry_Address;
               return OK;
            else
               return Already_Exists;
            end if;
         end if;
      end;

      --  if we are here, we really need to add a new entry.
      Goto_Last_Entry (Parent);

      if not Valid_Handle_Pointer (Parent.Dir_End) then
         --  this means the directory is completely empty. should never happen
         --  FIXME: for more robustness, we need to be able to start at index zero
         return Internal_Error;
      end if;

      --  now append
      Parent.Dir_End.Index := Parent.Dir_End.Index + 1;

      --  if we are here, then we reached the last entry in the directory.
      --  see whether block_off is small enough to accommodate another entry
      --  block_Off .. Block_Off + 31 must be available, otherwise we need a
      --  the next block
      Block_Off := Unsigned_16 (Unsigned_32
                                (Parent.Dir_End.Index) * ENTRY_SIZE
                                mod Parent.FS.Block_Size_In_Bytes);


      --  do we need a new block?
      if Block_Off = 0 and then Parent.Dir_End.Index > 0 then
         --  need a new block
         Parent.Dir_End.Block := Parent.Dir_End.Block + 1;
         --  do we need to allocate a new cluster for the new block?
         if Parent.Dir_End.Block -
           Parent.FS.Cluster_To_Block (Parent.Dir_End.Cluster) =
           Unsigned_32 (Parent.FS.Number_Of_Blocks_Per_Cluster)
         then
            --  need another cluster. find free one and allocate it
            declare
               New_Cluster : Unsigned_32;
               Status      : Status_Code;
            begin
               Status := Parent.FS.Append_Cluster
                 (Last_Cluster => Parent.Dir_End.Cluster,
                  New_Cluster => New_Cluster);
               if Status /= OK then
                  return Status;
               end if;
               Parent.Dir_End.Cluster := New_Cluster;
               Parent.Dir_End.Block :=
                 Parent.FS.Cluster_To_Block (Parent.Dir_End.Cluster);
            end;
         end if;
      end if;

      --  FIXME: refactor following to separate procedure (too long function)
      --  we added a new entry. make sure the directory has a terminator:
      --  check if there is more space in the cluster after the new entry
      --  if yes, terminate the directory by writing a zero.
      --  Actually, Microsoft recommends to zero the entire cluster to
      --  avoid this ugly hack. But this should be faster and is less wearout.
      declare
         Terminator_Off : constant Unsigned_16 :=
           Unsigned_16 (Unsigned_32
                        (Parent.Dir_End.Index + 1) * ENTRY_SIZE
                        mod Parent.FS.Block_Size_In_Bytes);
         Status : Status_Code;
      begin
         if Terminator_Off /= 0 then
            --  there is space in the same block, terminate at Terminator_Off.
            Status := Parent.FS.Ensure_Block (Parent.Dir_End.Block);
            if Status /= OK then
               return Status;
            end if;
            Parent.FS.Window (Terminator_Off) := 0;
            Status := Parent.FS.Write_Window (Parent.Dir_End.Block);
            if Status /= OK then
               return Status;
            end if;
         else
            --  need a new block. This is only relevant if it is still the same cluster
            declare
               Terminator_Block : constant Unsigned_32 := Parent.Dir_End.Block + 1;
            begin
               if Terminator_Block - Parent.FS.Cluster_To_Block (Parent.Dir_End.Cluster) <
                 Unsigned_32 (Parent.FS.Number_Of_Blocks_Per_Cluster)
               then
                  --  terminate in Terminator_Block at pos 0.
                  Status := Parent.FS.Ensure_Block (Terminator_Block);
                  if Status /= OK then
                     return Status;
                  end if;
                  Parent.FS.Window (Parent.FS.Window'First) := 0;
                  Status := Parent.FS.Write_Window (Terminator_Block);
                  if Status /= OK then
                     return Status;
                  end if;
               end if;
            end;
         end if;
      end;

      --  return the allocated space
      Ent_Addr.Block_LBA := Parent.Dir_End.Block;
      Ent_Addr.Block_Off := Block_Off;
      return OK;
   end Allocate_Entry;
end FAT_Filesystem.Directories;
