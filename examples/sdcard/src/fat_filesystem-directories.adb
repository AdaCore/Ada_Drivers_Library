with Ada.Unchecked_Conversion;

package body FAT_Filesystem.Directories is

   -------------------------
   -- Open_Root_Directory --
   -------------------------

   function Open_Root_Directory
     (FS  : FAT_Filesystem_Access;
      Dir : out Directory_Handle) return Status_Code
   is

   begin
      if FS.Version = FAT16 then
         Dir.Start_Cluster   := 0;
         Dir.Current_Block   :=
           Unsigned_32 (FS.Reserved_Blocks) +
           FS.FAT_Table_Size_In_Blocks *
             Unsigned_32 (FS.Number_Of_FATs);
      else
         Dir.Start_Cluster := FS.Root_Dir_Cluster;
         Dir.Current_Block := FS.Cluster_To_Block (FS.Root_Dir_Cluster);
      end if;

      Dir.FS := FS;
      Dir.Current_Cluster := Dir.Start_Cluster;
      Dir.Current_Index   := 0;

      return OK;
   end Open_Root_Directory;

   ----------
   -- Open --
   ----------

   function Open
     (E   : Directory_Entry;
      Dir : out Directory_Handle) return Status_Code
   is
   begin
      Dir.Start_Cluster := E.Start_Cluster;
      Dir.Current_Block := E.FS.Cluster_To_Block (E.Start_Cluster);
      Dir.FS := E.FS;
      Dir.Current_Cluster := Dir.Start_Cluster;
      Dir.Current_Index   := 0;

      return OK;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Dir : in out Directory_Handle)
   is
   begin
      Dir.FS              := null;
      Dir.Current_Index   := 0;
      Dir.Start_Cluster   := 0;
      Dir.Current_Cluster := 0;
      Dir.Current_Block   := 0;
   end Close;

   ----------
   -- Read --
   ----------

   function Read (Dir    : in out Directory_Handle;
                  DEntry : out Directory_Entry) return Status_Code
   is
      procedure Prepend
        (Name : Wide_String;
         Full : in out String;
         Idx  : in out Natural);

      -------------
      -- Prepend --
      -------------

      procedure Prepend
        (Name : Wide_String;
         Full : in out String;
         Idx  : in out Natural)
      is
         Val : Unsigned_16;
      begin
         for J in reverse Name'Range loop
            Val := Wide_Character'Pos (Name (J));
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

      Ret     : Status_Code;
      D_Entry : FAT_Directory_Entry;
      V_Entry : VFAT_Directory_Entry;
      subtype Entry_Data is Block (1 .. 32);
      function To_Entry is new Ada.Unchecked_Conversion
        (Entry_Data, FAT_Directory_Entry);
      function To_VFAT_Entry is new Ada.Unchecked_Conversion
        (FAT_Directory_Entry, VFAT_Directory_Entry);

      C           : Unsigned_8;
      Last_Seq    : VFAT_Sequence_Number := 0;
      CRC         : Unsigned_8 := 0;
      Matches     : Boolean;
      Current_CRC : Unsigned_8;
      Block_Off   : Unsigned_16;

   begin
      if Dir.Start_Cluster = 0
        and then Dir.Current_Index >= Dir.FS.Number_Of_Entries_In_Root_Dir
      then
         return Invalid_Object_Entry;
      elsif Dir.Current_Index = 16#FFFF# then
         return Invalid_Object_Entry;
      end if;

      Ret := Dir.FS.Ensure_Block (Dir.Current_Block);

      if Ret /= OK then
         return Ret;
      end if;

      DEntry.L_Name_First := DEntry.L_Name'Last + 1;

      loop
         Block_Off :=
           Unsigned_16
             (Unsigned_32 (Dir.Current_Index) * 32
              mod Dir.FS.Block_Size_In_Bytes);

         if Dir.Current_Index > 0
           and then Block_Off = 0
         then
            Dir.Current_Block := Dir.Current_Block + 1;
            if Dir.Current_Block -
              Dir.FS.Cluster_To_Block (Dir.Current_Cluster) =
              Unsigned_32 (Dir.FS.Number_Of_Blocks_Per_Cluster)
            then
               Dir.Current_Cluster := Dir.FS.Get_FAT (Dir.Current_Cluster);

               if Dir.Current_Cluster = 1
                 or else Dir.FS.EOC (Dir.Current_Cluster)
               then
                  return Internal_Error;
               end if;

               Dir.Current_Block :=
                 Dir.FS.Cluster_To_Block (Dir.Current_Cluster);
            end if;

            Ret := Dir.FS.Ensure_Block (Dir.Current_Block);

            if Ret /= OK then
               return Ret;
            end if;
         end if;

         if Dir.FS.Window (Block_Off) = 0 then
            --  End of entries
            Dir.Current_Index := 16#FFFF#;
            return Invalid_Object_Entry;
         end if;

         D_Entry := To_Entry (Dir.FS.Window (Block_Off .. Block_Off + 31));
         Dir.Current_Index := Dir.Current_Index + 1;

         --  Check if we have a VFAT entry here by checking that the
         --  attributes are 16#0F# (e.g. all attributes set except
         --  subdirectory and archive)
         if D_Entry.Attributes = VFAT_Directory_Entry_Attribute then
            V_Entry := To_VFAT_Entry (D_Entry);

            if V_Entry.VFAT_Attr.Stop_Bit then
               DEntry.L_Name_First := DEntry.L_Name'Last + 1;
               Last_Seq := 0;

            else
               if Last_Seq = 0
                 or else Last_Seq - 1 /= V_Entry.VFAT_Attr.Sequence
               then
                  DEntry.L_Name_First := DEntry.L_Name'Last + 1;
               end if;

               Last_Seq := V_Entry.VFAT_Attr.Sequence;

               Prepend (V_Entry.Name_3, DEntry.L_Name, DEntry.L_Name_First);
               Prepend (V_Entry.Name_2, DEntry.L_Name, DEntry.L_Name_First);
               Prepend (V_Entry.Name_1, DEntry.L_Name, DEntry.L_Name_First);

               if V_Entry.VFAT_Attr.Sequence = 1 then
                  CRC := V_Entry.Checksum;
               end if;
            end if;

         elsif not D_Entry.Attributes.Volume_Label --  Ignore Volumes
           and then Character'Pos (D_Entry.Filename (1)) /= 16#E5# --  Ignore deleted files
         then
            if DEntry.L_Name_First not in DEntry.L_Name'Range then
               Matches := False;
            else
               Current_CRC := 0;
               Last_Seq := 0;

               for Ch of String'(D_Entry.Filename & D_Entry.Extension) loop
                  C := Character'Enum_Rep (Ch);
                  Current_CRC := Shift_Right (Current_CRC and 16#FE#, 1)
                    or Shift_Left (Current_CRC and 16#01#, 7);
                  --  Modulo addition
                  Current_CRC := Current_CRC + C;
               end loop;

               Matches := Current_CRC = CRC;
            end if;

            declare
               Base   : String renames Trim (D_Entry.Filename);
               Ext    : String renames Trim (D_Entry.Extension);
               S_Name : constant String :=
                          Base &
                          (if Ext'Length > 0
                           then "." & Trim (D_Entry.Extension)
                           else "");
            begin
               DEntry.S_Name (1 .. S_Name'Length) := S_Name;
               DEntry.S_Name_Last := S_Name'Length;
            end;

            DEntry.Attributes    := D_Entry.Attributes;
            DEntry.Start_Cluster := Unsigned_32 (D_Entry.Cluster_L);
            DEntry.Size          := D_Entry.Size;
            DEntry.FS            := Dir.FS;

            if Dir.FS.Version = FAT32 then
               DEntry.Start_Cluster :=
                 DEntry.Start_Cluster or
                 Shift_Left (Unsigned_32 (D_Entry.Cluster_H), 16);
            end if;

            if not Matches then
               DEntry.L_Name_First := DEntry.L_Name'Last + 1;
            end if;

            return OK;
         end if;
      end loop;
   end Read;

   ----------
   -- Name --
   ----------

   function Name (E : Directory_Entry) return String
   is
   begin
      if E.L_Name_First in E.L_Name'Range then
         return E.L_Name (E.L_Name_First .. E.L_Name'Last);
      else
         return E.S_Name (E.S_Name'First .. E.S_Name_Last);
      end if;
   end Name;

end FAT_Filesystem.Directories;
