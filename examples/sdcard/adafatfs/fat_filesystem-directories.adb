with Ada.Unchecked_Conversion;

package body FAT_Filesystem.Directories is

   ----------
   -- Find --
   ----------

   function Find
     (FS     : FAT_Filesystem_Access;
      Path   : FAT_Path;
      DEntry : out Directory_Entry) return Status_Code
   is
      Status  : Status_Code;
      Full    : FAT_Path;
      Idx     : Natural := 2;
      Token   : FAT_Name;
      Current : Directory_Handle;

   begin
      Full := Path;
      Normalize (Full);
      Idx := 2;

      Status := Open_Root_Directory (FS, Current);

      if Status /= OK then
         return Status;
      end if;

      DEntry := Root_Entry (FS);

      while Idx <= Full.Len loop
         Token.Len := 0;

         for J in Idx .. Full.Len loop
            if Full.Name (J) = '/' then
               Idx := J + 1;
               exit;
            end if;

            Token.Len := Token.Len + 1;
            Token.Name (Token.Len) := Full.Name (J);
         end loop;

         Dir_Loop :
         loop
            Status := Read (Current, DEntry);

            if Status /= OK then
               Close (Current);

               return No_Such_File;
            end if;

            if Name (DEntry) = Token
              or else Short_Name (DEntry) = Token
            then
               Close (Current);

               if Idx < Full.Len then
                  --  Intermediate entry: needs to be a directory
                  if not Is_Subdirectory (DEntry) then
                     return No_Such_Path;
                  end if;

                  Status := Open_Dir (DEntry, Current);

                  if Status /= OK then
                     return Status;
                  end if;
               end if;

               exit Dir_Loop;
            end if;
         end loop Dir_Loop;
      end loop;

      return OK;
   end Find;

   ----------------
   -- Root_Entry --
   ----------------

   function Root_Entry (FS : FAT_Filesystem_Access) return Directory_Entry
   is
   begin
      return (FS         => FS,
              Attributes => (Subdirectory => True,
                             others       => False),
              Is_Root    => True,
              L_Name     => Get_Mount_Point (FS),
              others     => <>);
   end Root_Entry;

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

   function Open_Dir
     (E   : Directory_Entry;
      Dir : out Directory_Handle) return Status_Code
   is
   begin
      if E.Is_Root then
         return Open_Root_Directory (E.FS, Dir);
      else
         Dir.Start_Cluster := E.Start_Cluster;
         Dir.FS := E.FS;

         Reset (Dir);

         return OK;
      end if;
   end Open_Dir;

   -----------
   -- Reset --
   -----------

   procedure Reset_Dir (Dir : in out Directory_Handle)
   is
   begin
      Dir.Current_Block   := Cluster_To_Block (Dir.FS.all, Dir.Start_Cluster);
      Dir.Current_Cluster := Dir.Start_Cluster;
      Dir.Current_Index   := 0;
   end Reset_Dir;

   -----------
   -- Close --
   -----------

   procedure Close_Dir (Dir : in out Directory_Handle)
   is
   begin
      Dir.FS              := null;
      Dir.Current_Index   := 0;
      Dir.Start_Cluster   := 0;
      Dir.Current_Cluster := 0;
      Dir.Current_Block   := 0;
   end Close_Dir;

   ----------
   -- Read --
   ----------

   function Read_Dir
     (Dir    : in out Directory_Handle;
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

      C            : Unsigned_8;
      Last_Seq     : VFAT_Sequence_Number := 0;
      CRC          : Unsigned_8 := 0;
      Matches      : Boolean;
      Current_CRC  : Unsigned_8;
      Block_Off    : Unsigned_16;
      L_Name       : String (1 .. MAX_FILENAME_LENGTH);
      L_Name_First : Natural;

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

      L_Name_First := L_Name'Last + 1;

      loop
         Block_Off :=
           Unsigned_16
             (Unsigned_32 (Dir.Current_Index) * 32
              mod (Dir.FS.Block_Size_In_Bytes * CACHE_SIZE));

         if Block_Off = 0 then
            if Dir.Current_Index > 0 then
               Dir.Current_Block := Dir.Current_Block + CACHE_SIZE;
            end if;

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
               L_Name_First := L_Name'Last + 1;
               Last_Seq := 0;

            else
               if Last_Seq = 0
                 or else Last_Seq - 1 /= V_Entry.VFAT_Attr.Sequence
               then
                  L_Name_First := L_Name'Last + 1;
               end if;

               Last_Seq := V_Entry.VFAT_Attr.Sequence;

               Prepend (V_Entry.Name_3, L_Name, L_Name_First);
               Prepend (V_Entry.Name_2, L_Name, L_Name_First);
               Prepend (V_Entry.Name_1, L_Name, L_Name_First);

               if V_Entry.VFAT_Attr.Sequence = 1 then
                  CRC := V_Entry.Checksum;
               end if;
            end if;

         elsif not D_Entry.Attributes.Volume_Label --  Ignore Volumes
           and then Character'Pos (D_Entry.Filename (1)) /= 16#E5# --  Ignore deleted files
         then
            if L_Name_First not in L_Name'Range then
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

            if Matches then
               DEntry.L_Name := -L_Name (L_Name_First .. L_Name'Last);
            else
               DEntry.L_Name.Len := 0;
            end if;

            return OK;
         end if;
      end loop;
   end Read_Dir;

   ----------
   -- Name --
   ----------

   function Name (E : Directory_Entry) return FAT_Name
   is
   begin
      if E.L_Name.Len > 0 then
         return E.L_Name;
      else
         return Short_Name (E);
      end if;
   end Name;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name (E : Directory_Entry) return FAT_Name is
   begin
      return -E.S_Name (E.S_Name'First .. E.S_Name_Last);
   end Short_Name;

end FAT_Filesystem.Directories;
