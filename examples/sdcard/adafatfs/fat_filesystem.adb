with Ada.Unchecked_Conversion;
with FAT_Filesystem.Directories;

package body FAT_Filesystem is

   Volumes : array (1 .. MAX_VOLUMES) of aliased FAT_Filesystem;
   --  Mounted volumes

   Current_Path : FAT_Path := (Name => (1 => '/', others => ' '), Len => 1);

   procedure Initialize_FS
     (FS     : in out FAT_Filesystem;
      Status : out Status_Code);

   ---------
   -- "-" --
   ---------

   function "-" (Name : FAT_Name) return String
   is
   begin
      return Name.Name (1 .. Name.Len);
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (Name : String) return FAT_Name
   is
      Ret : FAT_Name;
   begin
      for J in Name'Range loop
         if Name (J) = '/' then
            raise Constraint_Error;
         end if;
      end loop;

      Ret.Len := Name'Length;
      Ret.Name (1 .. Name'Length) := Name;
      return Ret;
   end "-";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Name1, Name2 : FAT_Name) return Boolean
   is
   begin
      return Name1.Len = Name2.Len
        and then Name1.Name (1 .. Name1.Len) = Name2.Name (1 .. Name2.Len);
   end "=";

   ---------
   -- "-" --
   ---------

   function "-" (Path : FAT_Path) return String
   is
   begin
      return Path.Name (1 .. Path.Len);
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (Path : String) return FAT_Path
   is
      Ret : FAT_Path;
   begin
      Ret.Len := Path'Length;
      Ret.Name (1 .. Path'Length) := Path;
      return Ret;
   end "-";

   ------------
   -- Append --
   ------------

   procedure Append
     (Path : in out FAT_Path;
      Name : FAT_Name)
   is
   begin
      if Path.Name (Path.Len) /= '/' then
         Path.Len := Path.Len + 1;
         Path.Name (Path.Len) := '/';
      end if;

      Path.Name (Path.Len + 1 .. Path.Len + Name.Len) :=
        Name.Name (1 .. Name.Len);
      Path.Len := Path.Len + Name.Len;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Path     : in out FAT_Path;
      Sub_Path : FAT_Path)
   is
   begin
      if Path.Name (Path.Len) /= '/' then
         Path.Len := Path.Len + 1;
         Path.Name (Path.Len) := '/';
      end if;

      if Sub_Path.Len > 0
        and then Sub_Path.Name (1) = '/'
      then
         Path.Name (Path.Len + 1 .. Path.Len + Sub_Path.Len - 1) :=
           Sub_Path.Name (2 .. Sub_Path.Len);
         Path.Len := Path.Len + Sub_Path.Len - 1;
      else
         Path.Name (Path.Len + 1 .. Path.Len + Sub_Path.Len) :=
           Sub_Path.Name (1 .. Sub_Path.Len);
         Path.Len := Path.Len + Sub_Path.Len;
      end if;
   end Append;

   ---------
   -- "&" --
   ---------

   function "&" (Path : FAT_Path; Name : FAT_Name) return FAT_Path
   is
      Ret : FAT_Path := Path;
   begin
      Append (Ret, Name);

      return Ret;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Path : FAT_Path; Sub_Path : FAT_Path) return FAT_Path
   is
      Ret : FAT_Path := Path;
   begin
      Append (Ret, Sub_Path);

      return Ret;
   end "&";

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Path : FAT_Path) return Boolean
   is
   begin
      return Path.Len = 0 or else
        (Path.Len = 1 and then Path.Name (1) = '/');
   end Is_Root;

   ---------------
   -- To_Parent --
   ---------------

   procedure To_Parent (Path : in out FAT_Path)
   is
   begin
      if Path.Len = 0 then
         return;
      end if;

      if Path.Name (Path.Len) = '/' then
         Path.Len := Path.Len - 1;
      end if;

      for J in reverse 1 .. Path.Len loop
         if Path.Name (J) = '/' then
            Path.Len := J;

            return;
         end if;
      end loop;

      Path.Len := 0;
   end To_Parent;

   ------------
   -- Parent --
   ------------

   function Parent (Path : FAT_Path) return FAT_Path
   is
      Ret  : FAT_Path := Path;
   begin
      To_Parent (Ret);

      return Ret;
   end Parent;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (Path : in out FAT_Path)
   is
      Idx      : Natural := 1;
      Prev     : Natural;
      Token    : FAT_Name;

   begin
      if Path.Len = 0 then
         Path := Current_Path;
         return;

      elsif Path.Name (1) /= '/' then
         Path := Current_Path & Path;
      end if;

      --  Preserve initial '/'
      if Path.Name (1) = '/' then
         Idx := 2;
      end if;

      --  Below: Idx always points to the first character of a path element.

      while Idx <= Path.Len loop
         Token.Len := 0;

         for J in Idx .. Path.Len loop
            exit when Path.Name (J) = '/';
            Token.Len := Token.Len + 1;
            Token.Name (Token.Len) := Path.Name (J);
         end loop;

         if -Token = "." then
            --  Skip
            Path.Name (Idx .. Path.Len - 2) :=
              Path.Name (Idx + 2 .. Path.Len);

            if Idx + 2 > Path.Len then
               --  Path ends with just a '.'
               Path.Len := Path.Len - 1;
            else
               --  Nominal case: we remove the './'
               Path.Len := Path.Len - 2;
            end if;

         elsif -Token = ".." then
            if Idx = 1 or else
              (Idx = 2 and then Path.Name (1) = '/')
            then
               --  We have "/../foo/bar", invalid but we keep as-is
               Idx := Idx + 3;
            else
               Prev := 0;

               --  Find the parent directory separator
               for J in reverse 1 .. Idx - 2 loop
                  if Path.Name (J) = '/' then
                     Prev := J + 1;
                     exit;
                  else
                     Prev := J;
                  end if;
               end loop;

               --  No such separator, we have either '../foo/bar' or '..'
               if Prev = 0 then
                  if Idx + 1 >= Path.Len then
                     --  General case: there's something after ..
                     Path.Name (1 .. Path.Len - Idx - 1) :=
                       Path.Name (Idx + 1 .. Path.Len);
                     Path.Len := Path.Len - Idx;
                     Idx := 1;
                  else
                     --  For completeness, handle the case where the path is
                     --  just '..'
                     Path.Len := 0;

                     return;
                  end if;
               else
                  Path.Name (Prev .. Path.Len + Prev - Idx - 3) :=
                    Path.Name (Idx + 3 .. Path.Len);
                  Path.Len := Path.Len + Prev - Idx - 3;
                  Idx := Prev;
               end if;
            end if;

         elsif Token.Len = 0 then
            --  We have two consecutive slashes
            Path.Name (Idx .. Path.Len - 1) := Path.Name (Idx + 1 .. Path.Len);
            Path.Len := Path.Len - 1;

         else
            Idx := Idx + Token.Len + 1;

         end if;
      end loop;
   end Normalize;

   -----------------
   -- Mount_Point --
   -----------------

   function Mount_Point (Path : FAT_Path) return FAT_Name
   is
      First : Natural;
   begin
      if Path.Len > 0 and then Path.Name (1) = '/' then
         First := 2;
      else
         First := 1;
      end if;

      for J in First .. Path.Len loop
         if Path.Name (J) = '/' then
            return -Path.Name (First .. J - 1);
         end if;
      end loop;

      return -Path.Name (First .. Path.Len);
   end Mount_Point;

   -------------
   -- FS_Path --
   -------------

   function FS_Path (Path : FAT_Path) return FAT_Path
   is
      First : Natural;
   begin
      if Path.Len > 0 and then Path.Name (1) = '/' then
         First := 2;
      else
         First := 1;
      end if;

      for J in First .. Path.Len loop
         if Path.Name (J) = '/' then
            return -Path.Name (J .. Path.Len);
         end if;
      end loop;

      return Empty_Path;
   end FS_Path;

   ----------
   -- Trim --
   ----------

   function Trim (S : String) return String
   is
   begin
      for J in reverse S'Range loop
         if S (J) /= ' ' then
            return S (S'First .. J);
         end if;
      end loop;

      return "";
   end Trim;

   ----------
   -- Open --
   ----------

   function Open
     (Controller  : Media_Controller_Access;
      Mount_Point : FAT_Name;
      Status      : out Status_Code) return FAT_Filesystem_Access
   is
      subtype Word_Data is Media_Reader.Block (0 .. 3);
      function To_Word is new
        Ada.Unchecked_Conversion (Word_Data, Unsigned_32);
      Ret      : FAT_Filesystem_Access;
      P_Idx    : Unsigned_16;
      P_Data   : Media_Reader.Block (0 .. 15);
      N_Blocks : Unsigned_32;

   begin
      --  Find a free Volume handle
      Ret := null;

      for J in Volumes'Range loop
         if not Volumes (J).Mounted then
            Ret := Volumes (J)'Access;
            exit;
         end if;
      end loop;

      if Ret = null then
         Status := Too_Many_Open_Files;
         return Null_FAT_Volume;
      end if;

      Ret.Mounted     := True;
      Ret.Controller  := Controller;
      Ret.Mount_Point := Mount_Point;

      --  Let's read the MBR
      Status := Ret.Ensure_Block (0);

      if Status /= OK then
         Ret.Mounted := False;

         return null;
      end if;

      --  Check for the MBR magic number
      if Ret.Window (510 .. 511) /= (16#55#, 16#AA#) then
         Status := No_MBR_Found;
         Ret.Mounted := False;

         return null;
      end if;

      --  Now check the partition entries: 4 partitions for the MBR
      for P in 1 .. 4 loop
         --  Partitions are defined as an array of 16 bytes from
         --  base MBR + 446 (16#1BE#)
         P_Idx  := 446 + Unsigned_16 (P - 1) * 16;
         P_Data := Ret.Window (P_Idx .. P_Idx + 15);

         --  Retrieve the number of blocks in the partition.
         N_Blocks := To_Word (P_Data (12 .. 15));

         if N_Blocks > 0 then
            --  The partition is valid
            Ret.LBA := To_Word (P_Data (8 .. 11));

            exit;

         elsif P = 4 then
            --  Last of the partition is not valid: there's no valid partition
            Status := No_Partition_Found;
            Ret.Mounted := False;

            return null;
         end if;
      end loop;

      Initialize_FS (Ret.all, Status);

      if Status /= OK then
         Ret.Mounted := False;

         return null;
      end if;

      return Ret;
   end Open;

   ----------
   -- Open --
   ----------

   function Open
     (Controller  : Media_Controller_Access;
      LBA         : Unsigned_32;
      Mount_Point : FAT_Name;
      Status      : out Status_Code) return FAT_Filesystem_Access
   is
      Ret    : FAT_Filesystem_Access;

   begin
      --  Find a free Volume handle
      Ret := null;

      for J in Volumes'Range loop
         if not Volumes (J).Mounted then
            Ret := Volumes (J)'Access;
            exit;
         end if;
      end loop;

      if Ret = null then
         Status := Too_Many_Open_Files;
         return Null_FAT_Volume;
      end if;

      Ret.Mounted     := True;
      Ret.Controller  := Controller;
      Ret.Mount_Point := Mount_Point;
      Ret.LBA         := LBA;

      Initialize_FS (Ret.all, Status);

      if Status /= OK then
         Ret.Mounted := False;

         return null;
      end if;

      return Ret;
   end Open;

   ---------------------
   -- Get_Mount_Point --
   ---------------------

   function Get_Mount_Point
     (Mount_Point : FAT_Name;
      FS          : out FAT_Filesystem_Access) return Status_Code
   is
   begin
      --  Look for a mount point
      for J in Volumes'Range loop
         if Volumes (J).Mounted
           and then Volumes (J).Mount_Point = Mount_Point
         then
            FS := Volumes (J)'Access;
            return OK;
         end if;
      end loop;

      return Not_Mounted;
   end Get_Mount_Point;

   -------------------
   -- Initialize_FS --
   -------------------

   procedure Initialize_FS
     (FS     : in out FAT_Filesystem;
      Status : out Status_Code)
   is
      subtype Disk_Parameter_Block is Block (0 .. 91);
      function To_Disk_Parameter is new Ada.Unchecked_Conversion
        (Disk_Parameter_Block, FAT_Disk_Parameter);

      subtype FSInfo_Block is Block (0 .. 11);
      function To_FSInfo is new Ada.Unchecked_Conversion
        (FSInfo_Block, FAT_FS_Info);

   begin
      FS.Window_Block := 16#FFFF_FFFF#;
      Status := FS.Ensure_Block (FS.LBA);

      if Status /= OK then
         return;
      end if;

      if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
         Status := No_Filesystem;
         return;
      end if;

      FS.Disk_Parameters :=
        To_Disk_Parameter (FS.Window (0 .. 91));

      if FS.Version = FAT32 then
         Status :=
           FS.Ensure_Block (FS.LBA + Unsigned_32 (FS.FSInfo_Block_Number));

         if Status /= OK then
            return;
         end if;

         --  Check the generic FAT block signature
         if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
            Status := No_Filesystem;
            return;
         end if;

         FS.FSInfo :=
           To_FSInfo (FS.Window (16#1E4# .. 16#1EF#));
      end if;

      declare
         Data_Offset_In_Block : constant Unsigned_32 :=
                                  Unsigned_32 (FS.Reserved_Blocks) +
                                  FS.FAT_Table_Size_In_Blocks *
                                    Unsigned_32 (FS.Number_Of_FATs);
         Root_Dir_Size        : Unsigned_32 := 0;
      begin
         if FS.Version = FAT16 then
            --  Each directory entry is 32 Bytes.
            Root_Dir_Size :=
              Unsigned_32
                (FS.Number_Of_Entries_In_Root_Dir) * 32;

            if (Root_Dir_Size mod FS.Block_Size_In_Bytes) = 0 then
               Root_Dir_Size := Root_Dir_Size / FS.Block_Size_In_Bytes;
            else
               Root_Dir_Size := 1 + Root_Dir_Size / FS.Block_Size_In_Bytes;
            end if;
         end if;

         FS.FAT_Addr  := FS.LBA + Unsigned_32 (FS.Reserved_Blocks);
         FS.Data_Area := FS.LBA + Data_Offset_In_Block + Root_Dir_Size;
         FS.Num_Clusters :=
           (FS.Total_Number_Of_Blocks - Data_Offset_In_Block) /
           Unsigned_32 (FS.Number_Of_Blocks_Per_Cluster);
      end;
   end Initialize_FS;

   -----------
   -- Close --
   -----------

   procedure Close (FS : FAT_Filesystem_Access)
   is
   begin
      FS.Mounted := False;
   end Close;

   ------------------
   -- Ensure_Block --
   ------------------

   function Ensure_Block
     (FS    : in out FAT_Filesystem;
      Block : Unsigned_32) return Status_Code
   is
   begin
      if Block >= FS.Window_Block
        and then Block < FS.Window_Block + CACHE_SIZE
      then
         return OK;
      end if;

      if not FS.Controller.Read_Block (Block, FS.Window) then
         FS.Window_Block  := 16#FFFF_FFFF#;

         return Disk_Error;
      end if;

      FS.Window_Block := Block;

      return OK;
   end Ensure_Block;

   ----------------
   -- Change_Dir --
   ----------------

   function Change_Dir
     (Dir_Name : FAT_Path) return Boolean
   is
      Full : FAT_Path := Dir_Name;
      H    : Directory_Handle;
   begin
      Normalize (Full);

      if Open (Full, H) /= OK then
         return False;
      end if;

      Close (H);
      Current_Path := Full;

      return True;
   end Change_Dir;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (Dir_Name : FAT_Path)
   is
      Dead : Boolean with Unreferenced;
   begin
      Dead := Change_Dir (Dir_Name);
   end Change_Dir;

   -----------------------
   -- Current_Directory --
   -----------------------

   function Current_Directory return FAT_Path
   is
   begin
      return Current_Path;
   end Current_Directory;

   ----------
   -- Open --
   ----------

   function Open
     (Path   : FAT_Path;
      Handle : out Directory_Handle) return Status_Code
   is
      E    : Directories.Directory_Entry;
      Full : FAT_Path := Path;
      Mnt  : FAT_Name;
      Rel  : FAT_Path;
      Ret  : Status_Code;
      FS   : FAT_Filesystem_Access := null;

   begin
      Normalize (Full);

      if Is_Root (Full) then
         Handle := (Kind                 => Handle_Mount_Point,
                    Current_Volume_Index => -1);
         return OK;
      end if;

      Mnt := Mount_Point (Path);
      Rel := FS_Path (Path);

      for J in Volumes'Range loop
         if Volumes (J).Mounted and then Volumes (J).Mount_Point = Mnt then
            FS := Volumes (J)'Access;
         end if;
      end loop;

      if FS = null then
         return No_Such_Path;
      end if;

      if not Is_Root (Rel) then
         Ret := Directories.Find (FS, Rel, E);

         if Ret /= OK then
            return Ret;
         end if;

      else
         E := Directories.Root_Entry (FS);
      end if;

      if not Directories.Is_Subdirectory (E) then
         return No_Such_File;
      end if;

      return Directories.Open_Dir (E, Handle);
   end Open;

   ----------
   -- Read --
   ----------

   function Read (Dir    : in out Directory_Handle;
                  DEntry : out Directories.Directory_Entry)
                  return Status_Code
   is
   begin
      case Dir.Kind is
         when Handle_Mount_Point =>
            if Dir.Current_Volume_Index < 0 then
               Dir.Current_Volume_Index := Volumes'First - 1;
            end if;

            for J in Dir.Current_Volume_Index + 1 .. Volumes'Last loop
               if Volumes (J).Mounted then
                  DEntry := Directories.Root_Entry (Volumes (J)'Access);
                  Dir.Current_Volume_Index := J;

                  return OK;
               end if;
            end loop;

            return No_Such_File;

         when Handle_FAT =>
            return Directories.Read_Dir (Dir, DEntry);
      end case;
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (Dir : in out Directory_Handle)
   is
   begin
      case Dir.Kind is
         when Handle_Mount_Point =>
            Dir.Current_Volume_Index := -1;
         when Handle_FAT =>
            Directories.Reset_Dir (Dir);
      end case;
   end Reset;

   procedure Close (Dir : in out Directory_Handle)
   is
   begin
      case Dir.Kind is
         when Handle_Mount_Point =>
            Dir.Current_Volume_Index := -1;
         when Handle_FAT =>
            Directories.Close_Dir (Dir);
      end case;
   end Close;

   --     function Is_Directory (Path : FAT_Path) return Boolean;
--     function Is_File (Path : FAT_Path) return Boolean;
--     function Is_File_Or_Directory (Path : FAT_Path) return Boolean;

   -------------
   -- Get_FAT --
   -------------

   function Get_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Unsigned_32) return Unsigned_32
   is
      Idx       : Unsigned_16;
      FAT_Block : Block (0 .. 511);
      --  When reading the FAT table for the next cluster, don't use the
      --  FS.Window cache as we might already have part of this cluster in
      --  cache.

      subtype B2 is Block (1 .. 2);
      subtype B4 is Block (1 .. 4);
      function To_Int16 is new Ada.Unchecked_Conversion
        (B2, Unsigned_16);
      function To_Int32 is new Ada.Unchecked_Conversion
        (B4, Unsigned_32);

   begin
      if Cluster < 2 or else Cluster >= FS.Num_Clusters then
         return 1;
      end if;

      case FS.Version is
         when FAT16 =>
            if not Read_Block
              (FS.Controller.all,
               FS.FAT_Addr + Cluster * 2 / FS.Block_Size_In_Bytes,
               FAT_Block)
            then
               return 1;
            end if;

            Idx := Unsigned_16 ((Cluster * 2) mod FS.Block_Size_In_Bytes);

            return Unsigned_32 (To_Int16 (FAT_Block (Idx .. Idx + 1)));

         when FAT32 =>
            if not Read_Block
              (FS.Controller.all,
               FS.FAT_Addr + Cluster * 4 / FS.Block_Size_In_Bytes,
               FAT_Block)
            then
               return 1;
            end if;

            Idx := Unsigned_16 ((Cluster * 4) mod FS.Block_Size_In_Bytes);

            return To_Int32 (FAT_Block (Idx .. Idx + 3)) and 16#0FFF_FFFF#;
      end case;
   end Get_FAT;

end FAT_Filesystem;
