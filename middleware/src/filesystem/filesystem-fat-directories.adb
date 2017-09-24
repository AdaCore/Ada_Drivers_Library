------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

--  This packages provide a low level driver for the FAT file system
--  architecture. It is recommended to _not_ use this interface directly but to
--  access the file system using the File_IO package. For more info, see the
--  file system chapter of the documentation.

with Ada.Unchecked_Conversion;

package body Filesystem.FAT.Directories is

   subtype Entry_Data is Block (1 .. 32);
   function To_Data is new Ada.Unchecked_Conversion
     (FAT_Directory_Entry, Entry_Data);
   function To_Data is new Ada.Unchecked_Conversion
     (VFAT_Directory_Entry, Entry_Data);

   function Find_Empty_Entry_Sequence
     (Parent      : access FAT_Directory_Handle;
      Num_Entries : Natural) return Entry_Index;
   --  Finds a sequence of deleted entries that can fit Num_Entries.
   --  Returns the first entry of this sequence

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (E    : in out FAT_Node;
      Size : FAT_File_Size)
   is
   begin
      if E.Size /= Size then
         E.Size := Size;
         E.Is_Dirty := True;
      end if;
   end Set_Size;

   ----------
   -- Find --
   ----------

   function Find
     (Parent     : FAT_Node;
      Filename   : FAT_Name;
      DEntry     : out FAT_Node) return Status_Code
   is
      --  We use a copy of the handle, so as not to touch the state of initial
      --  handle
      Status  : Status_Code;
      Cluster : Cluster_Type := Parent.Start_Cluster;
      Block   : Block_Offset := Parent.FS.Cluster_To_Block (Cluster);
      Index   : Entry_Index  := 0;

   begin
      loop
         Status := Next_Entry
           (FS              => Parent.FS,
            Current_Cluster => Cluster,
            Current_Block   => Block,
            Current_Index   => Index,
            DEntry          => DEntry);

         if Status /= OK then
            return No_Such_File;
         end if;

         if Long_Name (DEntry) = Filename or else
           (DEntry.L_Name.Len = 0 and then Short_Name (DEntry) = Filename)
         then
            return OK;
         end if;
      end loop;
   end Find;

   ----------
   -- Find --
   ----------

   function Find
     (FS     : in out FAT_Filesystem;
      Path   : String;
      DEntry : out FAT_Node) return Status_Code
   is
      Status  : Status_Code;
      Idx     : Natural;  --  Idx is used to walk through the Path
      Token   : FAT_Name;

   begin
      DEntry := Root_Entry (FS);

      --  Looping through the Path. We start at 2 as we ignore the initial '/'
      Idx := Path'First + 1;

      while Idx <= Path'Last loop
         Token.Len := 0;

         for J in Idx .. Path'Last loop
            if Path (J) = '/' then
               exit;
            end if;

            Token.Len := Token.Len + 1;
            Token.Name (Token.Len) := Path (J);
         end loop;

         Idx := Idx + Token.Len + 1;

         Status := Find (DEntry, Token, DEntry);

         if Status /= OK then
            return No_Such_File;
         end if;

         if Idx < Path'Last then
            --  Intermediate entry: needs to be a directory
            if not Is_Subdirectory (DEntry) then
               return No_Such_Path;
            end if;
         end if;
      end loop;

      return OK;
   end Find;

   ------------------
   -- Update_Entry --
   ------------------

   function Update_Entry
     (Parent : FAT_Node;
      Value  : in out FAT_Node) return Status_Code
   is
      subtype Entry_Block is Block (1 .. 32);
      function To_Block is new
        Ada.Unchecked_Conversion (FAT_Directory_Entry, Entry_Block);
      function To_Entry is new
        Ada.Unchecked_Conversion (Entry_Block, FAT_Directory_Entry);

      Ent       : FAT_Directory_Entry;
      Cluster   : Cluster_Type := Parent.Start_Cluster;
      Offset    : FAT_File_Size := FAT_File_Size (Value.Index) * 32;
      Block_Off : Natural;
      Block     : Block_Offset;
      Ret       : Status_Code;

   begin
      if not Value.Is_Dirty then
         return OK;
      end if;

      while Offset > Parent.FS.Cluster_Size loop
         Cluster := Parent.FS.Get_FAT (Cluster);
         Offset := Offset - Parent.FS.Cluster_Size;
      end loop;

      Block     := Block_Offset (Offset / Parent.FS.Block_Size);
      Block_Off := Natural (Offset mod Parent.FS.Block_Size);

      Ret := Parent.FS.Ensure_Block
        (Parent.FS.Cluster_To_Block (Cluster) + Block);
      if Ret /= OK then
         return Ret;
      end if;

      Ent := To_Entry (Parent.FS.Window (Block_Off .. Block_Off + 31));

      --  For now only the size can be modified, so just apply this
      --  modification
      Ent.Size := Value.Size;
      Value.Is_Dirty := False;

      Parent.FS.Window (Block_Off .. Block_Off + 31) := To_Block (Ent);
      Ret := Parent.FS.Write_Window;

      return Ret;
   end Update_Entry;

   ----------------
   -- Root_Entry --
   ----------------

   function Root_Entry (FS : in out FAT_Filesystem) return FAT_Node
   is
      Ret : FAT_Node;
   begin
      Ret.FS            := FS'Unchecked_Access;
      Ret.Attributes    := (Subdirectory => True,
                            others       => False);
      Ret.Is_Root       := True;
      Ret.L_Name        := (Name => (others => ' '),
                            Len  => 0);
      if FS.Version = FAT16 then
         Ret.Start_Cluster := 0;
      else
         Ret.Start_Cluster := FS.Root_Dir_Cluster;
      end if;
      Ret.Index         := 0;

      return Ret;
   end Root_Entry;

   ----------------
   -- Next_Entry --
   ----------------

   function Next_Entry
     (FS              : access FAT_Filesystem;
      Current_Cluster : in out Cluster_Type;
      Current_Block   : in out Block_Offset;
      Current_Index   : in out Entry_Index;
      DEntry          : out    FAT_Directory_Entry) return Status_Code
   is
      subtype Entry_Data is Block (1 .. 32);
      function To_Entry is new Ada.Unchecked_Conversion
        (Entry_Data, FAT_Directory_Entry);

      Ret       : Status_Code;
      Block_Off : Natural;

   begin
      if Current_Index = 16#FFFF# then
         return No_More_Entries;
      end if;

      if Current_Cluster = 0 and then FS.Version = FAT16 then
         if Current_Index >
           Entry_Index (FS.FAT16_Root_Dir_Num_Entries)
         then
            return No_More_Entries;
         else
            Block_Off :=
              Natural (FAT_File_Size (Current_Index * 32) mod
                           FS.Block_Size);
            Current_Block :=
              FS.Root_Dir_Area +
                Block_Offset
                  (FAT_File_Size (Current_Index * 32) / FS.Block_Size);
         end if;

      else
         Block_Off := Natural
           (FAT_File_Size (Current_Index * 32) mod FS.Block_Size);

         --  Check if we're on a block boundare
         if Unsigned_32 (Block_Off) = 0 and then Current_Index /= 0 then
            Current_Block := Current_Block + 1;
         end if;

         --  Check if we're on the boundary of a new cluster
         if Current_Block - FS.Cluster_To_Block (Current_Cluster)
           = FS.Blocks_Per_Cluster
         then
            --  The block we need to read is outside of the current cluster.
            --  Let's move on to the next

            --  Read the FAT table to determine the next cluster
            Current_Cluster := FS.Get_FAT (Current_Cluster);

            if Current_Cluster = 1
              or else FS.Is_Last_Cluster (Current_Cluster)
            then
               return Internal_Error;
            end if;

            Current_Block := FS.Cluster_To_Block (Current_Cluster);
         end if;
      end if;

      Ret := FS.Ensure_Block (Current_Block);

      if Ret /= OK then
         return Ret;
      end if;

      if FS.Window (Block_Off) = 0 then
         --  End of entries: we stick the index here to make sure that further
         --  calls to Next_Entry always end-up here
         return No_More_Entries;
      end if;

      DEntry := To_Entry (FS.Window (Block_Off .. Block_Off + 31));
      Current_Index := Current_Index + 1;

      return OK;
   end Next_Entry;

   ----------------
   -- Next_Entry --
   ----------------

   function Next_Entry
     (FS              : access FAT_Filesystem;
      Current_Cluster : in out Cluster_Type;
      Current_Block   : in out Block_Offset;
      Current_Index   : in out Entry_Index;
      DEntry          : out    FAT_Node) return Status_Code
   is
      procedure Prepend
        (Name : Wide_String;
         Full : in out String;
         Idx  : in out Natural);
      --  Prepends Name to Full

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
               elsif Val = 16#F029# then
                  --  Path ends with a '.'
                  Full (Idx) := '.';
               elsif Val = 16#F028# then
                  --  Path ends with a ' '
                  Full (Idx) := ' ';
               else
                  Full (Idx) := '?';
               end if;
            end if;
         end loop;
      end Prepend;

      Ret     : Status_Code;
      D_Entry : FAT_Directory_Entry;
      V_Entry : VFAT_Directory_Entry;
      function To_VFAT_Entry is new Ada.Unchecked_Conversion
        (FAT_Directory_Entry, VFAT_Directory_Entry);

      C            : Unsigned_8;
      Last_Seq     : VFAT_Sequence_Number := 0;
      CRC          : Unsigned_8 := 0;
      Matches      : Boolean;
      Current_CRC  : Unsigned_8;
      L_Name       : String (1 .. MAX_FILENAME_LENGTH);
      L_Name_First : Natural;

   begin
      L_Name_First := L_Name'Last + 1;

      loop
         Ret := Next_Entry
           (FS,
            Current_Cluster => Current_Cluster,
            Current_Block   => Current_Block,
            Current_Index   => Current_Index,
            DEntry          => D_Entry);

         if Ret /= OK then
            return Ret;
         end if;

         --  Check if we have a VFAT entry here by checking that the
         --  attributes are 16#0F# (e.g. all attributes set except
         --  subdirectory and archive)
         if D_Entry.Attributes = VFAT_Directory_Entry_Attribute then
            V_Entry := To_VFAT_Entry (D_Entry);

            if V_Entry.VFAT_Attr.Stop_Bit then
               L_Name_First := L_Name'Last + 1;

            else
               if Last_Seq = 0
                 or else Last_Seq - 1 /= V_Entry.VFAT_Attr.Sequence
               then
                  L_Name_First := L_Name'Last + 1;
               end if;
            end if;

            Last_Seq := V_Entry.VFAT_Attr.Sequence;

            Prepend (V_Entry.Name_3, L_Name, L_Name_First);
            Prepend (V_Entry.Name_2, L_Name, L_Name_First);
            Prepend (V_Entry.Name_1, L_Name, L_Name_First);

            if V_Entry.VFAT_Attr.Sequence = 1 then
               CRC := V_Entry.Checksum;
            end if;

         --  Ignore Volumes and deleted files
         elsif not D_Entry.Attributes.Volume_Label
           and then Character'Pos (D_Entry.Filename (1)) /= 16#E5#
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

            DEntry :=
              (FS            => FAT_Filesystem_Access (FS),
               L_Name        => <>,
               S_Name        => D_Entry.Filename,
               S_Name_Ext    => D_Entry.Extension,
               Attributes    => D_Entry.Attributes,
               Start_Cluster => (if FS.Version = FAT16
                                 then Cluster_Type (D_Entry.Cluster_L)
                                 else Cluster_Type (D_Entry.Cluster_L) or
                                   Shift_Left
                                       (Cluster_Type (D_Entry.Cluster_H), 16)),
               Size          => D_Entry.Size,
               Index         => Current_Index - 1,
               Is_Root       => False,
               Is_Dirty      => False);

            if Matches then
               DEntry.L_Name := -L_Name (L_Name_First .. L_Name'Last);
            else
               DEntry.L_Name.Len := 0;
            end if;

            return OK;
         end if;
      end loop;
   end Next_Entry;

   ----------
   -- Read --
   ----------

   function Read
     (Dir    : in out FAT_Directory_Handle;
      DEntry : out FAT_Node) return Status_Code
   is
   begin
      return Next_Entry
        (Dir.FS,
         Current_Cluster => Dir.Current_Cluster,
         Current_Block   => Dir.Current_Block,
         Current_Index   => Dir.Current_Index,
         DEntry          => DEntry);
   end Read;

   -------------------
   -- Create_Subdir --
   -------------------

   function Create_Subdir
     (Dir     : FAT_Node;
      Name    : FAT_Name;
      New_Dir : out FAT_Node) return Status_Code
   is
      Handle       : FAT_Directory_Handle_Access;
      Ret          : Status_Code;
      Block        : Block_Offset;
      Dot          : FAT_Directory_Entry;
      Dot_Dot      : FAT_Directory_Entry;

   begin
      Ret := Dir.FAT_Open (Handle);

      if Ret /= OK then
         return Ret;
      end if;

      Ret := Allocate_Entry
        (Parent        => Handle,
         Name          => Name,
         Attributes    => (Read_Only    => False,
                           Hidden       => False,
                           System_File  => False,
                           Volume_Label => False,
                           Subdirectory => True,
                           Archive      => False),
         E             => New_Dir);

      if Ret /= OK then
         return Ret;
      end if;

      Block := Dir.FS.Cluster_To_Block (New_Dir.Start_Cluster);
      Ret := Handle.FS.Ensure_Block (Block);
      if Ret /= OK then
         return Ret;
      end if;

      --  Allocate '.', '..' and the directory entry terminator
      Dot :=
        (Filename   => (1 => '.', others => ' '),
         Extension  => (others => ' '),
         Attributes => (Read_Only    => False,
                        Hidden       => False,
                        System_File  => False,
                        Volume_Label => False,
                        Subdirectory => True,
                        Archive      => False),
         Reserved   => (others => ASCII.NUL),
         Cluster_H  =>
           Unsigned_16
             (Shift_Right
                  (Unsigned_32
                     (New_Dir.Start_Cluster) and 16#FFFF_0000#, 16)),
         Time       => 0,
         Date       => 0,
         Cluster_L  => Unsigned_16 (New_Dir.Start_Cluster and 16#FFFF#),
         Size       => 0);
      Dot_Dot :=
        (Filename   => (1 .. 2 => '.', others => ' '),
         Extension  => (others => ' '),
         Attributes => (Read_Only    => False,
                        Hidden       => False,
                        System_File  => False,
                        Volume_Label => False,
                        Subdirectory => True,
                        Archive      => False),
         Reserved   => (others => ASCII.NUL),
         Cluster_H  =>
           Unsigned_16
             (Shift_Right
                  (Unsigned_32 (Handle.Start_Cluster) and 16#FFFF_0000#, 16)),
         Time       => 0,
         Date       => 0,
         Cluster_L  => Unsigned_16 (Handle.Start_Cluster and 16#FFFF#),
         Size       => 0);

      Handle.FS.Window (0 .. 31)  := To_Data (Dot);
      Handle.FS.Window (32 .. 63) := To_Data (Dot_Dot);
      Handle.FS.Window (64 .. 95) := (others => 0);
      Ret := Handle.FS.Write_Window;

      Close (Handle.all);

      return Ret;
   end Create_Subdir;

   ----------------------
   -- Create_File_Node --
   ----------------------

   function Create_File_Node
     (Dir      : FAT_Node;
      Name     : FAT_Name;
      New_File : out FAT_Node) return Status_Code
   is
      Handle       : FAT_Directory_Handle_Access;
      Ret          : Status_Code;

   begin
      Ret := Dir.FAT_Open (Handle);

      if Ret /= OK then
         return Ret;
      end if;

      Ret := Allocate_Entry
        (Parent        => Handle,
         Name          => Name,
         Attributes    => (Read_Only    => False,
                           Hidden       => False,
                           System_File  => False,
                           Volume_Label => False,
                           Subdirectory => False,
                           Archive      => True),
         E             => New_File);

      Close (Handle.all);

      if Ret /= OK then
         return Ret;
      end if;

      return Ret;
   end Create_File_Node;

   -------------------
   -- Delete_Subdir --
   -------------------

   function Delete_Subdir
     (Dir       : FAT_Node;
      Recursive : Boolean) return Status_Code
   is
      Parent    : FAT_Node;
      Handle    : FAT_Directory_Handle_Access;
      Ent       : FAT_Node;
      Ret       : Status_Code;

   begin
      Ret := Dir.FAT_Open (Handle);

      if Ret /= OK then
         return Ret;
      end if;

      while Read (Handle.all, Ent) = OK loop
         if -Long_Name (Ent) = "." then
            null;
         elsif -Long_Name (Ent) = ".." then
            Parent := Ent;
         elsif not Recursive then
            return Non_Empty_Directory;
         else
            if Ent.Attributes.Subdirectory then
               Ret := Delete_Subdir (Ent, True);
            else
               Ret := Delete_Entry (Dir, Ent);
            end if;

            if Ret /= OK then
               Close (Handle.all);

               return Ret;
            end if;
         end if;
      end loop;

      Close (Handle.all);

      --  Free the clusters associated to the subdirectory
      Ret := Delete_Entry (Parent, Dir);

      if Ret /= OK then
         return Ret;
      end if;

      return Ret;
   end Delete_Subdir;

   ------------------
   -- Delete_Entry --
   ------------------

   function Delete_Entry
     (Dir : FAT_Node;
      Ent : FAT_Node) return Status_Code
   is
      Current   : Cluster_Type := Ent.Start_Cluster;
      Handle    : FAT_Directory_Handle_Access;
      Next      : Cluster_Type;
      Child_Ent : FAT_Node;
      Ret       : Status_Code;
      Block_Off : Natural;

   begin
      --  Mark the entry's cluster chain as available
      loop
         Next := Ent.FS.Get_FAT (Current);
         Ret := Ent.FS.Set_FAT (Current, FREE_CLUSTER_VALUE);

         exit when Ret /= OK;
         exit when Ent.FS.Is_Last_Cluster (Next);
         Current := Next;
      end loop;

      --  Mark the parent's entry as deleted
      Ret := Dir.FAT_Open (Handle);
      if Ret /= OK then
         return Ret;
      end if;

      while Read (Handle.all, Child_Ent) = OK loop
         if Long_Name (Child_Ent) = Long_Name (Ent) then
            Block_Off := Natural
              ((FAT_File_Size (Handle.Current_Index - 1) * 32)
                 mod Dir.FS.Block_Size);
            --  Mark the entry as deleted: first basename character set to
            --  16#E5#
            Handle.FS.Window (Block_Off) := 16#E5#;
            Ret := Handle.FS.Write_Window;

            exit;
         end if;
      end loop;

      Close (Handle.all);

      return Ret;
   end Delete_Entry;

   ---------------------
   -- Adjust_Clusters --
   ---------------------

   function Adjust_Clusters (Ent : FAT_Node) return Status_Code
   is
      B_Per_Cluster : constant FAT_File_Size :=
                        FAT_File_Size (Ent.FS.Blocks_Per_Cluster) *
                          Ent.FS.Block_Size;
      Size          : FAT_File_Size := Ent.Size;
      Current       : Cluster_Type := Ent.Start_Cluster;
      Next          : Cluster_Type;
      Ret           : Status_Code := OK;

   begin
      if Ent.Attributes.Subdirectory then
         --  ??? Do nothing for now
         return OK;
      end if;

      loop
         Next := Ent.FS.Get_FAT (Current);

         if Size > B_Per_Cluster then
            --  Current cluster is fully used
            Size := Size - B_Per_Cluster;
         elsif Size > 0 or else Current = Ent.Start_Cluster then
            --  Partially used cluster, but the last one
            Size := 0;
            if Next /= LAST_CLUSTER_VALUE then
               Ret := Ent.FS.Set_FAT (Current, LAST_CLUSTER_VALUE);
            end if;
         else
            --  We don't need more clusters
            Ret := Ent.FS.Set_FAT (Current, FREE_CLUSTER_VALUE);
         end if;

         exit when Ret /= OK;
         exit when Ent.FS.Is_Last_Cluster (Next);
         Current := Next;
         Size    := Size - B_Per_Cluster;
      end loop;

      return Ret;
   end Adjust_Clusters;

   -------------------------------
   -- Find_Empty_Entry_Sequence --
   -------------------------------

   function Find_Empty_Entry_Sequence
     (Parent      : access FAT_Directory_Handle;
      Num_Entries : Natural) return Entry_Index
   is
      Status   : Status_Code;
      D_Entry  : FAT_Directory_Entry;
      Sequence : Natural := 0;
      Ret      : Entry_Index;
      Cluster  : Cluster_Type := Parent.Start_Cluster;
      Block    : Block_Offset := Cluster_To_Block (Parent.FS.all, Cluster);
   begin
      Parent.Current_Index := 0;
      loop
         Status := Next_Entry
           (Parent.FS,
            Current_Cluster => Cluster,
            Current_Block   => Block,
            Current_Index   => Parent.Current_Index,
            DEntry          => D_Entry);

         if Status /= OK then
            return Null_Index;
         end if;

         if D_Entry.Attributes = VFAT_Directory_Entry_Attribute then
            if Sequence = 0 then
               --  Parent.Current_Index points to the next unread value.
               --  So the just read entry is at Parent.Current_Index - 1
               Ret := Parent.Current_Index - 1;
            end if;

            Sequence := Sequence + 1;

         elsif Character'Pos (D_Entry.Filename (1)) = 16#E5# then
            --  A deleted entry has been found
            if Sequence >= Num_Entries then
               return Ret;
            else
               Sequence := 0;
            end if;

         else
            Sequence := 0;
         end if;
      end loop;
   end Find_Empty_Entry_Sequence;

   --------------------
   -- Allocate_Entry --
   --------------------

   function Allocate_Entry
     (Parent     : access FAT_Directory_Handle;
      Name       : FAT_Name;
      Attributes : FAT_Directory_Entry_Attribute;
      E          : out FAT_Node) return Status_Code
   is
      subtype Short_Name is String (1 .. 8);
      subtype Extension  is String (1 .. 3);

      function Is_Legal_Character (C : Character) return Boolean
      is (C in 'A' .. 'Z'
          or else C in '0' .. '9'
          or else C = '!'
          or else C = '#'
          or else C = '$'
          or else C = '%'
          or else C = '&'
          or else C = '''
          or else C = '('
          or else C = ')'
          or else C = '-'
          or else C = '@'
          or else C = '^'
          or else C = '_'
          or else C = '`'
          or else C = '{'
          or else C = '}'
          or else C = '~');

      Block_Off   : Natural;
      Status      : Status_Code;
      DEntry      : FAT_Node;
      SName       : Short_Name := (others => ' ');
      SExt        : Extension  := (others => ' ');
      Index       : Entry_Index;

      --  Retrieve the number of VFAT entries that are needed, plus one for
      --  the regular FAT entry.
      N_Entries   : Natural := Get_Num_VFAT_Entries (Name) + 1;
      Bytes       : Entry_Data;

      procedure To_Short_Name
        (Name  : FAT_Name;
         SName : out Short_Name;
         Ext   : out Extension);
      --  Translates a long name into short 8.3 name
      --  If the long name is mixed or lower case. then 8.3 will be uppercased
      --  If the long name contains characters not allowed in an 8.3 name, then
      --   the name is stripped of invalid characters such as space and extra
      --   periods. Other unknown characters are changed to underscores.
      --   The stripped name is then truncated, followed by a ~1. Inc_SName
      --   below will increase the digit number in case there's overloaded 8.3
      --   names.
      --  If the long name is longer than 8.3, then ~1 suffix will also be
      --   used.

      function To_Upper (C : Character) return Character is
        (if C in 'a' .. 'z'
         then Character'Val
           (Character'Pos (C) + Character'Pos ('A') - Character'Pos ('a'))
         else C);

      function Value (S : String) return Natural;
      --  For a positive int represented in S, returns its value

      procedure Inc_SName (SName : in out String);
      --  Increment the suffix of the short FAT name
      --  e.g.:
      --  ABCDEFGH => ABCDEF~1
      --  ABC      => ABC~1
      --  ABC~9    => ABC~10
      --  ABCDEF~9 => ABCDE~10

      procedure To_WString
        (S   : FAT_Name;
         Idx : in out Natural;
         WS  : in out Wide_String);
      --  Dumps S (Idx .. Idx + WS'Length - 1) into WS and increments Idx

      -----------
      -- Value --
      -----------

      function Value (S : String) return Natural
      is
         Val   : constant String := Trim (S);
         Digit : Natural;
         Ret   : Natural := 0;
      begin
         for J in Val'Range loop
            Digit := Character'Pos (Val (J)) - Character'Pos ('0');
            Ret := Ret * 10 + Digit;
         end loop;

         return Ret;
      end Value;

      -------------------
      -- To_Short_Name --
      -------------------

      procedure To_Short_Name
        (Name  : FAT_Name;
         SName : out Short_Name;
         Ext   : out Extension)
      is
         S_Idx     : Natural := 0;
         Add_Tilde : Boolean := False;
         Last      : Natural := Name.Len;

      begin
         --  Copy the file extension

         Ext := (others => ' ');

         for J in reverse 1 .. Name.Len loop
            if Name.Name (J) = '.' then
               if J = Name.Len then
                  --  Take care of names ending with a '.' (e.g. no extension,
                  --  the final '.' is part of the basename)
                  Last := J;
                  Ext := (others => ' ');
               else
                  Last := J - 1;
                  S_Idx := Ext'First;

                  for K in J + 1 .. Name.Len loop
                     Ext (S_Idx) := To_Upper (Name.Name (K));
                     S_Idx := S_Idx + 1;
                     --  In case the extension is more than 3 characters, we
                     --  keep the first 3 ones.
                     exit when S_Idx > Ext'Last;
                  end loop;
               end if;

               exit;
            end if;
         end loop;

         S_Idx := 0;
         SName := (others => ' ');

         for J in 1 .. Last loop
            exit when Add_Tilde and then S_Idx >= 6;
            exit when not Add_Tilde and then S_Idx = 8;

            if Name.Name (J) in 'a' .. 'z' then
               S_Idx := S_Idx + 1;
               SName (S_Idx) := To_Upper (Name.Name (J));

            elsif Is_Legal_Character (Name.Name (J)) then
               S_Idx := S_Idx + 1;
               SName (S_Idx) := Name.Name (J);

            elsif Name.Name (J) = '.'
              or else Name.Name (J) = ' '
            then
               --  dots that are not used as extension delimiters are invalid
               --  in FAT short names and ignored in long names to short names
               --  translation
               Add_Tilde := True;

            else
               --  Any other character is translated as '_'
               Add_Tilde := True;
               S_Idx := S_Idx + 1;
               SName (S_Idx) := '_';
            end if;
         end loop;

         if Add_Tilde then
            if S_Idx >= 6 then
               SName (7 .. 8) := "~1";
            else
               SName (S_Idx + 1 .. S_Idx + 2) := "~1";
            end if;
         end if;
      end To_Short_Name;

      ---------------
      -- Inc_SName --
      ---------------

      procedure Inc_SName (SName : in out String)
      is
         Idx : Natural := 0;
         Num : Natural := 0;

      begin
         for J in reverse SName'Range loop
            if Idx = 0 then
               if SName (J) = ' ' then
                  null;
               elsif SName (J) in '0' .. '9' then
                  Idx := J;
               else
                  SName (SName'Last - 1 .. SName'Last) := "~1";

                  return;
               end if;

            elsif SName (J) in '0' .. '9' then
               Idx := J;

            elsif SName (J) = '~' then
               Num := Value (SName (Idx .. SName'Last)) + 1;
               --  make Idx point to '~'
               Idx := J;

               declare
                  N_Suffix : String := Natural'Image (Num);
               begin
                  N_Suffix (N_Suffix'First) := '~';

                  if Idx + N_Suffix'Length - 1 > SName'Last then
                     SName (SName'Last - N_Suffix'Length + 1 .. SName'Last) :=
                       N_Suffix;
                  else
                     SName (Idx .. Idx + N_Suffix'Length - 1) := N_Suffix;
                  end if;

                  return;
               end;

            else
               SName (SName'Last - 2 .. SName'Last) := "~1";

               return;
            end if;
         end loop;

         SName (SName'Last - 2 .. SName'Last) := "~1";
      end Inc_SName;

      ----------------
      -- To_WString --
      ----------------

      procedure To_WString
        (S   : FAT_Name;
         Idx : in out Natural;
         WS  : in out Wide_String)
      is
      begin
         for J in WS'Range loop
            if Idx = S.Len and then S.Name (Idx) = '.' then
               WS (J) := Wide_Character'Val (16#F029#);
            elsif Idx = S.Len and then S.Name (Idx) = ' ' then
               WS (J) := Wide_Character'Val (16#F028#);
            elsif Idx = S.Len + 1 then
               WS (J) := Wide_Character'Val (0);
            elsif Idx > S.Len + 1 then
               WS (J) := Wide_Character'Val (16#FFFF#);
            else
               WS (J) := Wide_Character'Val (Character'Pos (S.Name (Idx)));
            end if;

            Idx := Idx + 1;
         end loop;
      end To_WString;

   begin
      if Parent.FS.Version /= FAT32 then
         --  we only support FAT32 for now.
         return Internal_Error;
      end if;

      --  Compute an initial version of the short name
      To_Short_Name (Name, SName, SExt);

      --  Look for an already existing entry, and compute the short name
      Reset (Parent.all);

      loop
         Status := Read (Parent.all, DEntry);

         if Status /= OK then
            --  no such existing entry, we're good
            Status := OK;
            exit;
         end if;

         --  Can't create a new entry as an old entry with the same long name
         --  already exists
         if Long_Name (DEntry) = Name then
            return Already_Exists;
         end if;

         if DEntry.S_Name = SName
           and then DEntry.S_Name_Ext = SExt
         then
            Inc_SName (SName);
         end if;
      end loop;

      Reset (Parent.all);

      --  Look for an already existing entry that has been deleted and so that
      --  we could reuse
      Index := Find_Empty_Entry_Sequence (Parent, N_Entries);

      if Index = Null_Index then
         --  No such free sequence of directory entries are available, so we'll
         --  need to allocate new ones

         --  At this point, Parent.Current_Index points to the first invalid
         --  entry.
         Index := Parent.Current_Index;

         --  Indicate that a new Entry terminator needs to be added.
         N_Entries := N_Entries + 1;
      end if;

      if Status = OK then
         --  we now write down the new entry
         declare
            VFAT_Entry : array (1 .. Get_Num_VFAT_Entries (Name)) of
                           VFAT_Directory_Entry;
            FAT_Entry  : FAT_Directory_Entry;
            Idx        : Natural := Name.Name'First;
            CRC        : Unsigned_8 := 0;
            C          : Unsigned_8;
            N_Blocks   : Block_Offset;

         begin
            CRC := 0;

            for Ch of String'(SName & SExt) loop
               C := Character'Enum_Rep (Ch);
               CRC := Shift_Right (CRC and 16#FE#, 1)
                 or Shift_Left (CRC and 16#01#, 7);
               --  Modulo addition
               CRC := CRC + C;
            end loop;

            for J in reverse VFAT_Entry'Range loop
               VFAT_Entry (J).VFAT_Attr.Sequence :=
                 VFAT_Sequence_Number (VFAT_Entry'Last - J + 1);
               VFAT_Entry (J).VFAT_Attr.Stop_Bit := False;
               VFAT_Entry (J).Attribute := VFAT_Directory_Entry_Attribute;
               VFAT_Entry (J).Reserved  := 0;
               VFAT_Entry (J).Checksum  := CRC;
               VFAT_Entry (J).Cluster   := 0;
               To_WString (Name, Idx, VFAT_Entry (J).Name_1);
               To_WString (Name, Idx, VFAT_Entry (J).Name_2);
               To_WString (Name, Idx, VFAT_Entry (J).Name_3);
            end loop;

            VFAT_Entry (VFAT_Entry'First).VFAT_Attr.Stop_Bit := True;

            E :=
              (FS            => Parent.FS,
               L_Name        => Name,
               S_Name        => SName,
               S_Name_Ext    => SExt,
               Attributes    => Attributes,
               Start_Cluster => Parent.FS.New_Cluster,
               Size          => 0,
               Index         => Index + VFAT_Entry'Length,
               Is_Root       => False,
               Is_Dirty      => False);

            if E.Start_Cluster = INVALID_CLUSTER then
               return Disk_Full;
            end if;

            FAT_Entry :=
              (Filename   => SName,
               Extension  => SExt,
               Attributes => Attributes,
               Reserved   => (others => ASCII.NUL),
               Cluster_H  =>
                 Unsigned_16
                   (Shift_Right
                        (Unsigned_32 (E.Start_Cluster) and 16#FFFF_0000#, 16)),
               Time       => 0,
               Date       => 0,
               Cluster_L  => Unsigned_16 (E.Start_Cluster and 16#FFFF#),
               Size       => 0);

            --  Now write down the new entries:

            --  First reset the directory handle
            Reset (Parent.all);

            --  Retrieve the block number relative to the first block of the
            --  directory content
            N_Blocks := Block_Offset
              (FAT_File_Size (Index) * 32 / Parent.FS.Block_Size);

            --  Check if we need to change cluster
            while N_Blocks >= Parent.FS.Blocks_Per_Cluster loop
               Parent.Current_Cluster :=
                 Parent.FS.Get_FAT (Parent.Current_Cluster);
               N_Blocks := N_Blocks - Parent.FS.Blocks_Per_Cluster;
            end loop;

            Parent.Current_Block :=
              Parent.FS.Cluster_To_Block (Parent.Current_Cluster) + N_Blocks;

            Status := Parent.FS.Ensure_Block (Parent.Current_Block);
            if Status /= OK then
               return Status;
            end if;

            for J in 1 .. N_Entries loop
               if J <= VFAT_Entry'Last then
                  Bytes := To_Data (VFAT_Entry (J));
               elsif J = VFAT_Entry'Last + 1 then
                  Bytes := To_Data (FAT_Entry);
               else
                  Bytes := (others => 0);
               end if;

               Block_Off := Natural
                 ((FAT_File_Size (Index) * 32) mod Parent.FS.Block_Size);

               if J > 1 and then Block_Off = 0 then
                  Status := Parent.FS.Write_Window;
                  exit when Status /= OK;
                  N_Blocks := N_Blocks + 1;

                  if N_Blocks = Parent.FS.Blocks_Per_Cluster then
                     N_Blocks := 0;
                     if Parent.FS.Is_Last_Cluster
                       (Parent.FS.Get_FAT (Parent.Current_Cluster))
                     then
                        Parent.Current_Cluster :=
                          Parent.FS.New_Cluster (Parent.Current_Cluster);
                     else
                        Parent.Current_Cluster :=
                          Parent.FS.Get_FAT (Parent.Current_Cluster);
                     end if;

                     Parent.Current_Block :=
                       Parent.FS.Cluster_To_Block (Parent.Current_Cluster);
                  else
                     Parent.Current_Block := Parent.Current_Block + 1;
                  end if;

                  Status := Parent.FS.Ensure_Block (Parent.Current_Block);
                  exit when Status /= OK;
               end if;

               Parent.FS.Window (Block_Off .. Block_Off + 31) := Bytes;
               Index := Index + 1;
            end loop;

            Status := Parent.FS.Write_Window;
            Reset (Parent.all);
         end;
      end if;

      return Status;
   end Allocate_Entry;

end Filesystem.FAT.Directories;
