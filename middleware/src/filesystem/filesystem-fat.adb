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
with Filesystem.FAT.Directories;
with Filesystem.FAT.Files;

package body Filesystem.FAT is

   The_File_Handles :
     array (1 .. MAX_FILE_HANDLES) of aliased FAT_File_Handle;
   Last_File_Handle : Natural := 0;

   The_Dir_Handles :
     array (1 .. MAX_DIR_HANDLES) of aliased FAT_Directory_Handle;
   Last_Dir_Handle : Natural := 0;

   function Find_Free_Dir_Handle return FAT_Directory_Handle_Access;
   function Find_Free_File_Handle return FAT_File_Handle_Access;

   procedure Initialize_FS
     (FS     : in out FAT_Filesystem;
      Status : out Status_Code);

   --------------------------
   -- Find_Free_Dir_Handle --
   --------------------------

   function Find_Free_Dir_Handle return FAT_Directory_Handle_Access
   is
      Found : Boolean := False;

   begin
      for J in Last_Dir_Handle + 1 .. The_Dir_Handles'Last loop
         if The_Dir_Handles (J).Is_Free then
            The_Dir_Handles (J).Is_Free := False;
            Last_Dir_Handle := J;

            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         for J in The_Dir_Handles'First .. Last_Dir_Handle loop
            if The_Dir_Handles (J).Is_Free then
               The_Dir_Handles (J).Is_Free := False;
               Last_Dir_Handle := J;

               Found := True;
               exit;
            end if;
         end loop;
      end if;

      if not Found then
         return null;
      else
         return The_Dir_Handles (Last_Dir_Handle)'Access;
      end if;
   end Find_Free_Dir_Handle;

   ---------------------------
   -- Find_Free_File_Handle --
   ---------------------------

   function Find_Free_File_Handle return FAT_File_Handle_Access
   is

   begin
      for J in Last_File_Handle + 1 .. The_File_Handles'Last loop
         if The_File_Handles (J).Is_Free then
            The_File_Handles (J).Is_Free := False;
            Last_File_Handle := J;

            return The_File_Handles (Last_File_Handle)'Access;
         end if;
      end loop;

      for J in The_File_Handles'First .. Last_File_Handle loop
         if The_File_Handles (J).Is_Free then
            The_File_Handles (J).Is_Free := False;
            Last_File_Handle := J;

            return The_File_Handles (Last_File_Handle)'Access;
         end if;
      end loop;

      return null;
   end Find_Free_File_Handle;

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
      function To_Upper (C : Character) return Character
      is (if C in 'a' .. 'z'
          then Character'Val
            (Character'Pos (C) - Character'Pos ('a') + Character'Pos ('A'))
          else C);

   begin
      if Name1.Len /= Name2.Len then
         return False;
      end if;

      for J in 1 .. Name1.Len loop
         if To_Upper (Name1.Name (J)) /= To_Upper (Name2.Name (J)) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Path : String) return Boolean
   is
   begin
      return Path'Length = 0 or else
        (Path'Length = 1 and then Path (Path'First) = '/');
   end Is_Root;

   --------------
   -- Basename --
   --------------

   function Basename (Path : String) return String
   is
      Last  : Natural := Path'Last;

   begin
      if Path'Length = 0 then
         return "";
      end if;

      if Path (Last) = '/' then
         Last := Last - 1;
      end if;

      for J in reverse 1 .. Last loop
         if Path (J) = '/' then
            return Path (J + 1 .. Last);
         end if;
      end loop;

      return Path (Path'First .. Last);
   end Basename;

   ------------
   -- Parent --
   ------------

   function Parent (Path : String) return String
   is
      Last : Natural;
   begin
      if Path'Length = 0 then
         return "";
      end if;

      Last := (if Path (Path'Last) = '/' then Path'Last - 1 else Path'Last);

      for J in reverse Path'First .. Last loop
         if Path (J) = '/' then
            return Path (Path'First .. J);
         end if;
      end loop;

      return "";
   end Parent;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Path       : String;
                       Ensure_Dir : Boolean := False) return String
   is
      Idx      : Integer;
      Prev     : Natural;
      Token    : FAT_Name;
      Last     : Natural;
      Ret      : String := Path;

   begin
      if Ret'Length = 0 then
         return "/";
      end if;

      --  Preserve initial '/'
      if Ret (Ret'First) = '/' then
         Idx := Ret'First + 1;
      else
         Idx := Ret'First;
      end if;

      Last := Ret'Last;

      --  Below: Idx always points to the first character of a path element.

      while Idx <= Last loop
         Token.Len := 0;

         for J in Idx .. Last loop
            exit when Ret (J) = '/';
            Token.Len := Token.Len + 1;
            Token.Name (Token.Len) := Ret (J);
         end loop;

         if -Token = "." then
            --  Skip
            if Idx + 2 > Last then
               --  Ret ends with just a '.'
               --  remove it:
               Last := Last - 1;
            else
               Ret (Idx .. Last - 2) := Ret (Idx + 2 .. Last);
               Last := Last - 2;
            end if;

         elsif -Token = ".." then
            if Idx - 1 <= Ret'First then
               --  We have "/../<subdirs>", or "../<subdirs>".
               --  invalid but we keep as-is
               Idx := Idx + 3;
            else
               Prev := 0;

               --  Find the parent directory separator
               for J in reverse Ret'First .. Idx - 2 loop
                  if Ret (J) = '/' then
                     Prev := J + 1;
                     exit;
                  else
                     Prev := J;
                  end if;
               end loop;

               Ret (Prev .. Last + Prev - Idx - 3) := Ret (Idx + 3 .. Last);
               Last := Last + Prev - Idx - 3;
               Idx := Prev;
            end if;

         elsif Token.Len = 0 then
            --  We have two consecutive slashes
            Ret (Idx .. Last - 1) := Ret (Idx + 1 .. Last);
            Last := Last - 1;

         else
            Idx := Idx + Token.Len + 1;

         end if;
      end loop;

      if Last = 0 then
         if Ensure_Dir then
            return "/";
         else
            return "";
         end if;
      else
         if Ret (Ret'First) /= '/' then
            if Ensure_Dir and then Ret (Last) /= '/' then
               return "/" & Ret (Ret'First .. Last) & "/";
            else
               return "/" & Ret (Ret'First .. Last);
            end if;
         else
            if Ensure_Dir and then Ret (Last) /= '/' then
               return Ret (Ret'First .. Last) & "/";
            else
               return Ret (Ret'First .. Last);
            end if;
         end if;
      end if;
   end Normalize;

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
     (Controller  : HAL.Block_Drivers.Any_Block_Driver;
      LBA         : Block_Number;
      FS          : in out FAT_Filesystem) return Status_Code
   is
      Status : Status_Code;
   begin
      FS.Initialized := True;
      FS.Controller  := Controller;
      FS.LBA         := LBA;

      Initialize_FS (FS, Status);

      if Status /= OK then
         FS.Initialized := False;

         return Status;
      end if;

      return OK;
   end Open;

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
      Status := FS.Ensure_Block (0);

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
           FS.Ensure_Block (Block_Offset (FS.FSInfo_Block_Number));

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
         FS.FSInfo_Changed := False;
      end if;

      declare
         FAT_Size_In_Block : constant Unsigned_32 :=
                               FS.FAT_Table_Size_In_Blocks *
                                 Unsigned_32 (FS.Number_Of_FATs);
         Root_Dir_Size     : Block_Offset;
      begin
         FS.FAT_Addr  := Block_Offset (FS.Reserved_Blocks);
         FS.Data_Area := FS.FAT_Addr + Block_Offset (FAT_Size_In_Block);

         if FS.Version = FAT16 then
            --  Add space for the root directory
            FS.Root_Dir_Area := FS.Data_Area;
            Root_Dir_Size :=
              (Block_Offset (FS.FAT16_Root_Dir_Num_Entries) * 32 +
                   Block_Offset (FS.Block_Size) - 1) /
                Block_Offset (FS.Block_Size);
            --  Align on clusters
            Root_Dir_Size :=
              ((Root_Dir_Size + FS.Blocks_Per_Cluster - 1) /
                   FS.Blocks_Per_Cluster) *
                  FS.Blocks_Per_Cluster;

            FS.Data_Area := FS.Data_Area + Root_Dir_Size;
         end if;

         FS.Num_Clusters :=
           Cluster_Type
             ((FS.Total_Number_Of_Blocks - Unsigned_32 (FS.Data_Area)) /
                Unsigned_32 (FS.Blocks_Per_Cluster));
      end;

      FS.Root_Entry := Directories.Root_Entry (FS);
   end Initialize_FS;

   -----------
   -- Close --
   -----------

   overriding procedure Close (FS : in out FAT_Filesystem)
   is
   begin
      for J in The_File_Handles'Range loop
         if not The_File_Handles (J).Is_Free
           and then The_File_Handles (J).FS = FS'Unchecked_Access
         then
            The_File_Handles (J).Close;
         end if;
      end loop;

      for J in The_Dir_Handles'Range loop
         if not The_Dir_Handles (J).Is_Free
           and then The_Dir_Handles (J).FS = FS'Unchecked_Access
         then
            The_Dir_Handles (J).Close;
         end if;
      end loop;

      if FS.FSInfo_Changed then
         FS.Write_FSInfo;
         FS.FSInfo_Changed := False;
      end if;

      FS.Initialized := False;
   end Close;

   ------------------
   -- Ensure_Block --
   ------------------

   function Ensure_Block
     (FS    : in out FAT_Filesystem;
      Block : Block_Offset) return Status_Code
   is
   begin
      if Block = FS.Window_Block then
         return OK;
      end if;

      if not FS.Controller.Read
        (FS.LBA + Block, FS.Window)
      then
         FS.Window_Block  := 16#FFFF_FFFF#;

         return Disk_Error;
      end if;

      FS.Window_Block := Block;

      return OK;
   end Ensure_Block;

   ----------------
   -- Root_Entry --
   ----------------

   overriding function Root_Node
     (FS     : in out FAT_Filesystem;
      As     : String;
      Handle : out Any_Node_Handle)
      return Status_Code
   is
   begin
      FS.Root_Entry.L_Name := -As;
      Handle := FS.Root_Entry'Unchecked_Access;
      return OK;
   end Root_Node;

   ----------
   -- Open --
   ----------

   overriding function Open
     (FS     : in out FAT_Filesystem;
      Path   : String;
      Handle : out Any_Directory_Handle) return Status_Code
   is
   begin
      return FAT_Open (FS, Path, FAT_Directory_Handle_Access (Handle));
   end Open;

   --------------
   -- FAT_Open --
   --------------

   function FAT_Open
     (FS     : in out FAT_Filesystem;
      Path   : String;
      Handle : out FAT_Directory_Handle_Access) return Status_Code
   is
      E      : aliased FAT_Node;
      Full   : constant String := Normalize (Path);

      Status : Status_Code;
   begin
      if not Is_Root (Full) then
         Status := Directories.Find (FS, Full, E);

         if Status /= OK then
            return Status;
         end if;

      else
         E := FS.Root_Entry;
      end if;

      Status := OK;
      return E.FAT_Open (Handle);
   end FAT_Open;

   ----------
   -- Open --
   ----------

   function FAT_Open
     (D_Entry : FAT_Node;
      Handle  : out FAT_Directory_Handle_Access) return Status_Code
   is
   begin
      Handle := null;
      if not Is_Subdirectory (D_Entry) then
         return No_Such_File;
      end if;

      Handle := Find_Free_Dir_Handle;

      if Handle = null then
         return Too_Many_Open_Files;
      end if;

      Handle.FS            := D_Entry.FS;
      Handle.Current_Index := 0;

      if D_Entry.Is_Root then
         if D_Entry.FS.Version = FAT16 then
            Handle.Start_Cluster   := 0;
            Handle.Current_Block   := D_Entry.FS.Root_Dir_Area;
         else
            Handle.Start_Cluster := D_Entry.FS.Root_Dir_Cluster;
            Handle.Current_Block :=
              D_Entry.FS.Cluster_To_Block (D_Entry.FS.Root_Dir_Cluster);
         end if;
      else
         Handle.Start_Cluster := D_Entry.Start_Cluster;
         Handle.Current_Block :=
           D_Entry.FS.Cluster_To_Block (D_Entry.Start_Cluster);
      end if;

      Handle.Current_Cluster := Handle.Start_Cluster;

      return OK;
   end FAT_Open;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Dir : in out FAT_Directory_Handle)
   is
   begin
      Dir.Current_Block   := Cluster_To_Block (Dir.FS.all, Dir.Start_Cluster);
      Dir.Current_Cluster := Dir.Start_Cluster;
      Dir.Current_Index   := 0;
   end Reset;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Dir    : in out FAT_Directory_Handle;
      Handle : out Any_Node_Handle) return Status_Code
   is
      Node : FAT_Node;
      Status : Status_Code;
   begin
      Status := Directories.Read (Dir, Node);
      Dir.Current_Node := Node;
      Handle := Dir.Current_Node'Unchecked_Access;
      return Status;
   end Read;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Dir : in out FAT_Directory_Handle)
   is
   begin
      Dir.FS              := null;
      Dir.Current_Index   := 0;
      Dir.Start_Cluster   := 0;
      Dir.Current_Cluster := 0;
      Dir.Current_Block   := 0;
      Dir.Is_Free         := True;
   end Close;

   -----------------
   -- Create_File --
   -----------------

   overriding
   function Create_File (This : in out FAT_Filesystem;
                         Path : String)
                         return Status_Code
   is
      Parent_E : FAT_Node;
      Node     : FAT_Node;
      Ret      : Status_Code;
   begin
      if Directories.Find (This, Parent (Path), Parent_E) /= OK then
         return No_Such_File;
      end if;

      Ret := Directories.Create_File_Node (Parent_E, -Basename (Path), Node);

      return Ret;
   end Create_File;

   ------------
   -- Unlink --
   ------------

   overriding
   function Unlink (This : in out FAT_Filesystem;
                    Path : String)
                    return Status_Code
   is
      Parent_E : FAT_Node;
      Node     : FAT_Node;
   begin
      if Is_Root (Path) then
         return No_Such_File;
      end if;

      if Directories.Find (This, Parent (Path), Parent_E) /= OK then
         return No_Such_File;
      end if;

      if Directories.Find (Parent_E, -Basename (Path), Node) /= OK then
         return No_Such_File;
      end if;

      return Directories.Delete_Entry (Dir => Parent_E,
                                       Ent => Node);
   end Unlink;

   ----------------------
   -- Remove_Directory --
   ----------------------

   overriding
   function Remove_Directory (This : in out FAT_Filesystem;
                              Path : String)
                              return Status_Code
   is
      E      : aliased FAT_Node;
      Full   : constant String := Normalize (Path);

      Status : Status_Code;
   begin
      if not Is_Root (Full) then
         Status := Directories.Find (This, Full, E);

         if Status /= OK then
            return Status;
         end if;

      else
         return Invalid_Parameter;
      end if;

      return Directories.Delete_Subdir (E, False);
   end Remove_Directory;

   ----------
   -- Open --
   ----------

   overriding function Open
     (FS     : in out FAT_Filesystem;
      Path   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle) return Status_Code
   is
      Parent_E : FAT_Node;

   begin
      Handle := null;
      if Is_Root (Path) then
         return No_Such_File;
      end if;

      if Directories.Find (FS, Parent (Path), Parent_E) /= OK then
         return No_Such_File;
      end if;

      return Open (Parent => Parent_E,
                   Name   => Basename (Path),
                   Mode   => Mode,
                   Handle => Handle);
   end Open;

   ----------
   -- Open --
   ----------

   overriding function Open
     (Parent : FAT_Node;
      Name   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle) return Status_Code
   is
      FAT_Handle : FAT_File_Handle_Access;
      Ret : Status_Code;
   begin
      FAT_Handle := Find_Free_File_Handle;

      if FAT_Handle = null then
         return Too_Many_Open_Files;
      end if;

      Ret := Files.Open (Parent, -Name, Mode, FAT_Handle);
      Handle := Any_File_Handle (FAT_Handle);
      return Ret;
   end Open;

   ----------
   -- Size --
   ----------

   overriding function Size (File : FAT_File_Handle) return File_Size
   is
   begin
      return File_Size (File.D_Entry.Size);
   end Size;

   ----------
   -- Mode --
   ----------

   overriding function Mode (File : FAT_File_Handle) return File_Mode
   is
   begin
      return File.Mode;
   end Mode;

   ----------
   -- Read --
   ----------

   overriding function Read
     (File   : in out FAT_File_Handle;
      Addr   : System.Address;
      Length : in out File_Size) return Status_Code
   is
      L : FAT_File_Size := FAT_File_Size (Length);
      Ret : Status_Code;
   begin
      Ret := Files.Read (File, Addr, L);
      Length := File_Size (L);

      return Ret;
   end Read;

--     ----------
--     -- Read --
--     ----------
--
--     procedure Generic_Read
--       (Handle : File_Handle;
--        Value  : out T)
--     is
--        Ret : File_Size with Unreferenced;
--     begin
--        Ret := Files.Read (Handle, Value'Address, T'Size / 8);
--     end Generic_Read;

   ------------
   -- Offset --
   ------------

   overriding function Offset
     (File : FAT_File_Handle) return File_Size
   is (File_Size (File.File_Index));

   ----------------
   -- File_Write --
   ----------------

   overriding function Write
     (File   : in out FAT_File_Handle;
      Addr   : System.Address;
      Length : File_Size) return Status_Code
   is
   begin
      return Files.Write (File, Addr, FAT_File_Size (Length));
   end Write;

   ----------------
   -- File_Flush --
   ----------------

   overriding function Flush
     (File : in out FAT_File_Handle) return Status_Code
   is
   begin
      return Files.Flush (File);
   end Flush;

   ---------------
   -- File_Seek --
   ---------------

   overriding function Seek
     (File   : in out FAT_File_Handle;
      Origin : Seek_Mode;
      Amount : in out File_Size) return Status_Code
   is
      Num : FAT_File_Size := FAT_File_Size (Amount);
      Ret : Status_Code;
   begin
      Ret := Files.Seek (File, Num, Origin);
      Amount := File_Size (Num);

      return Ret;
   end Seek;

   ----------------
   -- File_Close --
   ----------------

   overriding procedure Close (File : in out FAT_File_Handle)
   is
   begin
      Files.Close (File);
      File.Is_Free := True;
   end Close;

   -------------
   -- Get_FAT --
   -------------

   function Get_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Cluster_Type) return Cluster_Type
   is
      Idx       : Natural;
      Block_Num : Block_Offset;

      subtype B4 is Block (1 .. 4);
      subtype B2 is Block (1 .. 2);
      function To_Cluster is new Ada.Unchecked_Conversion
        (B4, Cluster_Type);
      function To_U16 is new Ada.Unchecked_Conversion
        (B2, Unsigned_16);

   begin
      if Cluster < 2 or else Cluster > FS.Num_Clusters + 2 then
         return 1;
      end if;

      if FS.Version = FAT32 then
         Block_Num :=
           FS.FAT_Addr +
             Block_Offset (Cluster) * 4 / Block_Offset (FS.Block_Size);
      else
         Block_Num :=
           FS.FAT_Addr +
             Block_Offset (Cluster) * 2 / Block_Offset (FS.Block_Size);
      end if;

      if Block_Num /= FS.FAT_Block then
         FS.FAT_Block := Block_Num;

         if not FS.Controller.Read
           (FS.LBA + FS.FAT_Block,
            FS.FAT_Window)
         then
            FS.FAT_Block := 16#FFFF_FFFF#;
            return INVALID_CLUSTER;
         end if;
      end if;

      if FS.Version = FAT32 then
         Idx :=
           Natural (FAT_File_Size ((Cluster) * 4) mod FS.Block_Size);
         return To_Cluster (FS.FAT_Window (Idx .. Idx + 3)) and 16#0FFF_FFFF#;
      else
         Idx :=
           Natural (FAT_File_Size ((Cluster) * 2) mod FS.Block_Size);
         return Cluster_Type (To_U16 (FS.FAT_Window (Idx .. Idx + 1)));
      end if;

   end Get_FAT;

   -------------
   -- Set_FAT --
   -------------

   function Set_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Cluster_Type;
      Value   : Cluster_Type) return Status_Code
   is
      Idx       : Natural;
      Block_Num : Block_Offset;
      Dead      : Boolean with Unreferenced;

      subtype B4 is Block (1 .. 4);
      function From_Cluster is new Ada.Unchecked_Conversion
        (Cluster_Type, B4);

   begin
      if Cluster < Valid_Cluster'First or else Cluster > FS.Num_Clusters then
         return Internal_Error;
      end if;

      Block_Num :=
        FS.FAT_Addr +
          Block_Offset (Cluster) * 4 / Block_Offset (FS.Block_Size);

      if Block_Num /= FS.FAT_Block then
         FS.FAT_Block := Block_Num;

         if not FS.Controller.Read (FS.LBA + FS.FAT_Block, FS.FAT_Window) then
            FS.FAT_Block := 16#FFFF_FFFF#;
            return Disk_Error;
         end if;
      end if;

      Idx := Natural (FAT_File_Size (Cluster * 4) mod FS.Block_Size);

      FS.FAT_Window (Idx .. Idx + 3) := From_Cluster (Value);

      if not FS.Controller.Write (FS.LBA + FS.FAT_Block, FS.FAT_Window) then
         return Disk_Error;
      end if;

      return OK;
   end Set_FAT;

   ------------------
   -- Write_FSInfo --
   ------------------

   procedure Write_FSInfo
     (FS : in out FAT_Filesystem)
   is
      subtype FSInfo_Block is Block (0 .. 11);
      function From_FSInfo is new Ada.Unchecked_Conversion
        (FAT_FS_Info, FSInfo_Block);

      Status        : Status_Code;
      FAT_Begin_LBA : constant Block_Offset :=
                        Block_Offset (FS.FSInfo_Block_Number);
      Ret           : Status_Code with Unreferenced;

   begin
      Status := FS.Ensure_Block (FAT_Begin_LBA);

      if Status /= OK then
         return;
      end if;

      --  again, check the generic FAT block signature
      if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
         return;
      end if;

      --  good. now we got the entire FSinfo in our window.
      --  modify that part of the window and writeback.
      FS.Window (16#1E4# .. 16#1EF#) := From_FSInfo (FS.FSInfo);
      Ret := FS.Write_Window;
   end Write_FSInfo;

   ----------------------
   -- Get_Free_Cluster --
   ----------------------

   function Get_Free_Cluster
     (FS       : in out FAT_Filesystem;
      Previous : Cluster_Type := INVALID_CLUSTER) return Cluster_Type
   is
      Candidate : Cluster_Type := Previous;
   begin
      --  First check for a cluster that is just after the previous one
      --  allocated for the entry
      if Candidate in Valid_Cluster'Range
        and then Candidate < FS.Num_Clusters
      then
         Candidate := Candidate + 1;
         if FS.Is_Free_Cluster (FS.Get_FAT (Candidate)) then
            return Candidate;
         end if;
      end if;

      --  Next check the most recently allocated cluster
      Candidate := FS.Most_Recently_Allocated_Cluster + 1;

      if Candidate not in Valid_Cluster'Range then
         Candidate := Valid_Cluster'First;
      end if;

      while Candidate in Valid_Cluster'Range
        and then Candidate < FS.Num_Clusters
      loop
         if FS.Is_Free_Cluster (FS.Get_FAT (Candidate)) then
            return Candidate;
         end if;

         Candidate := Candidate + 1;
      end loop;

      Candidate := Valid_Cluster'First;
      while Candidate <= FS.Most_Recently_Allocated_Cluster loop
         if FS.Is_Free_Cluster (FS.Get_FAT (Candidate)) then
            return Candidate;
         end if;

         Candidate := Candidate + 1;
      end loop;

      return INVALID_CLUSTER;
   end Get_Free_Cluster;

   -----------------
   -- New_Cluster --
   -----------------

   function New_Cluster
     (FS : in out FAT_Filesystem) return Cluster_Type
   is
   begin
      return FS.New_Cluster (INVALID_CLUSTER);
   end New_Cluster;

   -----------------
   -- New_Cluster --
   -----------------

   function New_Cluster
     (FS       : in out FAT_Filesystem;
      Previous : Cluster_Type) return Cluster_Type
   is
      Ret : Cluster_Type;
   begin
      pragma Assert
        (FS.Version = FAT32,
         "FS write only supported on FAT32 for now");

      Ret := FS.Get_Free_Cluster (Previous);

      if Ret = INVALID_CLUSTER then
         return Ret;
      end if;

      if Previous /= INVALID_CLUSTER then
         if FS.Set_FAT (Previous, Ret) /= OK then
            return INVALID_CLUSTER;
         end if;
      end if;

      if FS.Set_FAT (Ret, LAST_CLUSTER_VALUE) /= OK then
         return INVALID_CLUSTER;
      end if;

      FS.FSInfo.Free_Clusters := FS.FSInfo.Free_Clusters - 1;
      FS.FSInfo.Last_Allocated_Cluster := Ret;
      FS.FSInfo_Changed := True;

      return Ret;
   end New_Cluster;

   ------------------
   -- Write_Window --
   ------------------

   function Write_Window
     (FS : in out FAT_Filesystem) return Status_Code
   is
   begin
      if FS.Controller.Write (FS.LBA + FS.Window_Block, FS.Window) then
         return OK;
      else
         return Disk_Error;
      end if;
   end Write_Window;

   -----------
   -- Close --
   -----------

   overriding procedure Close (E : in out FAT_Node) is
   begin
      null;
   end Close;

   ----------
   -- Size --
   ----------

   overriding function Size (E : FAT_Node) return File_Size
   is (File_Size (E.Size));

end Filesystem.FAT;
