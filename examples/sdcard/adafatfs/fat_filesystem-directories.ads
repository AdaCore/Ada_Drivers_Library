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

package FAT_Filesystem.Directories is

   type Directory_Entry is private;

   function Find
     (FS     : FAT_Filesystem_Access;
      Path   : FAT_Path;
      DEntry : out Directory_Entry) return Status_Code;

   function Find
     (Parent   : Directory_Handle;
      Filename : FAT_Name;
      DEntry   : out Directory_Entry) return Status_Code;

   function Find
     (Parent   : Directory_Entry;
      Filename : FAT_Name;
      DEntry   : out Directory_Entry) return Status_Code;

   function Root_Entry (FS : FAT_Filesystem_Access) return Directory_Entry;

   function Open_Root_Directory
     (FS  : FAT_Filesystem_Access;
      Dir : out Directory_Handle) return Status_Code;

   function Open_Dir
     (E   : Directory_Entry;
      Dir : out Directory_Handle) return Status_Code
   with Pre => Is_Subdirectory (E);

   procedure Reset_Dir (Dir : in out Directory_Handle);

   procedure Close_Dir (Dir : in out Directory_Handle);

   function Read_Dir (Dir    : in out Directory_Handle;
                      DEntry : out Directory_Entry) return Status_Code;

   -------------------------------------
   -- Operations on directory entries --
   -------------------------------------

   function Create_Subdir
     (Dir     : Directory_Entry;
      Name    : FAT_Name;
      New_Dir : out Directory_Entry) return Status_Code;

   function Create_File_Node
     (Dir      : Directory_Entry;
      Name     : FAT_Name;
      New_File : out Directory_Entry) return Status_Code;

   function Delete_Subdir
     (Dir       : Directory_Entry;
      Recursive : Boolean) return Status_Code;

   function Delete_Entry
     (Dir : Directory_Entry;
      Ent : Directory_Entry) return Status_Code;
   --  Mark the clusters related to Ent as free

   function Adjust_Clusters
     (Ent : Directory_Entry) return Status_Code;
   --  Adjust the number of clusters depending on Ent size

   --------------------------------
   -- Directory_Entry properties --
   --------------------------------

   function Name (E : Directory_Entry) return FAT_Name;

   function Short_Name (E : Directory_Entry) return FAT_Name;

   function Is_Read_Only (E : Directory_Entry) return Boolean;

   function Is_Hidden (E : Directory_Entry) return Boolean;

   function Is_System_File (E : Directory_Entry) return Boolean;

   function Is_Subdirectory (E : Directory_Entry) return Boolean;

   function Is_Archive (E : Directory_Entry) return Boolean;

   function Get_Start_Cluster (E : Directory_Entry) return Cluster_Type;

   function Get_Size (E : Directory_Entry) return Unsigned_32;

   function Get_FS (E : Directory_Entry) return FAT_Filesystem_Access;

   procedure Set_Size
     (E    : in out Directory_Entry;
      Size : Unsigned_32);

   function Update_Entry
     (Parent : Directory_Entry;
      Value  : in out Directory_Entry) return Status_Code;

private

   type Directory_Entry is record
      FS            : FAT_Filesystem_Access;
      L_Name        : FAT_Name;
      S_Name        : String (1 .. 8);
      S_Name_Ext    : String (1 .. 3);
      Attributes    : FAT_Directory_Entry_Attribute;
      Start_Cluster : Cluster_Type; --  The content of this entry
      Size          : Unsigned_32;

      Index         : Entry_Index;
      --  Index of the FAT_Directory_Intry within Parent's content

      Is_Root       : Boolean := False;
      --  Is it the root directory ?

      Is_Dirty      : Boolean := False;
      --  Whether changes need to be written on disk
   end record;

   function Is_Read_Only (E : Directory_Entry) return Boolean
   is (E.Attributes.Read_Only);

   function Is_Hidden (E : Directory_Entry) return Boolean
   is (E.Attributes.Hidden);

   function Is_System_File (E : Directory_Entry) return Boolean
   is (E.Attributes.System_File);

   function Is_Subdirectory (E : Directory_Entry) return Boolean
   is (E.Attributes.Subdirectory);

   function Is_Archive (E : Directory_Entry) return Boolean
   is (E.Attributes.Archive);

   function Get_Start_Cluster (E : Directory_Entry) return Cluster_Type
   is (E.Start_Cluster);

   function Get_Size (E : Directory_Entry) return Unsigned_32
   is (E.Size);

   function Get_FS (E : Directory_Entry) return FAT_Filesystem_Access
   is (E.FS);

   ---------------------------
   -- Low_Level subprograms --
   ---------------------------

   function Allocate_Entry
     (Parent     : in out Directory_Handle;
      Name       : FAT_Name;
      Attributes : FAT_Directory_Entry_Attribute;
      E          : out Directory_Entry) return Status_Code;

end FAT_Filesystem.Directories;
