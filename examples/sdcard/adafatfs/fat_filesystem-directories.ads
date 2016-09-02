------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
