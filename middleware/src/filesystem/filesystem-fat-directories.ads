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

private package Filesystem.FAT.Directories is

   function Find
     (FS     : in out FAT_Filesystem;
      Path   : String;
      DEntry : out FAT_Node) return Status_Code;

   function Find
     (Parent     : FAT_Node;
      Filename   : FAT_Name;
      DEntry     : out FAT_Node) return Status_Code
     with Pre => Is_Subdirectory (Parent);

   function Root_Entry (FS : in out FAT_Filesystem) return FAT_Node;

   function Read
     (Dir    : in out FAT_Directory_Handle;
      DEntry : out FAT_Node) return Status_Code;

   function Next_Entry
     (FS              : access FAT_Filesystem;
      Current_Cluster : in out Cluster_Type;
      Current_Block   : in out Block_Offset;
      Current_Index   : in out Entry_Index;
      DEntry          : out    FAT_Directory_Entry) return Status_Code;

   function Next_Entry
     (FS              : access FAT_Filesystem;
      Current_Cluster : in out Cluster_Type;
      Current_Block   : in out Block_Offset;
      Current_Index   : in out Entry_Index;
      DEntry          : out    FAT_Node) return Status_Code;

   -------------------------------------
   -- Operations on directory entries --
   -------------------------------------

   function Create_Subdir
     (Dir     : FAT_Node;
      Name    : FAT_Name;
      New_Dir : out FAT_Node) return Status_Code;

   function Create_File_Node
     (Dir      : FAT_Node;
      Name     : FAT_Name;
      New_File : out FAT_Node) return Status_Code;

   function Delete_Subdir
     (Dir       : FAT_Node;
      Recursive : Boolean) return Status_Code;

   function Delete_Entry
     (Dir : FAT_Node;
      Ent : FAT_Node) return Status_Code;
   --  Mark the clusters related to Ent as free

   function Adjust_Clusters
     (Ent : FAT_Node) return Status_Code;
   --  Adjust the number of clusters depending on Ent size

   -------------------------
   -- FAT_Node properties --
   -------------------------

   procedure Set_Size
     (E    : in out FAT_Node;
      Size : FAT_File_Size);

   function Update_Entry
     (Parent : FAT_Node;
      Value  : in out FAT_Node) return Status_Code;

   ---------------------------
   -- Low_Level subprograms --
   ---------------------------

   function Allocate_Entry
     (Parent     : access FAT_Directory_Handle;
      Name       : FAT_Name;
      Attributes : FAT_Directory_Entry_Attribute;
      E          : out FAT_Node) return Status_Code;

end Filesystem.FAT.Directories;
