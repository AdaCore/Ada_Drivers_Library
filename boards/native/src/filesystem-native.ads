------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

--  This package provide a low level driver to access the native file system.
--  It is recommended to _not_ use this interface directly but to access the
--  file system using the File_IO package. For more info, see the file system
--  chapter of the documentation.

private with Ada.Containers.Vectors;
private with Ada.Direct_IO;
private with Ada.Strings.Unbounded;

with HAL.Filesystem; use HAL.Filesystem;
with System;

--  Simple wrappers around the Ada standard library to provide implementations
--  for HAL.Filesystem interfaces.

package Filesystem.Native is

   package HALFS renames HAL.Filesystem;

   ----------------------
   -- Native_FS_Driver --
   ----------------------

   type Native_FS_Driver is limited new HALFS.Filesystem_Driver with private;
   type Native_FS_Driver_Access is access all Native_FS_Driver;

   type File_Handle is limited new HALFS.File_Handle with private;
   type File_Handle_Access is access all File_Handle;

   type Directory_Handle is limited new HALFS.Directory_Handle with private;
   type Directory_Handle_Access is access all Directory_Handle;

   type Node_Handle is new HALFS.Node_Handle with private;
   type Node_Handle_Access is access all Node_Handle;

   ---------------------------
   --  Directory operations --
   ---------------------------

   overriding
   function Open
     (This   : in out Native_FS_Driver;
      Path   : String;
      Handle : out Any_Directory_Handle)
      return Status_Code;
   --  Open a new Directory Handle at the given Filesystem Path

   overriding
   function Create_File (This : in out Native_FS_Driver;
                         Path : String)
                         return Status_Code;

   overriding
   function Unlink (This : in out Native_FS_Driver;
                    Path : String)
                    return Status_Code;
   --  Remove the regular file located at Path in the This filesystem

   overriding
   function Remove_Directory (This : in out Native_FS_Driver;
                              Path : String)
                              return Status_Code;
   --  Remove the directory located at Path in the This filesystem

   overriding
   function Get_FS
     (This : Directory_Handle) return Any_Filesystem_Driver;
   --  Return the filesystem the handle belongs to.

   overriding
   function Root_Node
     (This   : in out Native_FS_Driver;
      As     : String;
      Handle : out Any_Node_Handle)
      return Status_Code;
   --  Open a new Directory Handle at the given Filesystem Path

   overriding
   function Read
     (This   : in out Directory_Handle;
      Handle : out Any_Node_Handle)
      return Status_Code;
   --  Reads the next directory entry. If no such entry is there, an error
   --  code is returned in Status.

   overriding
   procedure Reset (This : in out Directory_Handle);
   --  Resets the handle to the first node

   overriding
   procedure Close (This : in out Directory_Handle);
   --  Closes the handle, and free the associated resources.

   ---------------------
   -- Node operations --
   ---------------------

   overriding
   function Get_FS (This : Node_Handle) return Any_Filesystem_Driver;

   overriding
   function Basename (This : Node_Handle) return String;

   overriding
   function Is_Read_Only (This : Node_Handle) return Boolean;

   overriding
   function Is_Hidden (This : Node_Handle) return Boolean;

   overriding
   function Is_Subdirectory (This : Node_Handle) return Boolean;

   overriding
   function Is_Symlink (This : Node_Handle) return Boolean;

   overriding
   function Size (This : Node_Handle) return File_Size;

   overriding
   procedure Close (This : in out Node_Handle);

   ---------------------
   -- File operations --
   ---------------------

   overriding
   function Open
     (This   : in out Native_FS_Driver;
      Path   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code;
   --  Open a new File Handle at the given Filesystem Path

   overriding
   function Open
     (This   : Node_Handle;
      Name   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code
     with Pre'Class => Is_Subdirectory (This);

   overriding
   function Get_FS
     (This : in out File_Handle) return Any_Filesystem_Driver;

   overriding
   function Size
     (This : File_Handle) return File_Size;

   overriding
   function Mode
     (This : File_Handle) return File_Mode;

   overriding
   function Read
     (This   : in out File_Handle;
      Addr   : System.Address;
      Length : in out File_Size)
      return Status_Code;

   overriding
   function Write
     (This   : in out File_Handle;
      Addr   : System.Address;
      Length : File_Size)
      return Status_Code;

   overriding
   function Offset
     (This : File_Handle) return File_Size;

   overriding
   function Flush
     (This : in out File_Handle) return Status_Code;

   overriding
   function Seek
     (This   : in out File_Handle;
      Origin : Seek_Mode;
      Amount : in out File_Size)
      return Status_Code;

   overriding
   procedure Close (This : in out File_Handle);

   -------------------
   -- FS operations --
   -------------------

   overriding
   procedure Close (This : in out Native_FS_Driver);

   procedure Destroy (This : in out Native_FS_Driver_Access);

   function Create
     (FS       : out Native_FS_Driver;
      Root_Dir : String)
      return Status_Code;
   --  Create a Native_FS_Driver considering Root_Dir as its root directory.
   --  All other pathnames in this API are processed as relative to it.
   --
   --  Note that this does not provide real isolation: trying to access the
   --  ".." directory will access the parent of the root directory, not the
   --  root directory itself.

   type File_Kind is (Regular_File, Directory);

   function Create_Node
     (This : in out Native_FS_Driver;
      Path : String;
      Kind : File_Kind)
      return Status_Code;

   function Create_Directory
     (This : in out Native_FS_Driver;
      Path : String)
      return Status_Code;

   function Rename
     (This     : in out Native_FS_Driver;
      Old_Path : String;
      New_Path : String)
      return Status_Code;

   function Truncate_File
     (This   : in out Native_FS_Driver;
      Path   : String;
      Length : File_Size)
      return Status_Code;

   -------------
   -- Helpers --
   -------------

   function Join
     (Prefix, Suffix           : String;
      Ignore_Absolute_Suffixes : Boolean)
      return String;
   --  Like Ada.Directories.Compose, but also accepts a full path as Suffix.
   --  For instance:
   --
   --     Join ("/a", "b")   => "/a/b"
   --     Join ("/a", "b/c") => "/a/b/c"
   --
   --  If Ignore_Absolute_Suffixes is True, if Suffix is an absolute path, do
   --  as if it was a relative path instead:
   --
   --     Join ("/a", "/b")  => "/a/b"
   --
   --  Otherwise, if sufifx is an absolute path, just return Suffix.

private

   package Byte_IO is new Ada.Direct_IO (UInt8);

   type Native_FS_Driver is limited new Filesystem_Driver with record
      Root_Dir          : Ada.Strings.Unbounded.Unbounded_String;
      --  Path on the host file system to be used as root directory for this FS

      Free_File_Handles : File_Handle_Access;
      --  Linked list of file handles available to use

      Free_Dir_Handles : Directory_Handle_Access;
      --  Likend list of directory handles available to use
   end record;

   function Get_Handle
     (FS : in out Native_FS_Driver)
      return File_Handle_Access;
   function Get_Handle
     (FS : in out Native_FS_Driver)
      return Directory_Handle_Access;
   --  Return an existing free handle or create one if none is available

   procedure Add_Free_Handle
     (FS     : in out Native_FS_Driver;
      Handle : in out File_Handle_Access)
     with Pre => Handle /= null,
     Post => Handle = null;
   procedure Add_Free_Handle
     (FS     : in out Native_FS_Driver;
      Handle : in out Directory_Handle_Access)
     with Pre => Handle /= null,
     Post => Handle = null;
   --  Add Handle to the list of available handles

   type File_Handle is limited new HALFS.File_Handle with record
      FS   : Native_FS_Driver_Access;
      --  The filesystem that owns this handle

      Next : File_Handle_Access;
      --  If this handle is used, this is undefined. Otherwise,
      --  this is the next free handle in the list (see
      --  Native_FS_Driver.Free_File_Handles).

      File : Byte_IO.File_Type;

      Mode : File_Mode;
   end record;

   type Node_Handle is new HALFS.Node_Handle with record
      FS        : Native_FS_Driver_Access;
      Kind      : File_Kind;
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Read_Only : Boolean;
      Hidden    : Boolean;
      Symlink   : Boolean;
      Size      : File_Size;
   end record;
   --  Set of information we handle for a node

   package Node_Vectors is new Ada.Containers.Vectors
     (Positive, Node_Handle);

   type Directory_Handle is limited new HALFS.Directory_Handle with record
      FS   : Native_FS_Driver_Access;
      --  The filesystem that owns this handle

      Next      : Directory_Handle_Access;
      --  If this handle is used, this is undefined. Otherwise,
      --  this is the next free handle in the list (see
      --  Native_FS_Driver.Free_Dir_Handles).

      Full_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Absolute path for this directory

      Data : Node_Vectors.Vector;
      --  Vector of entries for this directory.
      --
      --  On one hand, HAL.Filesystem exposes an index-based API to access
      --  directory entries. On the other hand, Ada.Directories exposes a
      --  kind of single-linked list. What we do here is that when we open a
      --  directory, we immediately get all entries and build a vector out of
      --  it for convenient random access.

      Index : Positive;
      --  Current index in the vector of Node
   end record;

end Filesystem.Native;
