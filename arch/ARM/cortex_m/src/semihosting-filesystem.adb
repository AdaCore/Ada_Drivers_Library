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

--  This package use the ARM semihosting feature to provide a low level driver
--  to access the host file system. It is recommended to _not_ use this
--  interface directly but to access the file system using the File_IO package.
--  For more info, see the file system chapter of the documentation.

package body Semihosting.Filesystem is

   -----------------
   -- Create_File --
   -----------------

   overriding function Create_File
     (This : in out SHFS;
      Path : String)
      return Status_Code
   is
      pragma Unreferenced (This, Path);
   begin
      return Read_Only_File_System;
   end Create_File;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (This : in out SHFS;
      Path : String;
      Kind : File_Kind)
      return Status_Code
   is
      pragma Unreferenced (This, Path, Kind);
   begin
      return Read_Only_File_System;
   end Create_Node;

   ----------------------
   -- Create_Directory --
   ----------------------

   function Create_Directory
     (This : in out SHFS;
      Path : String)
      return Status_Code
   is
      pragma Unreferenced (This, Path);
   begin
      return Read_Only_File_System;
   end Create_Directory;

   ------------
   -- Unlink --
   ------------

   overriding function Unlink
     (This : in out SHFS;
      Path : String)
      return Status_Code
   is
      pragma Unreferenced (This, Path);
   begin
      return Read_Only_File_System;
   end Unlink;

   ----------------------
   -- Remove_Directory --
   ----------------------

   overriding function Remove_Directory
     (This : in out SHFS;
      Path : String)
      return Status_Code
   is
      pragma Unreferenced (This, Path);
   begin
      return Read_Only_File_System;
   end Remove_Directory;

   ------------
   -- Rename --
   ------------

   function Rename
     (This : in out SHFS;
      Old_Path : String;
      New_Path : String)
      return Status_Code
   is
      pragma Unreferenced (This, Old_Path, New_Path);
   begin
      return Read_Only_File_System;
   end Rename;

   ------------------------
   -- Change_Permissions --
   ------------------------

   function Truncate_File
     (This   : in out SHFS;
      Path   : String;
      Length : File_Size)
      return Status_Code
   is
      pragma Unreferenced (Path, Length, This);
   begin
      return Read_Only_File_System;
   end Truncate_File;

   ----------
   -- Open --
   ----------

   overriding function Open
     (This    : in out SHFS;
      Path    : String;
      Mode    : File_Mode;
      Handler : out Any_File_Handle)
      return Status_Code
   is
      FH : SHFS_File_Handle_Access;
      FD : SH_Word;
   begin
      if Path'Length = 0 then
         return No_Such_File;
      end if;

      if Mode /= Read_Only then
         return Read_Only_File_System;
      end if;

      FD := Semihosting.Open (Filename => Path,
                              Mode     => OPEN_FLAG_RB);

      if FD = SH_Word'Last then
         return No_Such_File;
      else
         FH := This.Get_File_Handle;
         FH.FS := This'Unchecked_Access;
         FH.FD := FD;
         FH.Is_Open := True;
         FH.Absolute_Position := 0;
         Handler := Any_File_Handle (FH);
         return OK;
      end if;
   end Open;

   ----------
   -- Open --
   ----------

   overriding function Open
     (This   : in out SHFS;
      Path   : String;
      Handle : out Any_Directory_Handle)
      return Status_Code
   is
      pragma Unreferenced (This, Path, Handle);
   begin
      return Operation_Not_Permitted;
   end Open;

   ---------------
   -- Root_Node --
   ---------------

   overriding
   function Root_Node
     (This   : in out SHFS;
      As     : String;
      Handle : out Any_Node_Handle)
      return Status_Code
   is
      pragma Unreferenced (As, Handle, This);
   begin
      return Operation_Not_Permitted;
   end Root_Node;

   ---------------------
   -- Get_File_Handle --
   ---------------------

   function Get_File_Handle (This : in out SHFS)
                             return not null SHFS_File_Handle_Access
   is
      Ret : SHFS_File_Handle_Access := This.File_Handles;
   begin

      --  Try to find a free handle
      while Ret /= null and then Ret.Is_Open loop
         Ret := Ret.Next;
      end loop;

      --  Allocate a new handle
      if Ret = null then
         Ret := new SHFS_File_Handle;
         Ret.Is_Open := False;
         Ret.Next := This.File_Handles;
         This.File_Handles := Ret;
      end if;

      return Ret;
   end Get_File_Handle;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (This : in out SHFS) is
   begin
      null;
   end Close;

   ----------
   -- Read --
   ----------

   overriding function Read
     (This   : in out SHFS_File_Handle;
      Addr   : System.Address;
      Length : in out File_Size)
      return Status_Code
   is
   begin
      if not This.Is_Open then
         return Invalid_Parameter;
      end if;

      if Semihosting.Read (File_Handle    => This.FD,
                           Buffer_Address => Addr,
                           Buffer_Size    => SH_Word (Length)) /= 0
      then
         return Input_Output_Error;
      else
         This.Absolute_Position := This.Absolute_Position + Length;
         return OK;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   overriding function Write
     (This : in out SHFS_File_Handle;
      Addr   : System.Address;
      Length : File_Size)
      return Status_Code
   is
      pragma Unreferenced (Addr, Length);
   begin
      if not This.Is_Open then
         return Invalid_Parameter;
      end if;

      return Read_Only_File_System;
   end Write;

   ----------
   -- Seek --
   ----------

   overriding
   function Seek (This   : in out SHFS_File_Handle;
                  Origin : Seek_Mode;
                  Amount : in out File_Size)
                  return Status_Code
   is
      Target : File_Size;
   begin
      if not This.Is_Open then
         return Invalid_Parameter;
      end if;

      case Origin is
         when Forward =>
            Target := This.Absolute_Position + Amount;
         when Backward =>
            Target := This.Absolute_Position - Amount;
         when From_Start =>
            Target := Amount;
         when From_End =>
            Target := This.Size - Amount;
      end case;

      if Semihosting.Seek (This.FD, SH_Word (Target)) /= 0
      then
         return Input_Output_Error;
      else
         This.Absolute_Position := Target;
         return OK;
      end if;
   end Seek;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (This   : in out SHFS_File_Handle)
   is
   begin
      if not This.Is_Open then
         return;
      end if;

      if Semihosting.Close (This.FD) /= 0 then
         This.Is_Open := False;
      end if;
   end Close;

   ------------
   -- Get_FS --
   ------------

   overriding
   function Get_FS
     (This : in out SHFS_File_Handle)
      return Any_Filesystem_Driver
   is (Any_Filesystem_Driver (This.FS));

   ----------
   -- Size --
   ----------

   overriding
   function Size
     (This : SHFS_File_Handle) return File_Size
   is
   begin
      raise Program_Error with "not implemented";
      return 0;
   end Size;

   ----------
   -- Mode --
   ----------

   overriding
   function Mode
     (This : SHFS_File_Handle) return File_Mode
   is (Read_Only);

   ------------
   -- Offset --
   ------------

   overriding
   function Offset
     (This : SHFS_File_Handle)
      return File_Size
   is
   begin
      raise Program_Error with "Not implemented";
      return 0;
   end Offset;

   -----------
   -- Flush --
   -----------

   overriding
   function Flush
     (This : in out SHFS_File_Handle)
      return Status_Code
   is (OK);

end Semihosting.Filesystem;
