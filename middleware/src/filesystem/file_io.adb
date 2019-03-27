------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2019, AdaCore                     --
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

with Filesystem;               use Filesystem;
with Filesystem.MBR;           use Filesystem.MBR;
with Filesystem.FAT;           use Filesystem.FAT;
with HAL.Filesystem;           use HAL.Filesystem;
with Ada.Unchecked_Conversion;

package body File_IO is

   package HALFS renames HAL.Filesystem;

   function Convert is new Ada.Unchecked_Conversion (HALFS.Status_Code, Status_Code);
   function Convert is new Ada.Unchecked_Conversion (File_Mode, HALFS.File_Mode);
   function Convert is new Ada.Unchecked_Conversion (File_Size, HALFS.File_Size);
   function Convert is new Ada.Unchecked_Conversion (HALFS.File_Size, File_Size);
   function Convert is new Ada.Unchecked_Conversion (Seek_Mode, HALFS.Seek_Mode);

   type Mount_Record is record
      Is_Free  : Boolean := True;
      Name     : String (1 .. Max_Mount_Name_Length);
      Name_Len : Positive;
      FS       : Any_Filesystem_Driver;
   end record;

   subtype Mount_Index is Integer range 0 .. Max_Mount_Points;
   subtype Valid_Mount_Index is Mount_Index range 1 .. Max_Mount_Points;
   type Mount_Array is array (Valid_Mount_Index) of Mount_Record;

   type VFS_Directory_Handle is new Directory_Handle with record
      Is_Free  : Boolean := True;
      Mount_Id : Mount_Index;
   end record;

   overriding function Get_FS
     (Dir : VFS_Directory_Handle) return Any_Filesystem_Driver;
   --  Return the filesystem the handle belongs to.

   overriding function Read
     (Dir    : in out VFS_Directory_Handle;
      Handle : out Any_Node_Handle) return HALFS.Status_Code;
   --  Reads the next directory entry. If no such entry is there, an error
   --  code is returned in Status.

   overriding procedure Reset (Dir : in out VFS_Directory_Handle);
   --  Resets the handle to the first node

   overriding procedure Close (Dir : in out VFS_Directory_Handle);
   --  Closes the handle, and free the associated resources.

   function Open
     (Path   : String;
      Handle : out Any_Directory_Handle)
      return Status_Code;

   function Open
     (Path   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code;

   Mount_Points : Mount_Array;

   Handles : array (1 .. 2) of aliased VFS_Directory_Handle;

   function Name (Point : Mount_Record) return Mount_Path;
   procedure Set_Name (Point : in out Mount_Record;
                       Path  : Mount_Path);
   procedure Split
     (Path        : String;
      FS          : out Any_Filesystem_Driver;
      Start_Index : out Natural);

   ----------
   -- Open --
   ----------

   function Open
     (File : in out File_Descriptor;
      Name : String;
      Mode : File_Mode)
      return Status_Code
   is
      Ret : Status_Code;
   begin
      if Is_Open (File) then
         return Invalid_Parameter;
      end if;
      Ret := Open (Name, Mode, File.Handle);

      if Ret /= OK then
         File.Handle := null;
      end if;

      return Ret;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Descriptor) is
   begin
      if File.Handle /= null then
         File.Handle.Close;
         File.Handle := null;
      end if;
   end Close;

   -------------
   -- Is_Open --
   -------------

   function Is_Open
     (File : File_Descriptor)
      return Boolean
   is (File.Handle /= null);

   -----------
   -- Flush --
   -----------

   function Flush
     (File : File_Descriptor)
      return Status_Code
   is
   begin
      if File.Handle /= null then
         return Convert (File.Handle.Flush);
      else
         return Invalid_Parameter;
      end if;
   end Flush;

   ----------
   -- Size --
   ----------

   function Size
     (File : File_Descriptor)
      return File_Size
   is
   begin
      if File.Handle = null then
         return 0;
      else
         return Convert (File.Handle.Size);
      end if;
   end Size;

   ----------
   -- Read --
   ----------

   function Read
     (File   : File_Descriptor;
      Addr   : System.Address;
      Length : File_Size)
      return File_Size
   is
      Ret    : HALFS.File_Size;
      Status : Status_Code;
   begin
      if File.Handle = null then
         return 0;
      end if;

      Ret := Convert (Length);
      Status := Convert (File.Handle.Read (Addr, Ret));

      if Status /= OK then
         return 0;
      else
         return Convert (Ret);
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   function Write
     (File   : File_Descriptor;
      Addr   : System.Address;
      Length : File_Size)
      return File_Size
   is
      Ret    : HALFS.File_Size;
      Status : Status_Code;
   begin
      if File.Handle = null then
         return 0;
      end if;

      Ret := Convert (Length);
      Status := Convert (File.Handle.Write (Addr, Ret));

      if Status /= OK then
         return 0;
      else
         return Convert (Ret);
      end if;
   end Write;

   ------------
   -- Offset --
   ------------

   function Offset
     (File : File_Descriptor)
      return File_Size
   is
   begin
      if File.Handle /= null then
         return Convert (File.Handle.Offset);
      else
         return 0;
      end if;
   end Offset;

   ----------
   -- Seek --
   ----------

   function Seek
     (File   : in out File_Descriptor;
      Origin : Seek_Mode;
      Amount : in out File_Size)
      return Status_Code
   is
      Ret          : Status_Code;
      HALFS_Amount : HALFS.File_Size;
   begin
      if File.Handle /= null then
         HALFS_Amount := Convert (Amount);
         Ret := Convert (File.Handle.Seek (Convert (Origin), HALFS_Amount));
         Amount := Convert (HALFS_Amount);
         return Ret;
      else
         return Invalid_Parameter;
      end if;
   end Seek;

   -------------------
   -- Generic_Write --
   -------------------

   function Generic_Write
     (File  : File_Descriptor;
      Value : T)
      return Status_Code
   is
   begin
      if File.Handle /= null then
         return Convert (File.Handle.Write (Value'Address, T'Size / 8));
      else
         return Invalid_Parameter;
      end if;
   end Generic_Write;

   ------------------
   -- Generic_Read --
   ------------------

   function Generic_Read
     (File  : File_Descriptor;
      Value : out T)
      return Status_Code
   is
      L : HALFS.File_Size := T'Size / 8;
   begin
      if File.Handle /= null then
         return Convert (File.Handle.Read (Value'Address, L));
      else
         return Invalid_Parameter;
      end if;
   end Generic_Read;

   ----------
   -- Open --
   ----------

   function Open
     (Dir  : in out Directory_Descriptor;
      Name : String)
      return Status_Code
   is
      Ret : Status_Code;
   begin
      if Dir.Handle /= null then
         return Invalid_Parameter;
      end if;
      Ret := Open (Name, Dir.Handle);

      if Ret /= OK then
         Dir.Handle := null;
      end if;

      return Ret;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Dir : in out Directory_Descriptor) is
   begin
      if Dir.Handle /= null then
         Dir.Handle.Close;
      end if;
   end Close;

   ----------
   -- Read --
   ----------

   function Read (Dir : in out Directory_Descriptor)
                  return Directory_Entry
   is
      Node   : Any_Node_Handle;
      Status : Status_Code;
   begin
      if Dir.Handle = null then
         return Invalid_Dir_Entry;
      end if;

      Status := Convert (Dir.Handle.Read (Node));
      if Status /= OK then
         return Invalid_Dir_Entry;
      end if;

      declare
         Name : constant String := Node.Basename;
         Ret  : Directory_Entry (Name_Length => Name'Length);
      begin
         Ret.Name         := Name;
         Ret.Subdirectory := Node.Is_Subdirectory;
         Ret.Read_Only    := Node.Is_Read_Only;
         Ret.Hidden       := Node.Is_Hidden;
         Ret.Symlink      := Node.Is_Symlink;
         Ret.Size         := Convert (Node.Size);
         Node.Close;
         return Ret;
      end;
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (Dir : in out Directory_Descriptor) is
   begin
      if Dir.Handle /= null then
         Dir.Handle.Reset;
      end if;
   end Reset;

   -----------------
   -- Create_File --
   -----------------

   function Create_File (Path : String) return Status_Code is
      Idx : Natural;
      FS  : Any_Filesystem_Driver;
   begin
      Split (Path, FS, Idx);

      if FS = null then
         return No_Such_Path;
      end if;

      return Convert (FS.Create_File (Path (Idx .. Path'Last)));
   end Create_File;

   ------------
   -- Unlink --
   ------------

   function Unlink (Path : String) return Status_Code is
      Idx : Natural;
      FS  : Any_Filesystem_Driver;
   begin
      Split (Path, FS, Idx);

      if FS = null then
         return No_Such_Path;
      end if;

      return Convert (FS.Unlink (Path (Idx .. Path'Last)));
   end Unlink;

   ----------------------
   -- Remove_Directory --
   ----------------------

   function Remove_Directory (Path : String) return Status_Code is
      Idx : Natural;
      FS  : Any_Filesystem_Driver;
   begin
      Split (Path, FS, Idx);

      if FS = null then
         return No_Such_Path;
      end if;

      return Convert (FS.Remove_Directory (Path (Idx .. Path'Last)));
   end Remove_Directory;

   ---------------
   -- Copy_File --
   ---------------

   function Copy_File (Source_Path, Destination_Path : String;
                       Buffer_Size : Positive := 512)
                        return Status_Code
   is
      Src, Dst         : File_Descriptor;
      Status           : Status_Code;
      Buffer           : HAL.UInt8_Array (1 .. Buffer_Size);
      Src_Len, Dst_Len : File_Size;
   begin
      Status := Open (Src, Source_Path, Read_Only);
      if Status /= OK then
         return Status;
      end if;

      Status := Create_File (Destination_Path);
      if Status /= OK then
         Close (Src);
         return Status;
      end if;

      Status := Open (Dst, Destination_Path, Write_Only);
      if Status /= OK then
         Close (Src);
         return Status;
      end if;

      loop
         Src_Len := Read (Src, Buffer'Address, Buffer'Length);

         exit when Src_Len = 0;

         Dst_Len := Write (Dst, Buffer'Address, Src_Len);

         if Dst_Len /= Src_Len then
            Close (Src);
            Close (Dst);
            return Input_Output_Error;
         end if;

         exit when Src_Len /= Buffer'Length;
      end loop;
      Close (Src);
      Close (Dst);
      return OK;
   end Copy_File;

   ----------
   -- Name --
   ----------

   function Name (Point : Mount_Record) return Mount_Path
   is (Point.Name (1 .. Point.Name_Len));

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Point : in out Mount_Record;
                       Path  : Mount_Path)
   is
   begin
      Point.Name (1 .. Path'Length) := Path;
      Point.Name_Len := Path'Length;
   end Set_Name;

   -----------
   -- Split --
   -----------

   procedure Split
     (Path        : String;
      FS          : out Any_Filesystem_Driver;
      Start_Index : out Natural)
   is
   begin
      if Path'Length >= 1 and then Path (Path'First) /= '/' then
         FS := null;
         Start_Index := 0;

         return;
      end if;

      Start_Index := Path'Last + 1;

      for J in Path'First + 1 .. Path'Last loop
         if Path (J) = '/' then
            Start_Index := J;

            exit;
         end if;
      end loop;

      for M of Mount_Points loop
         if not M.Is_Free
           and then Name (M) = Path (Path'First + 1 .. Start_Index - 1)
         then
            FS := M.FS;

            return;
         end if;
      end loop;

      FS := null;
      Start_Index := 0;
   end Split;

   ------------------
   -- Mount_Volume --
   ------------------

   function Mount_Volume
     (Mount_Point : Mount_Path;
      FS          : Any_Filesystem_Driver) return Status_Code
   is
      Idx : Natural := 0;
   begin
      for P in Mount_Points'Range loop
         if not Mount_Points (P).Is_Free
           and then Name (Mount_Points (P)) = Mount_Point
         then
            return Already_Exists;

         elsif Idx = 0 and then Mount_Points (P).Is_Free then
            Idx := P;
         end if;
      end loop;

      if Idx = 0 then
         return Too_Many_Open_Files;
      end if;

      Mount_Points (Idx).Is_Free := False;
      Mount_Points (Idx).FS := FS;
      Set_Name (Mount_Points (Idx), Mount_Point);

      return OK;
   end Mount_Volume;

   -----------------
   -- Mount_Drive --
   -----------------

   function Mount_Drive
     (Mount_Point : Mount_Path;
      Device      : HAL.Block_Drivers.Any_Block_Driver)
      return Status_Code
   is
      MBR    : Master_Boot_Record;
      Status : Status_Code;
      FAT_FS : FAT_Filesystem_Access;
   begin
      Status := Read (Device, MBR);

      if Status /= OK then
         return Status;
      end if;

      for P in Partition_Number'Range loop
         if Valid (MBR, P)
           and then Get_Type (MBR, P) in 6 | 11 .. 12
         then
            Status := OK;
            FAT_FS := new FAT_Filesystem;
            Status := Convert (Open (Controller => Device,
                                     LBA        => LBA (MBR, P),
                                     FS         => FAT_FS.all));
            return Mount_Volume (Mount_Point,
                                 HALFS.Any_Filesystem_Driver (FAT_FS));
         end if;
      end loop;

      return No_Filesystem;
   end Mount_Drive;

   -------------
   -- Unmount --
   -------------

   function Unmount (Mount_Point : Mount_Path) return Status_Code
   is
   begin
      for P in Mount_Points'Range loop
         if Name (Mount_Points (P)) = Mount_Point then
            Mount_Points (P).FS.Close;
            Mount_Points (P).Is_Free := True;

            return OK;
         end if;
      end loop;

      return Not_Mounted;
   end Unmount;

   ------------
   -- Get_FS --
   ------------

   overriding function Get_FS
     (Dir : VFS_Directory_Handle) return Any_Filesystem_Driver
   is
      pragma Unreferenced (Dir);
   begin
      return null;
   end Get_FS;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Dir    : in out VFS_Directory_Handle;
      Handle : out Any_Node_Handle)
      return HALFS.Status_Code
   is
   begin
      loop
         if Dir.Mount_Id = Mount_Points'Last then
            Handle := null;
            return No_More_Entries;
         end if;

         Dir.Mount_Id := Dir.Mount_Id + 1;

         if not Mount_Points (Dir.Mount_Id).Is_Free then
            return Mount_Points (Dir.Mount_Id).FS.Root_Node
              (Name (Mount_Points (Dir.Mount_Id)),
               Handle);
         end if;
      end loop;
   end Read;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Dir : in out VFS_Directory_Handle)
   is
   begin
      Dir.Mount_Id := 0;
   end Reset;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Dir : in out VFS_Directory_Handle)
   is
   begin
      Dir.Is_Free := True;
   end Close;

   ----------
   -- Open --
   ----------

   function Open
     (Path   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code
   is
      Idx : Natural;
      FS  : Any_Filesystem_Driver;
   begin
      Split (Path, FS, Idx);

      if FS = null then
         Handle := null;
         return No_Such_Path;
      end if;

      return Convert (FS.Open (Path (Idx .. Path'Last),
                      Convert (Mode),
                      Handle));
   end Open;

   ----------
   -- Open --
   ----------

   function Open
     (Path   : String;
      Handle : out Any_Directory_Handle)
      return Status_Code
   is
      Idx : Natural;
      FS  : Any_Filesystem_Driver;
   begin
      if Path = "/" then
         for J in Handles'Range loop
            if Handles (J).Is_Free then
               Handles (J).Is_Free := False;
               Handles (J).Mount_Id := 0;
               Handle := Handles (J)'Access;
               return OK;
            end if;
         end loop;

         Handle := null;
         return Too_Many_Open_Files;
      end if;

      Split (Path, FS, Idx);

      if FS = null then
         Handle := null;
         return No_Such_Path;
      end if;

      if Idx > Path'Last then
         return Convert (FS.Open ("/", Handle));
      else
         return Convert (FS.Open (Path (Idx .. Path'Last), Handle));
      end if;
   end Open;

end File_IO;
