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

with Filesystem.MBR; use Filesystem.MBR;
with Filesystem.FAT; use Filesystem.FAT;

package body Filesystem.VFS is

   Mount_Points : Mount_Array;

   Handles : array (1 .. 2) of aliased VFS_Directory_Handle;

   function Name (Point : Mount_Record) return Mount_Path;
   procedure Set_Name (Point : in out Mount_Record;
                       Path  : Mount_Path);
   procedure Split
     (Path        : String;
      FS          : out Any_Filesystem;
      Start_Index : out Natural);

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
      FS          : out Any_Filesystem;
      Start_Index : out Natural)
   is
   begin
      if Path (Path'First) /= '/' then
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
      FS          : Any_Filesystem) return Status_Code
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
      FAT_FS : access FAT_Filesystem;
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
            Status := Open (Controller => Device,
                            LBA        => LBA (MBR, P),
                            FS         => FAT_FS.all);
            return Mount_Volume (Mount_Point, FAT_FS);
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

   ----------
   -- Open --
   ----------

   function Open
     (Path   : String;
      Handle : out Any_Directory_Handle)
      return Status_Code
   is
      Idx : Natural;
      FS  : Any_Filesystem;
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
         return FS.Open ("/", Handle);
      else
         return FS.Open (Path (Idx .. Path'Last), Handle);
      end if;
   end Open;

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
      FS  : Any_Filesystem;
   begin
      Split (Path, FS, Idx);

      if FS = null then
         Handle := null;
         return No_Such_Path;
      end if;

      return FS.Open (Path (Idx .. Path'Last), Mode, Handle);
   end Open;

   ------------
   -- Unlink --
   ------------

   function Unlink (Path : String) return Status_Code is
      Idx : Natural;
      FS  : Any_Filesystem;
   begin
      Split (Path, FS, Idx);

      if FS = null then
         return No_Such_Path;
      end if;

      return FS.Unlink (Path (Idx .. Path'Last));
   end Unlink;

   ----------------------
   -- Remove_Directory --
   ----------------------

   function Remove_Directory (Path : String) return Status_Code is
      Idx : Natural;
      FS  : Any_Filesystem;
   begin
      Split (Path, FS, Idx);

      if FS = null then
         return No_Such_Path;
      end if;

      return FS.Remove_Directory (Path (Idx .. Path'Last));
   end Remove_Directory;

   ------------
   -- Get_FS --
   ------------

   overriding function Get_FS
     (Dir : VFS_Directory_Handle) return Any_Filesystem
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
      Handle : out Any_Node_Handle) return Status_Code
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

end Filesystem.VFS;
