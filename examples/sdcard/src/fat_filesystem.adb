with Ada.Unchecked_Conversion;

package body FAT_Filesystem is

   --  ??? Should use a memory pool here to store the global mounted volumes
   --  but hey, OK as long as it remains a simple demo here.
   Volume_FAT16 : aliased FAT16_Volume;
   Volume_FAT32 : aliased FAT32_Volume;

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
     (Controller : Media_Controller_Access;
      LBA        : Unsigned_32;
      Status     : out Status_Code) return FAT_Volume_Access
   is
      Sector : Block (0 .. 511);

      subtype Disk_Parameter_Block is Block (0 .. 91);
      function To_Disk_Parameter is new Ada.Unchecked_Conversion
        (Disk_Parameter_Block, FAT_Disk_Parameter);

      subtype FSInfo_Block is Block (0 .. 11);
      function To_FSInfo is new Ada.Unchecked_Conversion
        (FSInfo_Block, FAT_FS_Info);

   begin
      if not Controller.Read_Block (LBA, Sector) then
         Status := Media_Error;
         return Null_FAT_Volume;
      end if;

      Status := OK;

      if Sector (510 .. 511) /= (16#55#, 16#AA#) then
         Status := FAT_Error;
         return Null_FAT_Volume;
      end if;

      --  If byte 16#13# is not 0 then we assume a FAT16 filesystem, else we
      --  have a FAT32 FS
      if Sector (16#13#) /= 0 then
         Volume_FAT16.Controller := Controller;
         Volume_FAT16.Disk_Parameters :=
           To_Disk_Parameter (Sector (0 .. 91));
         Volume_FAT16.LBA := LBA;

         return Volume_FAT16'Access;

      else
         Volume_FAT32.Controller := Controller;
         Volume_FAT32.Disk_Parameters :=
           To_Disk_Parameter (Sector (0 .. 91));
         Volume_FAT32.LBA := LBA;

         if not Controller.Read_Block
           (LBA + Unsigned_32 (Volume_FAT32.FSInfo_Block_Number),
            Sector)
         then
            Status := Media_Error;
            return Null_FAT_Volume;
         end if;

         --  Check the generic FAT block signature
         if Sector (510 .. 511) /= (16#55#, 16#AA#) then
            Status := FAT_Error;
            return Null_FAT_Volume;
         end if;

         Volume_FAT32.FSInfo := To_FSInfo (Sector (16#1E4# .. 16#1EF#));

         return Volume_FAT32'Access;
      end if;
   end Open;

   -------------------------
   -- Open_Root_Directory --
   -------------------------

   function Open_Root_Directory
     (Volume : FAT_Volume_Access) return Directory_Handle
   is
      D : Directory_Handle;
   begin
      return D;
   end Open_Root_Directory;

end FAT_Filesystem;
