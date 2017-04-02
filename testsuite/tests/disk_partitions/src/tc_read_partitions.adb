with Ada.Text_IO;           use Ada.Text_IO;
with Native.Filesystem;     use Native.Filesystem;
with HAL.Filesystem;        use HAL.Filesystem;
with Partitions;            use Partitions;
with File_Block_Drivers;    use File_Block_Drivers;
with Test_Directories;      use Test_Directories;
with Pathname_Manipulation; use Pathname_Manipulation;
procedure TC_Read_Partitions is

   procedure List_Partitions (FS : in out FS_Driver'Class;
                              Path_To_Disk_Image : Pathname);

   ---------------------
   -- List_Partitions --
   ---------------------

   procedure List_Partitions (FS : in out FS_Driver'Class;
                              Path_To_Disk_Image : Pathname)
   is
      File : Any_File_Handle;
   begin
      if FS.Open (Path_To_Disk_Image,
                  Parse (Path_To_Disk_Image),
                  Read_Only,
                  File) /= Status_Ok
      then
         Put_Line ("Cannot open disk image '" & Path_To_Disk_Image & "'");
         return;
      end if;

      declare
         Disk    : aliased File_Block_Driver (File);
         Nbr     : Natural;
         P_Entry : Partition_Entry;
      begin
         Nbr := Number_Of_Partitions (Disk'Unchecked_Access);
         Put_Line ("Disk '" & Path_To_Disk_Image & "' has " &
                     Nbr'Img & " parition(s)");

         for Id in 1 .. Nbr loop
            if Get_Partition_Entry (Disk'Unchecked_Access,
                                    Id,
                                    P_Entry) /= Status_Ok
            then
               Put_Line ("Cannot read partition :" & Id'Img);
            else
               Put_Line (" - partition :" & Id'Img);
               Put_Line ("      Status:" & P_Entry.Status'Img);
               Put_Line ("      Kind: " & P_Entry.Kind'Img);
               Put_Line ("      LBA: " & P_Entry.First_Sector_LBA'Img);
               Put_Line ("      Number of sectors: " & P_Entry.Number_Of_Sectors'Img);
            end if;
         end loop;
      end;
   end List_Partitions;

   FS   : aliased Native_FS_Driver;
begin
   if FS.Create (Root_Dir => Test_Dir) /= Status_Ok then
      raise Program_Error with "Cannot create native file system at '" &
        Test_Dir & "'";
   end if;

   List_Partitions (FS, "disk_8_partitions.img");
end TC_Read_Partitions;
