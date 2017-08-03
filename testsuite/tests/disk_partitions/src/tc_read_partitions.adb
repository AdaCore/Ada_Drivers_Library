with Ada.Text_IO;        use Ada.Text_IO;
with Partitions;         use Partitions;
with File_Block_Drivers; use File_Block_Drivers;
with Test_Directories;   use Test_Directories;
with File_IO;

procedure TC_Read_Partitions is

   use type File_IO.Status_Code;

   procedure List_Partitions (Path_To_Disk_Image : String);

   ---------------------
   -- List_Partitions --
   ---------------------

   procedure List_Partitions (Path_To_Disk_Image : String)
   is
      Disk    : aliased File_Block_Driver;
      Nbr     : Natural;
      P_Entry : Partition_Entry;
      Status  : File_IO.Status_Code;
   begin
      Status := Disk.Open (Path_To_Disk_Image, File_IO.Read_Only);
      if Status /= File_IO.OK then
         Put_Line ("Cannot open disk image '" & Path_To_Disk_Image & "'");
         return;
      end if;

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
   end List_Partitions;

begin
   Mount_Test_Directory;

   List_Partitions ("/" & Test_Dir_Mount_Name & "/disk_8_partitions.img");
end TC_Read_Partitions;
