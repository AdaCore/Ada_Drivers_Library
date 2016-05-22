with Ada.Unchecked_Conversion;
with Interfaces;      use Interfaces;
with STM32.SDMMC;     use STM32.SDMMC;

with BMP_Fonts;
with LCD_Std_Out;         use LCD_Std_Out;

with FAT_Filesystem;      use FAT_Filesystem;
with Media_Reader.SDCard; use Media_Reader.SDCard;

procedure SDCard_Demo
is
   SD_Controller : aliased SDCard_Controller;
   SD_Card_Info  : Card_Information;

   Units         : constant array (Natural range <>) of Character :=
                     (' ', 'k', 'M', 'G', 'T');
   Capacity      : Unsigned_64;
   Error_State   : Boolean := False;

   --  Partitions reading from the MBR

   subtype Word_Data is Media_Reader.Block (0 .. 3);
   function To_Word is new Ada.Unchecked_Conversion (Word_Data, Unsigned_32);
   use type Media_Reader.Block;

   type Partition is record
      Available  : Boolean;
      Active     : Boolean;
      LBA_Base   : Unsigned_32;
      Num_Blocks : Unsigned_32;
      FS_Type    : Unsigned_8;
   end record;

   P_Data         : Media_Reader.Block (0 .. 15);
   P_Idx          : Unsigned_16;
   Partitions     : array (1 .. 4) of Partition;

   Status         : FAT_Filesystem.Status_Code;
--     FAT            : FAT_Disk_Parameter;
--     FSInfo         : FAT_FS_Info;

begin
   LCD_Std_Out.Set_Font (BMP_Fonts.Font12x12);
   SD_Controller.Initialize;

   loop
      if not SD_Controller.Card_Present then
         LCD_Std_Out.Clear_Screen;
         LCD_Std_Out.Put_Line ("No SD-Card detected");

         loop
            if Card_Present (SD_Controller) then
               exit;
            end if;
         end loop;

      else
         LCD_Std_Out.Clear_Screen;
         Error_State := False;

         SD_Card_Info := SD_Controller.Get_Card_Information;

         --  Dump general info about the SD-card
         Capacity := SD_Card_Info.Card_Capacity;

         for Unit of Units loop
            if Capacity < 1000 or else Unit = 'T' then
               LCD_Std_Out.Put_Line
                 ("Card size:   " & Capacity'Img & " " & Unit & "B");
               exit;
            end if;

            if Capacity mod 1000 >= 500 then
               Capacity := Capacity / 1000 + 1;
            else
               Capacity := Capacity / 1000;
            end if;
         end loop;

         LCD_Std_Out.Put_Line ("Name:         " &
                                 SD_Card_Info.SD_CID.Product_Name);
         LCD_Std_Out.Put_Line
           ("Manufactured: " &
              SD_Card_Info.SD_CID.Manufacturing_Date.Month'Img &
              SD_Card_Info.SD_CID.Manufacturing_Date.Year'Img);

         declare
            MBR_Data : Media_Reader.Block (0 .. 511);
         begin
            if not SD_Controller.Read_Block (0, MBR_Data) then
               LCD_Std_Out.Put_Line ("Cannot open the MBR block");
               Error_State := True;

            elsif MBR_Data (510 .. 511) /= (16#55#, 16#AA#) then
               --  MBR magic boot number not found
               LCD_Std_Out.Put_Line ("Not an MBR partition system");
               Error_State := True;

            else
               --  Now check the partition entries: 4 partitions for the MBR
               for P in Partitions'Range loop
                  --  Partitions are defined as an array of 16 bytes from
                  --  base MBR + 446 (16#1BE#)
                  P_Idx  := 446 + Unsigned_16 (P - 1) * 16;
                  P_Data := MBR_Data (P_Idx .. P_Idx + 15);

                  --  Bit 7 of the first byte tells if the partition is a
                  --  boot partition
                  Partitions (P).Active     := P_Data (0) = 16#80#;
                  Partitions (P).LBA_Base   := To_Word (P_Data (8 .. 11));
                  Partitions (P).Num_Blocks := To_Word (P_Data (12 .. 15));
                  Partitions (P).Available  := Partitions (P).Num_Blocks > 0;
                  Partitions (P).FS_Type    := P_Data (4);
               end loop;
            end if;
         end;

         if not Error_State then
            for P in Partitions'Range loop
               if Partitions (P).Available then
                  LCD_Std_Out.Put
                    ("Partition" & P'Img &
                     (if Partitions (P).Active then " (Active): " else ": "));

                  declare
                     Volume : constant FAT_Volume_Access :=
                                Open
                                  (SD_Controller'Unchecked_Access,
                                   Partitions (P).LBA_Base,
                                   Status);
                  begin
                     if Status /= OK then
                        LCD_Std_Out.New_Line;
                        LCD_Std_Out.Put_Line
                          ("!!! Error reading the FAT partition");
                        Error_State := True;

                     else
                        LCD_Std_Out.Put_Line
                          (Volume.Volume_Label & " (" &
                             Volume.File_System_Type & ")");

                        declare
                           D : constant Directory_Handle :=
                                 Open_Root_Directory (Volume);
                           pragma Unreferenced (D);
                        begin
                           null;
                        end;
                     end if;

                     Close (Volume);
                  end;
               end if;
            end loop;
         end if;

         loop
            if not Card_Present (SD_Controller) then
               exit;
            end if;
         end loop;
      end if;
   end loop;
end SDCard_Demo;
