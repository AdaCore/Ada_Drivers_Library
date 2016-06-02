with Ada.Unchecked_Conversion;
with Interfaces;                 use Interfaces;

with HAL.Bitmap;                 use HAL.Bitmap;
with HAL.Framebuffer;            use HAL.Framebuffer;
--  with HAL.Touch_Panel;            use HAL.Touch_Panel;
with Bitmapped_Drawing;          use Bitmapped_Drawing;

with STM32.SDMMC;                use STM32.SDMMC;
with STM32.Board;                use STM32.Board;

with BMP_Fonts;

with FAT_Filesystem;             use FAT_Filesystem;
with FAT_Filesystem.Directories; use FAT_Filesystem.Directories;
with Media_Reader.SDCard;        use Media_Reader.SDCard;

-----------------
-- SDCard_Demo --
-----------------

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
      Active     : Boolean;
      LBA_Base   : Unsigned_32;
      Num_Blocks : Unsigned_32;
      FS_Type    : Unsigned_8;
   end record;

   FS             : FAT_Filesystem_Access;

   P_Data         : Media_Reader.Block (0 .. 15);
   P_Idx          : Unsigned_16;
   Part           : Partition;

   Status         : FAT_Filesystem.Status_Code;
--     FAT            : FAT_Disk_Parameter;
--     FSInfo         : FAT_FS_Info;

   Y              : Natural := 0;
   D              : Directory_Handle;
   E              : Directory_Entry;

begin
   SD_Controller.Initialize;
   Display.Initialize (Landscape, Interrupt);
   Display.Initialize_Layer (1, RGB_565);

   loop
      if not SD_Controller.Card_Present then
         Display.Get_Hidden_Buffer (1).Fill (Black);
         Draw_String
           (Display.Get_Hidden_Buffer (1),
            (0, 0),
            "No SD-Card detected",
            BMP_Fonts.Font12x12,
            Red,
            Transparent);
         Display.Update_Layer (1);

         loop
            if Card_Present (SD_Controller) then
               exit;
            end if;
         end loop;

      else
         Display.Get_Hidden_Buffer (1).Fill (Black);
         Y := 0;
         Error_State := False;

         SD_Card_Info := SD_Controller.Get_Card_Information;

         --  Dump general info about the SD-card
         Capacity := SD_Card_Info.Card_Capacity;

         for Unit of Units loop
            if Capacity < 1000 or else Unit = 'T' then
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "SDcard size:" & Capacity'Img & " " & Unit & "B",
                  BMP_Fonts.Font12x12,
                  White,
                  Transparent);
               Y := Y + 13;

               exit;
            end if;

            if Capacity mod 1000 >= 500 then
               Capacity := Capacity / 1000 + 1;
            else
               Capacity := Capacity / 1000;
            end if;
         end loop;

         declare
            MBR_Data : Media_Reader.Block (0 .. 511);
         begin
            if not SD_Controller.Read_Block (0, MBR_Data) then
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "Cannot open the MBR block",
                  BMP_Fonts.Font12x12,
                  Red,
                  Transparent);
               Y := Y + 13;
               Error_State := True;

            elsif MBR_Data (510 .. 511) /= (16#55#, 16#AA#) then
               --  MBR magic boot number not found
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "Not an MBR partition system",
                  BMP_Fonts.Font12x12,
                  Red,
                  Transparent);
               Y := Y + 13;
               Error_State := True;

            else
               --  Now check the partition entries: 4 partitions for the MBR
               for P in 1 .. 4 loop
                  --  Partitions are defined as an array of 16 bytes from
                  --  base MBR + 446 (16#1BE#)
                  P_Idx  := 446 + Unsigned_16 (P - 1) * 16;
                  P_Data := MBR_Data (P_Idx .. P_Idx + 15);

                  Part.Num_Blocks := To_Word (P_Data (12 .. 15));
                  if Part.Num_Blocks > 0 then
                     --  Bit 7 of the first byte tells if the partition is a
                     --  boot partition
                     Part.Active     := P_Data (0) = 16#80#;
                     Part.LBA_Base   := To_Word (P_Data (8 .. 11));
                     Part.FS_Type    := P_Data (4);

                     exit;
                  elsif P = 4 then
                     Draw_String
                       (Display.Get_Hidden_Buffer (1),
                        (0, Y),
                        "No valid partition found",
                        BMP_Fonts.Font12x12,
                        Red,
                        Transparent);
                     Y := Y + 13;
                     Error_State := True;
                  end if;

               end loop;
            end if;
         end;

         if not Error_State then
            FS := Open (SD_Controller'Unchecked_Access,
                        Part.LBA_Base,
                        Status);

            if Status /= OK then
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "!!! Error reading the FAT partition",
                  BMP_Fonts.Font12x12,
                  Red,
                  Transparent);
               Y := Y + 13;
               Error_State := True;

            else
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  FS.Volume_Label & " (" & FS.File_System_Type & "):",
                  BMP_Fonts.Font12x12,
                  White,
                  Transparent);
               Y := Y + 25;

            end if;
         end if;

         if not Error_State then
            if Open_Root_Directory (FS, D) /= OK then
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "!!! Error reading the root directory",
                  BMP_Fonts.Font12x12,
                  Red,
                  Transparent);
               Close (FS);
               Y := Y + 13;
               Error_State := True;
            end if;
         end if;

         if not Error_State then
            while Read (D, E) = OK loop
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  Name (E) & (if Is_Subdirectory (E) then "/" else ""),
                  BMP_Fonts.Font12x12,
                  (if Is_Hidden (E) then Gray else White),
                  Transparent);
               Y := Y + 16;
            end loop;

            Close (D);
            Close (FS);
         end if;

         Display.Update_Layer (1);

         loop
            if not Card_Present (SD_Controller) then
               exit;
            end if;
         end loop;
      end if;
   end loop;
end SDCard_Demo;
