with Ada.Unchecked_Conversion;
with Interfaces;                 use Interfaces;

with HAL.Bitmap;                 use HAL.Bitmap;
with HAL.Framebuffer;            use HAL.Framebuffer;
--  with HAL.Touch_Panel;            use HAL.Touch_Panel;
with Bitmapped_Drawing;          use Bitmapped_Drawing;

with STM32.SDMMC;                use STM32.SDMMC;
with STM32.Board;                use STM32.Board;

with BMP_Fonts;

with FAT_Filesystem;                   use FAT_Filesystem;
with FAT_Filesystem.Directories;       use FAT_Filesystem.Directories;
with FAT_Filesystem.Directories.Files; use FAT_Filesystem.Directories.Files;
with Media_Reader.SDCard;              use Media_Reader.SDCard;

-----------------
-- SDCard_Demo --
-----------------

procedure SDCard_Demo with SPARK_Mode => Off
is
   SD_Controller : aliased SDCard_Controller;
   SD_Card_Info  : Card_Information;

   Units         : constant array (Natural range <>) of Character :=
                     (' ', 'k', 'M', 'G', 'T');
   Capacity      : Unsigned_64;
   Error_State   : Boolean := False;

   FS             : FAT_Filesystem_Access;

   Status         : FAT_Filesystem.Status_Code;

   Y              : Natural := 0;
   Dir            : Directory_Handle;
   Ent            : Directory_Entry;

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
            HAL.Bitmap.Red,
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
               Display.Update_Layer (1, True);
               Y := Y + 13;

               exit;
            end if;

            if Capacity mod 1000 >= 500 then
               Capacity := Capacity / 1000 + 1;
            else
               Capacity := Capacity / 1000;
            end if;
         end loop;

         FS := Open (SD_Controller'Unchecked_Access, Status);

         if Status /= OK then
            Error_State := True;

            if Status = No_MBR_Found then
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "Not an MBR partition system",
                  BMP_Fonts.Font12x12,
                  HAL.Bitmap.Red,
                  Transparent);
               Display.Update_Layer (1, True);
               Y := Y + 13;

            elsif Status = No_Partition_Found then
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "No valid partition found",
                  BMP_Fonts.Font12x12,
                  HAL.Bitmap.Red,
                  Transparent);
               Display.Update_Layer (1, True);
               Y := Y + 13;

            else
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "Error reading the card: " & Status'Img,
                  BMP_Fonts.Font12x12,
                  HAL.Bitmap.Red,
                  Transparent);
               Display.Update_Layer (1, True);
               Y := Y + 13;
            end if;
         end if;

         if not Error_State then
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               Volume_Label (FS.all) & " (" & File_System_Type (FS.all) & "):",
               BMP_Fonts.Font12x12,
               White,
               Transparent);
            Y := Y + 25;

            if Open_Root_Directory (FS, Dir) /= OK then
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "!!! Error reading the root directory",
                  BMP_Fonts.Font12x12,
                  HAL.Bitmap.Red,
                  Transparent);
               Display.Update_Layer (1, True);
               Close (FS);
               Y := Y + 13;
               Error_State := True;
            end if;
         end if;

         if not Error_State then
            while Read (Dir, Ent) = OK loop
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  Get_Name (Ent) & (if Is_Subdirectory (Ent) then "/" else ""),
                  BMP_Fonts.Font12x12,
                  (if Is_Hidden (Ent) then Gray else White),
                  Transparent);
               Y := Y + 16;
            end loop;

            Close (Dir);
            Close (FS);
         end if;

         --  demo: create a file with some text in the root directory
         if not Error_State and then Open_Root_Directory (FS, Dir) = OK then
            declare
               fh   : File_Handle;
               n_wr : Integer;
               ret  : Status_Code;
            begin
               if File_Create (Parent => Dir,
                               newname => "test.txt",
                               Overwrite => True,
                               File => fh) = OK
               then
                  n_wr := File_Write (File => fh,
                                      Data => To_File_Data ("adafatfs was here!"),
                                      Status => ret);
                  pragma Unreferenced (n_wr);
                  File_Close (fh);
               end if;
            end;
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
