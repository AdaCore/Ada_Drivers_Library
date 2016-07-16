with Ada.Exceptions;
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

   FS            : FAT_Filesystem_Access;

   Status        : FAT_Filesystem.Status_Code;

   Y             : Natural := 0;
   Dir, Sub      : Directory_Handle;
   E1, E2        : Directory_Entry;
   Path          : FAT_Path;

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

         FS := Open (SD_Controller'Unchecked_Access, -"sdcard", Status);

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

         Path := -"/";

         if not Error_State then
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               Volume_Label (FS.all) & " (" & File_System_Type (FS.all) & "):",
               BMP_Fonts.Font12x12,
               White,
               Transparent);
            Y := Y + 25;

            if Open (Path, Dir) /= OK then
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
            while Read (Dir, E1) = OK loop
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "/" & (-Name (E1)) & (if Is_Subdirectory (E1) then "/" else ""),
                  BMP_Fonts.Font12x12,
                  (if Is_Hidden (E1) then Gray else White),
                  Transparent);
               Y := Y + 16;

               if Is_Subdirectory (E1) then
                  Append (Path, Name (E1));

                  if not Change_Dir (Path)
                    or else Open (Current_Directory, Sub) /= OK
                  then
                     Error_State := True;

                     Draw_String
                       (Display.Get_Hidden_Buffer (1),
                        (0, Y),
                        "!!! Error reading " & (-Name (E1)),
                        BMP_Fonts.Font12x12,
                        HAL.Bitmap.Red,
                        Transparent);
                  end if;

                  if not Error_State then
                     while Read (Sub, E2) = OK loop
                        Draw_String
                          (Display.Get_Hidden_Buffer (1),
                           (0, Y),
                           "/" & (-Name (E1)) & "/" & (-Name (E2)) & (if Is_Subdirectory (E2) then "/" else ""),
                           BMP_Fonts.Font12x12,
                           (if Is_Hidden (E2) then Gray else White),
                           Transparent);
                        Y := Y + 16;
                     end loop;
                  end if;

                  Close (Sub);
                  To_Parent (Path);
                  Change_Dir (Path);
               end if;
            end loop;

            Close (Dir);
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

exception
   when E : others =>
      Display.Get_Hidden_Buffer (1).Fill (White);
      Draw_String
        (Display.Get_Hidden_Buffer (1),
         (0, 0),
         Ada.Exceptions.Exception_Information (E),
         BMP_Fonts.Font12x12,
         Black,
         White);
      Draw_String
        (Display.Get_Hidden_Buffer (1),
         (0, 14),
         Ada.Exceptions.Exception_Message (E),
         BMP_Fonts.Font12x12,
         Black,
         White);
      Display.Update_Layer (1);

      loop
         null;
      end loop;

end SDCard_Demo;
