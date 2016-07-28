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

   procedure Display_Current_Dir;

   -------------------------
   -- Display_Current_Dir --
   -------------------------

   procedure Display_Current_Dir
   is
      Dir : Directory_Handle;
      E   : Directory_Entry;
   begin
      if Open (Current_Directory, Dir) /= OK then
         Draw_String
           (Display.Get_Hidden_Buffer (1),
            (0, Y),
            "!!! Error reading the directory " & (-Current_Directory),
            BMP_Fonts.Font12x12,
            HAL.Bitmap.Red,
            Transparent);
         Display.Update_Layer (1, True);
         Close (FS);
         Y := Y + 13;
         Error_State := True;
      end if;

      while not Error_State and then Read (Dir, E) = OK loop
         if not Is_Hidden (E) then
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               -Current_Directory & (-Name (E)),
               BMP_Fonts.Font12x12,
               (if Is_Subdirectory (E) then Grey else Black),
               Transparent);
            Y := Y + 16;

            if Is_Subdirectory (E)
              and then -Name (E) /= "."
              and then -Name (E) /= ".."
            then
               Change_Dir (Name (E));
               Display_Current_Dir;
               Change_Dir (FAT_Name'(-".."));
            end if;
         end if;
      end loop;

      Close (Dir);
   end Display_Current_Dir;

begin
   SD_Controller.Initialize;
   Display.Initialize (Landscape, Interrupt);
   Display.Initialize_Layer (1, ARGB_1555);
   Display.Set_Background (255, 255, 255);

   loop
      if not SD_Controller.Card_Present then
         Display.Get_Hidden_Buffer (1).Fill (Transparent);
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
         Display.Get_Hidden_Buffer (1).Fill (Transparent);
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
                  Dark_Red,
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

         else
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               Volume_Label (FS.all) & " (" & File_System_Type (FS.all) & "):",
               BMP_Fonts.Font12x12,
               Dark_Red,
               Transparent);
            Y := Y + 25;

            Change_Dir (FAT_Path'(-"/"));
            Display_Current_Dir;
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
