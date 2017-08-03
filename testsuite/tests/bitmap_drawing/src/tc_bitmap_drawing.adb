with Test_Directories;     use Test_Directories;
with Ada.Text_IO;          use Ada.Text_IO;
with HAL;                  use HAL;
with HAL.Bitmap;           use HAL.Bitmap;
with Memory_Mapped_Bitmap; use Memory_Mapped_Bitmap;
with Bitmap_File_Output;   use Bitmap_File_Output;
with File_IO;              use File_IO;
with Compare_Files;

procedure TC_Bitmap_Drawing is

   BM_Width  : constant := 100;
   BM_Height : constant := 100;

   function Allocate_Bitmap return not null Any_Bitmap_Buffer;

   ---------------------
   -- Allocate_Bitmap --
   ---------------------

   function Allocate_Bitmap return not null Any_Bitmap_Buffer is
      type Pixel_Data is new HAL.UInt16_Array (1 .. BM_Height * BM_Height) with Pack;
      BM : constant Any_Memory_Mapped_Bitmap_Buffer := new Memory_Mapped_Bitmap_Buffer;
      Data : constant access Pixel_Data := new Pixel_Data;
   begin
      BM.Actual_Width := BM_Width;
      BM.Actual_Height := BM_Height;
      BM.Actual_Color_Mode := RGB_565;
      BM.Currently_Swapped := False;
      BM.Addr := Data.all'Address;
      return Any_Bitmap_Buffer (BM);
   end Allocate_Bitmap;

   BMP_File : File_Descriptor;
   Status   : Status_Code;
   BM       : constant not null Any_Bitmap_Buffer := Allocate_Bitmap;

   Filename : constant String := "/test.bmp";

   Filepath : constant String := "/" & Test_Dir_Mount_Name & Filename;
   --  Path of the file in the mounted directory
begin

   Test_Directories.Mount_Test_Directory;

   Status := Create_File (Filepath);

   if Status /= OK then
      raise Program_Error with "Cannot Create BMP file";
   end if;

   Status := Open (BMP_File, Filepath, Read_Write);

   if Status /= OK then
      raise Program_Error with "Cannot Open BMP file";
   end if;

   BM.Set_Source (Black);
   BM.Fill;

   BM.Set_Source (Green);
   BM.Fill_Rounded_Rect (Area   => ((5, 5), BM_Width / 2, BM_Height / 2),
                         Radius => 10);

   BM.Set_Source (Red);
   BM.Draw_Rounded_Rect (Area   => ((5, 5), BM_Width / 2, BM_Height / 2),
                         Radius => 10,
                         Thickness => 3);

   BM.Set_Source (Yellow);
   BM.Fill_Circle (Center => (BM_Width / 2, BM_Height / 2),
                   Radius => BM_Width / 4);

   BM.Set_Source (Blue);
   BM.Draw_Circle (Center => (BM_Width / 2, BM_Height / 2),
                   Radius => BM_Width / 4);

   BM.Set_Source (Violet);
   BM.Cubic_Bezier (P1        => (5, 5),
                    P2        => (0, BM_Height / 2),
                    P3        => (BM_Width / 2, BM_Height / 2),
                    P4        => (BM_Width - 5, BM_Height - 5),
                    N         => 100,
                    Thickness => 3);

   BM.Set_Source (White);
   BM.Draw_Line (Start     => (0, 0),
                 Stop      => (BM_Width - 1, BM_Height / 2),
                 Thickness => 1,
                 Fast      => True);

   BM.Set_Source (Red);
   BM.Set_Pixel ((0, 0));
   BM.Set_Source (Green);
   BM.Set_Pixel ((0, 1));
   BM.Set_Source (Blue);
   BM.Set_Pixel ((0, 2));

   Copy_Rect (Src_Buffer  => BM.all,
              Src_Pt      => (0, 0),
              Dst_Buffer  => BM.all,
              Dst_Pt      => (0, BM_Height / 2 + 10),
              Width       => BM_Width / 4,
              Height      => BM_Height / 4,
              Synchronous => True);


   Write_BMP_File (BMP_File, BM.all);
   Close (BMP_File);

   if not Compare_Files.Binnary_Equal (Test_Dir & "/" & Filename,
                                       Test_Dir & "/ref.bmp")
   then
      Put_Line ("Bitmap drawing test FAILED.");
      Put_Line ("Output BMP is different than the reference.");
      Put_Line ("This could mean that:");
      Put_Line (" 1 - Bitmap drawing is broken");
      Put_Line (" 2 - Bitmap file output is broken");
      Put_Line (" 3 - You changed/improved bitmap drawing");
      New_Line;
      Put_Line ("When 1 or 2, please fix the problem,");
      Put_Line ("When 3 please update the reference bitmap and exaplaining" &
                  " the changes you made.");
   else
      Put_Line ("Bitmap drawing test OK");
   end if;
end TC_Bitmap_Drawing;
