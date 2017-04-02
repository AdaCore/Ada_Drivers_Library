with Test_Directories;      use Test_Directories;
with Ada.Text_IO;           use Ada.Text_IO;
with Native.Filesystem;     use Native.Filesystem;
with HAL;                   use HAL;
with HAL.Filesystem;        use HAL.Filesystem;
with HAL.Bitmap;            use HAL.Bitmap;
with Memory_Mapped_Bitmap;  use Memory_Mapped_Bitmap;
with Bitmap_File_Output;    use Bitmap_File_Output;
with Compare_Files;
with Filesystem;            use Filesystem;

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

   FS       : aliased Native_FS_Driver;
   BMP_File : File_Descriptor;
   Status   : Status_Kind;
   BM       : constant not null Any_Bitmap_Buffer := Allocate_Bitmap;

   Filename  : constant String := "test.bmp";
   Mount_Dir : constant String := "test_dir";
   Path      : constant String := "/" & Mount_Dir & "/" & Filename;
begin
   if FS.Create (Root_Dir => Test_Dir) /= Status_Ok then
      raise Program_Error with "Cannot create native file system at '" &
        Test_Dir & "'";
   end if;

   if Mount (Mount_Dir, FS'Unchecked_Access) /= Status_Ok then
      raise Program_Error with "Cannot mount native filesystem";
   end if;

   Status := Create_File (Path);

   if Status /= Status_Ok then
      raise Program_Error with "Cannot Create BMP file";
   end if;


   Status := Open (Path, Write_Only, BMP_File);

   if Status /= Status_Ok then
      raise Program_Error with "Cannot open BMP file";
   end if;

   BM.Fill (Black);
   BM.Fill_Rounded_Rect (Color  => Green,
                         Area   => ((5, 5), BM_Width / 2, BM_Height / 2),
                         Radius => 10);

   BM.Draw_Rounded_Rect (Color  => Red,
                         Area   => ((5, 5), BM_Width / 2, BM_Height / 2),
                         Radius => 10,
                         Thickness => 3);

   BM.Fill_Circle (Color  => Yellow,
                   Center => (BM_Width / 2, BM_Height / 2),
                   Radius => BM_Width / 4);

   BM.Draw_Circle (Color  => Blue,
                   Center => (BM_Width / 2, BM_Height / 2),
                   Radius => BM_Width / 4);

   BM.Cubic_Bezier (Color     => Violet,
                    P1        => (5, 5),
                    P2        => (0, BM_Height / 2),
                    P3        => (BM_Width / 2, BM_Height / 2),
                    P4        => (BM_Width - 5, BM_Height - 5),
                    N         => 100,
                    Thickness => 3);

   BM.Draw_Line (Color     => White,
                 Start     => (0, 0),
                 Stop      => (BM_Width - 1, BM_Height / 2),
                 Thickness => 1,
                 Fast      => True);

   BM.Set_Pixel ((0, 0), Red);
   BM.Set_Pixel ((0, 1), Green);
   BM.Set_Pixel ((0, 2), Blue);

   Copy_Rect (Src_Buffer  => BM.all,
              Src_Pt      => (0, 0),
              Dst_Buffer  => BM.all,
              Dst_Pt      => (0, BM_Height / 2 + 10),
              Width       => BM_Width / 4,
              Height      => BM_Height / 4,
              Synchronous => True);


   Write_BMP_File (BMP_File, BM.all);
   if Close (BMP_File) /= Status_Ok then
      raise Program_Error with "Cannot close BMP file";
   end if;

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
