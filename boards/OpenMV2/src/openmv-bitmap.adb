with HAL.Bitmap; use HAL.Bitmap;
with HAL; use HAL;
with OpenMV.LCD_Shield;

package body OpenMV.Bitmap is

   subtype Pixel_Data is UInt16_Array
     (0 .. (OpenMV.LCD_Shield.Width * OpenMV.LCD_Shield.Height) - 1);

   --------------
   -- Allocate --
   --------------

   function Allocate return HAL.Bitmap.Bitmap_Buffer'Class is
      BM : Bitmap_Buffer;
      Data : constant access Pixel_Data := new Pixel_Data;
   begin
      BM.Width := OpenMV.LCD_Shield.Width;
      BM.Height := OpenMV.LCD_Shield.Height;
      BM.Color_Mode := RGB_565;
      BM.Swapped := False;
      BM.Addr := Data.all'Address;
      return BM;
   end Allocate;
end OpenMV.Bitmap;
