with HAL.Bitmap;

package OpenMV.LCD_Shield is
   Width  : constant := 128;
   Height : constant := 160;

   procedure Initialize;
   function Initialized return Boolean;

   function Get_Bitmap return HAL.Bitmap.Bitmap_Buffer'Class;

   procedure Display;
end OpenMV.LCD_Shield;
