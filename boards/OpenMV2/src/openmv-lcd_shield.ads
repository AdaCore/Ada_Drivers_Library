with HAL.Bitmap;

package OpenMV.LCD_Shield is
   Width  : constant := 128;
   Height : constant := 160;

   procedure Initialize;
   function Initialized return Boolean;

   function Get_Bitmap return HAL.Bitmap.Bitmap_Buffer'Class;

   procedure Rotate_Screen_90
     with Pre => Initialized;
   procedure Rotate_Screen_0
     with Pre => Initialized;
   procedure Display
     with Pre => Initialized;

end OpenMV.LCD_Shield;
