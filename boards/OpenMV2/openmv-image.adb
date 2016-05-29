with Interfaces; use Interfaces;

package body OpenMV.Image is

   function To_Short (C : Color) return Short;

   --------------
   -- To_Short --
   --------------

   function To_Short (C : Color) return Short is
      R, G, B : Short;
   begin
      B := Shift_Left (Short (C.B) and 16#1F#, 11);
      R := Shift_Left (Short (C.R) and 16#3F#, 5);
      G := Short (C.G) and 16#1F#;
      return R or G or B;
   end To_Short;

   ----------
   -- Fill --
   ----------

   procedure Fill (C : Color) is
      Raw : constant Short := To_Short (C);
   begin
      for Elt of FB.Data.all loop
         Elt := Raw;
      end loop;
   end Fill;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X : Width; Y : Height; C : Color) is
   begin
      FB.Data (X + Y * Image_Width) := To_Short (C);
   end Set_Pixel;

end OpenMV.Image;
