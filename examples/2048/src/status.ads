
with Bitmapped_Drawing;    use Bitmapped_Drawing;
with HAL.Bitmap;           use HAL.Bitmap;

package Status is

   procedure Init_Area (Buffer : HAL.Bitmap.Bitmap_Buffer'Class);

   procedure Progress (Pct : Float);
   procedure Clear_Progress;

   procedure Set_Score
     (Score : Natural);

   function Has_Buttons return Boolean;

   procedure Set_Autoplay
     (State : Boolean);

   function Get_Autoplay_Btn_Area return Rect;

end Status;
