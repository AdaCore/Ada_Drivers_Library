with Bitmapped_Drawing;
with Screen_Interface;

package Drawing is new Bitmapped_Drawing
  (Color     => Screen_Interface.Color,
   Set_Pixel => Screen_Interface.Set_Pixel);
