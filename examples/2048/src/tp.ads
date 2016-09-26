
with Bitmapped_Drawing; use Bitmapped_Drawing;

package TP is

   Max_Buttons : constant := 2;
   --  The maximum number of buttons handled by this packge

   type Touch_Vector is record
      Start_Point : Point;
      End_Point   : Point;
   end record;

   procedure Update;

   type Button_Callback is access procedure (X, Y : Natural);
   --  Called when a Button area has been clicked.
   --  Coord contains the click position relative to the button area

   procedure Add_Button_Area
     (Area     : Rect;
      Callback : Button_Callback);

   type Slide_Callback is access procedure (Vector : Touch_Vector);

   procedure Set_Slide_Callback
     (Callback : Slide_Callback);

end TP;
