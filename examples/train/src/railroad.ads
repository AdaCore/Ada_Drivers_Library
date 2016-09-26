
with HAL.Bitmap;

package Railroad is

   procedure Initialize;

   procedure Spawn_Train;

   procedure Step_Simulation;

   procedure Draw_Layout (Buffer : HAL.Bitmap.Bitmap_Buffer'Class;
                          Init   : Boolean := False);

   procedure Respond_To_Touch (X, Y : Natural);

end Railroad;
