with HAL.Bitmap;

package OpenMV.Sensor is
   procedure Initialize;
   function Initialized return Boolean;

   procedure Snapshot (BM : HAL.Bitmap.Bitmap_Buffer'Class);
end OpenMV.Sensor;
