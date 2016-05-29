with OpenMV;
with OpenMV.LCD_Shield;
with OpenMV.Sensor;

with Last_Chance_Handler;
pragma Unreferenced (Last_Chance_Handler);

procedure Main is
begin
   OpenMV.Initialize_LEDs;
   OpenMV.Set_RGB_LED (OpenMV.Off);
   OpenMV.LCD_Shield.Initialize;
   OpenMV.Sensor.Initialize;
   loop
      --  Take a snapshot...
      OpenMV.Sensor.Snapshot;

      --  ...and display it.
      OpenMV.LCD_Shield.Display;
   end loop;
end Main;
