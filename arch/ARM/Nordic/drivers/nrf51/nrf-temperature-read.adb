with HAL;

separate (nRF.Temperature)
function Read return Temp_Celsius is
   use type HAL.UInt32;

   Raw : RAW_Temp;

begin

   --  Clear event
   TEMP_Periph.EVENTS_DATARDY := 0;

   --  Start temperature measurement
   TEMP_Periph.TASKS_START := 1;

   while TEMP_Periph.EVENTS_DATARDY = 0 loop
      null;
   end loop;

   Raw := RAW_Temp (TEMP_Periph.TEMP);
   return Temp_Celsius (Raw / 4);
end Read;
