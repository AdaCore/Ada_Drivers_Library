separate (nRF.Temperature)
function Read return Temp_Celsius is
   Raw : RAW_Temp;
begin

   --  Clear event
   TEMP_Periph.EVENTS_DATARDY.EVENTS_DATARDY := False;

   --  Start temperature measurement
   TEMP_Periph.TASKS_START.TASKS_START := True;

   while TEMP_Periph.EVENTS_DATARDY.EVENTS_DATARDY = False loop
      null;
   end loop;

   Raw := RAW_Temp (TEMP_Periph.TEMP);
   return Temp_Celsius (Raw / 4);
end Read;
