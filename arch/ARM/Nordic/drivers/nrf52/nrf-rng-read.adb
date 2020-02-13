separate (nRF.RNG)
function Read return UInt8 is
begin
   --  Clear event
   RNG_Periph.EVENTS_VALRDY.EVENTS_VALRDY := False;

   --  Start random numnber generator
   RNG_Periph.TASKS_START.TASKS_START := True;

   while RNG_Periph.EVENTS_VALRDY.EVENTS_VALRDY = False loop
         null;
   end loop;

   return RNG_Periph.VALUE.VALUE;
end Read;
