separate (nRF.RNG)
function Read return UInt8 is
begin
   --  Clear event
   RNG_Periph.EVENTS_VALRDY := 0;

   --  Start random numnber generator
   RNG_Periph.TASKS_START := 1;

   while RNG_Periph.EVENTS_VALRDY = 0 loop
         null;
   end loop;

   return RNG_Periph.VALUE.VALUE;
end Read;
