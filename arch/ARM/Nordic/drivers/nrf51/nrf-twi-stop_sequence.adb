separate (nRF.TWI)
procedure Stop_Sequence (This : in out TWI_Master'Class) is
begin
   --  Stop sequence

   This.Periph.EVENTS_STOPPED := 0;

   This.Periph.TASKS_STOP := 1;

   while This.Periph.EVENTS_STOPPED = 0 loop
      null;
   end loop;
end Stop_Sequence;
