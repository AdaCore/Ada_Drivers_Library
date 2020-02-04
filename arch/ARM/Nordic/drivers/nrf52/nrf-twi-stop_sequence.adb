separate (nRF.TWI)
procedure Stop_Sequence (This : in out TWI_Master'Class) is
begin
   --  Stop sequence

   This.Periph.EVENTS_STOPPED.EVENTS_STOPPED := False;

   This.Periph.TASKS_STOP.TASKS_STOP := True;

   while This.Periph.EVENTS_STOPPED.EVENTS_STOPPED = False loop
      null;
   end loop;
end Stop_Sequence;
