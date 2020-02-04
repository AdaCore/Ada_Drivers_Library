separate(nRF.Timers)
procedure Stop (This : in out Timer) is
begin
   This.Periph.TASKS_STOP.TASKS_STOP := True;
end Stop;
