separate (nRF.Timers)
procedure Stop (This : in out Timer) is
begin
   This.Periph.TASKS_STOP := 1;
end Stop;
