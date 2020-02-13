separate (nRF.Timers)
procedure Start (This : in out Timer) is
begin
   This.Periph.TASKS_START := 1;
end Start;
