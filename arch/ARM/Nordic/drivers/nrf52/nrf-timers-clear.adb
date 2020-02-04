separate(nRF.Timers)
procedure Clear (This : in out Timer) is
begin
   This.Periph.TASKS_CLEAR.TASKS_CLEAR := True;
end Clear;
