separate (nRF.RTC)
procedure Clear (This : Real_Time_Counter) is
begin
   This.Periph.TASKS_CLEAR.TASKS_CLEAR := True;
end Clear;
