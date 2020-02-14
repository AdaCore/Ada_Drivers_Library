separate (nRF.RTC)
procedure Stop (This : Real_Time_Counter) is
begin
   This.Periph.TASKS_STOP := 1;
end Stop;
