separate(nRF.RTC)
procedure Stop (This : Real_Time_Counter) is
begin
   This.Periph.TASKS_STOP.TASKS_STOP := True;
end Stop;
