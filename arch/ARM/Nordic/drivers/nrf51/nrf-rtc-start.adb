separate(nRF.RTC)
procedure Start (This : Real_Time_Counter) is
begin
   This.Periph.TASKS_START := 1;
end Start;
