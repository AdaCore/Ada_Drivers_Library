separate(nRF.Timers)
procedure Capture (This : in out Timer;
		   Chan : Timer_Channel)
is
begin
   This.Periph.TASKS_CAPTURE (Integer (Chan)) := 1;
end Capture;
