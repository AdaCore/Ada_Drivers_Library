separate (nRF.UART)
procedure Disable (This : in out UART_Device) is
begin
   This.Periph.ENABLE.ENABLE := Disabled;
   This.Periph.PSELTXD := 16#FFFF_FFFF#;
   This.Periph.PSELRXD := 16#FFFF_FFFF#;

   --  Stop TX and RX
   This.Periph.TASKS_STOPTX.TASKS_STOPTX := True;
   This.Periph.TASKS_STOPRX.TASKS_STOPRX := True;

end Disable;
