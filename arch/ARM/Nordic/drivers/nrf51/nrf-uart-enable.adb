separate (nRF.UART)
procedure Enable (This : in out UART_Device;
                  Tx, Rx : GPIO_Pin_Index)
is
begin
   This.Periph.PSELTXD := UInt32 (Tx);
   This.Periph.PSELRXD := UInt32 (Rx);
   This.Periph.ENABLE.ENABLE := Enabled;

   --  Start TX and RX
   This.Periph.TASKS_STARTRX := 1;
   This.Periph.TASKS_STARTTX := 1;

   --  Send a first character to start the TXREADY events (See nRF Series
   --  Reference Manual Version 3.0 Figure 68: UART transmission).
   This.Periph.TXD.TXD := 0;
end Enable;
