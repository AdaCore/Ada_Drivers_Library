separate (nRF.UART)
overriding
procedure Transmit
  (This    : in out UART_Device;
   Data    :        UART_Data_8b;
   Status  :    out UART_Status;
   Timeout :        Natural := 1_000)
is
   pragma Unreferenced (Timeout);
begin
   if Data'Length = 0 then
      Status := HAL.UART.Ok;
      return;
   end if;

   for C of Data loop
      --  Wait for TX Ready event
      while UART0_Periph.EVENTS_TXDRDY = 0 loop
         null;
      end loop;

      --  Clear the event
      This.Periph.EVENTS_TXDRDY := 0;

      --  Send a character
      This.Periph.TXD.TXD := C;
   end loop;

   Status := HAL.UART.Ok;
end Transmit;
