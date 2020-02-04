separate(nRF.UART)
overriding
procedure Receive
  (This    : in out UART_Device;
   Data    :    out UART_Data_8b;
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
      --  Wait for RX Ready event
      while UART0_Periph.EVENTS_RXDRDY.EVENTS_RXDRDY = False loop
	 null;
      end loop;

      --  Read a character
      C := This.Periph.RXD.RXD;

      --  Clear the RX event for the character we just received
      UART0_Periph.EVENTS_RXDRDY.EVENTS_RXDRDY := False;
   end loop;

   Status := HAL.UART.Ok;
end Receive;
