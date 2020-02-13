separate (nRF.TWI)
overriding procedure Master_Receive
  (This    : in out TWI_Master;
   Addr    : I2C_Address;
   Data    : out I2C_Data;
   Status  : out I2C_Status;
   Timeout : Natural := 1000)
is
   pragma Unreferenced (Timeout);
begin
   if Data'Length = 0 then
      Status := Ok;
      return;
   end if;

   --  Clear errors
   This.Periph.ERRORSRC := (Clear, Clear, Clear, 0);

   --  Set Address
   This.Periph.ADDRESS.ADDRESS := UInt7 (Addr / 2);


   if Data'Length = 1 then
      --  Only one byte to receive so we stop at the next one
      This.Periph.SHORTS.BB_STOP := Enabled;
      This.Periph.SHORTS.BB_SUSPEND := Disabled;
   else
      --  Configure SHORTS to automatically suspend TWI port when receiving a
      --  byte.
      This.Periph.SHORTS.BB_SUSPEND := Enabled;
      This.Periph.SHORTS.BB_STOP := Disabled;
   end if;

   --  Start RX sequence
   This.Periph.TASKS_STARTRX.TASKS_STARTRX := True;

   for Index in Data'Range loop

      loop

         if This.Periph.EVENTS_ERROR.EVENTS_ERROR /= False then
            Status := Err_Error;
            --  Clear the error
            This.Periph.EVENTS_ERROR.EVENTS_ERROR := False;

            Stop_Sequence (This);

            return;
         end if;

         exit when This.Periph.EVENTS_RXDREADY.EVENTS_RXDREADY /= False;
      end loop;

      --  Clear the event
      This.Periph.EVENTS_RXDREADY.EVENTS_RXDREADY := False;
      Data (Index) := This.Periph.RXD.RXD;

      if Index = Data'Last - 1 and then This.Do_Stop_Sequence then

         --  Configure SHORTS to automatically stop the TWI port and produce
         --  a STOP event on the bus when receiving a byte.
         This.Periph.SHORTS.BB_SUSPEND := Disabled;
         This.Periph.SHORTS.BB_STOP    := Enabled;
      end if;

      This.Periph.TASKS_RESUME.TASKS_RESUME := True;
   end loop;

   Status := Ok;
end Master_Receive;
