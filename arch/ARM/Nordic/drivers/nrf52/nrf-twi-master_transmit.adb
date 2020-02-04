separate(nRF.TWI)
overriding procedure Master_Transmit
     (This    : in out TWI_Master;
      Addr    : I2C_Address;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
is
   pragma Unreferenced (Timeout);
   Index : Integer := Data'First + 1;
   Evt_Err : Boolean;
   Err_Src : ERRORSRC_Register with Unreferenced;
begin
   if Data'Length = 0 then
      Status := Ok;
      return;
   end if;

   --  Clear errors
   This.Periph.ERRORSRC := (Clear, Clear, Clear, 0);

   --  Set Address
   This.Periph.ADDRESS.ADDRESS := UInt7 (Addr / 2);

   --  Prepare first byte
   This.Periph.TXD.TXD := Data (Data'First);

   --  Start TX sequence
   This.Periph.TASKS_STARTTX.TASKS_STARTTX := True;

   loop

      loop

	 Evt_Err := This.Periph.EVENTS_ERROR.EVENTS_ERROR;
	 if Evt_Err /= False then
	    Err_Src := This.Periph.ERRORSRC;
	    Status := Err_Error;
	    --  Clear the error
	    This.Periph.EVENTS_ERROR.EVENTS_ERROR := False;

	    --  Stop sequence
	    This.Periph.TASKS_STOP.TASKS_STOP := True;

	    return;
	 end if;

	 exit when This.Periph.EVENTS_TXDSENT.EVENTS_TXDSENT /= False;
      end loop;

      --  Clear the event
      This.Periph.EVENTS_TXDSENT.EVENTS_TXDSENT := False;

      exit when Index > Data'Last;

      This.Periph.TXD.TXD := Data (Index);
      Index := Index + 1;
   end loop;

   if This.Do_Stop_Sequence then
      Stop_Sequence (This);
   end if;

   Status := Ok;
end Master_Transmit;
