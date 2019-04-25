with FT801.Registers; use FT801.Registers;

package body FT801.Display_List is

   -------------------
   -- Send_Cmd_List --
   -------------------

   procedure Send_Cmd_List (This : in out FT801_Device;
                            Cmds : Cmd_List)
   is
      Cmd_Arr : UInt8_Array (1 .. Cmds'Length * 4)
        with Address => Cmds'Address;
      Ptr_Arr : UInt8_Array (1 .. 4)
        with Address => This.Fifo_Ptr'Address;
   begin
      Host_Memory_Write (This    => This,
                         Address => RAM_CMD_Address + UInt22 (This.Fifo_Ptr),
                         Payload => Cmd_Arr);

      This.Fifo_Ptr := This.Fifo_Ptr + Cmds'Size / 8;

      Host_Memory_Write (This    => This,
                         Address => Register'Pos (REG_CMD_WRITE),
                         Payload => Ptr_Arr);
   end Send_Cmd_List;

end FT801.Display_List;
