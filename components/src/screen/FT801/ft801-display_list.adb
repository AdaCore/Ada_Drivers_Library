pragma Ada_2012;
package body FT801.Display_List is

   -------------------
   -- Send_Cmd_List --
   -------------------

   procedure Send_Cmd_List (This : FT801_Device;
                            Cmds : Cmd_List)
   is
   begin
      Host_Memory_Write (This    => This,
                         Address => RAM_CMD_Address + This.Fifo_Ptr,
                         Payload => Cmds);

      This.Fifo_Ptr := This.Fifo_Ptr + Cmds'Size / 8;

      Host_Memory_Write (This    => This,
                         Address => REG_CMD_WRITE,
                         Payload => (1 => This.Fifo_Ptr));
   end Send_Cmd_List;

end FT801.Display_List;
