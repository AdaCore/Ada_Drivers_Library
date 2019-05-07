with FT801.Registers; use FT801.Registers;

package body FT801.Display_List is

   procedure Poll_For_Ready (This : FT801_Device)
   is
      Dlswap : REG_DLSWAP_Reg;
   begin
      loop
         Read_Register (This => This,
                        Reg  => REG_DLSWAP,
                        Val  => Dlswap.Val);

         exit when Dlswap.Val = 0;
      end loop;

   end Poll_For_Ready;


   -------------------
   -- Send_Cmd_List --
   -------------------

   procedure Send_Cmd_List (This : in out FT801_Device;
                            Cmds : Cmd_List)
   is
      Cmd_Arr : UInt8_Array (1 .. Cmds'Size / 8)
        with Address => Cmds'Address;
   begin
      Host_Memory_Write (This    => This,
                         Address => RAM_DL_Address + UInt22 (This.Dl_Ptr),
                         Payload => Cmd_Arr);

      Poll_For_Ready (This => This);

   end Send_Cmd_List;

end FT801.Display_List;
