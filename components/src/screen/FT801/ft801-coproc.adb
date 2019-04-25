with FT801.Display_List;

package body FT801.Coproc is

   ---------------
   -- Cmd_Start --
   ---------------

   procedure Cmd_Start (This : in out FT801_Device)
   is
   begin
      Display_List.Send_Cmd_List (This => This,
                                  Cmds => (1 => CMD_DLSTART,
                                           2 => Display_List.Clear'(Color   => True,
                                                                    Stencil => True,
                                                                    Tag     => True,
                                                                    others  => <>).Val));
   end Cmd_Start;

   -------------
   -- Cmd_End --
   -------------

   procedure Cmd_End (This : in out FT801_Device)
   is
   begin
      Display_List.Send_Cmd_List (This => This,
                                  Cmds => (1 => Display_List.Display'(others => <>).Val,
                                           2 => CMD_SWAP));
   end Cmd_End;

end FT801.Coproc;
