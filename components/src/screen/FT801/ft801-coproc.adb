with FT801.Registers; use FT801.Registers;

package body FT801.Coproc is

   procedure Send_Coproc_Cmds (This : in out FT801_Device;
                               Cmds : Coproc_List)
   is
      Freespace : Fifo_Pointer_Type;
      Cmds_Arr  : UInt8_Array (1 .. Cmds'Size / 8)
        with Address => Cmds'Address;
   begin
      loop
         Freespace := Compute_Free_Space (This => This);
         exit when Freespace >= Cmds'Size / 8;
      end loop;

      Host_Memory_Write (This    => This,
                         Address => UInt22 (RAM_CMD_Address) + UInt22 (This.Fifo_Ptr),
                         Payload => Cmds_Arr);
      This.Fifo_Ptr := This.Fifo_Ptr + Cmds_Arr'Length;

      Write_Register (This => This,
                      Reg  => REG_CMD_WRITE,
                      Val  => UInt32 (This.Fifo_Ptr));

--        for I in Cmds'Range loop
--           declare
--              Payload : UInt8_Array (1 .. 4)
--                with Address => Cmds (I)'Address;
--           begin
--
--              Host_Memory_Write (This    => This,
--                                 Address => UInt22 (RAM_CMD_Address) + UInt22 (This.Fifo_Ptr),
--                                 Payload => Payload);
--              This.Fifo_Ptr := This.Fifo_Ptr + Payload'Length;
--
--              Write_Register (This => This,
--                              Reg  => REG_CMD_WRITE,
--                              Val  => UInt32 (This.Fifo_Ptr));
--           end;
--        end loop;

   end Send_Coproc_Cmds;

   function Compute_Free_Space (This : FT801_Device) return Fifo_Pointer_Type
   is
      Fullness : Fifo_Pointer_Type;
      Cmd_Write : UInt32;
      Cmd_Read : UInt32;
   begin
      Read_Register (This => This,
                     Reg  => REG_CMD_WRITE,
                     Val  => Cmd_Write);

      Read_Register (This => This,
                     Reg  => REG_CMD_READ,
                     Val  => Cmd_Read);

      Fullness := Fifo_Pointer_Type (Cmd_Write - Cmd_Read);

      return (4092 - Fullness);
   end Compute_Free_Space;

   function Fault_Occured (This : FT801_Device) return Boolean
   is
      Cmd_Read : UInt32;
   begin
      Read_Register (This => This,
                     Reg  => REG_CMD_READ,
                     Val  => Cmd_Read);

      if Cmd_Read = 16#FFF# then
         return True;
      end if;

      return False;
   end Fault_Occured;

   procedure Recover_Fault (This : FT801_Device)
   is
   begin
      Write_Register (This => This,
                      Reg  => REG_CPURESET,
                      Val  => 1);
      Write_Register (This => This,
                      Reg  => REG_CMD_READ,
                      Val  => 0);
      Write_Register (This => This,
                      Reg  => REG_CMD_WRITE,
                      Val  => 0);
      Write_Register (This => This,
                      Reg  => REG_CPURESET,
                      Val  => 0);
   end Recover_Fault;

end FT801.Coproc;
