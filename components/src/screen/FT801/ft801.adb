with Ada.Real_Time; use Ada.Real_Time;

with FT801.Registers; use FT801.Registers;
with FT801.Display_List;

package body FT801 is

   procedure Wait (Period : Time_Span)
   is
   begin
      delay until (Period + Clock);
   end Wait;

   procedure Host_Memory_Write (This : FT801_Device;
                                Address : UInt22;
                                Payload : UInt8_Array)
   is
      Status : SPI_Status;
      Packet : constant UInt8_Array := Create_Header (Address   => Address,
                                                      Direction => Write) & Payload;
   begin

      This.Port.Transmit (Data => SPI_Data_8b (Packet),
                          Status => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
   end Host_Memory_Write;

   procedure Host_Memory_Read (This : FT801_Device;
                               Address : UInt22;
                               Payload : out UInt8_Array)
   is
      Status : SPI_Status;
      TX_Packet : constant UInt8_Array := Create_Header (Address   => Address,
                                                         Direction => Read) & 0;
   begin
      This.Port.Transmit (Data   => SPI_Data_8b (TX_Packet),
                          Status => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      for I in Payload'Range loop
         declare
            Tx : constant SPI_Data_8b (1 .. 1) := (1 => 0);
            Rx : SPI_Data_8b (1 .. 1)
              with Address => Payload (I)'Address;
         begin
            This.Port.Transmit (Data => Tx,
                                Status => Status);
            if Status /= Ok then
               raise Program_Error;
            end if;

            This.Port.Receive (Data => Rx,
                               Status => Status);

            if Status /= Ok then
               raise Program_Error;
            end if;
         end;
      end loop;
   end Host_Memory_Read;

   function Create_Header (Address : UInt22;
                           Direction : Read_Or_Write) return Header
   is
      Cast_Address : UInt24 := UInt24 (Address);
      Mapped_Address : UInt8_Array (1 .. 3)
        with Address => Cast_Address'Address;
      Ret            : Header;
   begin
      Ret (1) := 2#0011_1111# and Mapped_Address (3);
      case Direction is
         when Read =>
            null;
         when Write =>
            Ret (1) := 2#1000_0000# or Ret (1);
      end case;

      Ret (2) := Mapped_Address (2);
      Ret (3) := Mapped_Address (1);

      return Ret;
   end Create_Header;

   procedure Send_Host_Command (This : in out FT801_Device;
                                Cmd  : Command_Table)
   is
      Arr : constant SPI_Data_8b := (Command_Table'Pos (Cmd), 0, 0);
      Status : SPI_Status;
   begin
      This.Port.Transmit (Data => Arr,
                          Status => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      Wait (Period => Milliseconds (20));
   end Send_Host_Command;


   procedure Initialize (This : in out FT801_Device;
                         Settings : Display_Settings)
   is
      GPIO_Dir_Reg : REG_GPIO_DIR_Reg;
   begin
      This.Settings := Settings;

      --  Set SPI Freq under 11 MHz

      --  set PD high
      Set (This => This.PD.all);

      --  bootup graphics controller
      Reset (This => This);

      --  Configure display registers
      Write_Register (This => This,
                      Reg  => REG_HCYCLE,
                      Val  => REG_HCYCLE_Reg'(Cycles => This.Settings.HCycle,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_HOFFSET,
                      Val  => REG_HOFFSET_Reg'(Cycles => This.Settings.Hoffset,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_HSYNC0,
                      Val  => REG_HSYNC0_Reg'(Cycles => This.Settings.Hsync0,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_HSYNC1,
                      Val  => REG_HSYNC1_Reg'(Cycles => This.Settings.Hsync1,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_VCYCLE,
                      Val  => REG_VCYCLE_Reg'(Lines => This.Settings.VCycle,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_VOFFSET,
                      Val  => REG_VOFFSET_Reg'(Lines => This.Settings.Voffset,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_VSYNC0,
                      Val  => REG_VSYNC0_Reg'(Lines => This.Settings.Vsync0,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_VSYNC1,
                      Val  => REG_VSYNC1_Reg'(Lines => This.Settings.Vsync1,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_SWIZZLE,
                      Val  => REG_SWIZZLE_Reg'(Pin_Cfg => This.Settings.Swizzle,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_PCLK_POL,
                      Val  => REG_PCLK_POL_Reg'(Falling_Edge => This.Settings.Polarity,
                                               others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_CSPREAD,
                      Val  => REG_CSPREAD_Reg'(Early => True,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_HSIZE,
                      Val  => REG_HSIZE_Reg'(Cycles => This.Settings.Width,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_VSIZE,
                      Val  => REG_VSIZE_Reg'(Lines => This.Settings.Height,
                                              others => <>).Val);

      if This.Settings.Ext_Clock then
         Send_Host_Command (This => This,
                            Cmd  => CLKEXT);
      else
         Send_Host_Command (This => This,
                            Cmd => CLKINT);
      end if;

      Read_Register (This => This,
                            Reg => REG_GPIO_DIR,
                          Val => GPIO_Dir_Reg.Val);

      GPIO_Dir_Reg.GPIO7_Dir := Output;

      Write_Register (This => This,
                            Reg => REG_GPIO_DIR,
                           Val => GPIO_Dir_Reg.Val);


      --  Set SPI Freq back up to 30MHz

   end Initialize;

   procedure Cycle_PD (This : in out FT801_Device)
   is
   begin
      Set (This => This.PD.all);
      Wait (Period => Milliseconds (20));
      Clear (This => This.PD.all);
      Wait (Period => Milliseconds (20));
      Set (This => This.PD.all);
      Wait (Period => Milliseconds (20));
   end Cycle_PD;

   procedure Internal_Clock (This : in out FT801_Device)
   is
   begin
      Send_Host_Command (This => This,
                         Cmd  => ACTIVE);
      Wait (Period => Milliseconds (20));

      --  TODO: fill this in
   end Internal_Clock;

   procedure Reset (This : in out FT801_Device)
   is
   begin
      Cycle_PD (This => This);
      Internal_Clock (This => This);
   end Reset;


   procedure Display_On (This : in out FT801_Device)
   is
      Reg : REG_GPIO_Reg;
   begin
      Read_Register (This => This,
                     Reg  => REG_GPIO,
                     Val  => Reg.Val);
      Reg.GPIO7 := True;

      Write_Register (This => This,
                      Reg  => REG_GPIO,
                      Val  => Reg.Val);
   end Display_On;

   procedure Display_Off (This : in out FT801_Device)
   is
      Reg : REG_GPIO_Reg;
   begin
      Read_Register (This => This,
                     Reg  => REG_GPIO,
                     Val  => Reg.Val);
      Reg.GPIO7 := False;

      Write_Register (This => This,
                      Reg  => REG_GPIO,
                      Val  => Reg.Val);
   end Display_Off;

   procedure Enable_Interrupts (This : FT801_Device;
                                Mask : Interrupts)
   is

   begin
      Write_Register (This => This,
                      Reg  => REG_INT_EN,
                      Val  => REG_INT_EN_Reg'(Enable => True,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_INT_MASK,
                      Val  => REG_INT_MASK_Reg'(Mask => Mask,
                                               others => <>).Val);
   end Enable_Interrupts;

   procedure Disable_Interrupts (This : FT801_Device)
   is
   begin
      Write_Register (This => This,
                      Reg  => REG_INT_EN,
                      Val  => REG_INT_EN_Reg'(Enable => False,
                                              others => <>).Val);
   end Disable_Interrupts;

   function Read_Interrupts (This : FT801_Device) return Interrupts
   is
      Reg : REG_INT_FLAGS_Reg;
   begin
      Read_Register (This => This,
                     Reg  => REG_INT_FLAGS,
                     Val  => Reg.Val);
      return Reg.Mask;
   end Read_Interrupts;

   procedure Draw_Bitmap (This : FT801_Device;
                          Format : Graphics_Bitmap_Format;
                          Width  : UInt10;
                          Height : UInt10;
                          Img : UInt8_Array)
   is
      Status : SPI_Status;
   begin
      Host_Memory_Write (This    => This,
                         Address => RAM_G_Address,
                         Payload => Img);

      This.CPCMD_DL_START;


      Display_List.Send_Cmd_List (This => This,
                                  Cmds => (Display_List.Clear'(Color   => True,
                                                               Stencil => True,
                                                               Tag     => True,
                                                               others  => <>).Val,
                                           Display_List.Bitmap_Source'(Addr => RAM_G_Address,
                                                                       others  => <>).Val,
                                           Display_List.Bitmap_Layout'(Format     => Format,
                                                                       Linestride => Width,
                                                                       Height     => Height,
                                                                       others     => <>).Val,
                                           Display_List.Bitmap_Size'(Filter => False,
                                                                     Wrapx  => False,
                                                                     WrapY  => False,
                                                                     Width  => Width,
                                                                     Height => Height,
                                                                     others  => <>).Val,
                                           Display_List.Cmd_Begin'(Prim => BITMAPS,
                                                                   others  => <>).Val,
                                           Display_List.Cmd_End'(others => <>).Val));

      Display_List.Send_Cmd_List (This => This,
                                  Cmds => ())
      This.DL (Cmds => DLCMD_Display'(others => <>).Val);
      This.CPCMD_SWAP;
      --  TODO: write to REG_CMD_WRITE with ending FIFO address
      --  TODO: wait for REG_CMD_READ to equal the ending FIFO address

   end Draw_Bitmap;












end FT801;
