with Ada.Real_Time; use Ada.Real_Time;

with FT801.Registers; use FT801.Registers;
with FT801.Display_List;
with FT801.Coproc;

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
      Tx_Header : constant UInt8_Array := Create_Header (Address   => Address,
                                                         Direction => Read) & 0;
      Tx_Payload : constant UInt8_Array (Payload'Range) := (others => 0);
      Tx_Packet  : constant UInt8_Array := Tx_Header & Tx_Payload;
      Rx_Packet : UInt8_Array (Tx_Packet'Range);
   begin
      This.Port.Transfer (Tx_Data => SPI_Data_8b (Tx_Packet),
                          Rx_Data => SPI_Data_8b (Rx_Packet),
                          Status  => Status);

      if Status /= Ok then
         raise Program_Error;
      end if;

      Payload := Rx_Packet (Tx_Header'First + Tx_Header'Length .. Rx_Packet'Last);
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
      Arr : constant SPI_Data_8b := (Command_Table'Enum_Rep (Cmd), 0, 0);
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
      GPIO_Reg : REG_GPIO_Reg;
      Chipid       : UInt32;
   begin
      This.Settings := Settings;

      This.Fifo_Ptr := 0;

      --  set PD high
      --  Set (This => This.PD.all);

      --  bootup graphics controller
      Reset (This => This);

      Read_Register (This => This,
                     Reg  => REG_ID,
                     Val  => Chipid);

      if Chipid /= 16#7C# then
         raise Program_Error;
      end if;

      --  Configure display registers
      Write_Register (This => This,
                      Reg  => REG_VSYNC0,
                      Val  => REG_VSYNC0_Reg'(Lines  => This.Settings.Vsync0,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_VSYNC1,
                      Val  => REG_VSYNC1_Reg'(Lines => This.Settings.Vsync1,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_VOFFSET,
                      Val  => REG_VOFFSET_Reg'(Lines => This.Settings.Voffset,
                                               others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_VCYCLE,
                      Val  => REG_VCYCLE_Reg'(Lines => This.Settings.VCycle,
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
                      Reg  => REG_HOFFSET,
                      Val  => REG_HOFFSET_Reg'(Cycles => This.Settings.Hoffset,
                                               others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_HCYCLE,
                      Val  => REG_HCYCLE_Reg'(Cycles => This.Settings.HCycle,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_HSIZE,
                      Val  => REG_HSIZE_Reg'(Cycles => This.Settings.Width,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_VSIZE,
                      Val  => REG_VSIZE_Reg'(Lines => This.Settings.Height,
                                             others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_PCLK_POL,
                      Val  => REG_PCLK_POL_Reg'(Falling_Edge => This.Settings.Polarity,
                                               others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_SWIZZLE,
                      Val  => REG_SWIZZLE_Reg'(Pin_Cfg => This.Settings.Swizzle,
                                              others => <>).Val);
      Write_Register (This => This,
                      Reg  => REG_CSPREAD,
                      Val  => REG_CSPREAD_Reg'(Early => True,
                                              others => <>).Val);

      Write_Register (This => This,
                      Reg  => REG_PCLK,
                      Val  => REG_PCLK_Reg'(Div    => This.Settings.PClk,
                                            others => <>).Val);

      if This.Settings.Ext_Clock then
         Send_Host_Command (This => This,
                            Cmd  => CLKEXT);
      else
         Send_Host_Command (This => This,
                            Cmd  => CLKINT);
      end if;

      Read_Register (This => This,
                     Reg  => REG_GPIO_DIR,
                     Val  => GPIO_Dir_Reg.Val);

      GPIO_Dir_Reg.GPIO7_Dir := Output;

      Write_Register (This => This,
                      Reg  => REG_GPIO_DIR,
                      Val  => GPIO_Dir_Reg.Val);

      Read_Register (This => This,
                     Reg  => REG_GPIO,
                     Val  => GPIO_Reg.Val);

      GPIO_Reg.GPIO7 := True;

      Write_Register (This => This,
                      Reg  => REG_GPIO,
                      Val  => GPIO_Reg.Val);

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
      Bootup : constant UInt8_Array := (0, 0, 0, 2,
                                        7, 0, 0, 38,
                                        0, 0, 0, 0);
      Dummy_Read : UInt8_Array (1 .. 1);
      pragma Unreferenced (Dummy_Read);
   begin
   --   Send_Host_Command (This => This,
   --                      Cmd  => ACTIVE);
   --  Send ACTIVE, Dummy read from Address 0 generates ACTIVE command
      Host_Memory_Read (This    => This,
                        Address => 0,
                        Payload => Dummy_Read);

      Wait (Period => Milliseconds (20));

      Host_Memory_Write (This    => This,
                         Address => RAM_DL_Address,
                         Payload => Bootup);
      Write_Register (This => This,
                      Reg  => REG_DLSWAP,
                      Val  => REG_DLSWAP_Reg'(others => <>).Val);
      This.Fifo_Ptr := 0;

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

   procedure Draw_Bitmap (This   : in out FT801_Device;
                          Format : Graphics_Bitmap_Format;
                          Width  : UInt9;
                          Height : UInt9;
                          X      : UInt9;
                          Y      : UInt9;
                          Img    : UInt8_Array)
   is
   begin
      Fill_G_Ram (This   => This,
                  Start  => 0,
                  Buffer => Img);

      Publish_G_Ram_Bitmap (This   => This,
                            Width  => Width,
                            Height => Height,
                            X      => X,
                            Y      => Y,
                            Format => Format);

   end Draw_Bitmap;

   procedure Fill_G_Ram (This   : in out FT801_Device;
                         Start  : UInt22;
                         Buffer : UInt8_Array)
   is
   begin
      Host_Memory_Write (This    => This,
                         Address => RAM_G_Address + Start,
                         Payload => Buffer);
   end Fill_G_Ram;


   procedure Publish_G_Ram_Bitmap (This   : in out FT801_Device;
                                   Width  : UInt9;
                                   Height : UInt9;
                                   X      : UInt9;
                                   Y      : UInt9;
                                   Format : Graphics_Bitmap_Format)
   is

   begin
      Coproc.Send_Coproc_Cmds (This => This,
                               Cmds => (Coproc.CMD_DLSTART,
                                        Display_List.Clear'(Color   => True,
                                                            Stencil => True,
                                                            Tag     => True,
                                                            others  => <>).Val,
                                        Display_List.Bitmap_Source'(Addr    => RAM_G_Address,
                                                                    others  => <>).Val,
                                        Display_List.Bitmap_Layout'(Format     => Format,
                                                                    Linestride => UInt10 (Width * 2),
                                                                    Height     => Height,
                                                                    others     => <>).Val,
                                        Display_List.Bitmap_Size'(Filter  => Display_List.NEAREST,
                                                                  Wrapx   => Display_List.BORDER,
                                                                  WrapY   => Display_List.BORDER,
                                                                  Width   => Width,
                                                                  Height  => Height,
                                                                  others  => <>).Val,
                                        Display_List.Cmd_Begin'(Prim    => Display_List.BITMAPS,
                                                                others  => <>).Val,
                                        Display_List.ColorRGB'(Red    => 255,
                                                               Blue   => 255,
                                                               Green  => 255,
                                                               others => <>).Val,
                                        Display_List.Vertex2ii'(X      => X,
                                                                Y      => Y,
                                                                Handle => 0,
                                                                Cell   => 0,
                                                                others => <>).Val,
                                        Display_List.Cmd_End'(others => <>).Val,
                                        Display_List.Display'(others => <>).Val,
                                        Coproc.CMD_DLSWAP));

   end Publish_G_Ram_Bitmap;

   procedure Clear_Screen (This : in out FT801_Device)
   is
   begin
      Coproc.Send_Coproc_Cmds (This => This,
                               Cmds => (1 => Display_List.Clear'(Color   => True,
                                                                 Stencil => True,
                                                                 Tag     => True,
                                                                 others  => <>).Val));

   end Clear_Screen;

   procedure Wait_For_Coproc_Sync (This : FT801_Device)
   is
      Read : UInt32 := 0;
      Write : UInt32 := 1;
   begin
      while Read /= Write loop
         Read_Register (This => This,
                        Reg  => REG_CMD_READ,
                        Val  => Read);
         Read_Register (This => This,
                        Reg  => REG_CMD_WRITE,
                        Val  => Write);
      end loop;

   end Wait_For_Coproc_Sync;


   procedure Draw_Logo (This : in out FT801_Device)
   is
   begin
      Coproc.Send_Coproc_Cmds (This => This,
                               Cmds => (Coproc.CMD_DLSTART,
                                        Display_List.Clear'(Color   => True,
                                                            Stencil => True,
                                                            Tag     => True,
                                                            others  => <>).Val,
                                        Display_List.Cmd_Begin'(Prim   => Display_List.BITMAPS,
                                                                others => <>).Val,
                                        Display_List.Vertex2ii'(X      => 220,
                                                                 Y      => 110,
                                                                 Handle => 31,
                                                                 Cell   => Character'Pos ('F'),
                                                                 others => <>).Val,
                                        Display_List.Vertex2ii'(X      => 244,
                                                                 Y      => 110,
                                                                 Handle => 31,
                                                                 Cell   => Character'Pos ('T'),
                                                                 others => <>).Val,
                                        Display_List.Vertex2ii'(X      => 270,
                                                                 Y      => 110,
                                                                 Handle => 31,
                                                                 Cell   => Character'Pos ('D'),
                                                                 others => <>).Val,
                                        Display_List.Vertex2ii'(X      => 299,
                                                                 Y      => 110,
                                                                 Handle => 31,
                                                                 Cell   => Character'Pos ('I'),
                                                                others => <>).Val,
                                        Display_List.Cmd_End'(others => <>).Val,
                                        Display_List.ColorRGB'(Red    => 160,
                                                               Blue   => 22,
                                                               Green  => 22,
                                                               others => <>).Val,
                                        Display_List.Point_Size'(Size   => 320,
                                                                 others => <>).Val,
                                        Display_List.Cmd_Begin'(Prim   => Display_List.POINTS,
                                                                others => <>).Val,
                                        Display_List.Vertex2ii'(X      => 192,
                                                                Y      => 133,
                                                                Handle => 0,
                                                                Cell   => 0,
                                                                others => <>).Val,
                                        Display_List.Cmd_End'(others => <>).Val,
                                        Display_List.Display'(others => <>).Val,
                                        Coproc.CMD_DLSWAP));
   end Draw_Logo;

   procedure Play_Logo (This : in out FT801_Device)
   is
      Cmd_Read, Cmd_Write : UInt32 := 1;
   begin
      Coproc.Send_Coproc_Cmds (This => This,
                               Cmds => (1 => CMD_LOGO));
      Wait (Period => Milliseconds (2600));

      while Cmd_Read /= 0 and Cmd_Write /= 0 loop
         Read_Register (This => This,
                        Reg  => REG_CMD_READ,
                        Val  => Cmd_Read);
         Read_Register (This => This,
                        Reg  => REG_CMD_WRITE,
                        Val  => Cmd_Write);
      end loop;
   end Play_Logo;


end FT801;
