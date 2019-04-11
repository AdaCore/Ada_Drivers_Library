with Ada.Real_Time; use Ada.Real_Time;

package body FT801 is
   
   procedure Wait (Period : Time_Span)
   is
   begin
      delay until (Period + Clock);
   end Wait;
   
   
   
   procedure Initialize (This : in out FT801_Device)
   is
       GPIO_Dir_Reg : UInt8 := This.Read_Reg8 (Reg => REG_GPIO_DIR);
   begin
      --  Set SPI Freq under 11 MHz
      
      --  set PD high
      Set (This => This.PD);
      
      --  bootup graphics controller
      This.Reset;
      
      --  Configure display registers
      This.Write_Reg16 (Reg => REG_HCYCLE,
                   Val => This.Settings.HCycle);
      This.Write_Reg16 (Reg => REG_HOFFSET,
                   Val => This.Settings.Hoffset);
      This.Write_Reg16 (Reg => REG_HSYNC0,
                   Val => This.Settings.Hsync0);
      This.Write_Reg16 (Reg => REG_HSYNC1,
                   Val => This.Settings.Hsync1);
      This.Write_Reg16 (Reg => REG_VCYCLE,
                   Val => This.Settings.Vcycle);
      This.Write_Reg16 (Reg => REG_VOFFSET,
                   Val => This.Settings.Voffset);
      This.Write_Reg16 (Reg => REG_VSYNC0,
                   Val => This.Settings.Vsync0);
      This.Write_Reg16 (Reg => REG_VSYNC1,
                   Val => This.Settings.Vsync1);
      This.Write_Reg8 (Reg => REG_SWIZZLE,
                  Val => This.Settings.Swizzle);
      This.Write_Reg8 (Reg => REG_PCLK_POL,
                  Val => This.Settings.Polarity);
      This.Write_Reg8 (Reg => REG_CSPREAD,
                  Val => 1);
      This.Write_Reg16 (Reg => REG_HSIZE,
                   Val => This.Settings.Width);
      This.Write_Reg16 (Reg => REG_VSIZE,
                   Val => This.Settings.Height);
        
      if This.Settings.Ext_Clock then
         This.Send_Host_Command (Cmd => CLKEXT);
      else
         This.Send_Host_Command (Cmd => CLKINT);
      end if;
      
      GPIO_Dir_Reg := GPIO_Dir_Reg or Shift_Left (Value  => 1,
                                                  Amount => 7);
      This.Write_Reg8 (Reg => REG_GPIO_DIR,
                       Val => GPIO_Dir_Reg);
                              
      --  Set SPI Freq back up to 30MHz
      
   end Initialize;
   
   procedure Cycle_PD (This : in out FT801_Device)
   is
   begin
      Set (This => This.PD);
      Wait (Period => Milliseconds (20));
      Clear (This => This.PD);
      Wait (Period => Milliseconds (20));
      Set (This => This.PD);
      Wait (Period => Milliseconds (20));
   end Cycle_PD;
   
   procedure Internal_Clock (This : in out FT801_Device)
   is
   begin
      This.Send_Host_Command (Cmd => ACTIVE);
      Wait (Period => Milliseconds (20));
      
      --  TODO: fill this in
   end Internal_Clock;
   
   procedure Reset (This : in out FT801_Device)
   is
   begin
      This.Cycle_PD;
      This.Internal_Clock;
   end Reset;
   
   procedure Send_Host_Command (This : in out FT801_Device;
                                Cmd  : Command_Table)
   is
      Arr : SPI_Data_8b := (Cmd, 0, 0);
      Status : SPI_Status;
   begin
      This.Port.Transmit (Data => Arr,
                          Status => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;   
   end Send_Host_Command;
   
   procedure Write_Reg8 (This : FT801_Device;
                         Reg : UInt24;
                         Val : UInt8)
   is
      Reg_Arr : UInt8_Array (1 .. 3)
        with Address => Reg'Address;
      Data : SPI_Data_8b := (Reg_Arr (3) or 16#80#,
                             Reg_Arr (2),
                             Reg_Arr (1),
                             Val);
      Status : SPI_Status;
   begin
      This.Port.Transmit (Data => Data,
                          Status => Status);
      if Status /= OK then
         raise Program_Error;
      end if;
      
   end Write_Reg8;
   
   function Read_Reg8 (This : FT801_Device;
                        Reg : UInt24)
                        return UInt8
   is
      Reg_Arr : UInt8_Array (1 .. 3)
        with Address => Reg'Address;
      Data : SPI_Data_8b := (Reg_Arr (3),
                             Reg_Arr (2),
                             Reg_Arr (1),
                             0);
      Status : SPI_Status;
      Ret : SPI_Data_8b (1 .. 1);
   begin
      This.Port.Transmit (Data => Data,
                          Status => Status);
      if Status /= OK then
         raise Program_Error;
      end if;
      This.Port.Receive (Data => Ret,
                         Status => Status);
      if Status /= OK then
         raise Program_Error;
      end if;
      
      return Ret (1);
   end Read_Reg8;
   
   procedure Write_Reg16 (This : FT801_Device;
                          Reg : UInt24;
                          Val : UInt16)
   is
      Reg_Arr : UInt8_Array (1 .. 3)
        with Address => Reg'Address;
      Val_Array : UInt8_Array (1 .. 2)
        with Address => Val'Address;
      Data : SPI_Data_8b := (Reg_Arr (3) or 16#80#,
                             Reg_Arr (2),
                             Reg_Arr (1),
                             Val (1),
                             Val (2));
      Status : SPI_Status;
   begin
      This.Port.Transmit (Data => Data,
                          Status => Status);
      if Status /= OK then
         raise Program_Error;
      end if;
      
   end Write_Reg16;
   
   procedure Write_Reg32 (This : FT801_Device;
                          Reg : UInt24;
                          Val : UInt32)
   is
      Reg_Arr : UInt8_Array (1 .. 3)
        with Address => Reg'Address;
      Val_Array : UInt8_Array (1 .. 4)
        with Address => Val'Address;
      Data : SPI_Data_8b := (Reg_Arr (3) or 16#80#,
                             Reg_Arr (2),
                             Reg_Arr (1),
                             Val (1),
                             Val (2),
                             Val (3),
                             Val (4));
      Status : SPI_Status;
   begin
      This.Port.Transmit (Data => Data,
                          Status => Status);
      if Status /= OK then
         raise Program_Error;
      end if;
      
   end Write_Reg32;
   
   function Read_Reg16 (This : FT801_Device;
                        Reg : UInt24)
                        return UInt8
   is
      Reg_Arr : UInt8_Array (1 .. 3)
        with Address => Reg'Address;
      Data : SPI_Data_8b := (Reg_Arr (3),
                             Reg_Arr (2),
                             Reg_Arr (1),
                             0);
      Status : SPI_Status;
      Ret : SPI_Data_16b (1 .. 1);
   begin
      This.Port.Transmit (Data => Data,
                          Status => Status);
      if Status /= OK then
         raise Program_Error;
      end if;
      This.Port.Receive (Data => Ret,
                         Status => Status);
      if Status /= OK then
         raise Program_Error;
      end if;
      
      return Ret (1);
   end Read_Reg16;
   
   procedure DL (This : FT801_Device;
                 Cmds : Cmd_List)
   is
      Counter : Positive := 0;
   begin
      for I in Cmds'Range loop
         This.Write_Reg32 (Reg => RAM_DL_Address + Counter,
                           Val => Cmds (I));
         Counter = Counter + 4;
      end loop;
   end DL;
   
   procedure Display_On (This : in out FT801_Device)
   is
      Reg : UInt8 := This.Read_Reg8 (Reg => REG_GPIO);
   begin
      Reg := Reg or Shift_Left (Value  => 1,
                                Amount => 7);
      This.Write_Reg8 (Reg => REG_GPIO,
                       Val => Reg); 
   end Display_On;
   
   procedure Display_Off (This : in out FT801_Device)
   is
      Reg : UInt8 := This.Read_Reg8 (Reg => REG_GPIO);
   begin
      Reg := Reg or not Shift_Left (Value  => 1,
                                    Amount => 7);
      This.Write_Reg8 (Reg => REG_GPIO,
                       Val => Reg);
   end Display_Off;
   
   procedure Enable_Interrupts (This : FT801_Device;
                                Mask : UInt8 := 0)
   is
   begin
      This.Write_Reg8 (Reg => REG_INT_EN,
                       Val => 1);
      This.Write_Reg8 (Reg => REG_INT_MASK,
                       Val => Mask);
   end Enable_Interrupts;
   
   procedure Disable_Interrupts (This : FT801_Device)
   is
   begin
      This.Write_Reg8 (Reg => REG_INT_EN,
                       Val => 0);
   end Disable_Interrupts;
   
   function Read_Interrupt_Flags (This : FT801_Device) return UInt8
   is
   begin
      return This.Read_Reg8 (Reg => REG_INT_FLAGS);
   end Read_Interrupt_Flags;
                        
   procedure Draw_Bitmap (This : in out FT801_Device;
                          Format : Graphics_Bitmap_Format;
                          Img : UInt8_Array)
   is
   begin
      --  TODO: write image to RAM_G
      
      This.DL (Cmds => (CMD_CLEAR (True, True, True),
                        CMD_BEGIN (BITMAPS),
                        CMD_END,
                        CMD_DISPLAY));
                        
                        
      
   end Draw_Bitmap;   
      
  
                        
                        
   
   
      
   
   
   
     

end FT801;
