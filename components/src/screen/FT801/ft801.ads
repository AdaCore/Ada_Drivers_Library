with HAL;                  use HAL;
with HAL.SPI;              use HAL.SPI;
with HAL.GPIO;             use HAL.GPIO;

package FT801 is
   
-------------------------------------------------------------------------------------------------------------------------------------------------------------
--  Display parameters used for various options are
--  FT800_DisplayResolution         Width   Height  Swizzle Polarity    PClk    HCycle  Hoffset     Hsync0      Hsync1      VCycle  Voffset     Vsync0  Vsync1  
--  FT_DISPLAY_QVGA_320x240         320     240     3       0           8       408     70          0           10          263         13      0       2
--  FT_DISPLAY_WQVGA_480x272        480     272     0       1           5       548     43          0           41          292         12      0       10
--------------------------------------------------------------------------------------------------------------------------------------------------------------
   
   type Display_Settings is record
      Width       : UInt16;
      Height      : UInt16;
      Swizzle     : UInt8 := 0;
      Polarity    : UInt8 := 1;
      PClk        : UInt8 := 5;
      HCycle      : UInt16 := 548;
      Hoffset     : UInt16 := 43;
      Hsync0      : UInt16 := 0;
      Hsync1      : UInt16 := 41;
      VCycle      : UInt16 := 292;
      Voffset     : UInt16 := 12;
      Vsync0      : UInt16 := 0;
      Vsync1      : UInt16 := 10;
      Ext_Clock   : Boolean := True;
      Disp_Signal : Positive := 31;
   end record;
      
   type FT801_Device (Port     : not null Any_SPI_Port;
                      PD       : not null Any_GPIO_Point;
                      Settings : Display_Settings) is private;
   
   procedure Initialize (This : in out FT801_Device);
   procedure Reset (This : in out FT801_Device);
   
   type Graphics_Bitmap_Format is
     (ARGB1555,
      L1,
      L4,
      L8,
      RGB332,
      ARGB2,
      ARGB4,
      RGB565,
      PALETTED,
      TEXT8x8,
      TEXTVGA,
      BARGRAPH);
   
   for Graphics_Bitmap_Format use
     (ARGB1555 => 0,
      L1       => 1,
      L4       => 2,
      L8       => 3,
      RGB332   => 4,
      ARGB2    => 5,
      ARGB4    => 6,
      RGB565   => 7,
      PALETTED => 8,
      TEXT8x8  => 9,
      TEXTVGA  => 10,
      BARGRAPH => 11);
   
   procedure Draw_Bitmap (This   : in out FT801_Device;
                          Format : Graphics_Bitmap_Format;
                          Img    : UInt8_Array);
   
private
   
   type Command_Table is
     (--  Power Modes
      ACTIVE,
      --  Switch from Standby/Sleep modes to 
      --  active mode. Dummy read from address
      --  0 generates ACTIVE command.
      STANDBY,
      --  Put FT801 core to standby mode. Clock
      --  gate off, PLL and Oscillator remain on
      --  (default).
      SLEEP,
      --  Put FT801 core to sleep mode. Clock
      --  gate off, PLL and Oscillator off.
      PWRDOWN,
      --  Switch off 1.2V internal regulator. Clock,
      --  PLL and Oscillator off.
      --------------------------------------------------
      --  Clock Switching
      CLKEXT,
      --  Select PLL input from Crystal oscillator
      --  or external input clock.
      CLKINT,
      --  Select PLL input from Internal relaxation
      --  oscillator (default).
      CLK48M,
      --  Switch PLL output clock to 48MHz
      --  (default).
      CLK36M,
      --  Switch PLL output clock to 36MHz.
      --------------------------------------------------
      --  Miscellaneous
      --  Send reset pulse to FT801 core. All
      --  registers and state machines will be
      --  reset.
      CORERST) with Size => 8;
   
   for Command_Table use
     (ACTIVE  => 16#00#,
      STANDBY => 16#41#,
      SLEEP   => 16#42#,
      PWRDOWN => 16#50#,
      CLKEXT  => 16#44#,
      CLKINT  => 16#48#,
      CLK48M  => 16#62#,
      CLK36M  => 16#61#,
      CORERST => 16#68#);
   

   
   procedure Send_Host_Command (This : in out FT801_Device;
                                Cmd  : Host_Command);
   
  
   
   RAM_G_Address : constant := 16#0000_0000#
   
   ROM_CHIPID_Address : constant := 16#0C_0000#;
   
   ROM_FONT_Address : constant := 16#B0_B23C#;
   
   ROM_FONT_ADDR_Address : constant := 16#0F_FFFC#;
   
   RAM_DL_Address : constant := 16#10_0000#;
   
   RAM_PAL_Address : constant := 16#10_2000#;      
   
   REG_Address : constant := 16#10_2400#;
   
   RAM_CMD_Address : constant := 16#10_8000#;
   
   RAM_SCREENSHOT_Address : constant := 16#1C_2000#;
   
   
   --  Register Addresses
   --  Identification register, always reads as 7Ch
   REG_ID : constant := 16#10_2400#;
   
   --  Horizontal total cycle count
   REG_HCYCLE : constant := 16#10_2428#;
   
   --  Horizontal display start offset
   REG_HOFFSET : constant := 16#10_242C#;
   --  Horizontal display pixel count
   REG_HSIZE : constant := 16#10_2430#; 
   --  Horizontal sync fall offset
   REG_HSYNC0 : constant := 16#10_2434#; 
   --  Horizontal sync rise offset
   REG_HSYNC1 : constant := 16#10_2438#;
   --  Vertical total cycle count
   REG_VCYCLE : constant := 16#10_243C#; 
   --  Vertical display start offset
   REG_VOFFSET : constant := 16#10_2440#;
   --  Vertical display line count
   REG_VSIZE : constant := 16#10_2444#;
   --  Vertical sync fall offset
   REG_VSYNC0 : constant := 16#10_2448#;
   --  Vertical sync rise offset
   REG_VSYNC1 : constant := 16#10_244C#; 
   --  Display list swap control
   REG_DLSWAP : constant := 16#10_2450#;  
   
   --  Output RGB signal swizzle
   REG_SWIZZLE : constant := 16#10_2460#; 
   --  Output clock spreading enable
   REG_CSPREAD : constant := 16#10_2464#;
   --  PCLK polarity: 0 = out rising, 1 = out falling
   REG_PCLK_POL : constant := 16#10_2468#; 
   --  PCLK frequency divider, 0 = disable
   REG_PCLK : constant := 16#10_246C#;
   
  
    
   --  GPIO pin direction, 0 = input , 1 = output
   REG_GPIO_DIR : constant := 16#10_248C#;
   --  GPIO pin value (bit 0,1,7); output pin drive strength(bit 2-6)
   REG_GPIO : constant := 16#10_2490#;
   --  Interrupt flags, clear by read
   REG_INT_FLAGS : constant := 16#10_2498#;
   --  Global interrupt enable
   REG_INT_EN : constant := 16#10_249C#;
   --  Interrupt enable mask
   REG_INT_MASK : constant := 16#10_24A0#;
   
   type Read_Write is
     (READ, WRITE);
   
   for Read_Write use
     (READ  => 0,
      WRITE => 1);    
   
   type Bitmap_Header is record
      Format : UInt8;
      Width  : UInt16;
      Height : UInt16;
      Stride : UInt16;
      Offset : UInt32;
   end record;
   
   type Graphics_Primitives is
     (BITMAPS,
      POINTS,
      LINES,
      LINE_STRIP,
      EDGE_STRIP_R,
      EDGE_STRIP_L,
      EDGE_STRIP_A,
      EDGE_STRIP_B,
      RECTS)
     with Size => 4;
   
   for Graphics_Primitives use
     (BITMAPS      => 1,
      POINTS       => 2,
      LINES        => 3,
      LINE_STRIP   => 4,
      EDGE_STRIP_R => 5,
      EDGE_STRIP_L => 6,
      EDGE_STRIP_A => 7,
      EDGE_STRIP_B => 8,
      RECTS        => 9);
   
   type Graphics_Bitmap_Format is
     (ARGB1555,
      L1,
      L4,
      L8,
      RGB332,
      ARGB2,
      ARGB4,
      RGB565,
      PALETTED,
      TEXT8x8,
      TEXTVGA,
      BARGRAPH);
   
   for Graphics_Bitmap_Format use
     (ARGB1555 => 0,
      L1       => 1,
      L4       => 2,
      L8       => 3,
      RGB332   => 4,
      ARGB2    => 5,
      ARGB4    => 6,
      RGB565   => 7,
      PALETTED => 8,
      TEXT8x8  => 9,
      TEXTVGA  => 10,
      BARGRAPH => 11);
   
   --  Coprocessor related commands
   CMD_APPEND : constant := 16#FFFF_FF1E#;
   CMD_BGCOLOR : constant := 16#FFFF_FF09#;
   CMD_BITMAP_TRANSFORM : constant := 16#FFFF_FF21#;
   CMD_BUTTON : constant := 16#FFFF_FF0D#;
   CMD_CALIBRATE : constant := 16#FFFF_FF15#;
   CMD_CLOCK : constant := 16#FFFF_FF14#;
   CMD_COLDSTART : constant := 16#FFFF_FF32#;
   CMD_CRC : constant := 16#FFFF_FF03#;
   CMD_DIAL : constant := 16#FFFF_FF2D#;
   CMD_DLSTART : constant := 16#FFFF_FF00#;
   CMD_EXECUTE : constant := 16#FFFF_FF07#;
   CMD_FGCOLOR : constant := 16#FFFF_FF0A#;
   CMD_GAUGE : constant := 16#FFFF_FF13#;
   CMD_GETMATRIX : constant := 16#FFFF_FF33#;
   CMD_GETPOINT : constant := 16#FFFF_FF08#;
   CMD_GETPROPS : constant := 16#FFFF_FF25#;
   CMD_GETPTR : constant := 16#FFFF_FF23#;
   CMD_GRADCOLOR : constant := 16#FFFF_FF34#;
   CMD_GRADIENT : constant := 16#FFFF_FF0B#;
   CMD_HAMMERAUX : constant := 16#FFFF_FF04#;
   CMD_IDCT : constant := 16#FFFF_FF06#;
   CMD_INFLATE : constant := 16#FFFF_FF22#;
   CMD_INTERRUPT : constant := 16#FFFF_FF02#;
   CMD_KEYS : constant := 16#FFFF_FF0E#;
   CMD_LOADIDENTITY : constant := 16#FFFF_FF26#;
   CMD_LOADIMAGE : constant := 16#FFFF_FF24#;
   CMD_LOGO : constant := 16#FFFF_FF31#;
   CMD_MARCH : constant := 16#FFFF_FF05#;
   CMD_MEMCPY : constant := 16#FFFF_FF1D#;
   CMD_MEMCRC : constant := 16#FFFF_FF18#;
   CMD_MEMSET : constant := 16#FFFF_FF1B#;
   CMD_MEMWRITE : constant := 16#FFFF_FF1A#;
   CMD_MEMZERO : constant := 16#FFFF_FF1C#;
   CMD_NUMBER : constant := 16#FFFF_FF2E#;
   CMD_PROGRESS : constant := 16#FFFF_FF0F#;
   CMD_REGREAD : constant := 16#FFFF_FF19#;
   CMD_ROTATE : constant := 16#FFFF_FF29#;
   CMD_SCALE : constant :=  16#FFFF_FF28#;
   CMD_SCREENSAVER : constant := 16#FFFF_FF2F#;
   CMD_SCROLLBAR : constant := 16#FFFF_FF11#;
   CMD_SETFONT : constant := 16#FFFF_FF2B#;
   CMD_SETMATRIX : constant := 16#FFFF_FF2A#;
   CMD_SKETCH : constant := 16#FFFF_FF30#;
   CMD_SLIDER : constant := 16#FFFF_FF10#;
   CMD_SNAPSHOT : constant := 16#FFFF_FF1F#;
   CMD_SPINNER : constant := 16#FFFF_FF16#;
   CMD_STOP : constant := 16#FFFF_FF17#;
   CMD_SWAP : constant := 16#FFFF_FF01#;
   CMD_TEXT : constant := 16#FFFF_FF0C#;
   CMD_TOGGLE : constant := 16#FFFF_FF12#;
   CMD_TOUCH_TRANSFORM : constant := 16#FFFF_FF20#;
   CMD_TRACK : constant := 16#FFFF_FF2C#;
   CMD_TRANSLATE : constant := 16#FFFF_FF27#;
   
   procedure Write_Reg8 (This : in out FT801_Device;
                         Reg  : UInt24;
                         Val  : Uint8);
   
   procedure Write_Reg16 (This : in out FT801_Device;
                          Reg  : UInt24;
                          Val  : Uint16);
               
   procedure Wait (Period : Time_Span);
   
   procedure Set_Display_Enable (This   : in out FT801_Device;
                                 Enable : Boolean);
      
   procedure Display_On (This : in out FT801_Device);
   procedure Display_Off (This : in out FT801_Device);
   
   procedure Cycle_PD (This : in out FT801_Device);
   procedure Internal_Clock (This : in out FT801_Device);
   
   procedure Enable_Interrupts (This : FT801_Device;
                                Mask : UInt8 := 0);
   procedure Disable_Interrupts (This : FT801_Device);
   function Read_Interrupt_Flags (This : FT801_Device) return UInt8;
   
   type Cmd_List is array (Natural range <>) of UInt32;
   
   procedure DL (This : FT801_Device;
                 Cmds : Cmd_List);
   
   function CMD_BEGIN (Prim : Graphics_Primitives) return UInt32 is
     (16#1f00_0000# or Graphics_Primitives'Pos (Prim));
   
   function CMD_BITMAP_HANDLE (Handle : UInt5) return UInt32 is
     (16#0500_0000# or Handle);
   
   function CMD_BITMAP_LAYOUT (Format : Graphics_Bitmap_Format;
                               Stride : UInt10;
                               Height : UInt9)
                               return UInt32 is
     (16#0700_0000# or Shift_Left (Graphics_Bitmap_Format'Pos (Format), 19) or 
        Shift_Left (Stride, 9) or Height);
   
   function CMD_BITMAP_SIZE (Filter : Bit;
                             Wrapx  : Bit;
                             Wrapy  : Bit;
                             Width  : UInt9;
                             Height : UInt9)
                             return UInt32 is
     (16#0800_0000# or Shift_Left (Filter, 20) or Shift_Left (Wrapx, 19) or 
          Shift_Left (Wrapy, 18) or Shift_Left (Width, 9) or Height);
   
   function CMD_BITMAP_SOURCE (Addr : UInt20) return UInt32 is
     (16#0100_0000# or Addr);
   
   function CMD_CLEAR (Color   : Bit;
                       Stencil : Bit;
                       Tag     : Bit)
                       return UInt32 is
     (16#2600_0000# or Shift_Left (Color, 2) or Shift_Left (Stencil, 1) or Tag);
   
   function CMD_DISPLAY return UInt32 is
     (16#0#);
   
   function CMD_END return UInt32 is
     (16#2100_0000#);

end FT801;
