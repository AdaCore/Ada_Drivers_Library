with HAL;                  use HAL;
with HAL.SPI;              use HAL.SPI;
with HAL.GPIO;             use HAL.GPIO;

private with Ada.Real_Time;

package FT801 is

   pragma Warnings (Off);
-------------------------------------------------------------------------------------------------------------------------------------------------------------
--  Display parameters used for various options are
--  FT800_DisplayResolution         Width   Height  Swizzle Polarity    PClk    HCycle  Hoffset     Hsync0      Hsync1      VCycle  Voffset     Vsync0  Vsync1
--  FT_DISPLAY_QVGA_320x240         320     240     3       0           8       408     70          0           10          263         13      0       2
--  FT_DISPLAY_WQVGA_480x272        480     272     0       1           5       548     43          0           41          292         12      0       10
--------------------------------------------------------------------------------------------------------------------------------------------------------------
   pragma Warnings (On);

   type Display_Settings is record
      Width       : UInt10;
      Height      : UInt10;
      Swizzle     : UInt4 := 0;
      Polarity    : Boolean := True;
      PClk        : UInt8 := 5;
      HCycle      : UInt10 := 548;
      Hoffset     : UInt10 := 43;
      Hsync0      : UInt10 := 0;
      Hsync1      : UInt10 := 41;
      VCycle      : UInt10 := 292;
      Voffset     : UInt10 := 12;
      Vsync0      : UInt10 := 0;
      Vsync1      : UInt10 := 10;
      Ext_Clock   : Boolean := True;
      Disp_Signal : Positive := 31;
   end record;

   type FT801_Device (Port     : not null Any_SPI_Port;
                      PD       : not null Any_GPIO_Point) is private;

   procedure Initialize (This : in out FT801_Device;
                         Settings : Display_Settings);

   procedure Reset (This : in out FT801_Device);

   type Interrupts is record
      Swap : Boolean;
      Rsv  : Boolean;
      Tag  : Boolean;
      Sound : Boolean;
      Playback          : Boolean;
      CmdEmpty          : Boolean;
      CmdFlag           : Boolean;
      ConvComplete      : Boolean;
   end record
     with Size => 8;

   for Interrupts use record
      Swap at 0 range 0 .. 0;
      Rsv at 0 range 1 .. 1;
      Tag at 0 range 2 .. 2;
      Sound at 0 range 3 .. 3;
      Playback at 0 range 4 .. 4;
      CmdEmpty at 0 range 5 .. 5;
      CmdFlag at 0 range 6 .. 6;
      ConvComplete at 0 range 7 .. 7;
   end record;


   function Read_Interrupts (This : FT801_Device) return Interrupts;

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
      BARGRAPH)
     with Size => 5;

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

   type Screen_Coordinate is record
      X, Y : Integer;
   end record;

   procedure Draw_Bitmap (This   : FT801_Device;
                          Format : Graphics_Bitmap_Format;
                          Width  : UInt10;
                          Height : UInt10;
                          Img    : UInt8_Array);

--     procedure Draw_Rectangle (This : FT801_Device;
--                               Lower_Left : Screen_Coordinate;
--                               Upper_Right : Screen_Coordinate;
--                               Fill : Boolean);
--
--     procedure Draw_Button (This : FT801_Device;
--                            Lower_Left : Screen_Coordinate;
--                            Upper_Right : Screen_Coordinate;
--                            Text : String);

private
   
   type Fifo_Pointer_Type is mod 4096;

   type FT801_Device (Port     : not null Any_SPI_Port;
                      PD       : not null Any_GPIO_Point)
   is record
      Settings : Display_Settings;
      Fifo_Ptr : Fifo_Pointer_Type := 0;
   end record;

   type Command_Table is
     (
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
      CLKEXT,
      --  Select PLL input from Crystal oscillator
      --  or external input clock.
      CLKINT,
      --  Select PLL input from Internal relaxation
      --  oscillator (default).
       PWRDOWN,
      --  Switch off 1.2V internal regulator. Clock,
      --  PLL and Oscillator off.
      CLK36M,
      --  Switch PLL output clock to 36MHz.
      CLK48M,
      --  Switch PLL output clock to 48MHz
      --  (default).
      --  Send reset pulse to FT801 core. All
      --  registers and state machines will be
      --  reset.
      CORERST) with Size => 8;

   for Command_Table use
     (ACTIVE  => 16#00#,
      STANDBY => 16#41#,
      SLEEP   => 16#42#,
      CLKEXT  => 16#44#,
      CLKINT  => 16#48#,
      PWRDOWN => 16#50#,
      CLK36M  => 16#61#,
      CLK48M  => 16#62#,

      CORERST => 16#68#);



   procedure Send_Host_Command (This : in out FT801_Device;
                                Cmd  : Command_Table);

   procedure Host_Memory_Write (This : FT801_Device;
                                Address : UInt22;
                                Payload : UInt8_Array);

   procedure Host_Memory_Read (This : FT801_Device;
                               Address : UInt22;
                               Payload : out UInt8_Array);

   subtype Header is UInt8_Array (1 .. 3);

   type Read_Or_Write is (Read, Write);

   function Create_Header (Address : UInt22;
                           Direction : Read_Or_Write) return Header;








   RAM_G_Address : constant := 16#0000_0000#;

   ROM_CHIPID_Address : constant := 16#0C_0000#;

   ROM_FONT_Address : constant := 16#B0_B23C#;

   ROM_FONT_ADDR_Address : constant := 16#0F_FFFC#;

   RAM_DL_Address : constant := 16#10_0000#;

   RAM_PAL_Address : constant := 16#10_2000#;

   REG_Address : constant := 16#10_2400#;

   RAM_CMD_Address : constant := 16#10_8000#;

   RAM_SCREENSHOT_Address : constant := 16#1C_2000#;

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

   procedure Wait (Period : Ada.Real_Time.Time_Span);

--     procedure Set_Display_Enable (This   : in out FT801_Device;
--                                   Enable : Boolean);

   procedure Display_On (This : in out FT801_Device);
   procedure Display_Off (This : in out FT801_Device);

   procedure Cycle_PD (This : in out FT801_Device);
   procedure Internal_Clock (This : in out FT801_Device);

   procedure Enable_Interrupts (This : FT801_Device;
                                Mask : Interrupts);
   procedure Disable_Interrupts (This : FT801_Device);



end FT801;
