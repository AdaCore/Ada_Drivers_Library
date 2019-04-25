package FT801.Registers is

   type Register is
     (REG_ID,
      REG_FRAMES,
      REG_CLOCK,
      REG_FREQUENCY,
      REG_SCREENSHOT_EN,
      REG_SCREENSHOT_Y,
      REG_SCREENSHOT_START,
      REG_CPURESET,
      REG_TAP_CRC,
      REG_TAP_MASK,
      REG_HCYCLE,
      REG_HOFFSET,
      REG_HSIZE,
      REG_HSYNC0,
      REG_HSYNC1,
      REG_VCYCLE,
      REG_VOFFSET,
      REG_VSIZE,
      REG_VSYNC0,
      REG_VSYNC1,
      REG_DLSWAP,
      REG_ROTATE,
      REG_OUTBITS,
      REG_DITHER,
      REG_SWIZZLE,
      REG_CSPREAD,
      REG_PCLK_POL,
      REG_PCLK,
      REG_TAG_X,
      REG_TAG_Y,
      REG_TAG,
      REG_VOL_PB,
      REG_VOL_SOUND,
      REG_SOUND,
      REG_PLAY,
      REG_GPIO_DIR,
      REG_GPIO,
      REG_INT_FLAGS,
      REG_INT_EN,
      REG_INT_MASK,
      REG_PLAYBACK_START,
      REG_PLAYBACK_LENGTH,
      REG_PLAYBACK_READPT,
      REG_PLAYBACK_FREQ,
      REG_PLAYBACK_FORMAT,
      REG_PLAYBACK_LOOP,
      REG_PLAYBACK_PLAY,
      REG_PWM_HZ,
      REG_PWM_DUTY,
      REG_MACRO0,
      REG_MACRO1,
      REG_SCREENSHOT_BUS_Y,
      REG_CMD_READ,
      REG_CMD_WRITE,
      REG_CMD_DL,
      REG_TOUCH_MODE,
      REG_CTOUCH_EXTENDED,
      REG_CTOUCH_REG,
      REG_CTOUCH_TOUCH1_XY,
      REG_CTOUCH_TOUCH4_Y,
      REG_CTOUCH_TOUCH0_XY,
      REG_TOUCH_TAG_XY,
      REG_TOUCH_TAG,
      REG_CTOUCH_TRANSFORM_A,
      REG_COUTCH_TRANSFORM_B,
      REG_CTOUCH_TRANSFORM_C,
      REG_CTOUCH_TRANSFORM_D,
      REG_CTOUCH_TRANSFORM_E,
      REG_CTOUCH_TRANSFORM_F,
      REG_CTOUCH_TOUCH4_X,
      REG_SCREENSHOT_READ,
      REG_TRIM,
      REG_CTOUCH_TOUCH2_XY,
      REG_CTOUCH_TOUCH3_XY,
      REG_TRACKER)
     with Size => 22;

   for Register use
     (REG_ID => 16#102400#,
      REG_FRAMES => 16#102404#,
      REG_CLOCK => 16#102408#,
      REG_FREQUENCY => 16#10240C#,
      REG_SCREENSHOT_EN => 16#102410#,
      REG_SCREENSHOT_Y => 16#102414#,
      REG_SCREENSHOT_START => 16#102418#,
      REG_CPURESET => 16#10241C#,
      REG_TAP_CRC => 16#102420#,
      REG_TAP_MASK => 16#102424#,
      REG_HCYCLE => 16#102428#,
      REG_HOFFSET => 16#10242C#,
      REG_HSIZE => 16#102430#,
      REG_HSYNC0 => 16#102434#,
      REG_HSYNC1 => 16#102438#,
      REG_VCYCLE => 16#10243C#,
      REG_VOFFSET => 16#102440#,
      REG_VSIZE => 16#102444#,
      REG_VSYNC0 => 16#102448#,
      REG_VSYNC1 => 16#10244C#,
      REG_DLSWAP => 16#102450#,
      REG_ROTATE => 16#102454#,
      REG_OUTBITS => 16#102458#,
      REG_DITHER => 16#10245C#,
      REG_SWIZZLE => 16#102460#,
      REG_CSPREAD => 16#102464#,
      REG_PCLK_POL => 16#102468#,
      REG_PCLK => 16#10246C#,
      REG_TAG_X => 16#102470#,
      REG_TAG_Y => 16#102474#,
      REG_TAG => 16#102478#,
      REG_VOL_PB => 16#10247C#,
      REG_VOL_SOUND => 16#102480#,
      REG_SOUND => 16#102484#,
      REG_PLAY => 16#102488#,
      REG_GPIO_DIR => 16#10248C#,
      REG_GPIO => 16#102490#,
      REG_INT_FLAGS => 16#102498#,
      REG_INT_EN => 16#10249C#,
      REG_INT_MASK => 16#1024A0#,
      REG_PLAYBACK_START => 16#1024A4#,
      REG_PLAYBACK_LENGTH => 16#1024A8#,
      REG_PLAYBACK_READPT => 16#1024AC#,
      REG_PLAYBACK_FREQ => 16#1024B0#,
      REG_PLAYBACK_FORMAT => 16#1024B4#,
      REG_PLAYBACK_LOOP => 16#1024B8#,
      REG_PLAYBACK_PLAY => 16#1024BC#,
      REG_PWM_HZ => 16#1024C0#,
      REG_PWM_DUTY => 16#1024C4#,
      REG_MACRO0 => 16#1024C8#,
      REG_MACRO1 => 16#1024CC#,
      REG_SCREENSHOT_BUS_Y => 16#1024D8#,
      REG_CMD_READ => 16#1024E4#,
      REG_CMD_WRITE => 16#1024E8#,
      REG_CMD_DL => 16#1024EC#,
      REG_TOUCH_MODE => 16#1024F0#,
      REG_CTOUCH_EXTENDED => 16#1024F4#,
      REG_CTOUCH_REG => 16#1024F8#,
      REG_CTOUCH_TOUCH1_XY => 16#102508#,
      REG_CTOUCH_TOUCH4_Y => 16#10250C#,
      REG_CTOUCH_TOUCH0_XY => 16#102510#,
      REG_TOUCH_TAG_XY => 16#102514#,
      REG_TOUCH_TAG => 16#102518#,
      REG_CTOUCH_TRANSFORM_A => 16#10251C#,
      REG_COUTCH_TRANSFORM_B => 16#102520#,
      REG_CTOUCH_TRANSFORM_C => 16#102524#,
      REG_CTOUCH_TRANSFORM_D => 16#102528#,
      REG_CTOUCH_TRANSFORM_E => 16#10252C#,
      REG_CTOUCH_TRANSFORM_F => 16#102530#,
      REG_CTOUCH_TOUCH4_X => 16#102538#,
      REG_SCREENSHOT_READ => 16#102554#,
      REG_TRIM => 16#10256C#,
      REG_CTOUCH_TOUCH2_XY => 16#102574#,
      REG_CTOUCH_TOUCH3_XY => 16#102578#,
      REG_TRACKER => 16#109000#);

   procedure Write_Register (This : FT801_Device;
                             Reg  : Register;
                             Val  : UInt32);

   procedure Read_Register (This : FT801_Device;
                            Reg  : Register;
                            Val  : out UInt32);

   type REG_PCLK_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt24 := 0;
            Div : UInt8;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_PCLK_Reg use record
      RSVD at 0 range 8 .. 31;
      Div at 0 range 0 .. 7;
      Val at 0 range 0 .. 31;
   end record;

   type REG_PCLK_POL_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt31 := 0;
            Falling_Edge : Boolean := False;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_PCLK_POL_Reg use record
      RSVD at 0 range 1 .. 31;
      Falling_Edge at 0 range 0 .. 0;
      Val at 0 range 0 .. 31;
   end record;

   type REG_CSPREAD_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt31 := 0;
            Early : Boolean := True;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_CSPREAD_Reg use record
      RSVD at 0 range 1 .. 31;
      Early at 0 range 0 .. 0;
      Val at 0 range 0 .. 31;
   end record;

   type REG_SWIZZLE_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt28 := 0;
            Pin_Cfg : UInt4 := 0;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_SWIZZLE_Reg use record
      RSVD at 0 range 4 .. 31;
      Pin_Cfg at 0 range 0 .. 3;
      Val at 0 range 0 .. 31;
   end record;

   type REG_DITHER_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt31 := 16#1B6#;
            Enable : Boolean := False;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_DITHER_Reg use record
      RSVD at 0 range 1 .. 31;
      Enable at 0 range 0 .. 0;
      Val at 0 range 0 .. 31;
   end record;

   type REG_OUTBITS_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt23 := 0;
            B    : UInt3 := 6;
            G    : UInt3 := 6;
            R    : UInt3 := 6;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_OUTBITS_Reg use record
      RSVD at 0 range 9 .. 31;
      B at 0 range 0 .. 2;
      G at 0 range 3 .. 5;
      R at 0 range 6 .. 8;
      Val at 0 range 0 .. 31;
   end record;

   type REG_ROTATE_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt31 := 0;
            Enable : Boolean := False;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_ROTATE_Reg use record
      RSVD at 0 range 1 .. 31;
      Enable at 0 range 0 .. 0;
      Val at 0 range 0 .. 31;
   end record;

   type REG_VSYNC1_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Lines : UInt10 := 16#0A#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_VSYNC1_Reg use record
      RSVD at 0 range 10 .. 31;
      Lines at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type REG_VSYNC0_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Lines : UInt10 := 0;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_VSYNC0_Reg use record
      RSVD at 0 range 10 .. 31;
      Lines at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type REG_VSIZE_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Lines : UInt10 := 16#110#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_VSIZE_Reg use record
      RSVD at 0 range 10 .. 31;
      Lines at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type REG_VOFFSET_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Lines : UInt10 := 16#00C#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_VOFFSET_Reg use record
      RSVD at 0 range 10 .. 31;
      Lines at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type REG_VCYCLE_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Lines : UInt10 := 16#124#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_VCYCLE_Reg use record
      RSVD at 0 range 10 .. 31;
      Lines at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type REG_HSYNC1_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Cycles : UInt10 := 16#029#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_HSYNC1_Reg use record
      RSVD at 0 range 10 .. 31;
      Cycles at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type REG_HSYNC0_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Cycles : UInt10 := 0;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_HSYNC0_Reg use record
      RSVD at 0 range 10 .. 31;
      Cycles at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type REG_HSIZE_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Cycles : UInt10 := 16#1E0#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_HSIZE_Reg use record
      RSVD at 0 range 10 .. 31;
      Cycles at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type REG_HOFFSET_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Cycles : UInt10 := 16#2B#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_HOFFSET_Reg use record
      RSVD at 0 range 10 .. 31;
      Cycles at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type REG_HCYCLE_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt22 := 0;
            Cycles : UInt10 := 16#224#;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_HCYCLE_Reg use record
      RSVD at 0 range 10 .. 31;
      Cycles at 0 range 0 .. 9;
      Val at 0 range 0 .. 31;
   end record;

   type GPIO_Direction is
     (Input, Output)
     with Size => 1;

   for GPIO_Direction use
     (Input => 0,
      Output => 1);

   type REG_GPIO_DIR_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt24 := 0;
            GPIO0_Dir : GPIO_Direction := Input;
            GPIO1_Dir : GPIO_Direction := Input;
            GPIO2_Dir : GPIO_Direction := Input;
            GPIO3_Dir : GPIO_Direction := Input;
            GPIO4_Dir : GPIO_Direction := Input;
            GPIO5_Dir : GPIO_Direction := Input;
            GPIO6_Dir : GPIO_Direction := Input;
            GPIO7_Dir : GPIO_Direction := Output;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_GPIO_DIR_Reg use record
      RSVD at 0 range 8 .. 31;
      GPIO0_Dir at 0 range 0 .. 0;
      GPIO1_Dir at 0 range 1 .. 1;
      GPIO2_Dir at 0 range 2 .. 2;
      GPIO3_Dir at 0 range 3 .. 3;
      GPIO4_Dir at 0 range 4 .. 4;
      GPIO5_Dir at 0 range 5 .. 5;
      GPIO6_Dir at 0 range 6 .. 6;
      GPIO7_Dir at 0 range 7 .. 7;
      Val at 0 range 0 .. 31;
   end record;

   type REG_GPIO_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt24 := 0;
            GPIO0 : Boolean := False;
            GPIO1 : Boolean := False;
            GPIO2 : Boolean := False;
            GPIO3 : Boolean := False;
            GPIO4 : Boolean := False;
            GPIO5 : Boolean := False;
            GPIO6 : Boolean := False;
            GPIO7 : Boolean := False;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_GPIO_Reg use record
      RSVD at 0 range 8 .. 31;
      GPIO0 at 0 range 0 .. 0;
      GPIO1 at 0 range 1 .. 1;
      GPIO2 at 0 range 2 .. 2;
      GPIO3 at 0 range 3 .. 3;
      GPIO4 at 0 range 4 .. 4;
      GPIO5 at 0 range 5 .. 5;
      GPIO6 at 0 range 6 .. 6;
      GPIO7 at 0 range 7 .. 7;
      Val at 0 range 0 .. 31;
   end record;

   type REG_INT_MASK_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt24 := 0;
            Mask : Interrupts := (others => True);
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_INT_MASK_Reg use record
      RSVD at 0 range 8 .. 31;
      Mask at 0 range 0 .. 7;
      Val at 0 range 0 .. 31;
   end record;

   type REG_INT_EN_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt31 := 0;
            Enable : Boolean := False;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_INT_EN_Reg use record
      RSVD at 0 range 1 .. 31;
      Enable at 0 range 0 .. 0;
      Val at 0 range 0 .. 31;
   end record;

   type REG_INT_FLAGS_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt24 := 0;
            Mask : Interrupts := (others => False);
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_INT_FLAGS_Reg use record
      RSVD at 0 range 8 .. 31;
      Mask at 0 range 0 .. 7;
      Val at 0 range 0 .. 31;
   end record;

   type Swap_Type is
     (Immediate, After_Scan_Out)
     with Size => 2;

   for Swap_Type use
     (Immediate      => 1,
      After_Scan_Out => 2);

   type REG_DLSWAP_Reg (As_Val : Boolean := False) is record
      case As_Val is
         when False =>
            RSVD : UInt30 := 0;
            Swap : Swap_Type := After_Scan_Out;
         when True =>
            Val : UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for REG_DLSWAP_Reg use record
      RSVD at 0 range 2 .. 31;
      Swap at 0 range 0 .. 1;
      Val at 0 range 0 .. 31;
   end record;

end FT801.Registers;
