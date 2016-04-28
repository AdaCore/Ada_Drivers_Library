package ILI9341_Regs is

   ILI9341_RESET         : constant := 16#01#;
   ILI9341_SLEEP_OUT     : constant := 16#11#;
   ILI9341_GAMMA         : constant := 16#26#;
   ILI9341_DISPLAY_OFF   : constant := 16#28#;
   ILI9341_DISPLAY_ON    : constant := 16#29#;
   ILI9341_COLUMN_ADDR   : constant := 16#2A#;
   ILI9341_PAGE_ADDR     : constant := 16#2B#;
   ILI9341_GRAM          : constant := 16#2C#;
   ILI9341_MAC           : constant := 16#36#;
   ILI9341_PIXEL_FORMAT  : constant := 16#3A#;
   ILI9341_WDB           : constant := 16#51#;
   ILI9341_WCD           : constant := 16#53#;
   ILI9341_RGB_INTERFACE : constant := 16#B0#;
   ILI9341_FRC           : constant := 16#B1#;
   ILI9341_BPC           : constant := 16#B5#;
   ILI9341_DFC           : constant := 16#B6#;
   ILI9341_POWER1        : constant := 16#C0#;
   ILI9341_POWER2        : constant := 16#C1#;
   ILI9341_VCOM1         : constant := 16#C5#;
   ILI9341_VCOM2         : constant := 16#C7#;
   ILI9341_POWERA        : constant := 16#CB#;
   ILI9341_POWERB        : constant := 16#CF#;
   ILI9341_PGAMMA        : constant := 16#E0#;
   ILI9341_NGAMMA        : constant := 16#E1#;
   ILI9341_DTCA          : constant := 16#E8#;
   ILI9341_DTCB          : constant := 16#EA#;
   ILI9341_POWER_SEQ     : constant := 16#ED#;
   ILI9341_3GAMMA_EN     : constant := 16#F2#;
   ILI9341_INTERFACE     : constant := 16#F6#;
   ILI9341_PRC           : constant := 16#F7#;

end ILI9341_Regs;
