with Ada.Real_Time; use Ada.Real_Time;

package body OTM8009A is

   lcdRegData1  : constant DSI_Data := (16#80#, 16#09#, 16#01#, 16#FF#);
   lcdRegData2  : constant DSI_Data := (16#80#, 16#09#, 16#FF#);
   lcdRegData3  : constant DSI_Data :=
                    (16#00#, 16#09#, 16#0F#, 16#0E#, 16#07#, 16#10#, 16#0B#,
                     16#0A#, 16#04#, 16#07#, 16#0B#, 16#08#, 16#0F#, 16#10#,
                     16#0A#, 16#01#, 16#E1#);
   lcdRegData4  : constant DSI_Data :=
                    (16#00#, 16#09#, 16#0F#, 16#0E#, 16#07#, 16#10#, 16#0B#,
                     16#0A#, 16#04#, 16#07#, 16#0B#, 16#08#, 16#0F#, 16#10#,
                     16#0A#, 16#01#, 16#E2#);
   lcdRegData5  : constant DSI_Data := (16#79#, 16#79#, 16#D8#);
   lcdRegData6  : constant DSI_Data := (16#00#, 16#01#, 16#B3#);
   lcdRegData7  : constant DSI_Data :=
                    (16#85#, 16#01#, 16#00#, 16#84#,
                     16#01#, 16#00#, 16#CE#);
   lcdRegData8  : constant DSI_Data :=
                    (16#18#, 16#04#, 16#03#, 16#39#, 16#00#, 16#00#, 16#00#,
                     16#18#, 16#03#, 16#03#, 16#3A#, 16#00#, 16#00#, 16#00#,
                     16#CE#);
   lcdRegData9  : constant DSI_Data :=
                    (16#18#, 16#02#, 16#03#, 16#3B#, 16#00#, 16#00#, 16#00#,
                     16#18#, 16#01#, 16#03#, 16#3C#, 16#00#, 16#00#, 16#00#,
                     16#CE#);
   lcdRegData10 : constant DSI_Data :=
                    (16#01#, 16#01#, 16#20#, 16#20#, 16#00#, 16#00#, 16#01#,
                     16#02#, 16#00#, 16#00#, 16#CF#);
   lcdRegData11 : constant DSI_Data :=
                    (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#CB#);
   lcdRegData12 : constant DSI_Data :=
                    (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#CB#);
   lcdRegData13 : constant DSI_Data :=
                    (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#CB#);
   lcdRegData14 : constant DSI_Data :=
                    (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#CB#);
   lcdRegData15 : constant DSI_Data :=
                    (16#00#, 16#04#, 16#04#, 16#04#, 16#04#, 16#04#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#CB#);
   lcdRegData16 : constant DSI_Data :=
                    (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#04#,
                     16#04#, 16#04#, 16#04#, 16#04#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#CB#);
   lcdRegData17 : constant DSI_Data :=
                    (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#CB#);
   lcdRegData18 : constant DSI_Data :=
                    (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
                     16#FF#, 16#FF#, 16#FF#, 16#CB#);
   lcdRegData19 : constant DSI_Data :=
                    (16#00#, 16#26#, 16#09#, 16#0B#, 16#01#, 16#25#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#CC#);
   lcdRegData20 : constant DSI_Data :=
                    (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#00#, 16#26#, 16#0A#, 16#0C#,
                     16#02#, 16#CC#);
   lcdRegData21 : constant DSI_Data :=
                    (16#25#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#CC#);
   lcdRegData22 : constant DSI_Data :=
                    (16#00#, 16#25#, 16#0C#, 16#0A#, 16#02#, 16#26#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#CC#);
   lcdRegData23 : constant DSI_Data :=
                    (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#00#, 16#25#, 16#0B#, 16#09#,
                     16#01#, 16#CC#);
   lcdRegData24 : constant DSI_Data :=
                    (16#26#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                     16#00#, 16#CC#);
   lcdRegData25 : constant DSI_Data := (16#FF#, 16#FF#, 16#FF#, 16#FF#);

   --  CASET value (Column Address Set) : X direction LCD GRAM boundaries
   --  depending on LCD orientation mode and PASET value (Page Address Set)
   --  : Y direction LCD GRAM boundaries depending on LCD orientation mode
   --  XS[15:0] = 16#000 = 0, XE[15:0] = 16#31F = 799 for landscape mode :
   --   apply to CASET
   --  YS[15:0] = 16#000 = 0, YE[15:0] = 16#1DF = 479 for landscape mode :
   --   apply to PASET
   lcdRegData27_Landscape : constant DSI_Data :=
                    (16#00#, 16#00#, 16#03#, 16#1F#, CMD_CASET);
   lcdRegData28_Landscape : constant DSI_Data :=
                    (16#00#, 16#00#, 16#01#, 16#DF#, CMD_PASET);

   --  XS[15:0] = 16#000 = 0, XE[15:0] = 16#1DF = 479 for portrait mode :
   --   apply to CASET
   --  YS[15:0] = 16#000 = 0, YE[15:0] = 16#31F = 799 for portrait mode :
   --   apply to PASET
   lcdRegData27_Portrait  : constant DSI_Data :=
                    (16#00#, 16#00#, 16#01#, 16#DF#, CMD_CASET);
   lcdRegData28_Portrait  : constant DSI_Data :=
                    (16#00#, 16#00#, 16#03#, 16#1F#, CMD_PASET);


   ShortRegData1  : constant DSI_Data := (CMD_NOP, 16#00#);
   ShortRegData2  : constant DSI_Data := (CMD_NOP, 16#80#);
   ShortRegData3  : constant DSI_Data := (16#C4#, 16#30#);
   ShortRegData4  : constant DSI_Data := (CMD_NOP, 16#8A#);
   ShortRegData5  : constant DSI_Data := (16#C4#, 16#40#);
   ShortRegData6  : constant DSI_Data := (CMD_NOP, 16#B1#);
   ShortRegData7  : constant DSI_Data := (16#C5#, 16#A9#);
   ShortRegData8  : constant DSI_Data := (CMD_NOP, 16#91#);
   ShortRegData9  : constant DSI_Data := (16#C5#, 16#34#);
   ShortRegData10 : constant DSI_Data := (CMD_NOP, 16#B4#);
   ShortRegData11 : constant DSI_Data := (16#C0#, 16#50#);
   ShortRegData12 : constant DSI_Data := (16#D9#, 16#4E#);
   ShortRegData13 : constant DSI_Data := (CMD_NOP, 16#81#);
   ShortRegData14 : constant DSI_Data := (16#C1#, 16#66#);
   ShortRegData15 : constant DSI_Data := (CMD_NOP, 16#A1#);
   ShortRegData16 : constant DSI_Data := (16#C1#, 16#08#);
   ShortRegData17 : constant DSI_Data := (CMD_NOP, 16#92#);
   ShortRegData18 : constant DSI_Data := (16#C5#, 16#01#);
   ShortRegData19 : constant DSI_Data := (CMD_NOP, 16#95#);
   ShortRegData20 : constant DSI_Data := (CMD_NOP, 16#94#);
   ShortRegData21 : constant DSI_Data := (16#C5#, 16#33#);
   ShortRegData22 : constant DSI_Data := (CMD_NOP, 16#A3#);
   ShortRegData23 : constant DSI_Data := (16#C0#, 16#1B#);
   ShortRegData24 : constant DSI_Data := (CMD_NOP, 16#82#);
   ShortRegData25 : constant DSI_Data := (16#C5#, 16#83#);
   ShortRegData26 : constant DSI_Data := (16#C4#, 16#83#);
   ShortRegData27 : constant DSI_Data := (16#C1#, 16#0E#);
   ShortRegData28 : constant DSI_Data := (CMD_NOP, 16#A6#);
   ShortRegData29 : constant DSI_Data := (CMD_NOP, 16#A0#);
   ShortRegData30 : constant DSI_Data := (CMD_NOP, 16#B0#);
   ShortRegData31 : constant DSI_Data := (CMD_NOP, 16#C0#);
   ShortRegData32 : constant DSI_Data := (CMD_NOP, 16#D0#);
   ShortRegData33 : constant DSI_Data := (CMD_NOP, 16#90#);
   ShortRegData34 : constant DSI_Data := (CMD_NOP, 16#E0#);
   ShortRegData35 : constant DSI_Data := (CMD_NOP, 16#F0#);
   ShortRegData36 : constant DSI_Data := (CMD_SLPOUT, 16#00#);
   ShortRegData39l : constant DSI_Data := (CMD_MADCTR, MADCTR_MODE_LANDSCAPE);
   ShortRegData39p : constant DSI_Data := (CMD_MADCTR, MADCTR_MODE_PORTRAIT);

   ShortRegData44 : constant DSI_Data := (CMD_DISPON, 16#00#);
   ShortRegData45 : constant DSI_Data := (CMD_RAMWR, 16#00#);
   ShortRegData46 : constant DSI_Data := (16#CF#, 16#00#);
   ShortRegData47 : constant DSI_Data := (16#C5#, 16#66#);
   ShortRegData48 : constant DSI_Data := (CMD_NOP, 16#B6#);
   ShortRegData49 : constant DSI_Data := (16#F5#, 16#06#);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Color_Mode  : OTM8009A_Color_Mode;
      Orientation : LCD_Orientation)
   is
   begin
      --  Enable CMD2 to access vendor specific commands
      --  Enter in command 2 mode and set EXTC to enable address shift
      --  function (16#00#)
      DSI_IO_WriteCmd (ShortRegData1);
      DSI_IO_WriteCmd (lcdRegData1);

      --  Enter ORISE Command 2
      DSI_IO_WriteCmd (ShortRegData2); --  Shift address to 0x80
      DSI_IO_WriteCmd (lcdRegData2);

      ----------------------------------------------------------------------
      --   SD_PCH_CTRL - 0xC480h - 129th parameter - Default 0x00         --
      --   Set SD_PT                                                      --
      --  -> Source output level during porch and non-display area to GND --
      DSI_IO_WriteCmd (ShortRegData2);
      DSI_IO_WriteCmd (ShortRegData3);
      delay until Clock + Milliseconds (10);
      --  Not documented...
      DSI_IO_WriteCmd (ShortRegData4);
      DSI_IO_WriteCmd (ShortRegData5);
      delay until Clock + Milliseconds (10);
      ----------------------------------------------------------------------

      ----------------------------------------------------------------------
      --   PWR_CTRL4 - 0xC4B0h - 178th parameter - Default 0xA8           --
      --  Set gvdd_en_test                                                --
      --  -> enable GVDD test mode !!!                                    --
      DSI_IO_WriteCmd (ShortRegData6);
      DSI_IO_WriteCmd (ShortRegData7);
      ----------------------------------------------------------------------

      ----------------------------------------------------------------------
      --  PWR_CTRL2 - 0xC590h - 146th parameter - Default 0x79            --
      --  Set pump 4 vgh voltage                                          --
      --  -> from 15.0v down to 13.0v                                     --
      --  Set pump 5 vgh voltage                                          --
      --  -> from -12.0v downto -9.0v                                     --
      DSI_IO_WriteCmd (ShortRegData8);
      DSI_IO_WriteCmd (ShortRegData9);
      ----------------------------------------------------------------------

      --   P_DRV_M - 0xC0B4h - 181th parameter - Default 0x00
      --  -> Column inversion
      DSI_IO_WriteCmd (ShortRegData10);
      DSI_IO_WriteCmd (ShortRegData11);

      --  VCOMDC - 0xD900h - 1st parameter - Default 0x39h
      --  VCOM Voltage settings
      --  -> from - 1.0000v down to - 1.2625v
      DSI_IO_WriteCmd (ShortRegData1);
      DSI_IO_WriteCmd (ShortRegData12);

      --  Oscillator adjustment for Idle/Normal mode (LPDT only) set to 65Hz
      --  (default is 60Hz)
      DSI_IO_WriteCmd (ShortRegData13);
      DSI_IO_WriteCmd (ShortRegData14);

      --  Video mode internal
      DSI_IO_WriteCmd (ShortRegData15);
      DSI_IO_WriteCmd (ShortRegData16);


      --  PWR_CTRL2 - 0xC590h - 147h parameter - Default 0x00
      --  Set pump 4&5 x6
      --  -> ONLY VALID when PUMP4_EN_ASDM_HV = "0"
      DSI_IO_WriteCmd (ShortRegData17);
      DSI_IO_WriteCmd (ShortRegData18);

      --  PWR_CTRL2 - 0xC590h - 150th parameter - Default 0x33h
      --  Change pump4 clock ratio
      --  -> from 1 line to 1/2 line
      DSI_IO_WriteCmd (ShortRegData19);
      DSI_IO_WriteCmd (ShortRegData9);

      --  GVDD/NGVDD settings
      DSI_IO_WriteCmd (ShortRegData1);
      DSI_IO_WriteCmd (lcdRegData5);

      --  PWR_CTRL2 - 0xC590h - 149th parameter - Default 0x33h
      --  Rewrite the default value !
      DSI_IO_WriteCmd (ShortRegData20);
      DSI_IO_WriteCmd (ShortRegData21);

      --  Panel display timing Setting 3
      DSI_IO_WriteCmd (ShortRegData22);
      DSI_IO_WriteCmd (ShortRegData23);

      --  Power control 1
      DSI_IO_WriteCmd (ShortRegData24);
      DSI_IO_WriteCmd (ShortRegData25);

      --  Source driver precharge
      DSI_IO_WriteCmd (ShortRegData13);
      DSI_IO_WriteCmd (ShortRegData26);

      DSI_IO_WriteCmd (ShortRegData15);
      DSI_IO_WriteCmd (ShortRegData27);

      DSI_IO_WriteCmd (ShortRegData28);
      DSI_IO_WriteCmd (lcdRegData6);

      --  GOAVST
      DSI_IO_WriteCmd (ShortRegData2);
      DSI_IO_WriteCmd (lcdRegData7);

      DSI_IO_WriteCmd (ShortRegData29);
      DSI_IO_WriteCmd (lcdRegData8);

      DSI_IO_WriteCmd (ShortRegData30);
      DSI_IO_WriteCmd (lcdRegData9);

      DSI_IO_WriteCmd (ShortRegData31);
      DSI_IO_WriteCmd (lcdRegData10);

      DSI_IO_WriteCmd (ShortRegData32);
      DSI_IO_WriteCmd (ShortRegData46);

      DSI_IO_WriteCmd (ShortRegData2);
      DSI_IO_WriteCmd (lcdRegData11);

      DSI_IO_WriteCmd (ShortRegData33);
      DSI_IO_WriteCmd (lcdRegData12);

      DSI_IO_WriteCmd (ShortRegData29);
      DSI_IO_WriteCmd (lcdRegData13);

      DSI_IO_WriteCmd (ShortRegData30);
      DSI_IO_WriteCmd (lcdRegData14);

      DSI_IO_WriteCmd (ShortRegData31);
      DSI_IO_WriteCmd (lcdRegData15);

      DSI_IO_WriteCmd (ShortRegData32);
      DSI_IO_WriteCmd (lcdRegData16);

      DSI_IO_WriteCmd (ShortRegData34);
      DSI_IO_WriteCmd (lcdRegData17);

      DSI_IO_WriteCmd (ShortRegData35);
      DSI_IO_WriteCmd (lcdRegData18);

      DSI_IO_WriteCmd (ShortRegData2);
      DSI_IO_WriteCmd (lcdRegData19);

      DSI_IO_WriteCmd (ShortRegData33);
      DSI_IO_WriteCmd (lcdRegData20);

      DSI_IO_WriteCmd (ShortRegData29);
      DSI_IO_WriteCmd (lcdRegData21);

      DSI_IO_WriteCmd (ShortRegData30);
      DSI_IO_WriteCmd (lcdRegData22);

      DSI_IO_WriteCmd (ShortRegData31);
      DSI_IO_WriteCmd (lcdRegData23);

      DSI_IO_WriteCmd (ShortRegData32);
      DSI_IO_WriteCmd (lcdRegData24);

      -----------------------------------------------------------
      --  PWR_CTRL1 - 0xc580h - 130th parameter - default 0x00 --
      --  Pump 1 min and max DM                                --
      DSI_IO_WriteCmd (ShortRegData13);
      DSI_IO_WriteCmd (ShortRegData47);
      DSI_IO_WriteCmd (ShortRegData48);
      DSI_IO_WriteCmd (ShortRegData49);
      -----------------------------------------------------------

      --  Exit CMD2 mode
      DSI_IO_WriteCmd (ShortRegData1);
      DSI_IO_WriteCmd (lcdRegData25);

      ----------------------------------
      --  Standard DCS Initialization --
      ----------------------------------

      --  NOP - goes back to DCS std command ?
      DSI_IO_WriteCmd (ShortRegData1);

      --  Gamma correction 2.2+ table (HSDT possible)
      DSI_IO_WriteCmd (ShortRegData1);
      DSI_IO_WriteCmd (lcdRegData3);

      --  Gamma correction 2.2- table (HSDT possible)
      DSI_IO_WriteCmd (ShortRegData1);
      DSI_IO_WriteCmd (lcdRegData4);

      --  Send Sleep Out command to display : no parameter
      DSI_IO_WriteCmd (ShortRegData36);

      --  Wait for Sleep Out exit
      delay until Clock + Milliseconds (120);

      case Color_Mode is
         when RGB565 =>
            DSI_IO_WriteCmd ((CMD_COLMOD, COLMOD_RGB565));
         when RGB888 =>
            DSI_IO_WriteCmd ((CMD_COLMOD, COLMOD_RGB888));
      end case;

      if Orientation = Landscape then
         --  Send command to configure display in landscape orientation
         --  mode. By default the orientation mode is portrait
         DSI_IO_WriteCmd (ShortRegData39l);
         DSI_IO_WriteCmd (lcdRegData27_Landscape);
         DSI_IO_WriteCmd (lcdRegData28_Landscape);
      else
         DSI_IO_WriteCmd (ShortRegData39p);
         DSI_IO_WriteCmd (lcdRegData27_Portrait);
         DSI_IO_WriteCmd (lcdRegData28_Portrait);
      end if;

      --------------------------------------------------------------
      --  CABC : Content Adaptive Backlight Control section start --
      --------------------------------------------------------------
      --  Note :
      --    defaut is 0 (lowest Brightness),
      --    0xFF is highest Brightness,
      --    try 0x7F : intermediate value
      DSI_IO_WriteCmd ((CMD_WRDISBV, 16#FF#));

      --  defaut is 0, try 0x2C - Brightness Control Block, Display Dimming
      --  & BackLight on
      DSI_IO_WriteCmd ((CMD_WRCTRLD, 16#2C#));

      --  defaut is 0, try 0x02 - image Content based Adaptive Brightness
      --  [Still Picture]
      DSI_IO_WriteCmd ((CMD_WRCABC, 16#02#));

      --  defaut is 0 (lowest Brightness), 0xFF is highest Brightness
      DSI_IO_WriteCmd ((CMD_WRCABCMB, 16#FF#));

      ------------------------------------------------------------
      --  CABC : Content Adaptive Backlight Control section end --
      ------------------------------------------------------------

      --  Send Command Display On
      DSI_IO_WriteCmd (ShortRegData44);

      --  NOP command
      DSI_IO_WriteCmd (ShortRegData1);

      --  Send Command GRAM memory write (no parameters) : this initiates
      --  frame write via other DSI commands sent by DSI host from LTDC
      --  incoming pixels in video mode
      DSI_IO_WriteCmd (ShortRegData45);
   end Initialize;

end OTM8009A;
