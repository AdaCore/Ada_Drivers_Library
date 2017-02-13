------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

package body OTM8009A is

   ADDR_SHIFT_CMD : constant := 16#00#;

   -----------
   -- Write --
   -----------

   procedure Write (This    : in out OTM8009A_Device;
                    Address : UInt16;
                    Data    : DSI_Data)
   is
      MSB, LSB : UInt8;
   begin
      MSB := UInt8 (Shift_Right (Address and 16#FF00#, 8));
      LSB := UInt8 (Address and 16#FF#);
      if LSB /= This.Current_Shift then
         This.DSI_IO_WriteCmd ((ADDR_SHIFT_CMD, LSB));
         This.Current_Shift := LSB;
      end if;
      This.DSI_IO_WriteCmd (MSB & Data);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (This   : in out OTM8009A_Device;
                    S_Addr : UInt8;
                    Data   : DSI_Data)
   is
   begin
      This.DSI_IO_WriteCmd (S_Addr & Data);
   end Write;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This        : in out OTM8009A_Device;
      Color_Mode  : OTM8009A_Color_Mode;
      Orientation : LCD_Orientation)
   is
   begin
      --  Enable CMD2 to access vendor specific commands
      --  Enter in command 2 mode and set EXTC to enable address shift
      --  function (16#00#)
      This.Write (Address => 16#FF00#,
                  Data    => (16#80#, 16#09#, 16#01#));

      --  Enter ORISE Command 2
      This.Write (Address => 16#FF80#,
                  Data    => (16#80#, 16#09#));

      ----------------------------------------------------------------------
      --   SD_PCH_CTRL - 0xC480h - 129th parameter - Default 0x00         --
      --   Set SD_PT                                                      --
      --  -> Source output level during porch and non-display area to GND --
      This.Write (Address => 16#C480#,
                  Data    => (1 => 16#30#));
      This.Time.Delay_Milliseconds (10);
      --  Not documented...
      This.Write (Address => 16#C48A#,
                  Data    => (1 => 16#40#));
      This.Time.Delay_Milliseconds (10);
      ----------------------------------------------------------------------

      ----------------------------------------------------------------------
      --   PWR_CTRL4 - 0xC5B1h - 178th parameter - Default 0xA8           --
      --  Set gvdd_en_test                                                --
      --  -> enable GVDD test mode
      This.Write (Address => 16#C5B1#,
                  Data    => (1 => 16#A9#));
      ----------------------------------------------------------------------

      ----------------------------------------------------------------------
      --  PWR_CTRL2 - 0xC590h - 146th parameter - Default 0x79            --
      --  Set pump 4 vgh voltage                                          --
      --  -> from 15.0v down to 13.0v                                     --
      --  Set pump 5 vgh voltage                                          --
      --  -> from -12.0v downto -9.0v                                     --
      This.Write (Address => 16#C591#,
                  Data    => (1 => 16#34#));
      ----------------------------------------------------------------------

      --   P_DRV_M - 0xC0B4h - 181th parameter - Default 0x00
      --  -> Column inversion
      This.Write (Address => 16#C0B4#,
                  Data    => (1 => 16#50#));

      --  VCOMDC - 0xD900h - 1st parameter - Default 0x39h
      --  VCOM Voltage settings
      --  -> from - 1.0000v down to - 1.2625v
      This.Write (Address => 16#D900#,
                  Data    => (1 => 16#4E#));

      --  Oscillator adjustment for Idle/Normal mode (LPDT only) set to 65Hz
      --  (default is 60Hz)
      This.Write (Address => 16#C181#,
                  Data    => (1 => 16#66#));

      --  Video mode internal
      This.Write (Address => 16#C1A1#,
                  Data    => (1 => 16#08#));

      --  PWR_CTRL2 - 0xC590h - 147h parameter - Default 0x00
      --  Set pump 4&5 x6
      --  -> ONLY VALID when PUMP4_EN_ASDM_HV = "0"
      This.Write (Address => 16#C592#,
                  Data    => (1 => 16#01#));

      --  PWR_CTRL2 - 0xC590h - 150th parameter - Default 0x33h
      --  Change pump4 clock ratio
      --  -> from 1 line to 1/2 line
      This.Write (Address => 16#C595#,
                  Data    => (1 => 16#34#));

      --  GVDD/NGVDD settings
      This.Write (Address => 16#D800#,
                  Data    => (16#79#, 16#79#));

      --  PWR_CTRL2 - 0xC590h - 149th parameter - Default 0x33h
      --  Rewrite the default value !
      This.Write (Address => 16#C594#,
                  Data    => (1 => 16#33#));

      --  SD_CTRL = 0xC0A2h - 164th parameter - default 0x6h
      --  Panel display timing Setting 3
      This.Write (Address => 16#C0A3#,
                  Data    => (1 => 16#1B#));

      --  PWR_CTRL1 - 0xC580h - 131st parameter - Default 0x80h
      --  Power control 1
      This.Write (Address => 16#C582#,
                  Data    => (1 => 16#83#));

      --  SD_PCH_CTRL - 0xC480h - 130th parameter - default 84h
      --  Source driver precharge
      This.Write (Address => 16#C481#,
                  Data    => (1 => 16#83#));

      --  RGB Video mode setting:
      This.Write (Address => 16#C1A1#,
                  Data    => (1 => 16#0E#));

      --  Panel type setting:
      --  Normal panel: ZIGOPT=1, SIGSET=0
      This.Write (Address => 16#B3A6#,
                  Data    => (16#00#, 16#01#));

      --  GOAVST
      This.Write (Address => 16#CE80#,
                  Data    => (16#85#, 16#01#, 16#00#, 16#84#, 16#01#, 16#00#));

      --  GOACLKA1 setting
      This.Write (Address => 16#CEA0#,
                  Data    => (16#18#, 16#04#, 16#03#, 16#39#,
                              16#00#, 16#00#, 16#00#));
      --  GOACLKA2 setting
      This.Write (Address => 16#CEA7#,
                  Data    => (16#18#, 16#03#, 16#03#, 16#3A#,
                              16#00#, 16#00#, 16#00#));
      --  GOACLKA3 setting
      This.Write (Address => 16#CEB0#,
                  Data    => (16#18#, 16#02#, 16#03#, 16#3B#,
                              16#00#, 16#00#, 16#00#));
      --  GOACLKA4 setting
      This.Write (Address => 16#CEB7#,
                  Data    => (16#18#, 16#01#, 16#03#, 16#3C#,
                              16#00#, 16#00#, 16#00#));
      --  GOA ECLK setting
      This.Write (Address => 16#CFC0#,
                  Data    => (16#01#, 16#01#, 16#20#, 16#20#,
                              16#00#, 16#00#));
      --  GOA Other options
      This.Write (Address => 16#CFC6#,
                  Data    => (1 => 16#01#));
      --  GOA Signal toggle setting
      This.Write (Address => 16#CFC7#,
                  Data    => (16#02#, 16#00#, 16#00#));
      --  undocumented...
      This.Write (Address => 16#CFD0#,
                  Data    => (1 => 16#00#));
      This.Write (Address => 16#CB80#,
                  Data    => (1 .. 10 => 16#00#));
      This.Write (Address => 16#CB90#,
                  Data    => (1 .. 15 => 16#00#));
      This.Write (Address => 16#CBA0#,
                  Data    => (1 .. 15 => 16#00#));
      This.Write (Address => 16#CBB0#,
                  Data    => (1 .. 10 => 16#00#));
      This.Write (Address => 16#CBC0#,
                  Data    => (16#00#, 16#04#, 16#04#, 16#04#, 16#04#, 16#04#,
                              16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#));
      This.Write (Address => 16#CBD0#,
                  Data    => (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                              16#04#, 16#04#, 16#04#, 16#04#, 16#04#, 16#00#,
                              16#00#, 16#00#, 16#00#));
      This.Write (Address => 16#CBE0#,
                  Data    => (1 .. 10 => 16#00#));
      This.Write (Address => 16#CBF0#,
                  Data    => (1 .. 10 => 16#FF#));
      This.Write (Address => 16#CC80#,
                  Data    => (16#00#, 16#26#, 16#09#, 16#0B#, 16#01#, 16#25#,
                              16#00#, 16#00#, 16#00#, 16#00#));
      This.Write (Address => 16#CC90#,
                  Data    => (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#26#,
                              16#0A#, 16#0C#, 16#02#));
      This.Write (Address => 16#CCA0#,
                  Data    => (16#25#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#));
      This.Write (Address => 16#CCB0#,
                  Data    => (16#00#, 16#25#, 16#0C#, 16#0A#,
                              16#02#, 16#26#, 16#00#, 16#00#,
                              16#00#, 16#00#));
      This.Write (Address => 16#CCC0#,
                  Data    => (16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#25#,
                              16#0B#, 16#09#, 16#01#));
      This.Write (Address => 16#CCD0#,
                  Data    => (16#26#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#));

      -----------------------------------------------------------
      --  PWR_CTRL1 - 0xc580h - 130th parameter - default 0x00 --
      --  Pump 1 min and max DM                                --
      This.Write (Address => 16#C581#,
                  Data    => (1 => 16#66#));
      This.Write (Address => 16#F5B6#,
                  Data    => (1 => 16#06#));
      -----------------------------------------------------------

      --  Exit CMD2 mode
      This.Write (Address => 16#FF00#,
                  Data    => (16#FF#, 16#FF#, 16#FF#));

      ----------------------------------
      --  Standard DCS Initialization --
      ----------------------------------

      --  NOP - goes back to DCS std command ?
      This.Write (Address => CMD_NOP,
                  Data    => (1 .. 0 => <>));

      --  Gamma correction 2.2+ table (HSDT possible)
      This.Write (Address => 16#E100#,
                  Data    => (16#00#, 16#09#, 16#0F#, 16#0E#,
                              16#07#, 16#10#, 16#0B#, 16#0A#,
                              16#04#, 16#07#, 16#0B#, 16#08#,
                              16#0F#, 16#10#, 16#0A#, 16#01#));

      --  Gamma correction 2.2- table (HSDT possible)
      This.Write (Address => 16#E200#,
                  Data    => (16#00#, 16#09#, 16#0F#, 16#0E#,
                              16#07#, 16#10#, 16#0B#, 16#0A#,
                              16#04#, 16#07#, 16#0B#, 16#08#,
                              16#0F#, 16#10#, 16#0A#, 16#01#));

      --  Send Sleep Out command to display : no parameter
      This.Write (S_Addr => CMD_SLPOUT,
                  Data   => (1 .. 0 => <>));

      --  Wait for Sleep Out exit
      This.Time.Delay_Milliseconds (120);

      case Color_Mode is
         when RGB565 =>
            This.Write (S_Addr => CMD_COLMOD,
                        Data   => (1 => COLMOD_RGB565));
         when RGB888 =>
            This.Write (S_Addr => CMD_COLMOD,
                        Data   => (1 => COLMOD_RGB888));
      end case;

      if Orientation = Landscape then
         --  Send command to configure display in landscape orientation
         --  mode. By default the orientation mode is portrait
         This.Write (S_Addr => CMD_MADCTR,
                     Data   => (1 => MADCTR_MODE_LANDSCAPE));
         --  CASET value (Column Address Set) : X direction LCD GRAM
         --  boundaries depending on LCD orientation mode and PASET value (Page
         --  Address Set) : Y direction LCD GRAM boundaries depending on LCD
         --  orientation mode
         --  XS[15:0] = 16#000 = 0, XE[15:0] = 16#31F = 799 for landscape mode
         --   apply to CASET
         --  YS[15:0] = 16#000 = 0, YE[15:0] = 16#1DF = 479 for landscape mode
         --   apply to PASET
         This.Write (S_Addr => CMD_CASET,
                     Data   => (16#00#, 16#00#, 16#03#, 16#1F#));
         This.Write (S_Addr => CMD_PASET,
                     Data   => (16#00#, 16#00#, 16#01#, 16#DF#));
      else
         This.Write (S_Addr => CMD_MADCTR,
                     Data   => (1 => MADCTR_MODE_PORTRAIT));
         This.Write (S_Addr => CMD_CASET,
                     Data   => (16#00#, 16#00#, 16#01#, 16#DF#));
         This.Write (S_Addr => CMD_PASET,
                     Data   => (16#00#, 16#00#, 16#03#, 16#1F#));
      end if;

      --------------------------------------------------------------
      --  CABC : Content Adaptive Backlight Control section start --
      --------------------------------------------------------------
      --  Note :
      --    defaut is 0 (lowest Brightness),
      --    0xFF is highest Brightness,
      This.Write (S_Addr => CMD_WRDISBV, Data => (1 => 16#FF#));

      --  defaut is 0, try 0x2C - Brightness Control Block, Display Dimming
      --  & BackLight on
      This.Write (S_Addr => CMD_WRCTRLD, Data => (1 => 16#2C#));

      --  defaut is 0, try 0x02 - image Content based Adaptive Brightness
      --  [Still Picture]
      This.Write (S_Addr => CMD_WRCABC, Data => (1 => 16#02#));

      --  defaut is 0 (lowest Brightness), 0xFF is highest Brightness
      This.Write (S_Addr => CMD_WRCABCMB, Data => (1 => 16#FF#));

      ------------------------------------------------------------
      --  CABC : Content Adaptive Backlight Control section end --
      ------------------------------------------------------------

      --  Send Command Display On
      This.Write (S_Addr => CMD_DISPON, Data => (1 => 16#00#));

      --  NOP command
      This.Write (S_Addr => CMD_NOP, Data => (1 => 16#00#));

      --  Send Command GRAM memory write (no parameters) : this initiates
      --  frame write via other DSI commands sent by DSI host from LTDC
      --  incoming pixels in video mode
      This.Write (S_Addr => CMD_RAMWR, Data => (1 => 16#00#));
   end Initialize;

   ---------------------
   -- DSI_IO_WriteCmd --
   ---------------------

   procedure DSI_IO_WriteCmd (This : in out OTM8009A_Device;
                              Data : HAL.DSI.DSI_Data)
   is
   begin
      if Data'Length = 0 then
         return;
      elsif Data'Length = 1 then
         This.DSI_Host.DSI_Short_Write (This.Channel_ID,
                                        Mode   => DCS_Short_Pkt_Write_P0,
                                        Param1 => Data (1),
                                        Param2 => 16#00#);
      elsif Data'Length = 2 then
         This.DSI_Host.DSI_Short_Write (This.Channel_ID,
                                        Mode   => DCS_Short_Pkt_Write_P1,
                                        Param1 => Data (1),
                                        Param2 => Data (2));
      else
         This.DSI_Host.DSI_Long_Write
           (This.Channel_ID,
            Mode       => DCS_Long_Pkt_Write,
            Param1     => Data (Data'First),
            Parameters => Data (Data'First + 1 .. Data'Last));
      end if;
   end DSI_IO_WriteCmd;

end OTM8009A;
