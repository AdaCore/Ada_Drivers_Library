with Interfaces; use Interfaces;
with HAL;        use HAL;
with HAL.DSI;

package OTM8009A is

--  List of OTM8009A commands
--  Detailed in OTM8009A data Sheet
   CMD_NOP       : constant := 16#00#;
   CMD_SWRESET   : constant := 16#01#;
   CMD_RDDMADCTL : constant := 16#0B#; --  Read memory display access ctrl
   CMD_RDDCOLMOD : constant := 16#0C#; --  Read display pixel format
   CMD_SLPIN     : constant := 16#10#; --  Sleep In command
   CMD_SLPOUT    : constant := 16#11#; --  Sleep Out command
   CMD_PTLON     : constant := 16#12#; --  Partial mode On command
   CMD_DISPOFF   : constant := 16#28#; --  Display OFF command
   CMD_DISPON    : constant := 16#29#; --  Display OFF command
   CMD_CASET     : constant := 16#2A#; --  Column address set command
   CMD_PASET     : constant := 16#2B#; --  Page address set command
   CMD_RAMWR     : constant := 16#2C#; --  Memory (GRAM) write command
   CMD_RAMRD     : constant := 16#2E#; --  Memory (GRAM) read command
   CMD_PLTAR     : constant := 16#30#; --  Partial area command (4 parameters)
   CMD_TEOFF     : constant := 16#34#; --  Tearing Effect Line off command
   CMD_TEEON     : constant := 16#35#; --  Tearing Effect Line on command

   --  Parameter for CMD_TEEON
   TEEON_VBLANKING_INFO_ONLY          : constant := 0;
   TEEON_VBLANKING_AND_HBLANKING_INFO : constant := 1;

   CMD_MADCTR    : constant := 16#36#; --  Memory Access write control command
   --  Parameter for MADCTR
   MADCTR_MODE_PORTRAIT  : constant := 16#00#;
   MADCTR_MODE_LANDSCAPE : constant := 16#60#;

   CMD_IDMOFF    : constant := 16#38#; --  Idle mode Off command
   CMD_IDMON     : constant := 16#39#; --  Idle mode On command

   CMD_COLMOD    : constant := 16#3A#; --  Interface pixel format command
   COLMOD_RGB565 : constant := 16#55#;
   COLMOD_RGB888 : constant := 16#77#;

   CMD_RAMWRC    : constant := 16#3C#; --  Memory write continue command
   CMD_RAMRDC    : constant := 16#3E#; --  Memory read continue command
   CMD_WRTESCN   : constant := 16#44#; --  Write Tearing Effect Scan line
   CMD_RDSCNL    : constant := 16#45#; --  Read  Tearing Effect Scan line
   CMD_WRDISBV   : constant := 16#51#; --  Write Display Brightness command
   CMD_WRCTRLD   : constant := 16#53#; --  Write CTRL Display command
   CMD_WRCABC    : constant := 16#55#; --  Write Content Adaptive Brightness
   CMD_WRCABCMB  : constant := 16#5E#; --  Write CABC minimum brightness

   FREQUENCY_DIVIDER : constant := 2; --  LCD frequency divider

   type OTM8009A_Color_Mode is (RGB565, RGB888);

   type LCD_Orientation is (Portrait, Landscape);

   type OTM8009A_Device (DSI_Host   : HAL.DSI.DSI_Port_Ref;
                         Channel_ID : HAL.DSI.DSI_Virtual_Channel_ID) is
     tagged limited private;

   procedure Initialize
     (This        : in out OTM8009A_Device;
      Color_Mode  : OTM8009A_Color_Mode;
      Orientation : LCD_Orientation);

private
   type OTM8009A_Device (DSI_Host   : HAL.DSI.DSI_Port_Ref;
                         Channel_ID : HAL.DSI.DSI_Virtual_Channel_ID) is
     tagged limited record
      Current_Shift : Byte := 0;
   end record;

   procedure DSI_IO_WriteCmd (This : in out OTM8009A_Device;
                              Data : HAL.DSI.DSI_Data);

   procedure Write (This    : in out OTM8009A_Device;
                    Address : Unsigned_16;
                    Data    : HAL.DSI.DSI_Data);

   procedure Write (This   : in out OTM8009A_Device;
                    S_Addr : Byte;
                    Data   : HAL.DSI.DSI_Data);

end OTM8009A;
