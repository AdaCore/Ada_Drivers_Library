with Ada.Real_Time; use Ada.Real_Time;

with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with STM32.DSI;     use STM32.DSI;

with STM32_SVD.RCC; use STM32_SVD.RCC;

with OTM8009A;

package body STM32.LCDInit is

   LCD_Channel : constant DSI_Virtual_Channel_ID := 0;
   --  Only one display on this board, constant to 0

   procedure DSI_IO_WriteCmd (Data : DSI_Data);

   package LCD_Display is new OTM8009A
     (DSI_Data, DSI_IO_WriteCmd);

   ---------------------
   -- DSI_IO_WriteCmd --
   ---------------------

   procedure DSI_IO_WriteCmd (Data : DSI_Data)
   is
   begin
      if Data'Length = 0 then
         return;
      elsif Data'Length = 1 then
         DSI_Short_Write (LCD_Channel,
                          Mode   => DCS_Short_Pkt_Write_P0,
                          Param1 => Data (1),
                          Param2 => 16#00#);
      elsif Data'Length = 2 then
         DSI_Short_Write (LCD_Channel,
                          Mode   => DCS_Short_Pkt_Write_P1,
                          Param1 => Data (1),
                          Param2 => Data (2));
      else
         DSI_Long_Write (LCD_Channel,
                         Mode       => DCS_Long_Pkt_Write,
                         Param1     => Data (Data'Last),
                         Parameters => Data (Data'First .. Data'Last - 1));
      end if;
   end DSI_IO_WriteCmd;

   ---------------
   -- LCD_Reset --
   ---------------

   procedure LCD_Reset is
   begin
      Enable_Clock (LCD_XRES);
      Configure_IO (LCD_XRES,
                    (Mode        => Mode_Out,
                     Output_Type => Open_Drain,
                     Speed       => Speed_50MHz,
                     Resistors   => Floating));
--        Enable_Clock (LCD_BL_CTRL);
--        Configure_IO (LCD_BL_CTRL,
--                      (Mode        => Mode_Out,
--                       Output_Type => Push_Pull,
--                       Speed       => Speed_50MHz,
--                       Resistors   => Pull_Down));

      --  Activate XRES active low
      Reset (LCD_XRES);

      delay until Clock + Milliseconds (20);

      Set (LCD_XRES);

      delay until Clock + Milliseconds (10);

      Reset (LCD_BL_CTRL);
--        Set (LCD_BL_CTRL);
   end LCD_Reset;

   ---------------------
   -- Default_Preinit --
   ---------------------

   procedure Default_Preinit is
   begin
      Pre_LTDC_Initialize (Default_Orientation);
   end Default_Preinit;

   ----------------------
   -- Default_Postinit --
   ----------------------

   procedure Default_Postinit is
   begin
      Post_LTDC_Initialize (Default_Orientation);
   end Default_Postinit;

   -------------------------
   -- Pre_LTDC_Initialize --
   -------------------------

   procedure Pre_LTDC_Initialize (Orientation : LCD_Orientation)
   is
      Lane_Byte_Clk_kHz : constant := 62500;
      LCD_Clock_kHz     : constant := 27429;
      Clk_Ratio         : constant Word := Lane_Byte_Clk_kHz / LCD_Clock_kHz;
      X_Size            : constant UInt15 := (if Orientation = Landscape
                                              then LCD_WIDTH else LCD_HEIGHT);
      Y_Size            : constant UInt14 := (if Orientation = Landscape
                                              then LCD_HEIGHT else LCD_WIDTH);

   begin
      LCD_Reset;

      --  Init clocks on DSI, LTDC and DMA2D
      RCC_Periph.APB2ENR.LTDCEN := 1;
      RCC_Periph.APB2RSTR.LTDCRST := 1;
      RCC_Periph.APB2RSTR.LTDCRST := 0;

      RCC_Periph.AHB1ENR.DMA2DEN := 1;
      RCC_Periph.AHB1RSTR.DMA2DRST := 1;
      RCC_Periph.AHB1RSTR.DMA2DRST := 0;

      RCC_Periph.APB2ENR.DSIEN := 1;
      RCC_Periph.APB2RSTR.DSIRST := 1;
      RCC_Periph.APB2RSTR.DSIRST := 0;

      --  Now setup the DSI

      STM32.DSI.DSI_Deinit;

      --  HSE input: 25MHz / IN_Div * N_Div => 1000 MHz = VCO
      --  VCO / ODF => 500 MHz
      STM32.DSI.DSI_Initialize
        (Auto_Clock_Lane_Control  => False,
         TX_Escape_Clock_Division => Byte (Lane_Byte_Clk_kHz / 15_620),
         Number_Of_Lanes          => Two_Data_Lanes,
         PLL_N_Div                => 125,
         PLL_IN_Div               => PLL_IN_DIV2,
         PLL_OUT_Div              => PLL_OUT_DIV1);

      STM32.DSI.DSI_Setup_Video
        (Virtual_Channel             => LCD_Channel, --  Only one display
         Color_Coding                => RGB888,
         Loosely_Packed              => False,
         Video_Mode                  => Video_Mode_Burst,
         Packet_Size                 => X_Size,
         Number_Of_Chunks            => 0,
         Null_Packet_Size            => 16#FFF#,
         HSync_Polarity              => Active_High,
         VSync_Polarity              => Active_High,
         DataEn_Polarity             => Active_High,
         HSync_Active_Duration       => UInt13 (HSYNC * Clk_Ratio),
         HSync_BackPorch             => UInt13 (HBP   * Clk_Ratio),
         HLine_Duration              => UInt15 (Word (X_Size + HSYNC + HBP + HFP) * Clk_Ratio),
         VSync_Active_Duration       => VSYNC,
         VSync_BackPorch             => VBP,
         VSync_FrontPorch            => VFP,
         Vertical_Active_Duration    => Y_Size,
         LP_Command_Enabled          => True,
         LP_Largest_Packet_Size      => 64,
         LP_VACT_Largest_Packet_Size => 64,
         LP_H_Front_Porch_Enable     => True,
         LP_H_Back_Porch_Enable      => True,
         LP_V_Active_Enable          => True,
         LP_V_Front_Porch_Enable     => True,
         LP_V_Back_Porch_Enable      => True,
         LP_V_Sync_Active_Enable     => True,
         Frame_BTA_Ack_Enable        => False);

      --  Enable the DSI Host and Wrapper, before activating the LTDC
      STM32.DSI.DSI_Start;
   end Pre_LTDC_Initialize;

   -------------------
   -- LCD_Pins_Init --
   -------------------

   procedure Post_LTDC_Initialize (Orientation : LCD_Orientation)
   is
   begin
      --  LCD panel init
      LCD_Display.Initialize
        (LCD_Display.RGB888,
         (if Orientation = Landscape
          then LCD_Display.Landscape
          else LCD_Display.Portrait));
   end Post_LTDC_Initialize;

end STM32.LCDInit;
