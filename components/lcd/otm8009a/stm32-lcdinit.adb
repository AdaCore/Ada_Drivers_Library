with Ada.Real_Time;        use Ada.Real_Time;
--  with Ada.Interrupts.Names;
with Ada.Unchecked_Conversion;

with STM32.Board;          use STM32.Board;
with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with STM32.DSI;            use STM32.DSI;

with STM32_SVD.DSIHOST;    use STM32_SVD.DSIHOST;
with STM32_SVD.RCC;        use STM32_SVD.RCC;
with STM32_SVD.LTDC;       use STM32_SVD.LTDC;

with OTM8009A;

package body STM32.LCDInit is

   LCD_Channel : constant DSI_Virtual_Channel_ID := 0;
   --  Only one display on this board, constant to 0

--     type Area is (Left, Right);
--
--     Col_Left    : constant DSI_Data := (16#00#, 16#00#, 16#01#, 16#8F#);
--     -- 0 -> 399
--
--     Col_Right    : constant DSI_Data := (16#01#, 16#90#, 16#03#, 16#1F#);
--     -- 400 -> 799
--
--     Page         : constant DSI_Data := (16#00#, 16#00#, 16#01#, 16#DF#);
--     --  Full height: 0 -> 399
--
--     Sync_Left    : constant DSI_Data := (16#02#, 16#15#);
--     --  Scan @ 533

   procedure DSI_IO_WriteCmd (Data : DSI_Data);

   package LCD_Display is new OTM8009A
     (DSI_Data, DSI_IO_WriteCmd);

--     type Layer_Buffer_Array is array (Bit) of System.Address;
--
--     protected DSI_Interrupt_Handler
--     is
--        procedure Set_Pending
--          (Layer  : Bit;
--           Buffer : System.Address);
--        entry Wait_Buffer;
--
--     private
--        procedure Interrupt;
--        pragma Attach_Handler (Interrupt, Ada.Interrupts.Names.DSI_Interrupt);
--
--        Active_Area : Area := Left;
--
--        Buffers : Layer_Buffer_Array := (others => System.Null_Address);
--        Pending : Boolean := False;
--     end DSI_Interrupt_Handler;
--
--     protected body DSI_Interrupt_Handler is
--        ----------------
--        -- Set_Buffer --
--        ----------------
--
--        procedure Set_Pending
--          (Layer  : Bit;
--           Buffer : System.Address)
--        is
--        begin
--           Pending := True;
--           Buffers (Layer) := Buffer;
--        end Set_Pending;
--
--        -----------------
--        -- Wait_Buffer --
--        -----------------
--
--        entry Wait_Buffer when not Pending is
--        begin
--           null;
--        end Wait_Buffer;
--
--        ---------------
--        -- Interrupt --
--        ---------------
--
--        procedure Interrupt
--        is
--        begin
--           if DSIHOST_Periph.DSI_WISR.ERIF = 1 then
--              DSIHOST_Periph.DSI_WIFCR.CERIF := 1;
--
--              if Active_Area = Left then
--                 --  Mask the Tear Off
--                 DSI_Short_Write (LCD_Channel, DCS_Short_Pkt_Write_P1,
--                                  LCD_Display.CMD_TEOFF, 0);
--
--                 --  Disable the DSI wrapper
--                 DSIHOST_Periph.DSI_WCR.DSIEN := 0;
--
--                 LTDC_Periph.L1CFBAR := LTDC_Periph.L1CFBAR + 400 * 4;
--                 LTDC_Periph.L2CFBAR := LTDC_Periph.L2CFBAR + 400 * 4;
--                 LTDC_Periph.SRCR.IMR := 1;
--
--                 --  Enable the DSI wrapper
--                 DSIHOST_Periph.DSI_WCR.DSIEN := 1;
--
--                 DSI_Long_Write (LCD_Channel,
--                                 DCS_Long_Pkt_Write,
--                                 LCD_Display.CMD_CASET,
--                                 Col_Right);
--                 DSI_Refresh;
--                 Active_Area := Right;
--
--              else
--                 --  Disable the DSI wrapper
--                 DSIHOST_Periph.DSI_WCR.DSIEN := 0;
--
--                 LTDC_Periph.L1CFBAR := LTDC_Periph.L1CFBAR - 400 * 4;
--                 LTDC_Periph.L2CFBAR := LTDC_Periph.L2CFBAR - 400 * 4;
--                 LTDC_Periph.SRCR.IMR := 1;
--
--                 --  Enable the DSI wrapper
--                 DSIHOST_Periph.DSI_WCR.DSIEN := 1;
--
--                 DSI_Long_Write (LCD_Channel,
--                                 DCS_Long_Pkt_Write,
--                                 LCD_Display.CMD_CASET,
--                                 Col_Left);
--
--                 Active_Area := Left;
--              end if;
--           end if;
--        end Interrupt;
--     end DSI_Interrupt_Handler;

--     ----------------
--     -- Set_Buffer --
--     ----------------
--
--     procedure Set_Buffer (Layer  : Bit;
--                           Buffer : System.Address)
--     is
--     begin
--        DSI_Interrupt_Handler.Set_Pending (Layer, Buffer);
--     end Set_Buffer;
--
--     -----------------
--     -- Wait_Buffer --
--     -----------------
--
--     procedure Wait_Buffer
--     is
--     begin
--        DSI_Long_Write (LCD_Channel,
--                        DCS_Long_Pkt_Write,
--                        LCD_Display.CMD_WRTESCN,
--                        Sync_Left);
--  --        DSI.DSI_Refresh;
--        DSI_Interrupt_Handler.Wait_Buffer;
--     end Wait_Buffer;

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
                         Param1     => Data (Data'First),
                         Parameters => Data (Data'First + 1 .. Data'Last));
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

      --  Activate XRES active low
      Clear (LCD_XRES);

      delay until Clock + Microseconds (20);

      Set (LCD_XRES);

      delay until Clock + Microseconds (10);
   end LCD_Reset;

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

   procedure Pre_LTDC_Initialize
   is
      LCD_Clock_kHz       : constant := 1_000 * PLLSAIN / PLLSAIR / PLLSAI_DIVR;
      Lane_Byte_Clock_kHz : constant := 500_000 / 8;
   begin
      LCD_Reset;

      RCC_Periph.AHB1ENR.CRCEN := 1;

--        Enable_Clock (LCD_BL_CTRL);
--        Configure_IO (LCD_BL_CTRL,
--                      (Mode        => Mode_Out,
--                       Output_Type => Open_Drain,
--                       Speed       => Speed_Low,
--                       Resistors   => Floating));
--        Set (LCD_BL_CTRL);

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
        (Auto_Clock_Lane_Control  => True,
         TX_Escape_Clock_Division => 4, -- 62500 / 4 = 15625 kHz < 20kHz (max)
         Number_Of_Lanes          => Two_Data_Lanes,
         PLL_N_Div                => 125,
         PLL_IN_Div               => PLL_IN_DIV2,
         PLL_OUT_Div              => PLL_OUT_DIV1);

      STM32.DSI.DSI_Setup_Video_Mode
        (Virtual_Channel             => LCD_Channel,
         Color_Coding                => DSI_LCD_Color_Mode,
         Loosely_Packed              => False,
         Video_Mode                  => Video_Mode_Burst,
         Packet_Size                 => LCD_WIDTH,
         Number_Of_Chunks            => 0,
         Null_Packet_Size            => 16#FFF#,
         HSync_Polarity              => Active_High,
         VSync_Polarity              => Active_High,
         DataEn_Polarity             => Active_High,
         HSync_Active_Duration       => UInt13 (Word (HSYNC * Lane_Byte_Clock_kHz / LCD_Clock_kHz)),
         Horizontal_BackPorch        => UInt13 (Word (HBP * Lane_Byte_Clock_kHz / LCD_Clock_kHz)),
         Horizontal_Line             => UInt15 (Word (HSYNC + LCD_WIDTH + HBP + HFP) * Lane_Byte_Clock_kHz / LCD_Clock_kHz),
         VSync_Active_Duration       => VSYNC,
         Vertical_BackPorch          => VBP,
         Vertical_FrontPorch         => VFP,
         Vertical_Active             => LCD_HEIGHT,
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
--        STM32.DSI.DSI_Setup_Adapted_Command_Mode
--          (Virtual_Channel         => LCD_Channel,
--           Color_Coding            => DSI_LCD_Color_Mode,
--           Command_Size            => LCD_WIDTH / 2,
--           Tearing_Effect_Source   => TE_DSI_Link,
--           Tearing_Effect_Polarity => Rising_Edge,
--           HSync_Polarity          => Active_High,
--           VSync_Polarity          => Active_High,
--           DataEn_Polarity         => Active_High,
--           VSync_Edge              => Falling_Edge,
--           Automatic_Refresh       => True,
--           TE_Acknowledge_Request  => True);
--        STM32.DSI.DSI_Setup_Command
--          (LP_Gen_Short_Write_No_P  => True,
--           LP_Gen_Short_Write_One_P => True,
--           LP_Gen_Short_Write_Two_P => True,
--           LP_Gen_Short_Read_No_P   => True,
--           LP_Gen_Short_Read_One_P  => True,
--           LP_Gen_Short_Read_Two_P  => True,
--           LP_Gen_Long_Write        => True,
--           LP_DCS_Short_Write_No_P  => True,
--           LP_DCS_Short_Write_One_P => True,
--           LP_DCS_Short_Read_No_P   => True,
--           LP_DCS_Long_Write        => True,
--           LP_Max_Read_Packet       => False,
--           Acknowledge_Request      => False);
   end Pre_LTDC_Initialize;

   -------------------
   -- LCD_Pins_Init --
   -------------------

   procedure Post_LTDC_Initialize (Orientation : LCD_Orientation)
   is
   begin
      --  Enable the DSI Host and Wrapper, before activating the LTDC
      STM32.DSI.DSI_Start;

--        DSIHOST_Periph.DSI_WIFCR.CTEIF := 1;
--        DSIHOST_Periph.DSI_WIER.TEIE := 0;
--        DSIHOST_Periph.DSI_WIER.ERIE := 1;

      --  LCD panel init
      pragma Warnings (Off, "condition is always *");
      LCD_Display.Initialize
        ((if DSI_LCD_Color_Mode = RGB565
         then LCD_Display.RGB565
         else LCD_Display.RGB888),
         (if Orientation = Landscape
          then LCD_Display.Landscape
          else LCD_Display.Portrait));
      pragma Warnings (On, "condition is always *");


--        STM32.DSI.DSI_Setup_Command
--          (LP_Gen_Short_Write_No_P  => False,
--           LP_Gen_Short_Write_One_P => False,
--           LP_Gen_Short_Write_Two_P => False,
--           LP_Gen_Short_Read_No_P   => False,
--           LP_Gen_Short_Read_One_P  => False,
--           LP_Gen_Short_Read_Two_P  => False,
--           LP_Gen_Long_Write        => False,
--           LP_DCS_Short_Write_No_P  => False,
--           LP_DCS_Short_Write_One_P => False,
--           LP_DCS_Short_Read_No_P   => False,
--           LP_DCS_Long_Write        => False,
--           LP_Max_Read_Packet       => False,
--           Acknowledge_Request      => False);
--
--        STM32.DSI.DSI_Setup_Flow_Control (STM32.DSI.Flow_Control_BTA);
--
--        STM32.DSI.DSI_Refresh;
--
--        DSI_Long_Write (LCD_Channel,
--                        DCS_Long_Pkt_Write,
--                        LCD_Display.CMD_CASET,
--                        Col_Left);
--        DSI_Long_Write (LCD_Channel,
--                        DCS_Long_Pkt_Write,
--                        LCD_Display.CMD_PASET,
--                        Page);
   end Post_LTDC_Initialize;

end STM32.LCDInit;
