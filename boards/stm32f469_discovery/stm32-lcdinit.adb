------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f469i_discovery_lcd.c                                          --
--   @author  MCD Application Team                                          --
--   @version V1.0.2                                                        --
--   @date    13-January-2016                                              --
--                                                                          --
--   COPYRIGHT(c) 2016 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Real_Time;        use Ada.Real_Time;
with Ada.Unchecked_Conversion;

with STM32.Board;          use STM32.Board;
with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with STM32.DSI;            use STM32.DSI;

with STM32_SVD.DSIHOST;    use STM32_SVD.DSIHOST;
with STM32_SVD.RCC;        use STM32_SVD.RCC;
with STM32_SVD.LTDC;       use STM32_SVD.LTDC;
with HAL.DSI;
with OTM8009A;

package body STM32.LCDInit is

   LCD_Channel : constant HAL.DSI.DSI_Virtual_Channel_ID := 0;
   --  Only one display on this board, constant to 0

   LCD_Display : OTM8009A.OTM8009A_Device (DSI_Host   => DSI_1'Access,
                                           Channel_Id => LCD_Channel);

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
   begin
      LCD_Reset;

      --  Init clocks on DSI, LTDC and DMA2D
      RCC_Periph.APB2ENR.LTDCEN := True;
      RCC_Periph.APB2RSTR.LTDCRST := True;
      RCC_Periph.APB2RSTR.LTDCRST := False;

      RCC_Periph.AHB1ENR.DMA2DEN := True;
      RCC_Periph.AHB1RSTR.DMA2DRST := True;
      RCC_Periph.AHB1RSTR.DMA2DRST := False;

      RCC_Periph.APB2ENR.DSIEN := True;
      RCC_Periph.APB2RSTR.DSIRST := True;
      RCC_Periph.APB2RSTR.DSIRST := False;
   end Pre_LTDC_Initialize;

   -------------------
   -- LCD_Pins_Init --
   -------------------

   procedure Post_LTDC_Initialize (Orientation : LCD_Orientation)
   is
      LCD_Clock_kHz       : constant := 1_000 * PLLSAIN / PLLSAIR / PLLSAI_DIVR;
      Lane_Byte_Clock_kHz : constant := 500_000 / 8;
   begin
      --  Now setup the DSI

      DSI_1.DSI_Deinit;

      --  HSE input: 25MHz / IN_Div * N_Div => 1000 MHz = VCO
      --  VCO / ODF => 500 MHz
      DSI_1.DSI_Initialize
        (Auto_Clock_Lane_Control  => True,
         TX_Escape_Clock_Division => 3, -- 62500 / 4 = 15625 kHz < 20kHz (max)
         Number_Of_Lanes          => Two_Data_Lanes,
         PLL_N_Div                => 125,
         PLL_IN_Div               => PLL_IN_DIV2,
         PLL_OUT_Div              => PLL_OUT_DIV1);

      DSI_1.DSI_Setup_Video_Mode
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
         HSync_Active_Duration       => UInt13 (Word (HSYNC * Lane_Byte_Clock_kHz) / LCD_Clock_kHz),
         Horizontal_BackPorch        => UInt13 (Word (HBP * Lane_Byte_Clock_kHz) / LCD_Clock_kHz),
         Horizontal_Line             => UInt15 (Word ((LCD_WIDTH + HSYNC + HBP + HFP) * Lane_Byte_Clock_kHz) / LCD_Clock_kHz),
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
         Frame_BTA_Ack_Enable        => True);

      --  Enable the DSI Host and Wrapper
      DSI_1.DSI_Start;

      --  LCD panel init
      pragma Warnings (Off, "condition is always *");
      LCD_Display.Initialize
        ((if DSI_LCD_Color_Mode = RGB565
          then OTM8009A.RGB565
          else OTM8009A.RGB888),
         (if Orientation = Landscape
          then OTM8009A.Landscape
          else OTM8009A.Portrait));
      pragma Warnings (On, "condition is always *");
   end Post_LTDC_Initialize;

end STM32.LCDInit;
