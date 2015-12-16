------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--   @file    stm32f429i_discovery_lcd.h                                    --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file contains all the functions prototypes for the       --
--            stm32f429i_discovery_lcd.c driver.                            --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with System;
with STM32.GPIO; use STM32.GPIO;
with STM32.SPI;  use STM32.SPI;
with STM32.RCC;  use STM32.RCC;

with STM32_Board;  use STM32_Board;

package STM32.LTDC is

   LCD_PIXEL_WIDTH  : constant := 240;
   LCD_PIXEL_HEIGHT : constant := 320;

   type LCD_Layer is (Layer1, Layer2);

   subtype Width is Natural range 0 .. (LCD_PIXEL_WIDTH - 1);
   subtype Height is Natural range 0 .. (LCD_PIXEL_HEIGHT - 1);

   subtype Pixel is Half_Word;

   Black      : constant Pixel := 16#8000#;
   White      : constant Pixel := 16#FFFF#;
   Red        : constant Pixel := 2**15 or (31 * (2**10));
   Green      : constant Pixel := 2**15 or (31 * (2**5));
   Blue       : constant Pixel := 2**15 or 31;
   Gray       : constant Pixel := 2**15 or 23 * (2**10) or 23 * (2**5) or 23;
   Light_Gray : constant Pixel := 2**15 or 30 * (2**10) or 30 * (2**5) or 30;
   Sky_Blue   : constant Pixel := 2**15 or 19 * (2**17) or 26 * (2**5) or 31;
   Yellow     : constant Pixel := 2**15 or 31 * (2**10) or 31 * (2**5);
   Orange     : constant Pixel := 2**15 or 31 * (2**10) or 21 * (2**5);
   Pink       : constant Pixel := 2**15 or 31 * (2**10) or 13 * (2**5) or 23;
   Violet     : constant Pixel := 2**15 or 19 * (2**10) or 6 * (2**5) or 26;

   type Frame_Buffer_Range is
     range 0 .. (LCD_PIXEL_HEIGHT * LCD_PIXEL_WIDTH) - 1;

   type Frame_Buffer is array (Frame_Buffer_Range) of Pixel
     with Pack, Volatile;

   type Frame_Buffer_Access is not null access all Frame_Buffer;

   --  Pixel_Fmt_ARGB8888 : constant := 2#000#;
   --  Pixel_Fmt_RGB888   : constant := 2#001#;
   Pixel_Fmt_RGB565   : constant := 2#010#;
   Pixel_Fmt_ARGB1555 : constant := 2#011#;
   Pixel_Fmt_ARGB4444 : constant := 2#100#;
   --  Pixel_Fmt_L8       : constant := 2#101#;
   --  Pixel_Fmt_AL44     : constant := 2#110#;
   --  Pixel_Fmt_AL88     : constant := 2#111#;

   BF1_Constant_Alpha : constant := 2#100#;
   BF1_Pixel_Alpha    : constant := 2#110#;
   BF2_Constant_Alpha : constant := 2#101#;
   BF2_Pixel_Alpha    : constant := 2#111#;

   Default_Pixel_Fmt  : constant := Pixel_Fmt_ARGB1555;

   type Layer_State is (Enabled, Disabled);

   procedure Initialize;

   procedure Initialize_Layer
     (Layer             : LCD_Layer;
      Pixel_Fmt         : Word;
      Blending_Factor_1 : Bits_3;
      Blending_Factor_2 : Bits_3);

   procedure Set_Background (R, G, B : Byte);

   procedure Set_Layer_State
     (Layer : LCD_Layer;
      State : Layer_State);

   function Current_Frame_Buffer
     (Layer : LCD_Layer)
      return Frame_Buffer_Access;

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height;
      Value : Pixel);

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height)
      return Pixel;

   procedure Reload_Config (Immediate : Boolean := True);

private

   NCS_GPIO : GPIO_Port renames GPIO_C;
   WRX_GPIO : GPIO_Port renames GPIO_D;

   NCS_Pin  : GPIO_Pin renames Pin_2;
   WRX_Pin  : GPIO_Pin renames Pin_13;

   SCK_GPIO  : GPIO_Port renames GPIO_F;
   MISO_GPIO : GPIO_Port renames GPIO_F;
   MOSI_GPIO : GPIO_Port renames GPIO_F;

   SCK_Pin   : GPIO_Pin renames Pin_7;
   MISO_Pin  : GPIO_Pin renames Pin_8;
   MOSI_Pin  : GPIO_Pin renames Pin_9;

   SCK_AF  : GPIO_Alternate_Function renames GPIO_AF_SPI5;
   MISO_AF : GPIO_Alternate_Function renames GPIO_AF_SPI5;
   MOSI_AF : GPIO_Alternate_Function renames GPIO_AF_SPI5;

   LCD_SPI : SPI_Port renames SPI_5;

   --  Layer Control Register
   type LC_Registers is record
      Len        : Bits_1; --  Layer Enable
      Colken     : Bits_1; --  Color Keying Enable
      Reserved_1 : Bits_2;
      Cluten     : Bits_1; --  Color Look-Up Table Enable
      Reserved_2 : Bits_27;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Layerx Window Horizontal Position Configuration Register
   type LWHPC_Registers is record
      Horizontal_Start : Bits_12; --  Window Horizontal Start Position
      Reserved_1       : Bits_4;
      Horizontal_Stop  : Bits_12; --  Window Horizontal Stop Position
      Reserved_2       : Bits_4;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Layerx Window Vertical Position Configuration Register
   type LWVPC_Registers is record
      Vertical_Start : Bits_11; --  Window Vertical Start Position
      Reserved_1     : Bits_5;
      Vertical_Stop  : Bits_11; --  Window Vertical Stop Position
      Reserved_2     : Bits_5;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Layerx Color Keying Configuration Register
   type LCKC_Registers is record
      CKBlue     : Byte;
      CKGreen    : Byte;
      CKRed      : Byte;
      Reserved_1 : Byte;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Layerx Pixel Format Configuration Register
   subtype LPFC_Register is Word;

   --  Layer Constant Alpha Configuration Register
   type LCAC_Registers is record
      CONSTA   : Byte;
      Reserved : Bits_24;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Layer Default Color Configuration Register
   type LDCC_Registers is record
      DCBlue  : Byte;
      DCGreen : Byte;
      DCRed   : Byte;
      DCAlpha : Byte;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Layer Blending Factors Configuration Register
   type LBFC_Registers is record
      BF2        : Bits_3; --  Blending Factor 2
      Reserved_1 : Bits_5;
      BF1        : Bits_3; --  Blending Factor 1
      Reserved_2 : Bits_21;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Layer Color Frame Buffer Length Register
   type LCFBL_Registers is record
      CFBLL      : Bits_13; --  Color Frame Buffer Line Length
      Reserved_1 : Bits_3;
      CFBP       : Bits_13; --  Color Frame Pitch in bytes
      Reserved_2 : Bits_3;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Layer Color Frame Buffer Line Number Register
   type LCFBLN_Registers is record
      CFBLNBR  : Bits_11; --  Frame Buffer Line Number
      Reserved : Bits_21;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Layer CLUT Write Register
   type LCLUTW_Registers is record
      Blue    : Byte;
      Green   : Byte;
      Red     : Byte;
      CLUTADD : Byte;
   end record with Pack, Volatile_Full_Access, Size => 32;

   type Layer is record
      Ctrl       : LC_Registers;
      WHPC       : LWHPC_Registers;
      WVPC       : LWVPC_Registers;
      CKC        : LCKC_Registers;
      PFC        : LPFC_Register;
      CAC        : LCAC_Registers;
      DCC        : LDCC_Registers;
      BFC        : LBFC_Registers;
      Reserved_1 : Word;
      Reserved_2 : Word;
      --  Layer Color Frame Buffer Address Register
      CFBA       : Word;
      CFBL       : LCFBL_Registers;
      CFBLN      : LCFBLN_Registers;
      Reserved_3 : Word;
      Reserved_4 : Word;
      Reserved_5 : Word;
      CLUTW      : LCLUTW_Registers;
   end record with Pack, Volatile, Size => 17 * 32;

   --  Synchronization Size Configuration Register
   type SSC_Registers is record
      VSH        : Bits_11; --  Vertical Synchronization Height
      Reserved_1 : Bits_5;
      HSW        : Bits_12; --  Horizontal Synchronization Width
      Reserved_2 : Bits_4;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Back Porch Configuration Register
   type BPC_Registers is record
      AVBP       : Bits_11; --  Accumulated Vertical back porch
      Reserved_1 : Bits_5;
      AHBP       : Bits_12; --  Accumulated Horizontal back porch
      Reserved_2 : Bits_4;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Active Width Configuration Register
   type AWC_Registers is record
      AAH        : Bits_11; --  Accumulated Active Height
      Reserved_1 : Bits_5;
      AAW        : Bits_12; --  Accumulated Active Width
      Reserved_2 : Bits_4;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Total Width Configuration Register
   type TWC_Registers is record
      TOTALH     : Bits_11; --  Total Height
      Reserved_1 : Bits_5;
      TOTALW     : Bits_12; --  Total Width
      Reserved_2 : Bits_4;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Global Control Register
   type GC_Registers is record
      LTDCEN     : Bits_1; --  Controller Enable
      Reserved_1 : Bits_3;
      DBW        : Bits_3; --  Dither Blue Width
      Reserved_2 : Bits_1;
      DGW        : Bits_3; --  Dither Green Width
      Reserved_3 : Bits_1;
      DRW        : Bits_3; --  Dither Red Width
      Reserved_4 : Bits_1;
      DEN        : Bits_1; --  Dither Enable
      Reserved_5 : Bits_11;
      PCPOL      : Bits_1; --  Pixel Clock Polarity
      DEPOL      : Bits_1; --  Data Enable Polarity
      VSPOL      : Bits_1; --  Vertical Synchronization Polarity
      HSPOL      : Bits_1; --  Horizontal Synchronization Polarity
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Shadow Reload Configuration Register
   type SRC_Registers is record
      IMR      : Bits_1; --  Immediate Reload
      VBR      : Bits_1; --  Vertical Blanking Reload
      Reserved : Bits_30;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Background Color Configuration Register
   type BCC_Registers is record
      BCBlue   : Byte;
      BCGreen  : Byte;
      BCRed    : Byte;
      Reserved : Byte;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Interrupt Enable Register
   type IE_Registers is record
      LIE      : Bits_1; --  Line Interrupt Enable
      FUIE     : Bits_1; --  FIFO Underrun Interrupt Enable
      TERRIE   : Bits_1; --  Transfer Error Interrupt Enable
      RRIE     : Bits_1; --  Register Reload interrupt enable
      Reserved : Bits_28;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Interrupt Status Register
   type IS_Registers is record
      LIF      : Bits_1; --  Line Interrupt flag
      FUIF     : Bits_1; --  FIFO Underrun Interrupt flag
      TERRIF   : Bits_1; --  Transfer Error Interrupt flag
      RRIF     : Bits_1; --  Register Reload interrupt flag
      Reserved : Bits_28;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Interrupt Clear Register
   type IC_Registers is record
      CLIF     : Bits_1; --  Clear Line Interrupt flag
      CFUIF    : Bits_1; --  Clear FIFO Underrun Interrupt flag
      CTERRIF  : Bits_1; --  Clear Transfer Error Interrupt flag
      CRRIF    : Bits_1; --  Clear Register Reload interrupt flag
      Reserved : Bits_28;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Line Interrupt Position Configuration Register
   type LIPC_Registers is record
      LIPOS    : Bits_11; --  Line Interrupt Position
      Reserved : Bits_21;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Current Position Status Register
   type CPS_Registers is record
      CYPOS : Bits_16;
      CXPOS : Bits_16;
   end record with Pack, Volatile_Full_Access, Size => 32;

   --  Current Display Status Register
   type CDS_Registers is record
      VDES     : Bits_1; --  Vertical Data Enable display Status
      HDES     : Bits_1; --  Horizontal Data Enable display Status
      VSYNCS   : Bits_1; --  Vertical Synchronization Enable display Status
      HSYNCS   : Bits_1; --  Horizontal Synchronization Enable display Status
      Reserved : Bits_28;
   end record with Pack, Volatile_Full_Access, Size => 32;

   Polarity_Active_Low  : constant := 0;
   Polarity_Active_High : constant := 1;

   type LTDCR is record
      SSC        : SSC_Registers;
      BPC        : BPC_Registers;
      AWC        : AWC_Registers;
      TWC        : TWC_Registers;
      GC         : GC_Registers;
      Reserved_1 : Word;
      Reserved_2 : Word;
      SRC        : SRC_Registers;
      Reserved_3 : Word;
      BCC        : BCC_Registers;
      Reserved_4 : Word;
      IE         : IE_Registers;
      ISR        : IS_Registers;
      IC         : IC_Registers;
      LIPC       : LIPC_Registers;
      CPS        : CPS_Registers;
      CDS        : CDS_Registers;
   end record with Pack, Volatile, Size => 17 * 32;

   Peripheral_Base      : constant := 16#4000_0000#;
   APB2_Peripheral_Base : constant := Peripheral_Base + 16#0001_0000#;
   LTDC_Base            : constant := APB2_Peripheral_Base + 16#6800#;
   SSCR_Base            : constant := LTDC_Base + 16#008#;
   Layer1_Base          : constant := LTDC_Base + 16#084#;
   Layer2_Base          : constant := LTDC_Base + 16#104#;

   LTDC     : LTDCR with Volatile, Address => System'To_Address (SSCR_Base);
   Layer1_Reg : aliased Layer
     with Volatile, Address => System'To_Address (Layer1_Base);
   Layer2_Reg : aliased Layer
     with Volatile, Address => System'To_Address (Layer2_Base);

   type Layer_Access is access all Layer;

   Frame_Buffer_Array : array (LCD_Layer) of aliased Frame_Buffer
     with Volatile, Address => System'To_Address (16#D000_0000#);

   --  LCD Registers
   LCD_SLEEP_OUT     : constant := 16#11#;  --  Sleep out register
   LCD_GAMMA         : constant := 16#26#;  --  Gamma register
   LCD_DISPLAY_OFF   : constant := 16#28#;  --  Display off register
   LCD_DISPLAY_ON    : constant := 16#29#;  --  Display on register
   LCD_COLUMN_ADDR   : constant := 16#2A#;  --  Colomn address register
   LCD_PAGE_ADDR     : constant := 16#2B#;  --  Page address register
   LCD_GRAM          : constant := 16#2C#;  --  GRAM register
   LCD_MAC           : constant := 16#36#;  --  Memory Access Control register
   LCD_PIXEL_FORMAT  : constant := 16#3A#;  --  Pixel Format register
   LCD_WDB           : constant := 16#51#;  --  Write Brightness Display register
   LCD_WCD           : constant := 16#53#;  --  Write Control Display register
   LCD_RGB_INTERFACE : constant := 16#B0#;  --  RGB Interface Signal Control
   LCD_FRC           : constant := 16#B1#;  --  Frame Rate Control register
   LCD_BPC           : constant := 16#B5#;  --  Blanking Porch Control register
   LCD_DFC           : constant := 16#B6#;  --  Display Function Control register
   LCD_POWER1        : constant := 16#C0#;  --  Power Control 1 register
   LCD_POWER2        : constant := 16#C1#;  --  Power Control 2 register
   LCD_VCOM1         : constant := 16#C5#;  --  VCOM Control 1 register
   LCD_VCOM2         : constant := 16#C7#;  --  VCOM Control 2 register
   LCD_POWERA        : constant := 16#CB#;  --  Power control A register
   LCD_POWERB        : constant := 16#CF#;  --  Power control B register
   LCD_PGAMMA        : constant := 16#E0#;  --  Positive Gamma Correction register
   LCD_NGAMMA        : constant := 16#E1#;  --  Negative Gamma Correction register
   LCD_DTCA          : constant := 16#E8#;  --  Driver timing control A
   LCD_DTCB          : constant := 16#EA#;  --  Driver timing control B
   LCD_POWER_SEQ     : constant := 16#ED#;  --  Power on sequence register
   LCD_3GAMMA_EN     : constant := 16#F2#;  --  3 Gamma enable register
   LCD_INTERFACE     : constant := 16#F6#;  --  Interface control register
   LCD_PRC           : constant := 16#F7#;  --  Pump ratio control register

end STM32.LTDC;
