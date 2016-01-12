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
--  This file is based on:                                                  --
--                                                                          --
--   @file    ili9341.h                                                     --
--   @author  MCD Application Team                                          --
--   @version V1.0.2                                                        --
--   @date    02-December-2014                                              --
--   @brief   This file includes the LCD driver for ILI9341 LCD.            --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides the component driver for the ILI9341 LCD on the
--  STM32F429 Discovery boards, among others. Support does not include the
--  TFT hardware.

--  The package is an abstract data machine, i.e., it contains state info
--  representing the current configuration of the device. This state info
--  can be queried by the functions provided.

--  See the "a-Si TFT LCD Single Chip Driver" specification by ILITEK, file
--  name "ILI9341_DS_V1.02" for details.

with STM32F4.GPIO; use STM32F4.GPIO;
with STM32F4.SPI;  use STM32F4.SPI;

package STM32F4.ILI9341 is
   pragma Elaborate_Body;

   Device_Width  : constant := 240;
   Device_Height : constant := 320;

   --  The operational upper bounds for width and height depend on the selected
   --  orientation so these subtypes cannot specify an upper bound using the
   --  device max height/width. In particular, if the orientation is rotated,
   --  the width becomes the height, and vice versa. Hence these are mainly for
   --  readability.
   subtype Width  is Natural;
   subtype Height is Natural;

   type Colors is
     (Black,
      Blue,
      Light_Blue,
      Green,
      Cyan,
      Gray,
      Magenta,
      Light_Green,
      Brown,
      Red,
      Orange,
      Yellow,
      White);

   for Colors use
     (Black       => 16#0000#,
      Blue        => 16#001F#,
      Light_Blue  => 16#051D#,
      Green       => 16#07E0#,
      Cyan        => 16#07FF#,
      Gray        => 16#7BEF#,
      Magenta     => 16#A254#,
      Light_Green => 16#B723#,
      Brown       => 16#BBCA#,
      Red         => 16#F800#,
      Orange      => 16#FBE4#,
      Yellow      => 16#FFE0#,
      White       => 16#FFFF#);

   procedure Initialize
     (Chip_Select             : GPIO_Point;
      Enable_CS_GPIO_Clock    : not null access procedure;
      WRX                     : GPIO_Point;
      Enable_WRX_GPIO_Clock   : not null access procedure;
      Reset                   : GPIO_Point;
      Enable_Reset_GPIO_Clock : not null access procedure;
      SPI_Chip                : access SPI_Port;
      Enable_SPI_Clock        : not null access procedure;
      SPI_GPIO                : access GPIO_Port;
      Enable_SPI_GPIO_Clock   : not null access procedure;
      SPI_AF                  : GPIO_Alternate_Function;
      SCK_Pin                 : GPIO_Pin;
      MISO_Pin                : GPIO_Pin;
      MOSI_Pin                : GPIO_Pin);
   --  Initializes the device. Afterward, the device is also enabled so there
   --  is no immediate need to call Enable_Display.

   procedure Set_Pixel (X : Width; Y : Height; Color : Colors) with Inline;

   procedure Fill (Color : Colors);

   --  Descriptions assume the USB power/debug connector at the top
   type Orientations is
     (Portrait_1,   -- origin at lower right, text going right to left
      Portrait_2,   -- origin at upper left, text going left to right
      Landscape_1,  -- origin at lower left, text going up
      Landscape_2); -- origin at upper right, text going down

   procedure Set_Orientation (To : Orientations);

   procedure Enable_Display;

   procedure Disable_Display;

   --  These values reflect the currently selected orientation

   function Current_Width return Natural with Inline;

   function Current_Height return Natural with Inline;

   function Current_Orientation return Orientations;

private

   Chip_Select : GPIO_Point;
   WRX         : GPIO_Point;
   Reset       : GPIO_Point;
   SPI_Chip    : access SPI_Port;

   Selected_Orientation : Orientations;

   --  The following objects' upper bounds vary with the selected orientation.
   Selected_Width  : Natural;
   Selected_Height : Natural;

   Initialized : Boolean := False;

   procedure Send_Data    (Data : Byte) with Inline;
   procedure Send_Command (Cmd : Byte)  with Inline;

   procedure Set_Cursor_Position
     (X1 : Width;
      Y1 : Height;
      X2 : Width;
      Y2 : Height)
     with Inline;

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

end STM32F4.ILI9341;
