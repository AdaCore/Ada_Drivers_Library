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

--  See the "a-Si TFT LCD Single Chip Driver" specification by ILITEK, file
--  name "ILI9341_DS_V1.02" for details.

with HAL;      use HAL;
with HAL.SPI;  use HAL.SPI;
with HAL.GPIO; use HAL.GPIO;
with HAL.Time;

package ILI9341 is

   type ILI9341_Device
     (Port        : not null access SPI_Port'Class;
      Chip_Select : not null Any_GPIO_Point;
      WRX         : not null Any_GPIO_Point;
      Reset       : not null Any_GPIO_Point;
      Time        : not null HAL.Time.Any_Delays)
   is tagged limited private;

   type ILI9341_Mode is
     (RGB_Mode,
      SPI_Mode);

   procedure Initialize
     (This : in out ILI9341_Device;
      Mode : ILI9341_Mode);
   --  Initializes the device. Afterward, the device is also enabled so there
   --  is no immediate need to call Enable_Display.

   procedure Send_Command (This : in out ILI9341_Device; Cmd : UInt8);

   procedure Send_Data (This : in out ILI9341_Device; Data : UInt8);

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

   procedure Set_Pixel
     (This  : in out ILI9341_Device;
      X     : Width;
      Y     : Height;
      Color : Colors) with Inline;

   procedure Fill (This : in out ILI9341_Device; Color : Colors);

   --  Descriptions assume the USB power/debug connector at the top
   type Orientations is
     (Portrait_1,   -- origin at lower right, text going right to left
      Portrait_2,   -- origin at upper left, text going left to right
      Landscape_1,  -- origin at lower left, text going up
      Landscape_2); -- origin at upper right, text going down

   procedure Set_Orientation
     (This : in out ILI9341_Device;
      To   : Orientations);

   procedure Enable_Display (This : in out ILI9341_Device);

   procedure Disable_Display (This : in out ILI9341_Device);

   --  These values reflect the currently selected orientation

   function Current_Width (This : ILI9341_Device) return Natural
     with Inline;

   function Current_Height (This : ILI9341_Device) return Natural
     with Inline;

   function Current_Orientation (This : ILI9341_Device) return Orientations;

private

   type ILI9341_Device
     (Port        : not null access SPI_Port'Class;
      Chip_Select : not null Any_GPIO_Point;
      WRX         : not null Any_GPIO_Point;
      Reset       : not null Any_GPIO_Point;
      Time        : not null HAL.Time.Any_Delays)
   is tagged limited record
      Selected_Orientation : Orientations;

      --  The following objects' upper bounds vary with the selected
      --  orientation.
      Selected_Width  : Natural;
      Selected_Height : Natural;

      Initialized : Boolean := False;
   end record;

   procedure Set_Cursor_Position
     (This : in out ILI9341_Device;
      X1   : Width;
      Y1   : Height;
      X2   : Width;
      Y2   : Height)
     with Inline;

   procedure Chip_Select_High (This : in out ILI9341_Device) with Inline;
   procedure Chip_Select_Low  (This : in out ILI9341_Device) with Inline;

   procedure Init_LCD (This : in out ILI9341_Device;
                       Mode : ILI9341_Mode);

end ILI9341;
