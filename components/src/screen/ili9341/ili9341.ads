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

with Interfaces.Bit_Types; use Interfaces.Bit_Types;
with HAL.SPI; use HAL.SPI;
with HAL.GPIO; use HAL.GPIO;

package ILI9341 is

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

   type ILI9341_Device (Port        : not null access SPI_Port'Class;
                        Chip_Select : GPIO_Point_Ref;
                        WRX         : GPIO_Point_Ref;
                        Reset       : GPIO_Point_Ref)
   is tagged limited private;

   procedure Initialize (This : in out ILI9341_Device);
   --  Initializes the device. Afterward, the device is also enabled so there
   --  is no immediate need to call Enable_Display.

   procedure Send_Command (This : in out ILI9341_Device; Cmd : Byte);
   procedure Send_Data (This : in out ILI9341_Device; Data : Byte);

   procedure Set_Pixel (This  : in out ILI9341_Device;
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

   procedure Set_Orientation (This : in out ILI9341_Device;
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

   type ILI9341_Device (Port        : not null access SPI_Port'Class;
                        Chip_Select : GPIO_Point_Ref;
                        WRX         : GPIO_Point_Ref;
                        Reset       : GPIO_Point_Ref)
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
   procedure Chip_Select_Low (This : in out ILI9341_Device) with Inline;

   procedure Init_LCD (This : in out ILI9341_Device);

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

end ILI9341;
