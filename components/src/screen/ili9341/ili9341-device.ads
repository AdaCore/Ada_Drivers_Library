------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2023, AdaCore                     --
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

--  This file provides the generic implementation of the component driver for
--  the ILI9341 LCD on the STM32F429 Discovery boards, among others. Support
--  does not include the TFT hardware.

--  See the "a-Si TFT LCD Single Chip Driver" specification by ILITEK, file
--  name "ILI9341_DS_V1.02" for details.

with HAL;
with HAL.Time;

generic
   type ILI9341_Connector is limited private;
   --  The abstraction of the transport to ILI9341 chip

   with procedure Send_Command
     (This : ILI9341_Connector;
      Cmd  : HAL.UInt8;
      Data : HAL.UInt8_Array);
   --  Send an ILI9341 command and corresponding arguments if any

   Connection : Interface_Kind;
   --  The method of connection to ILI9341

   Connector : ILI9341_Connector;
   --  The transport to ILI9341 chip

package ILI9341.Device is

   type ILI9341_Device is tagged limited private;
   --  The 240x320 TFT LCD display

   procedure Initialize
     (This : ILI9341_Device;
      Time : not null HAL.Time.Any_Delays);
   --  Initializes the device. Afterward, the device is also enabled so there
   --  is no immediate need to call Enable_Display.

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

private

   type ILI9341_Device is tagged limited record
      Orientation : Orientations := Portrait_1;
   end record;

end ILI9341.Device;
