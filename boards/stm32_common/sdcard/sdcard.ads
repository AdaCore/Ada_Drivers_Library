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
--  This file is based on:                                                  --
--   @file    stm32f769i_discovery_sd.h                                     --
--   @author  MCD Application Team                                          --
------------------------------------------------------------------------------

with HAL.SDMMC;
with STM32.SDMMC;

with HAL;               use HAL;
with HAL.Block_Drivers; use HAL.Block_Drivers;

package SDCard is

   type SDCard_Controller
     (Device : not null access STM32.SDMMC.SDMMC_Controller) is
   limited new Block_Driver with private;

   Device_Error : exception;

   procedure Initialize
     (This : in out SDCard_Controller);
   --  Initilizes the Controller's pins

   function Card_Present
     (This : in out SDCard_Controller) return Boolean;
   --  Whether a SD-Card is present in the sdcard reader
   --
   --  If there is no separate pin on the device for card detection, then a pin
   --  shared with the data transmission line is used. In this case, card
   --  detection cannot work simultaneously with other subprograms, and caution
   --  is required when using it. In this configuration, the card detection
   --  mode operates immediately after the Initialize function is called until
   --  the Card_Present function returns True. From that moment on, the
   --  controller switches to normal operation mode, and the Card_Present
   --  function will keep returning True until the next Initialize function
   --  call.

   function Has_SD_Detect_Pin return Boolean;
   --  Whether the dedicated SD card detection pin is present on the device

   function Get_Card_Information
     (This : in out SDCard_Controller)
      return HAL.SDMMC.Card_Information
     with Pre => This.Card_Present;
   --  Retrieves the card informations

   function Block_Size
     (This : in out SDCard_Controller)
     return UInt32;
   --  The insterted card block size. 512 Bytes for sd-cards

   overriding function Read
     (This         : in out SDCard_Controller;
      Block_Number : UInt64;
      Data         : out Block) return Boolean
     with Pre => Data'Length <= 16#10000#;
   --  Reads Data.
   --  Data size needs to be a multiple of the card's block size and maximum
   --  length is 2**16

   overriding function Write
     (This         : in out SDCard_Controller;
      Block_Number : UInt64;
      Data         : Block) return Boolean
     with Pre => Data'Length <= 16#10000#;
   --  Writes Data.
   --  Data size needs to be a multiple of the card's block size and maximum
   --  length is 2**16

private

   type SDCard_Controller
     (Device : not null access STM32.SDMMC.SDMMC_Controller) is
   limited new HAL.Block_Drivers.Block_Driver with record
      Card_Detected : Boolean := False;
   end record;

end SDCard;
