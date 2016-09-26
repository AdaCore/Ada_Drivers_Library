------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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
------------------------------------------------------------------------------


with Interfaces;           use Interfaces;
with Ada.Interrupts.Names;

with STM32.SDMMC;

with HAL.Block_Drivers; use HAL.Block_Drivers;

package SDCard is

   SD_Rx_IRQ         : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.DMA2_Stream3_Interrupt;
   SD_Tx_IRQ         : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.DMA2_Stream6_Interrupt;
   SD_Interrupt      : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.SDIO_Interrupt;

   type SDCard_Controller
     (Device : not null access STM32.SDMMC.SDMMC_Controller) is
   limited new HAL.Block_Drivers.Block_Driver with private;

   Device_Error : exception;

   procedure Initialize
     (Controller : in out SDCard_Controller);
   --  Initilizes the Controller's pins

   function Card_Present
     (Controller : in out SDCard_Controller) return Boolean;
   --  Whether a SD-Card is present in the sdcard reader

   function Get_Card_Information
     (Controller : in out SDCard_Controller)
      return STM32.SDMMC.Card_Information
     with Pre => Controller.Card_Present;
   --  Retrieves the card informations

   function Block_Size
     (Controller : in out SDCard_Controller)
     return Unsigned_32;
   --  The insterted card block size. 512 Bytes for sd-cards

   overriding function Read
     (Controller   : in out SDCard_Controller;
      Block_Number : Unsigned_32;
      Data         : out Block) return Boolean
     with Pre => (Data'Size mod Controller.Block_Size) = 0
                 and then Data'Size < 2 ** 16;
   --  Reads Data.
   --  Data size needs to be a multiple of the card's block size and maximum
   --  length is 2**16

   overriding function Write
     (Controller   : in out SDCard_Controller;
      Block_Number : Unsigned_32;
      Data         : Block) return Boolean
     with Pre => (Data'Size mod Controller.Block_Size) = 0
                 and then Data'Size < 2 ** 16;
   --  Writes Data.
   --  Data size needs to be a multiple of the card's block size and maximum
   --  length is 2**16

private

   type SDCard_Controller
     (Device : not null access STM32.SDMMC.SDMMC_Controller) is
   limited new HAL.Block_Drivers.Block_Driver with record
      Info          : STM32.SDMMC.Card_Information;
      Has_Info      : Boolean := False;
      Card_Detected : Boolean := False;
   end record;

end SDCard;
