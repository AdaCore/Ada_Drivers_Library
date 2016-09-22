------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
