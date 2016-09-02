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

with STM32.SDMMC;

package Media_Reader.SDCard is

   type SDCard_Controller
     (Device : not null access STM32.SDMMC.SDMMC_Controller) is
   limited new Media_Controller with private;

   Device_Error : exception;

   procedure Initialize
     (Controller : in out SDCard_Controller);

   function Card_Present
     (Controller : in out SDCard_Controller) return Boolean;

   function Get_Card_Information
     (Controller : in out SDCard_Controller)
      return STM32.SDMMC.Card_Information
     with Pre => Controller.Card_Present;

   overriding function Block_Size
     (Controller : in out SDCard_Controller)
      return Unsigned_32;

   overriding function Read_Block
     (Controller   : in out SDCard_Controller;
      Block_Number : Unsigned_32;
      Data         : out Block) return Boolean;

   overriding function Write_Block
     (Controller   : in out SDCard_Controller;
      Block_Number : Unsigned_32;
      Data         : Block) return Boolean;

private

   type SDCard_Controller
     (Device : not null access STM32.SDMMC.SDMMC_Controller) is
   limited new Media_Controller with record
      Info          : STM32.SDMMC.Card_Information;
      Has_Info      : Boolean := False;
      Card_Detected : Boolean := False;
   end record;

end Media_Reader.SDCard;
