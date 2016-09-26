
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
      Block_Number : Unsigned_64;
      Data         : out Block) return Boolean
     with Pre => Data'Size < 2 ** 16;
   --  Reads Data.
   --  Data size needs to be a multiple of the card's block size and maximum
   --  length is 2**16

   overriding function Write
     (Controller   : in out SDCard_Controller;
      Block_Number : Unsigned_64;
      Data         : Block) return Boolean
     with Pre => Data'Size < 2 ** 16;
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
