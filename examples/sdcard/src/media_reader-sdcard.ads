with STM32.SDMMC;

package Media_Reader.SDCard is

   type SDCard_Controller is limited new Media_Controller with private;

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

private

   type SDCard_Controller is limited new Media_Controller with record
      Device        : STM32.SDMMC.SDMMC_Controller;
      Info          : STM32.SDMMC.Card_Information;
      Has_Info      : Boolean := False;
      Card_Detected : Boolean := False;
   end record;

end Media_Reader.SDCard;
