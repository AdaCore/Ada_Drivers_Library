with Ada.IO_Exceptions;

with STM32.SDMMC;     use STM32.SDMMC;
with STM32.GPIO;      use STM32.GPIO;
with STM32.Device;    use STM32.Device;

with STM32_SVD.RCC;
with STM32_SVD.SDMMC;

package body Media_Reader.SDCard is

   SD_Pins       : constant STM32.GPIO.GPIO_Points :=
                     (PC8, PC9, PC10, PC11, PC12, PD2);
   SD_Detect_Pin : constant STM32.GPIO.GPIO_Point :=
                     PC13;

   procedure Ensure_Card_Informations
     (Controller : in out SDCard_Controller) with Inline_Always;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Controller : in out SDCard_Controller)
   is
   begin
      STM32_SVD.RCC.RCC_Periph.APB2ENR.SDMMC1EN := True;
      STM32_SVD.RCC.RCC_Periph.APB2RSTR.SDMMC1RST := True;
      STM32_SVD.RCC.RCC_Periph.APB2RSTR.SDMMC1RST := False;

      Enable_Clock (SD_Pins & SD_Detect_Pin);
      Configure_IO
        (SD_Pins,
         (Mode        => Mode_AF,
          Output_Type => Push_Pull,
          Speed       => Speed_High,
          Resistors   => Pull_Up));
      Configure_Alternate_Function (SD_Pins, GPIO_AF_SDIO);

      Configure_IO
        (SD_Detect_Pin,
         (Mode        => Mode_In,
          Output_Type => Open_Drain,
          Speed       => Speed_High,
          Resistors   => Pull_Up));

      Controller.Device :=
        STM32.SDMMC.As_Controller (STM32_SVD.SDMMC.SDMMC1_Periph'Access);
   end Initialize;

   ------------------
   -- Card_Present --
   ------------------

   function Card_Present
     (Controller : in out SDCard_Controller) return Boolean
   is
      pragma Unreferenced (Controller);
   begin
      if STM32.GPIO.Set (SD_Detect_Pin) then
         --  No card
         Controller.Has_Info := False;
         return False;
      else
         return True;
      end if;
   end Card_Present;

   ------------------------------
   -- Ensure_Card_Informations --
   ------------------------------

   procedure Ensure_Card_Informations
     (Controller : in out SDCard_Controller)
   is
      Ret : SD_Error;
   begin
      if Controller.Has_Info then
         return;
      end if;

      Ret := STM32.SDMMC.Initialize
        (Controller.Device, Controller.Info);

      if Ret = OK then
         Ret := Configure_Wide_Bus_Mode (Controller.Device, Wide_Bus_4B);
      end if;

      if Ret = OK then
         Controller.Has_Info := True;
      else
         Controller.Has_Info := False;
      end if;
   end Ensure_Card_Informations;

   --------------------------
   -- Get_Card_information --
   --------------------------

   function Get_Card_Information
     (Controller : in out SDCard_Controller)
      return STM32.SDMMC.Card_Information
   is
   begin
      Ensure_Card_Informations (Controller);

      if not Controller.Has_Info then
         --  Issue reading the SD-card information
         Ensure_Card_Informations (Controller);
      end if;

      if not Controller.Has_Info then
         raise Ada.IO_Exceptions.Device_Error;
      end if;

      return Controller.Info;
   end Get_Card_Information;

   ----------------
   -- Block_Size --
   ----------------

   overriding function Block_Size
     (Controller : in out SDCard_Controller)
      return Unsigned_32
   is
   begin
      Ensure_Card_Informations (Controller);

      return Controller.Info.Card_Block_Size;
   end Block_Size;

   ----------------
   -- Read_Block --
   ----------------

   overriding function Read_Block
     (Controller : in out SDCard_Controller;
      Sector     : Unsigned_32;
      Data       : out Block) return Boolean
   is
      Ret : SD_Error;
   begin
      Ensure_Card_Informations (Controller);

      Ret := Read_Blocks
        (Controller.Device,
         Unsigned_64 (Sector) * Unsigned_64 (Controller.Info.Card_Block_Size),
         Data => SD_Data (Data));

      return Ret = OK;
   end Read_Block;

end Media_Reader.SDCard;
