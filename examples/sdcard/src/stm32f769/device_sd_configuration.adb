with STM32_SVD.RCC; use STM32_SVD.RCC;

package body Device_SD_Configuration is

   -------------------------
   -- Enable_Clock_Device --
   -------------------------

   procedure Enable_Clock_Device
   is
   begin
      RCC_Periph.APB2ENR.SDMMC2EN := True;
   end Enable_Clock_Device;

   ------------------
   -- Reset_Device --
   ------------------

   procedure Reset_Device
   is
   begin
      RCC_Periph.APB2RSTR.SDMMC2RST := True;
      RCC_Periph.APB2RSTR.SDMMC2RST := False;
   end Reset_Device;

end Device_SD_Configuration;
