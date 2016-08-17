with Ada.Interrupts.Names;

with STM32.GPIO;
with STM32.DMA;    use STM32.DMA;
with STM32.Device; use STM32.Device;

with STM32_SVD.SDMMC;

package Device_SD_Configuration is

   SD_Pins       : constant STM32.GPIO.GPIO_AF_Points :=
                     ((PC8,  GPIO_AF_12_SDMMC1),
                      (PC9,  GPIO_AF_12_SDMMC1),
                      (PC10, GPIO_AF_12_SDMMC1),
                      (PC11, GPIO_AF_12_SDMMC1),
                      (PC12, GPIO_AF_12_SDMMC1),
                      (PD2,  GPIO_AF_12_SDMMC1));
   SD_Detect_Pin : constant STM32.GPIO.GPIO_Point :=
                     PC13;

   SD_DMA            : DMA_Controller renames DMA_2;
   SD_DMA_Rx_Channel : constant DMA_Channel_Selector :=
                         Channel_4;
   SD_DMA_Rx_Stream  : constant DMA_Stream_Selector :=
                         Stream_3;
   Rx_IRQ            : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.DMA2_Stream3_Interrupt;
   SD_DMA_Tx_Channel : constant DMA_Channel_Selector :=
                         Channel_4;
   SD_DMA_Tx_Stream  : constant DMA_Stream_Selector :=
                         Stream_6;
   Tx_IRQ            : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.DMA2_Stream6_Interrupt;

   SD_Interrupt      : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.SDMMC1_Interrupt;
   SD_Device         : STM32_SVD.SDMMC.SDMMC_Peripheral renames
                         STM32_SVD.SDMMC.SDMMC_Periph;

   procedure Enable_Clock_Device;
   procedure Reset_Device;

end Device_SD_Configuration;
