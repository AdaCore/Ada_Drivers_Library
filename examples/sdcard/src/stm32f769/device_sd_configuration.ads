with Ada.Interrupts.Names;

with STM32.GPIO;
with STM32.DMA;    use STM32.DMA;
with STM32.Device; use STM32.Device;

with STM32_SVD.SDMMC;

package Device_SD_Configuration is

   SD_Pins           : constant STM32.GPIO.GPIO_AF_Points :=
                         ((PB3, GPIO_AF_10_SDMMC2),
                          (PB4, GPIO_AF_10_SDMMC2),
                          (PD6, GPIO_AF_11_SDMMC2),
                          (PD7, GPIO_AF_11_SDMMC2),
                          (PG9, GPIO_AF_11_SDMMC2),
                          (PG10, GPIO_AF_11_SDMMC2));

   SD_Detect_Pin     : constant STM32.GPIO.GPIO_Point := PI15;

   SD_DMA            : DMA_Controller renames DMA_2;
   SD_DMA_Rx_Channel : constant DMA_Channel_Selector :=
                         Channel_11;
   SD_DMA_Rx_Stream  : constant DMA_Stream_Selector :=
                         Stream_0;
   Rx_IRQ            : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.DMA2_Stream0_Interrupt;
   SD_DMA_Tx_Channel : constant DMA_Channel_Selector :=
                         Channel_11;
   SD_DMA_Tx_Stream  : constant DMA_Stream_Selector :=
                         Stream_5;
   Tx_IRQ            : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.DMA2_Stream5_Interrupt;

   SD_Interrupt      : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.SDMMC2_Interrupt;
   SD_Device         : STM32_SVD.SDMMC.SDMMC_Peripheral renames
                         STM32_SVD.SDMMC.SDMMC2_Periph;

   procedure Enable_Clock_Device;
   procedure Reset_Device;

end Device_SD_Configuration;
