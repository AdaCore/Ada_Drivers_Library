--  Automatically generated from STM32F46_79x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

with Ada.Interrupts;  use Ada.Interrupts;
with STM32_SVD;

--  Definition of the device's interrupts
package STM32_SVD.Interrupts is

   ----------------
   -- Interrupts --
   ----------------

   --  Window Watchdog interrupt
   WWDG_Interrupt                : constant Interrupt_ID := 2;

   --  PVD through EXTI line detection interrupt
   PVD_Interrupt                 : constant Interrupt_ID := 3;

   --  Tamper and TimeStamp interrupts through the EXTI line
   TAMP_STAMP_Interrupt          : constant Interrupt_ID := 4;

   --  RTC Wakeup interrupt through the EXTI line
   RTC_WKUP_Interrupt            : constant Interrupt_ID := 5;

   --  Flash global interrupt
   FLASH_Interrupt               : constant Interrupt_ID := 6;

   --  RCC global interrupt
   RCC_Interrupt                 : constant Interrupt_ID := 7;

   --  EXTI Line0 interrupt
   EXTI0_Interrupt               : constant Interrupt_ID := 8;

   --  EXTI Line1 interrupt
   EXTI1_Interrupt               : constant Interrupt_ID := 9;

   --  EXTI Line2 interrupt
   EXTI2_Interrupt               : constant Interrupt_ID := 10;

   --  EXTI Line3 interrupt
   EXTI3_Interrupt               : constant Interrupt_ID := 11;

   --  EXTI Line4 interrupt
   EXTI4_Interrupt               : constant Interrupt_ID := 12;

   --  DMA1 Stream0 global interrupt
   DMA1_Stream0_Interrupt        : constant Interrupt_ID := 13;

   --  DMA1 Stream1 global interrupt
   DMA1_Stream1_Interrupt        : constant Interrupt_ID := 14;

   --  DMA1 Stream2 global interrupt
   DMA1_Stream2_Interrupt        : constant Interrupt_ID := 15;

   --  DMA1 Stream3 global interrupt
   DMA1_Stream3_Interrupt        : constant Interrupt_ID := 16;

   --  DMA1 Stream4 global interrupt
   DMA1_Stream4_Interrupt        : constant Interrupt_ID := 17;

   --  DMA1 Stream5 global interrupt
   DMA1_Stream5_Interrupt        : constant Interrupt_ID := 18;

   --  DMA1 Stream6 global interrupt
   DMA1_Stream6_Interrupt        : constant Interrupt_ID := 19;

   --  ADC1 global interrupt
   ADC_Interrupt                 : constant Interrupt_ID := 20;

   --  CAN1 TX interrupts
   CAN1_TX_Interrupt             : constant Interrupt_ID := 21;

   --  CAN1 RX0 interrupts
   CAN1_RX0_Interrupt            : constant Interrupt_ID := 22;

   --  CAN1 RX1 interrupts
   CAN1_RX1_Interrupt            : constant Interrupt_ID := 23;

   --  CAN1 SCE interrupt
   CAN1_SCE_Interrupt            : constant Interrupt_ID := 24;

   --  EXTI Line[9:5] interrupts
   EXTI9_5_Interrupt             : constant Interrupt_ID := 25;

   --  TIM1 Break interrupt and TIM9 global interrupt
   TIM1_BRK_TIM9_Interrupt       : constant Interrupt_ID := 26;

   --  TIM1 Update interrupt and TIM10 global interrupt
   TIM1_UP_TIM10_Interrupt       : constant Interrupt_ID := 27;

   --  TIM1 Trigger and Commutation interrupts and TIM11 global interrupt
   TIM1_TRG_COM_TIM11_Interrupt  : constant Interrupt_ID := 28;

   --  TIM1 Capture Compare interrupt
   TIM1_CC_Interrupt             : constant Interrupt_ID := 29;

   --  TIM2 global interrupt
   TIM2_Interrupt                : constant Interrupt_ID := 30;

   --  TIM3 global interrupt
   TIM3_Interrupt                : constant Interrupt_ID := 31;

   --  TIM4 global interrupt
   TIM4_Interrupt                : constant Interrupt_ID := 32;

   --  I2C1 event interrupt
   I2C1_EV_Interrupt             : constant Interrupt_ID := 33;

   --  I2C1 error interrupt
   I2C1_ER_Interrupt             : constant Interrupt_ID := 34;

   --  I2C2 event interrupt
   I2C2_EV_Interrupt             : constant Interrupt_ID := 35;

   --  I2C2 error interrupt
   I2C2_ER_Interrupt             : constant Interrupt_ID := 36;

   --  SPI1 global interrupt
   SPI1_Interrupt                : constant Interrupt_ID := 37;

   --  SPI2 global interrupt
   SPI2_Interrupt                : constant Interrupt_ID := 38;

   --  USART1 global interrupt
   USART1_Interrupt              : constant Interrupt_ID := 39;

   --  USART2 global interrupt
   USART2_Interrupt              : constant Interrupt_ID := 40;

   --  USART3 global interrupt
   USART3_Interrupt              : constant Interrupt_ID := 41;

   --  EXTI Line[15:10] interrupts
   EXTI15_10_Interrupt           : constant Interrupt_ID := 42;

   --  RTC Alarms (A and B) through EXTI line interrupt
   RTC_Alarm_Interrupt           : constant Interrupt_ID := 43;

   --  USB On-The-Go FS Wakeup through EXTI line interrupt
   OTG_FS_WKUP_Interrupt         : constant Interrupt_ID := 44;

   --  TIM8 Break interrupt and TIM12 global interrupt
   TIM8_BRK_TIM12_Interrupt      : constant Interrupt_ID := 45;

   --  TIM8 Update interrupt and TIM13 global interrupt
   TIM8_UP_TIM13_Interrupt       : constant Interrupt_ID := 46;

   --  TIM8 Trigger and Commutation interrupts and TIM14 global interrupt
   TIM8_TRG_COM_TIM14_Interrupt  : constant Interrupt_ID := 47;

   --  TIM8 Capture Compare interrupt
   TIM8_CC_Interrupt             : constant Interrupt_ID := 48;

   --  DMA1 Stream7 global interrupt
   DMA1_Stream7_Interrupt        : constant Interrupt_ID := 49;

   --  FMC global interrupt
   FMC_Interrupt                 : constant Interrupt_ID := 50;

   --  SDIO global interrupt
   SDIO_Interrupt                : constant Interrupt_ID := 51;

   --  TIM5 global interrupt
   TIM5_Interrupt                : constant Interrupt_ID := 52;

   --  SPI3 global interrupt
   SPI3_Interrupt                : constant Interrupt_ID := 53;

   --  UART4 global interrupt
   UART4_Interrupt               : constant Interrupt_ID := 54;

   --  UART5 global interrupt
   UART5_Interrupt               : constant Interrupt_ID := 55;

   --  TIM6 global interrupt, DAC1 and DAC2 underrun error interrupt
   TIM6_DAC_Interrupt            : constant Interrupt_ID := 56;

   --  TIM7 global interrupt
   TIM7_Interrupt                : constant Interrupt_ID := 57;

   --  DMA2 Stream0 global interrupt
   DMA2_Stream0_Interrupt        : constant Interrupt_ID := 58;

   --  DMA2 Stream1 global interrupt
   DMA2_Stream1_Interrupt        : constant Interrupt_ID := 59;

   --  DMA2 Stream2 global interrupt
   DMA2_Stream2_Interrupt        : constant Interrupt_ID := 60;

   --  DMA2 Stream3 global interrupt
   DMA2_Stream3_Interrupt        : constant Interrupt_ID := 61;

   --  DMA2 Stream4 global interrupt
   DMA2_Stream4_Interrupt        : constant Interrupt_ID := 62;

   --  Ethernet global interrupt
   ETH_Interrupt                 : constant Interrupt_ID := 63;

   --  Ethernet Wakeup through EXTI line interrupt
   ETH_WKUP_Interrupt            : constant Interrupt_ID := 64;

   --  CAN2 TX interrupts
   CAN2_TX_Interrupt             : constant Interrupt_ID := 65;

   --  CAN2 RX0 interrupts
   CAN2_RX0_Interrupt            : constant Interrupt_ID := 66;

   --  CAN2 RX1 interrupts
   CAN2_RX1_Interrupt            : constant Interrupt_ID := 67;

   --  CAN2 SCE interrupt
   CAN2_SCE_Interrupt            : constant Interrupt_ID := 68;

   --  USB On The Go FS global interrupt
   OTG_FS_Interrupt              : constant Interrupt_ID := 69;

   --  DMA2 Stream5 global interrupt
   DMA2_Stream5_Interrupt        : constant Interrupt_ID := 70;

   --  DMA2 Stream6 global interrupt
   DMA2_Stream6_Interrupt        : constant Interrupt_ID := 71;

   --  DMA2 Stream7 global interrupt
   DMA2_Stream7_Interrupt        : constant Interrupt_ID := 72;

   --  USART6 global interrupt
   USART6_Interrupt              : constant Interrupt_ID := 73;

   --  I2C3 event interrupt
   I2C3_EV_Interrupt             : constant Interrupt_ID := 74;

   --  I2C3 error interrupt
   I2C3_ER_Interrupt             : constant Interrupt_ID := 75;

   --  USB On The Go HS End Point 1 Out global interrupt
   OTG_HS_EP1_OUT_Interrupt      : constant Interrupt_ID := 76;

   --  USB On The Go HS End Point 1 In global interrupt
   OTG_HS_EP1_IN_Interrupt       : constant Interrupt_ID := 77;

   --  USB On The Go HS Wakeup through EXTI interrupt
   OTG_HS_WKUP_Interrupt         : constant Interrupt_ID := 78;

   --  USB On The Go HS global interrupt
   OTG_HS_Interrupt              : constant Interrupt_ID := 79;

   --  DCMI global interrupt
   DCMI_Interrupt                : constant Interrupt_ID := 80;

   --  CRYP crypto global interrupt
   CRYP_Interrupt                : constant Interrupt_ID := 81;

   --  Hash and Rng global interrupt
   HASH_RNG_Interrupt            : constant Interrupt_ID := 82;

   --  UART 7 global interrupt
   UART7_Interrupt               : constant Interrupt_ID := 84;

   --  UART 8 global interrupt
   UART8_Interrupt               : constant Interrupt_ID := 85;

   --  SPI 4 global interrupt
   SPI4_Interrupt                : constant Interrupt_ID := 86;

   --  SPI 5 global interrupt
   SPI5_Interrupt                : constant Interrupt_ID := 87;

   --  SPI 6 global interrupt
   SPI6_Interrupt                : constant Interrupt_ID := 88;

   --  SAI1 global interrupt
   SAI1_Interrupt                : constant Interrupt_ID := 89;

   --  LTDC global interrupt
   LCD_TFT_Interrupt             : constant Interrupt_ID := 90;

   --  LTDC global error interrupt
   LCD_TFT_1_Interrupt           : constant Interrupt_ID := 91;

   --  DMA2D global interrupt
   DMA2D_Interrupt               : constant Interrupt_ID := 92;

   --  QuadSPI global interrupt
   QUADSPI_Interrupt             : constant Interrupt_ID := 93;

   --  DSI event interrupt
   DSI_Interrupt                 : constant Interrupt_ID := 94;

end STM32_SVD.Interrupts;
