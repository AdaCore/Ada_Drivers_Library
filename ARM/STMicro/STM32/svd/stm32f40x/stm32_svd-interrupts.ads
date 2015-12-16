--  Automatically generated from CMSIS-SVD description file by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

with Ada.Interrupts;  use Ada.Interrupts;

--  Definition of the device's interrupts
package STM32_SVD.Interrupts is

   ----------------
   -- Interrupts --
   ----------------

   WWDG_IRQ_Interrupt: constant Interrupt_ID := 2;

   PVD_IRQ_Interrupt: constant Interrupt_ID := 3;

   TAMP_STAMP_IRQ_Interrupt: constant Interrupt_ID := 4;

   RTC_WKUP_IRQ_Interrupt: constant Interrupt_ID := 5;

   RCC_IRQ_Interrupt: constant Interrupt_ID := 7;

   EXTI0_IRQ_Interrupt: constant Interrupt_ID := 8;

   EXTI1_IRQ_Interrupt: constant Interrupt_ID := 9;

   EXTI2_IRQ_Interrupt: constant Interrupt_ID := 10;

   EXTI3_IRQ_Interrupt: constant Interrupt_ID := 11;

   EXTI4_IRQ_Interrupt: constant Interrupt_ID := 12;

   DMA1_Stream0_IRQ_Interrupt: constant Interrupt_ID := 13;

   DMA1_Stream1_IRQ_Interrupt: constant Interrupt_ID := 14;

   DMA1_Stream2_IRQ_Interrupt: constant Interrupt_ID := 15;

   DMA1_Stream3_IRQ_Interrupt: constant Interrupt_ID := 16;

   DMA1_Stream4_IRQ_Interrupt: constant Interrupt_ID := 17;

   DMA1_Stream5_IRQ_Interrupt: constant Interrupt_ID := 18;

   DMA1_Stream6_IRQ_Interrupt: constant Interrupt_ID := 19;

   ADC_IRQ_Interrupt: constant Interrupt_ID := 20;

   CAN1_TX_IRQ_Interrupt: constant Interrupt_ID := 21;

   CAN1_RX0_IRQ_Interrupt: constant Interrupt_ID := 22;

   CAN1_RX1_IRQ_Interrupt: constant Interrupt_ID := 23;

   CAN1_SCE_IRQ_Interrupt: constant Interrupt_ID := 24;

   EXTI9_5_IRQ_Interrupt: constant Interrupt_ID := 25;

   TIM1_BRK_TIM9_IRQ_Interrupt: constant Interrupt_ID := 26;

   TIM1_UP_TIM10_IRQ_Interrupt: constant Interrupt_ID := 27;

   TIM1_TRG_COM_TIM11_IRQ_Interrupt: constant Interrupt_ID := 28;

   TIM1_CC_IRQ_Interrupt: constant Interrupt_ID := 29;

   TIM2_IRQ_Interrupt: constant Interrupt_ID := 30;

   TIM3_IRQ_Interrupt: constant Interrupt_ID := 31;

   TIM4_IRQ_Interrupt: constant Interrupt_ID := 32;

   I2C1_EV_IRQ_Interrupt: constant Interrupt_ID := 33;

   I2C1_ER_IRQ_Interrupt: constant Interrupt_ID := 34;

   I2C2_EV_IRQ_Interrupt: constant Interrupt_ID := 35;

   I2C2_ER_IRQ_Interrupt: constant Interrupt_ID := 36;

   SPI1_IRQ_Interrupt: constant Interrupt_ID := 37;

   SPI2_IRQ_Interrupt: constant Interrupt_ID := 38;

   USART1_IRQ_Interrupt: constant Interrupt_ID := 39;

   USART2_IRQ_Interrupt: constant Interrupt_ID := 40;

   USART3_IRQ_Interrupt: constant Interrupt_ID := 41;

   EXTI15_10_IRQ_Interrupt: constant Interrupt_ID := 42;

   RTC_Alarm_IRQ_Interrupt: constant Interrupt_ID := 43;

   OTG_FS_WKUP_IRQ_Interrupt: constant Interrupt_ID := 44;

   TIM8_BRK_TIM12_IRQ_Interrupt: constant Interrupt_ID := 45;

   TIM8_UP_TIM13_IRQ_Interrupt: constant Interrupt_ID := 46;

   TIM8_TRG_COM_TIM14_IRQ_Interrupt: constant Interrupt_ID := 47;

   TIM8_CC_IRQ_Interrupt: constant Interrupt_ID := 48;

   DMA1_Stream7_IRQ_Interrupt: constant Interrupt_ID := 49;

   FSMC_IRQ_Interrupt: constant Interrupt_ID := 50;

   SDIO_IRQ_Interrupt: constant Interrupt_ID := 51;

   TIM5_IRQ_Interrupt: constant Interrupt_ID := 52;

   SPI3_IRQ_Interrupt: constant Interrupt_ID := 53;

   UART4_IRQ_Interrupt: constant Interrupt_ID := 54;

   UART5_IRQ_Interrupt: constant Interrupt_ID := 55;

   TIM6_DAC_IRQ_Interrupt: constant Interrupt_ID := 56;

   TIM7_IRQ_Interrupt: constant Interrupt_ID := 57;

   DMA2_Stream0_IRQ_Interrupt: constant Interrupt_ID := 58;

   DMA2_Stream1_IRQ_Interrupt: constant Interrupt_ID := 59;

   DMA2_Stream2_IRQ_Interrupt: constant Interrupt_ID := 60;

   DMA2_Stream3_IRQ_Interrupt: constant Interrupt_ID := 61;

   DMA2_Stream4_IRQ_Interrupt: constant Interrupt_ID := 62;

   ETH_IRQ_Interrupt: constant Interrupt_ID := 63;

   ETH_WKUP_IRQ_Interrupt: constant Interrupt_ID := 64;

   CAN2_TX_IRQ_Interrupt: constant Interrupt_ID := 65;

   CAN2_RX0_IRQ_Interrupt: constant Interrupt_ID := 66;

   CAN2_RX1_IRQ_Interrupt: constant Interrupt_ID := 67;

   CAN2_SCE_IRQ_Interrupt: constant Interrupt_ID := 68;

   OTG_FS_IRQ_Interrupt: constant Interrupt_ID := 69;

   DMA2_Stream5_IRQ_Interrupt: constant Interrupt_ID := 70;

   DMA2_Stream6_IRQ_Interrupt: constant Interrupt_ID := 71;

   DMA2_Stream7_IRQ_Interrupt: constant Interrupt_ID := 72;

   USART6_IRQ_Interrupt: constant Interrupt_ID := 73;

   I2C3_EV_IRQ_Interrupt: constant Interrupt_ID := 74;

   I2C3_ER_IRQ_Interrupt: constant Interrupt_ID := 75;

   OTG_HS_EP1_OUT_IRQ_Interrupt: constant Interrupt_ID := 76;

   OTG_HS_EP1_IN_IRQ_Interrupt: constant Interrupt_ID := 77;

   OTG_HS_WKUP_IRQ_Interrupt: constant Interrupt_ID := 78;

   OTG_HS_IRQ_Interrupt: constant Interrupt_ID := 79;

   DCMI_IRQ_Interrupt: constant Interrupt_ID := 80;

end STM32_SVD.Interrupts;
