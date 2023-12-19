------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2023, AdaCore                         --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  This file provides declarations for devices on the STM32 F4VE board
--  manufactured by DevEBox.

with Ada.Interrupts.Names;    use Ada.Interrupts;

with System;

with STM32.Device;            use STM32.Device;
with STM32.DMA;               use STM32.DMA;
with STM32.DMA.Interrupts;    use STM32.DMA.Interrupts;
with STM32.GPIO;              use STM32.GPIO;
with STM32.USARTs;

with SDCard;
with W25Q16;
with Display_ILI9341;
with Touch_Panel_XPT2046;

package STM32.Board is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Point;

   D1_LED   : User_LED renames PA6;
   D2_LED   : User_LED renames PA7;

   All_LEDs : GPIO_Points := (D1_LED, D2_LED);
   LCH_LED  : GPIO_Point renames D1_LED;

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure Turn_On  (This : in out User_LED) renames STM32.GPIO.Clear;
   procedure Turn_Off (This : in out User_LED) renames STM32.GPIO.Set;
   procedure Toggle   (This : in out User_LED) renames STM32.GPIO.Toggle;

   procedure All_LEDs_Off with Inline;
   procedure All_LEDs_On  with Inline;
   procedure Toggle_LEDs (These : in out GPIO_Points)
     renames STM32.GPIO.Toggle;

   ----------
   -- FSMC --
   ----------

   FSMC_D : constant GPIO_Points :=
     (PD14, PD15, PD0, PD1, PE7, PE8, PE9, PE10,
      PE11, PE12, PE13, PE14, PE15, PD8, PD9, PD10);
   --  Data pins (D0 .. D15)

   FSMC_A18 : GPIO_Point renames PD13;
   --  Only one address pin is connected to the TFT header

   FSMC_NE1 : GPIO_Point renames PD7;  --  Chip select pin for TFT LCD
   FSMC_NWE : GPIO_Point renames PD5;  --  Write enable pin
   FSMC_NOE : GPIO_Point renames PD4;  --  Output enable pin

   TFT_Pins  : constant GPIO_Points :=
     FSMC_A18 & FSMC_D & FSMC_NE1 & FSMC_NWE & FSMC_NOE;

   procedure Initialize_FSMC (Pins : GPIO_Points);
   --  Enable FSMC and initialize given FSMC pins

   ---------
   -- TFT --
   ---------

   SPI2_SCK     : GPIO_Point renames PB13;
   SPI2_MISO    : GPIO_Point renames PB14;
   SPI2_MOSI    : GPIO_Point renames PB15;

   TFT_RS       : GPIO_Point renames PC5;  --  PEN IRQ
   TFT_BLK      : GPIO_Point renames PB1;  --  LCD backlight
   TFT_CS       : GPIO_Point renames PB12;

   Display : aliased Display_ILI9341.Display;

   TFT_Bitmap : aliased Display_ILI9341.Bitmap_Buffer := Display.Buffer;

   Touch_Panel : aliased Touch_Panel_XPT2046.Touch_Panel;

   --------------------------
   -- micro SD card reader --
   --------------------------

   SD_Detect_Pin     : STM32.GPIO.GPIO_Point renames PC11;
   --  There is no dedicated pin for card detection, reuse DAT3 pin

   SD_DMA            : DMA_Controller renames DMA_2;
   SD_DMA_Rx_Stream  : DMA_Stream_Selector renames Stream_3;
   SD_DMA_Rx_Channel : DMA_Channel_Selector renames Channel_4;
   SD_DMA_Tx_Stream  : DMA_Stream_Selector renames Stream_6;
   SD_DMA_Tx_Channel : DMA_Channel_Selector renames Channel_4;
   SD_Pins           : constant GPIO_Points :=
                         (PC8, PC9, PC10, PC11, PC12, PD2);
   SD_Pins_AF        : constant GPIO_Alternate_Function := GPIO_AF_SDIO_12;
   SD_Pins_2         : constant GPIO_Points := (1 .. 0 => <>);
   SD_Pins_AF_2      : constant GPIO_Alternate_Function := GPIO_AF_SDIO_12;
   SD_Interrupt      : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.SDIO_Interrupt;

   DMA2_Stream3 : aliased DMA_Interrupt_Controller
     (DMA_2'Access, Stream_3,
      Ada.Interrupts.Names.DMA2_Stream3_Interrupt,
      System.Interrupt_Priority'Last);

   DMA2_Stream6 : aliased DMA_Interrupt_Controller
     (DMA_2'Access, Stream_6,
      Ada.Interrupts.Names.DMA2_Stream6_Interrupt,
      System.Interrupt_Priority'Last);

   SD_Rx_DMA_Int     : DMA_Interrupt_Controller renames DMA2_Stream3;
   SD_Tx_DMA_Int     : DMA_Interrupt_Controller renames DMA2_Stream6;

   SDCard_Device : aliased SDCard.SDCard_Controller (SDIO'Access);

   ------------------
   -- User buttons --
   ------------------

   K0_Button_Point       : GPIO_Point renames PE4;
   K1_Button_Point       : GPIO_Point renames PE3;
   Wake_Up_Button_Point  : GPIO_Point renames PA0;
   User_Button_Point     : GPIO_Point renames Wake_Up_Button_Point;
   User_Button_Interrupt : constant Interrupt_ID := Names.EXTI0_Interrupt;

   All_Buttons : GPIO_Points :=
     (K0_Button_Point, K1_Button_Point, Wake_Up_Button_Point);

   procedure Configure_User_Button_GPIO;
   --  Configures the GPIO port/pin for user buttons. Sufficient
   --  for polling the button, and necessary for having the button generate
   --  interrupts.

   ------------------
   -- Flash memory --
   ------------------

   Flash : W25Q16.Flash_Memory
     (SPI => STM32.Device.SPI_1'Access,
      CS  => STM32.Device.PA15'Access);

   procedure Initialize_Flash_Memory;
   --  MUST be called prior to any use of the Flash

   ----------
   -- UART --
   ----------

   UART : STM32.USARTs.USART renames STM32.Device.USART_1;

   procedure Initialize_UART
     (Speed  : STM32.USARTs.Baud_Rates := 115_200;
      Stop   : STM32.USARTs.Stop_Bits := STM32.USARTs.Stopbits_1;
      Parity : STM32.USARTs.Parities := STM32.USARTs.No_Parity;
      Flow   : STM32.USARTs.Flow_Control := STM32.USARTs.No_Flow_Control);

end STM32.Board;
