------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2018, AdaCore                     --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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

with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;

use STM32;  -- for base addresses
with STM32.SPI;
with STM32.SPI.DMA;
with STM32.Timers;         use STM32.Timers;
with STM32.DMA;            use STM32.DMA;
with STM32.DMA.Interrupts; use STM32.DMA.Interrupts;
with STM32.I2C;            use STM32.I2C;
with STM32.USARTs;         use STM32.USARTs;
with HAL.UART;

with Ada.Interrupts.Names;
with System;

package OpenMV is
   pragma Elaborate_Body;

   type LED_Color is (White, Red, Green, Blue, Magenta, Cyan, Yellow, Off);

   Image_Width  : constant := 128;
   Image_Height : constant := 160;

   subtype Width is Natural range 0 .. Image_Width - 1;
   subtype Height is Natural range 0 .. Image_Height - 1;

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure Set_RGB_LED (C : LED_Color);
   --  Set color of the RGB LED on the board

   procedure Turn_On_IR;
   --  Turn on infrared LEDs on the board

   procedure Turn_Off_IR;
   --  Turn off infrared LEDs on the board

   procedure Initialize_Shield_SPI;
   --  Initialize the SPI port available for shields (SPI2)

   procedure Initialize_Shield_USART (Baud : STM32.USARTs.Baud_Rates);
   --  Initialize the USART port available for shields (USART3)

   function Get_Shield_USART return not null HAL.UART.Any_UART_Port;
   --  Get the USART port available for shields (USART3)

   Shield_MOSI  : GPIO_Point renames PB15;
   Shield_MISO  : GPIO_Point renames PB14;
   Shield_SCK   : GPIO_Point renames PB13;
   Shield_SEL   : GPIO_Point renames PB12;
   Shield_TXD   : GPIO_Point renames PB10;
   Shield_RXD   : GPIO_Point renames PB11;
   Shield_ADC   : GPIO_Point renames PA5;
   Shield_SWC   : GPIO_Point renames PA14;
   Shield_SWD   : GPIO_Point renames PA13;
   Shield_PWM1  : GPIO_Point renames PD12;
   Shield_PWM2  : GPIO_Point renames PD13;

private

   --------------
   -- LED Pins --
   --------------

   Red_LED   : GPIO_Point renames PC0;
   Blue_LED  : GPIO_Point renames PC1;
   Green_LED : GPIO_Point renames PC2;
   IR_LED    : GPIO_Point renames PE2;
   All_LEDs  : constant GPIO_Points := (Red_LED, Blue_LED, Green_LED, IR_LED);

   ---------------
   -- SPI2 Pins --
   ---------------

   SPI2_SCK  : GPIO_Point renames PB13;
   SPI2_MISO : GPIO_Point renames PB14;
   SPI2_MOSI : GPIO_Point renames PB15;
   SPI2_NSS  : GPIO_Point renames PB12;

   Shield_SPI : STM32.SPI.DMA.SPI_Port_DMA renames STM32.Device.SPI_2_DMA;
   Shield_SPI_Points : constant STM32.GPIO.GPIO_Points :=
     (Shield_MISO,
      Shield_MOSI,
      Shield_SCK);

   Shield_SPI_DMA        : STM32.DMA.DMA_Controller renames DMA_1;
   Shield_SPI_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames
     STM32.DMA.Channel_0;
   Shield_SPI_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames
     STM32.DMA.Stream_4;

   DMA1_Stream4 : aliased DMA_Interrupt_Controller
     (DMA_1'Access, Stream_4,
      Ada.Interrupts.Names.DMA1_Stream4_Interrupt,
      System.Interrupt_Priority'Last);

   Shield_SPI_DMA_Int    : STM32.DMA.Interrupts.DMA_Interrupt_Controller renames
     DMA1_Stream4;

   ------------
   -- USART3 --
   ------------

   USART_3_TX : GPIO_Point renames PB10;
   USART_3_RX : GPIO_Point renames PB11;

   Shield_USART : USART renames USART_3;
   Shield_USART_Points : constant STM32.GPIO.GPIO_Points :=
     (USART_3_TX,
      USART_3_RX);
   Shield_USART_AF : constant GPIO_Alternate_Function := GPIO_AF_USART3_7;

   ---------------
   -- I2C1 Pins --
   ---------------

   Sensor_I2C     : I2C_Port renames I2C_1;
   Sensor_I2C_SCL : GPIO_Point renames PB8;
   Sensor_I2C_SDA : GPIO_Point renames PB9;
   Sensor_I2C_AF  : GPIO_Alternate_Function renames GPIO_AF_I2C1_4;

   -----------------
   --  Sensor DMA --
   -----------------

   DMA2_Stream1 : aliased DMA_Interrupt_Controller
     (DMA_2'Access, Stream_1,
      Ada.Interrupts.Names.DMA2_Stream1_Interrupt,
      System.Interrupt_Priority'Last);

   Sensor_DMA        : STM32.DMA.DMA_Controller renames DMA_2;
   Sensor_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames
     STM32.DMA.Channel_1;
   Sensor_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames
     STM32.DMA.Stream_1;
   Sensor_DMA_Int    : STM32.DMA.Interrupts.DMA_Interrupt_Controller renames
     DMA2_Stream1;

   ---------------
   -- I2C2 Pins --
   ---------------

   I2C2_SCL : GPIO_Point renames PB10;
   I2C2_SDA : GPIO_Point renames PB11;

   ---------------
   -- DCMI Pins --
   ---------------

   DCMI_HSYNC : GPIO_Point renames PA4;
   DCMI_PCLK  : GPIO_Point renames PA6;
   DCMI_RST   : GPIO_Point renames PA10;
   DCMI_PWDN  : GPIO_Point renames PB5;
   DCMI_VSYNC : GPIO_Point renames PB7;
   DCMI_D0    : GPIO_Point renames PC6;
   DCMI_D1    : GPIO_Point renames PC7;
   DCMI_D2    : GPIO_Point renames PE0;
   DCMI_D3    : GPIO_Point renames PE1;
   DCMI_D4    : GPIO_Point renames PE4;
   DCMI_D5    : GPIO_Point renames PB6;
   DCMI_D6    : GPIO_Point renames PE5;
   DCMI_D7    : GPIO_Point renames PE6;

   FS_IN : GPIO_Point renames PD3;

   SENSOR_CLK_IO   : GPIO_Point renames PA8;
   SENSOR_CLK_AF   : GPIO_Alternate_Function renames GPIO_AF_TIM1_1;
   SENSOR_CLK_TIM  : STM32.Timers.Timer renames Timer_1;
   SENSOR_CLK_CHAN : constant Timer_Channel := Channel_1;
   SENSOR_CLK_FREQ : constant := 12_000_000;

   ------------------
   -- USB OTG Pins --
   ------------------

   OTG_FS_DM : GPIO_Point renames PA11;
   OTG_FS_DP : GPIO_Point renames PA12;

   ---------------
   -- SDIO Pins --
   ---------------

   SDIO_CMD : GPIO_Point renames PD2;
   SDIO_CLK : GPIO_Point renames PC12;
   SDIO_D0  : GPIO_Point renames PC8;
   SDIO_D1  : GPIO_Point renames PC9;
   SDIO_D2  : GPIO_Point renames PC10;
   SDIO_D3  : GPIO_Point renames PC11;
   SD_CD    : GPIO_Point renames PA15;

   ---------------
   -- TIM4 Pins --
   ---------------

   TIM4_CH1 : GPIO_Point renames PD12;
   TIM4_CH2 : GPIO_Point renames PD13;

end OpenMV;
