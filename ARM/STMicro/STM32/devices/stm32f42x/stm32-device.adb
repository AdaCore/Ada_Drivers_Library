------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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

with STM32_SVD.RCC; use STM32_SVD.RCC;

package body STM32.Device is

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.AHB1ENR.GPIOAEN := 1;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.AHB1ENR.GPIOBEN := 1;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.AHB1ENR.GPIOCEN := 1;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.AHB1ENR.GPIODEN := 1;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.AHB1ENR.GPIOEEN := 1;
      elsif This'Address = GPIOF_Base then
         RCC_Periph.AHB1ENR.GPIOFEN := 1;
      elsif This'Address = GPIOG_Base then
         RCC_Periph.AHB1ENR.GPIOGEN := 1;
      elsif This'Address = GPIOH_Base then
         RCC_Periph.AHB1ENR.GPIOHEN := 1;
      elsif This'Address = GPIOI_Base then
         RCC_Periph.AHB1ENR.GPIOIEN := 1;
      elsif This'Address = GPIOJ_Base then
         RCC_Periph.AHB1ENR.GPIOJEN := 1;
      elsif This'Address = GPIOK_Base then
         RCC_Periph.AHB1ENR.GPIOKEN := 1;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Point : GPIO_Point)
   is
   begin
      Enable_Clock (Point.Port.all);
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Points : GPIO_Points)
   is
   begin
      for Point of Points loop
         Enable_Clock (Point.Port.all);
      end loop;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.AHB1RSTR.GPIOARST := 1;
         RCC_Periph.AHB1RSTR.GPIOARST := 0;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.AHB1RSTR.GPIOBRST := 1;
         RCC_Periph.AHB1RSTR.GPIOBRST := 0;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.AHB1RSTR.GPIOCRST := 1;
         RCC_Periph.AHB1RSTR.GPIOCRST := 0;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.AHB1RSTR.GPIODRST := 1;
         RCC_Periph.AHB1RSTR.GPIODRST := 0;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.AHB1RSTR.GPIOERST := 1;
         RCC_Periph.AHB1RSTR.GPIOERST := 0;
      elsif This'Address = GPIOF_Base then
         RCC_Periph.AHB1RSTR.GPIOFRST := 1;
         RCC_Periph.AHB1RSTR.GPIOFRST := 0;
      elsif This'Address = GPIOG_Base then
         RCC_Periph.AHB1RSTR.GPIOGRST := 1;
         RCC_Periph.AHB1RSTR.GPIOGRST := 0;
      elsif This'Address = GPIOH_Base then
         RCC_Periph.AHB1RSTR.GPIOHRST := 1;
         RCC_Periph.AHB1RSTR.GPIOHRST := 0;
      elsif This'Address = GPIOI_Base then
         RCC_Periph.AHB1RSTR.GPIOIRST := 1;
         RCC_Periph.AHB1RSTR.GPIOIRST := 0;
      elsif This'Address = GPIOJ_Base then
         RCC_Periph.AHB1RSTR.GPIOJRST := 1;
         RCC_Periph.AHB1RSTR.GPIOJRST := 0;
      elsif This'Address = GPIOK_Base then
         RCC_Periph.AHB1RSTR.GPIOKRST := 1;
         RCC_Periph.AHB1RSTR.GPIOKRST := 0;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Point : GPIO_Point) is
   begin
      Reset (Point.Port.all);
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Points : GPIO_Points)
   is
      Do_Reset : Boolean;
   begin
      for J in Points'Range loop
         Do_Reset := True;
         for K in Points'First .. J - 1 loop
            if Points (K).Port = Points (J).Port then
               Do_Reset := False;

               exit;
            end if;
         end loop;

         if Do_Reset then
            Reset (Points (J).Port.all);
         end if;
      end loop;
   end Reset;

   ---------------------
   -- As_GPIO_Port_Id --
   ---------------------

   function As_GPIO_Port_Id (Port : GPIO_Port) return GPIO_Port_Id is
   begin
      -- TODO: rather ugly to have this board-specific range here
      if Port'Address = GPIOA_Base then
         return GPIO_Port_A;
      elsif Port'Address = GPIOB_Base then
         return GPIO_Port_B;
      elsif Port'Address = GPIOC_Base then
         return GPIO_Port_C;
      elsif Port'Address = GPIOD_Base then
         return GPIO_Port_D;
      elsif Port'Address = GPIOE_Base then
         return GPIO_Port_E;
      elsif Port'Address = GPIOF_Base then
         return GPIO_Port_F;
      elsif Port'Address = GPIOG_Base then
         return GPIO_Port_G;
      elsif Port'Address = GPIOH_Base then
         return GPIO_Port_H;
      elsif Port'Address = GPIOI_Base then
         return GPIO_Port_I;
      elsif Port'Address = GPIOJ_Base then
         return GPIO_Port_J;
      elsif Port'Address = GPIOK_Base then
         return GPIO_Port_K;
      else
         raise Program_Error;
      end if;
   end As_GPIO_Port_Id;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out Analog_To_Digital_Converter) is
   begin
      if This'Address = ADC1_Base then
         RCC_Periph.APB2ENR.ADC1EN := 1;
      elsif This'Address = ADC2_Base then
         RCC_Periph.APB2ENR.ADC2EN := 1;
      elsif This'Address = ADC3_Base then
         RCC_Periph.APB2ENR.ADC3EN := 1;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -------------------------
   -- Reset_All_ADC_Units --
   -------------------------

   procedure Reset_All_ADC_Units is
   begin
      RCC_Periph.APB2RSTR.ADCRST := 1;
      RCC_Periph.APB2RSTR.ADCRST := 0;
   end Reset_All_ADC_Units;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.APB1ENR.DACEN := 1;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out Digital_To_Analog_Converter) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.APB1RSTR.DACRST := 1;
      RCC_Periph.APB1RSTR.DACRST := 0;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out USART) is
   begin
      if This'Address = USART1_Base then
         RCC_Periph.APB2ENR.USART1EN := 1;
      elsif This'Address = USART2_Base then
         RCC_Periph.APB1ENR.USART2EN := 1;
      elsif This'Address = USART3_Base then
         RCC_Periph.APB1ENR.USART3EN := 1;
      elsif This'Address = UART4_Base then
         RCC_Periph.APB1ENR.UART4EN := 1;
      elsif This'Address = UART5_Base then
         RCC_Periph.APB1ENR.UART5EN := 1;
      elsif This'Address = USART6_Base then
         RCC_Periph.APB2ENR.USART6EN := 1;
      elsif This'Address = UART7_Base then
         RCC_Periph.APB1ENR.UART7ENR := 1;
      elsif This'Address = UART8_Base then
         RCC_Periph.APB1ENR.UART8ENR := 1;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out USART) is
   begin
      if This'Address = USART1_Base then
         RCC_Periph.APB2RSTR.USART1RST := 1;
         RCC_Periph.APB2RSTR.USART1RST := 0;
      elsif This'Address = USART2_Base then
         RCC_Periph.APB1RSTR.UART2RST := 1;
         RCC_Periph.APB1RSTR.UART2RST := 0;
      elsif This'Address = USART3_Base then
         RCC_Periph.APB1RSTR.UART3RST := 1;
         RCC_Periph.APB1RSTR.UART3RST := 0;
      elsif This'Address = UART4_Base then
         RCC_Periph.APB1RSTR.UART4RST := 1;
         RCC_Periph.APB1RSTR.UART4RST := 0;
      elsif This'Address = UART5_Base then
         RCC_Periph.APB1RSTR.UART5RST := 1;
         RCC_Periph.APB1RSTR.UART5RST := 0;
      elsif This'Address = USART6_Base then
         RCC_Periph.APB2RSTR.USART6RST := 1;
         RCC_Periph.APB2RSTR.USART6RST := 0;
      elsif This'Address = UART7_Base then
         RCC_Periph.APB1RSTR.UART7RST := 1;
         RCC_Periph.APB1RSTR.UART7RST := 0;
      elsif This'Address = UART8_Base then
         RCC_Periph.APB1RSTR.UART8RST := 1;
         RCC_Periph.APB1RSTR.UART8RST := 0;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out DMA_Controller) is
   begin
      if This'Address = STM32_SVD.DMA1_Base then
         RCC_Periph.AHB1ENR.DMA1EN := 1;
      elsif This'Address = STM32_SVD.DMA2_Base then
         RCC_Periph.AHB1ENR.DMA2EN := 1;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out DMA_Controller) is
   begin
      if This'Address = STM32_SVD.DMA1_Base then
         RCC_Periph.AHB1RSTR.DMA1RST := 1;
         RCC_Periph.AHB1RSTR.DMA1RST := 0;
      elsif This'Address = STM32_SVD.DMA2_Base then
         RCC_Periph.AHB1RSTR.DMA2RST := 1;
         RCC_Periph.AHB1RSTR.DMA2RST := 0;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out I2C_Port) is
   begin
      if This'Address = I2C1_Base then
         RCC_Periph.APB1ENR.I2C1EN := 1;
      elsif This'Address = I2C2_Base then
         RCC_Periph.APB1ENR.I2C2EN := 1;
      elsif This'Address = I2C3_Base then
         RCC_Periph.APB1ENR.I2C3EN := 1;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out I2C_Port) is
   begin
      if This'Address = I2C1_Base then
         RCC_Periph.APB1RSTR.I2C1RST := 1;
         RCC_Periph.APB1RSTR.I2C1RST := 0;
      elsif This'Address = I2C2_Base then
         RCC_Periph.APB1RSTR.I2C2RST := 1;
         RCC_Periph.APB1RSTR.I2C2RST := 0;
      elsif This'Address = I2C3_Base then
         RCC_Periph.APB1RSTR.I2C3RST := 1;
         RCC_Periph.APB1RSTR.I2C3RST := 0;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out SPI_Port) is
   begin
      if This'Address = SPI1_Base then
         RCC_Periph.APB2ENR.SPI1EN := 1;
      elsif This'Address = SPI2_Base then
         RCC_Periph.APB1ENR.SPI2EN := 1;
      elsif This'Address = SPI3_Base then
         RCC_Periph.APB1ENR.SPI3EN := 1;
      elsif This'Address = SPI4_Base then
         RCC_Periph.APB2ENR.SPI4ENR := 1;
      elsif This'Address = SPI5_Base then
         RCC_Periph.APB2ENR.SPI5ENR := 1;
      elsif This'Address = SPI6_Base then
         RCC_Periph.APB2ENR.SPI6ENR := 1;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out SPI_Port) is
   begin
      if This'Address = SPI1_Base then
         RCC_Periph.APB2RSTR.SPI1RST := 1;
         RCC_Periph.APB2RSTR.SPI1RST := 0;
      elsif This'Address = SPI2_Base then
         RCC_Periph.APB1RSTR.SPI2RST := 1;
         RCC_Periph.APB1RSTR.SPI2RST := 0;
      elsif This'Address = SPI3_Base then
         RCC_Periph.APB1RSTR.SPI3RST := 1;
         RCC_Periph.APB1RSTR.SPI3RST := 0;
      elsif This'Address = SPI4_Base then
         RCC_Periph.APB2RSTR.SPI4RST := 1;
         RCC_Periph.APB2RSTR.SPI4RST := 0;
      elsif This'Address = SPI5_Base then
         RCC_Periph.APB2RSTR.SPI5RST := 1;
         RCC_Periph.APB2RSTR.SPI5RST := 0;
      elsif This'Address = SPI6_Base then
         RCC_Periph.APB2RSTR.SPI6RST := 1;
         RCC_Periph.APB2RSTR.SPI6RST := 0;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : in out Timer) is
   begin
      if This'Address = TIM1_Base then
         RCC_Periph.APB2ENR.TIM1EN := 1;
      elsif This'Address = TIM2_Base then
         RCC_Periph.APB1ENR.TIM2EN := 1;
      elsif This'Address = TIM3_Base then
         RCC_Periph.APB1ENR.TIM3EN := 1;
      elsif This'Address = TIM4_Base then
         RCC_Periph.APB1ENR.TIM4EN := 1;
      elsif This'Address = TIM5_Base then
         RCC_Periph.APB1ENR.TIM5EN := 1;
      elsif This'Address = TIM6_Base then
         RCC_Periph.APB1ENR.TIM6EN := 1;
      elsif This'Address = TIM7_Base then
         RCC_Periph.APB1ENR.TIM7EN := 1;
      elsif This'Address = TIM8_Base then
         RCC_Periph.APB2ENR.TIM8EN := 1;
      elsif This'Address = TIM9_Base then
         RCC_Periph.APB2ENR.TIM9EN := 1;
      elsif This'Address = TIM10_Base then
         RCC_Periph.APB2ENR.TIM10EN := 1;
      elsif This'Address = TIM11_Base then
         RCC_Periph.APB2ENR.TIM11EN := 1;
      elsif This'Address = TIM12_Base then
         RCC_Periph.APB1ENR.TIM12EN := 1;
      elsif This'Address = TIM13_Base then
         RCC_Periph.APB1ENR.TIM13EN := 1;
      elsif This'Address = TIM14_Base then
         RCC_Periph.APB1ENR.TIM14EN := 1;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Timer) is
   begin
      if This'Address = TIM1_Base then
         RCC_Periph.APB2RSTR.TIM1RST := 1;
         RCC_Periph.APB2RSTR.TIM1RST := 0;
      elsif This'Address = TIM2_Base then
         RCC_Periph.APB1RSTR.TIM2RST := 1;
         RCC_Periph.APB1RSTR.TIM2RST := 0;
      elsif This'Address = TIM3_Base then
         RCC_Periph.APB1RSTR.TIM3RST := 1;
         RCC_Periph.APB1RSTR.TIM3RST := 0;
      elsif This'Address = TIM4_Base then
         RCC_Periph.APB1RSTR.TIM4RST := 1;
         RCC_Periph.APB1RSTR.TIM4RST := 0;
      elsif This'Address = TIM5_Base then
         RCC_Periph.APB1RSTR.TIM5RST := 1;
         RCC_Periph.APB1RSTR.TIM5RST := 0;
      elsif This'Address = TIM6_Base then
         RCC_Periph.APB1RSTR.TIM6RST := 1;
         RCC_Periph.APB1RSTR.TIM6RST := 0;
      elsif This'Address = TIM7_Base then
         RCC_Periph.APB1RSTR.TIM7RST := 1;
         RCC_Periph.APB1RSTR.TIM7RST := 0;
      elsif This'Address = TIM8_Base then
         RCC_Periph.APB2RSTR.TIM8RST := 1;
         RCC_Periph.APB2RSTR.TIM8RST := 0;
      elsif This'Address = TIM9_Base then
         RCC_Periph.APB2RSTR.TIM9RST := 1;
         RCC_Periph.APB2RSTR.TIM9RST := 0;
      elsif This'Address = TIM10_Base then
         RCC_Periph.APB2RSTR.TIM10RST := 1;
         RCC_Periph.APB2RSTR.TIM10RST := 0;
      elsif This'Address = TIM11_Base then
         RCC_Periph.APB2RSTR.TIM11RST := 1;
         RCC_Periph.APB2RSTR.TIM11RST := 0;
      elsif This'Address = TIM12_Base then
         RCC_Periph.APB1RSTR.TIM12RST := 1;
         RCC_Periph.APB1RSTR.TIM12RST := 0;
      elsif This'Address = TIM13_Base then
         RCC_Periph.APB1RSTR.TIM13RST := 1;
         RCC_Periph.APB1RSTR.TIM13RST := 0;
      elsif This'Address = TIM14_Base then
         RCC_Periph.APB1RSTR.TIM14RST := 1;
         RCC_Periph.APB1RSTR.TIM14RST := 0;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   --------------------
   -- Set_PLLSAIDIVR --
   --------------------

   procedure Set_PLLSAIDIVR
     (Factor : UInt2)
   is
   begin
      STM32_SVD.RCC.RCC_Periph.DCKCFGR.PLLSAIDIVR := Factor;
   end Set_PLLSAIDIVR;

end STM32.Device;
