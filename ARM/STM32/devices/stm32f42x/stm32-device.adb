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

with System; use System;

pragma Warnings (Off, "* is an internal GNAT unit");
with System.BB.Parameters;
pragma Warnings (On, "* is an internal GNAT unit");

with STM32_SVD.RCC; use STM32_SVD.RCC;

package body STM32.Device is

   HSE_VALUE : constant Word :=
                 Word (System.BB.Parameters.HSE_Clock);
   --  External oscillator in Hz

   HSI_VALUE : constant := 16_000_000;
   --  Internal oscillator in Hz

   HPRE_Presc_Table : constant array (UInt4) of Word :=
     (1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 8, 16, 64, 128, 256, 512);

   PPRE_Presc_Table : constant array (UInt3) of Word :=
     (1, 1, 1, 1, 2, 4, 8, 16);

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out Internal_GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.AHB1ENR.GPIOAEN := True;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.AHB1ENR.GPIOBEN := True;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.AHB1ENR.GPIOCEN := True;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.AHB1ENR.GPIODEN := True;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.AHB1ENR.GPIOEEN := True;
      elsif This'Address = GPIOF_Base then
         RCC_Periph.AHB1ENR.GPIOFEN := True;
      elsif This'Address = GPIOG_Base then
         RCC_Periph.AHB1ENR.GPIOGEN := True;
      elsif This'Address = GPIOH_Base then
         RCC_Periph.AHB1ENR.GPIOHEN := True;
      elsif This'Address = GPIOI_Base then
         RCC_Periph.AHB1ENR.GPIOIEN := True;
      elsif This'Address = GPIOJ_Base then
         RCC_Periph.AHB1ENR.GPIOJEN := True;
      elsif This'Address = GPIOK_Base then
         RCC_Periph.AHB1ENR.GPIOKEN := True;
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
      Enable_Clock (Point.Periph.all);
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Points : GPIO_Points)
   is
   begin
      for Point of Points loop
         Enable_Clock (Point.Periph.all);
      end loop;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out Internal_GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.AHB1RSTR.GPIOARST := True;
         RCC_Periph.AHB1RSTR.GPIOARST := False;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.AHB1RSTR.GPIOBRST := True;
         RCC_Periph.AHB1RSTR.GPIOBRST := False;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.AHB1RSTR.GPIOCRST := True;
         RCC_Periph.AHB1RSTR.GPIOCRST := False;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.AHB1RSTR.GPIODRST := True;
         RCC_Periph.AHB1RSTR.GPIODRST := False;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.AHB1RSTR.GPIOERST := True;
         RCC_Periph.AHB1RSTR.GPIOERST := False;
      elsif This'Address = GPIOF_Base then
         RCC_Periph.AHB1RSTR.GPIOFRST := True;
         RCC_Periph.AHB1RSTR.GPIOFRST := False;
      elsif This'Address = GPIOG_Base then
         RCC_Periph.AHB1RSTR.GPIOGRST := True;
         RCC_Periph.AHB1RSTR.GPIOGRST := False;
      elsif This'Address = GPIOH_Base then
         RCC_Periph.AHB1RSTR.GPIOHRST := True;
         RCC_Periph.AHB1RSTR.GPIOHRST := False;
      elsif This'Address = GPIOI_Base then
         RCC_Periph.AHB1RSTR.GPIOIRST := True;
         RCC_Periph.AHB1RSTR.GPIOIRST := False;
      elsif This'Address = GPIOJ_Base then
         RCC_Periph.AHB1RSTR.GPIOJRST := True;
         RCC_Periph.AHB1RSTR.GPIOJRST := False;
      elsif This'Address = GPIOK_Base then
         RCC_Periph.AHB1RSTR.GPIOKRST := True;
         RCC_Periph.AHB1RSTR.GPIOKRST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Point : GPIO_Point) is
   begin
      Reset (Point.Periph.all);
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
            if Points (K).Periph = Points (J).Periph then
               Do_Reset := False;

               exit;
            end if;
         end loop;

         if Do_Reset then
            Reset (Points (J).Periph.all);
         end if;
      end loop;
   end Reset;

   ---------------------
   -- As_GPIO_Port_Id --
   ---------------------

   function As_GPIO_Port_Id (Port : Internal_GPIO_Port) return GPIO_Port_Id is
   begin
      --  TODO: rather ugly to have this board-specific range here
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

   procedure Enable_Clock (This : aliased in out Analog_To_Digital_Converter)
   is
   begin
      if This'Address = ADC1_Base then
         RCC_Periph.APB2ENR.ADC1EN := True;
      elsif This'Address = ADC2_Base then
         RCC_Periph.APB2ENR.ADC2EN := True;
      elsif This'Address = ADC3_Base then
         RCC_Periph.APB2ENR.ADC3EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -------------------------
   -- Reset_All_ADC_Units --
   -------------------------

   procedure Reset_All_ADC_Units is
   begin
      RCC_Periph.APB2RSTR.ADCRST := True;
      RCC_Periph.APB2RSTR.ADCRST := False;
   end Reset_All_ADC_Units;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter)
   is
      pragma Unreferenced (This);
   begin
      RCC_Periph.APB1ENR.DACEN := True;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out Digital_To_Analog_Converter) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.APB1RSTR.DACRST := True;
      RCC_Periph.APB1RSTR.DACRST := False;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out USART) is
   begin
      if This'Address = USART1_Base then
         RCC_Periph.APB2ENR.USART1EN := True;
      elsif This'Address = USART2_Base then
         RCC_Periph.APB1ENR.USART2EN := True;
      elsif This'Address = USART3_Base then
         RCC_Periph.APB1ENR.USART3EN := True;
      elsif This'Address = UART4_Base then
         RCC_Periph.APB1ENR.UART4EN := True;
      elsif This'Address = UART5_Base then
         RCC_Periph.APB1ENR.UART5EN := True;
      elsif This'Address = USART6_Base then
         RCC_Periph.APB2ENR.USART6EN := True;
      elsif This'Address = UART7_Base then
         RCC_Periph.APB1ENR.UART7ENR := True;
      elsif This'Address = UART8_Base then
         RCC_Periph.APB1ENR.UART8ENR := True;
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
         RCC_Periph.APB2RSTR.USART1RST := True;
         RCC_Periph.APB2RSTR.USART1RST := False;
      elsif This'Address = USART2_Base then
         RCC_Periph.APB1RSTR.UART2RST := True;
         RCC_Periph.APB1RSTR.UART2RST := False;
      elsif This'Address = USART3_Base then
         RCC_Periph.APB1RSTR.UART3RST := True;
         RCC_Periph.APB1RSTR.UART3RST := False;
      elsif This'Address = UART4_Base then
         RCC_Periph.APB1RSTR.UART4RST := True;
         RCC_Periph.APB1RSTR.UART4RST := False;
      elsif This'Address = UART5_Base then
         RCC_Periph.APB1RSTR.UART5RST := True;
         RCC_Periph.APB1RSTR.UART5RST := False;
      elsif This'Address = USART6_Base then
         RCC_Periph.APB2RSTR.USART6RST := True;
         RCC_Periph.APB2RSTR.USART6RST := False;
      elsif This'Address = UART7_Base then
         RCC_Periph.APB1RSTR.UART7RST := True;
         RCC_Periph.APB1RSTR.UART7RST := False;
      elsif This'Address = UART8_Base then
         RCC_Periph.APB1RSTR.UART8RST := True;
         RCC_Periph.APB1RSTR.UART8RST := False;
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
         RCC_Periph.AHB1ENR.DMA1EN := True;
      elsif This'Address = STM32_SVD.DMA2_Base then
         RCC_Periph.AHB1ENR.DMA2EN := True;
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
         RCC_Periph.AHB1RSTR.DMA1RST := True;
         RCC_Periph.AHB1RSTR.DMA1RST := False;
      elsif This'Address = STM32_SVD.DMA2_Base then
         RCC_Periph.AHB1RSTR.DMA2RST := True;
         RCC_Periph.AHB1RSTR.DMA2RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ----------------
   -- As_Port_Id --
   ----------------

   function As_Port_Id (Port : I2C_Port) return I2C_Port_Id is
   begin
      if Port.Periph.all'Address = I2C1_Base then
         return I2C_Id_1;
      elsif Port.Periph.all'Address = I2C2_Base then
         return I2C_Id_2;
      elsif Port.Periph.all'Address = I2C3_Base then
         return I2C_Id_3;
      else
         raise Unknown_Device;
      end if;
   end As_Port_Id;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : I2C_Port) is
   begin
      Enable_Clock (As_Port_Id (This));
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : I2C_Port_Id) is
   begin
      case This is
         when I2C_Id_1 =>
            RCC_Periph.APB1ENR.I2C1EN := True;
         when I2C_Id_2 =>
            RCC_Periph.APB1ENR.I2C2EN := True;
         when I2C_Id_3 =>
            RCC_Periph.APB1ENR.I2C3EN := True;
      end case;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : I2C_Port) is
   begin
      Reset (As_Port_Id (This));
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : I2C_Port_Id) is
   begin
      case This is
         when I2C_Id_1 =>
            RCC_Periph.APB1RSTR.I2C1RST := True;
            RCC_Periph.APB1RSTR.I2C1RST := False;
         when I2C_Id_2 =>
            RCC_Periph.APB1RSTR.I2C2RST := True;
            RCC_Periph.APB1RSTR.I2C2RST := False;
         when I2C_Id_3 =>
            RCC_Periph.APB1RSTR.I2C3RST := True;
            RCC_Periph.APB1RSTR.I2C3RST := False;
      end case;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : SPI_Port) is
   begin
      if This.Periph.all'Address = SPI1_Base then
         RCC_Periph.APB2ENR.SPI1EN := True;
      elsif This.Periph.all'Address = SPI2_Base then
         RCC_Periph.APB1ENR.SPI2EN := True;
      elsif This.Periph.all'Address = SPI3_Base then
         RCC_Periph.APB1ENR.SPI3EN := True;
      elsif This.Periph.all'Address = SPI4_Base then
         RCC_Periph.APB2ENR.SPI4ENR := True;
      elsif This.Periph.all'Address = SPI5_Base then
         RCC_Periph.APB2ENR.SPI5ENR := True;
      elsif This.Periph.all'Address = SPI6_Base then
         RCC_Periph.APB2ENR.SPI6ENR := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : SPI_Port) is
   begin
      if This.Periph.all'Address = SPI1_Base then
         RCC_Periph.APB2RSTR.SPI1RST := True;
         RCC_Periph.APB2RSTR.SPI1RST := False;
      elsif This.Periph.all'Address = SPI2_Base then
         RCC_Periph.APB1RSTR.SPI2RST := True;
         RCC_Periph.APB1RSTR.SPI2RST := False;
      elsif This.Periph.all'Address = SPI3_Base then
         RCC_Periph.APB1RSTR.SPI3RST := True;
         RCC_Periph.APB1RSTR.SPI3RST := False;
      elsif This.Periph.all'Address = SPI4_Base then
         RCC_Periph.APB2RSTR.SPI4RST := True;
         RCC_Periph.APB2RSTR.SPI4RST := False;
      elsif This.Periph.all'Address = SPI5_Base then
         RCC_Periph.APB2RSTR.SPI5RST := True;
         RCC_Periph.APB2RSTR.SPI5RST := False;
      elsif This.Periph.all'Address = SPI6_Base then
         RCC_Periph.APB2RSTR.SPI6RST := True;
         RCC_Periph.APB2RSTR.SPI6RST := False;
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
         RCC_Periph.APB2ENR.TIM1EN := True;
      elsif This'Address = TIM2_Base then
         RCC_Periph.APB1ENR.TIM2EN := True;
      elsif This'Address = TIM3_Base then
         RCC_Periph.APB1ENR.TIM3EN := True;
      elsif This'Address = TIM4_Base then
         RCC_Periph.APB1ENR.TIM4EN := True;
      elsif This'Address = TIM5_Base then
         RCC_Periph.APB1ENR.TIM5EN := True;
      elsif This'Address = TIM6_Base then
         RCC_Periph.APB1ENR.TIM6EN := True;
      elsif This'Address = TIM7_Base then
         RCC_Periph.APB1ENR.TIM7EN := True;
      elsif This'Address = TIM8_Base then
         RCC_Periph.APB2ENR.TIM8EN := True;
      elsif This'Address = TIM9_Base then
         RCC_Periph.APB2ENR.TIM9EN := True;
      elsif This'Address = TIM10_Base then
         RCC_Periph.APB2ENR.TIM10EN := True;
      elsif This'Address = TIM11_Base then
         RCC_Periph.APB2ENR.TIM11EN := True;
      elsif This'Address = TIM12_Base then
         RCC_Periph.APB1ENR.TIM12EN := True;
      elsif This'Address = TIM13_Base then
         RCC_Periph.APB1ENR.TIM13EN := True;
      elsif This'Address = TIM14_Base then
         RCC_Periph.APB1ENR.TIM14EN := True;
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
         RCC_Periph.APB2RSTR.TIM1RST := True;
         RCC_Periph.APB2RSTR.TIM1RST := False;
      elsif This'Address = TIM2_Base then
         RCC_Periph.APB1RSTR.TIM2RST := True;
         RCC_Periph.APB1RSTR.TIM2RST := False;
      elsif This'Address = TIM3_Base then
         RCC_Periph.APB1RSTR.TIM3RST := True;
         RCC_Periph.APB1RSTR.TIM3RST := False;
      elsif This'Address = TIM4_Base then
         RCC_Periph.APB1RSTR.TIM4RST := True;
         RCC_Periph.APB1RSTR.TIM4RST := False;
      elsif This'Address = TIM5_Base then
         RCC_Periph.APB1RSTR.TIM5RST := True;
         RCC_Periph.APB1RSTR.TIM5RST := False;
      elsif This'Address = TIM6_Base then
         RCC_Periph.APB1RSTR.TIM6RST := True;
         RCC_Periph.APB1RSTR.TIM6RST := False;
      elsif This'Address = TIM7_Base then
         RCC_Periph.APB1RSTR.TIM7RST := True;
         RCC_Periph.APB1RSTR.TIM7RST := False;
      elsif This'Address = TIM8_Base then
         RCC_Periph.APB2RSTR.TIM8RST := True;
         RCC_Periph.APB2RSTR.TIM8RST := False;
      elsif This'Address = TIM9_Base then
         RCC_Periph.APB2RSTR.TIM9RST := True;
         RCC_Periph.APB2RSTR.TIM9RST := False;
      elsif This'Address = TIM10_Base then
         RCC_Periph.APB2RSTR.TIM10RST := True;
         RCC_Periph.APB2RSTR.TIM10RST := False;
      elsif This'Address = TIM11_Base then
         RCC_Periph.APB2RSTR.TIM11RST := True;
         RCC_Periph.APB2RSTR.TIM11RST := False;
      elsif This'Address = TIM12_Base then
         RCC_Periph.APB1RSTR.TIM12RST := True;
         RCC_Periph.APB1RSTR.TIM12RST := False;
      elsif This'Address = TIM13_Base then
         RCC_Periph.APB1RSTR.TIM13RST := True;
         RCC_Periph.APB1RSTR.TIM13RST := False;
      elsif This'Address = TIM14_Base then
         RCC_Periph.APB1RSTR.TIM14RST := True;
         RCC_Periph.APB1RSTR.TIM14RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------------------
   -- System_Clock_Frequencies --
   ------------------------------

   function System_Clock_Frequencies return RCC_System_Clocks
   is
      Source       : constant UInt2 := RCC_Periph.CFGR.SWS;
      Result       : RCC_System_Clocks;
   begin
      case Source is
         when 0 =>
            --  HSI as source
            Result.SYSCLK := HSI_VALUE;
         when 1 =>
            --  HSE as source
            Result.SYSCLK := HSE_VALUE;
         when 2 =>
            --  PLL as source
            declare
               HSE_Source : constant Boolean := RCC_Periph.PLLCFGR.PLLSRC;
               Pllm       : constant Word :=
                              Word (RCC_Periph.PLLCFGR.PLLM);
               Plln       : constant Word :=
                              Word (RCC_Periph.PLLCFGR.PLLN);
               Pllp       : constant Word :=
                              (Word (RCC_Periph.PLLCFGR.PLLP) + 1) * 2;
               Pllvco     : Word;
            begin
               if not HSE_Source then
                  Pllvco := (HSI_VALUE / Pllm) * Plln;
               else
                  Pllvco := (HSE_VALUE / Pllm) * Plln;
               end if;
               Result.SYSCLK := Pllvco / Pllp;
            end;
         when others =>
            Result.SYSCLK := HSI_VALUE;
      end case;

      declare
         HPRE  : constant UInt4 := RCC_Periph.CFGR.HPRE;
         PPRE1 : constant UInt3 := RCC_Periph.CFGR.PPRE.Arr (1);
         PPRE2 : constant UInt3 := RCC_Periph.CFGR.PPRE.Arr (2);
      begin
         Result.HCLK  := Result.SYSCLK / HPRE_Presc_Table (HPRE);
         Result.PCLK1 := Result.HCLK / PPRE_Presc_Table (PPRE1);
         Result.PCLK2 := Result.HCLK / PPRE_Presc_Table (PPRE2);

         --  Timer clocks
         --  See Dedicated clock cfg register documentation.
         if not RCC_Periph.DCKCFGR.TIMPRE then
            --  Mode 0:
            --  If the APB prescaler (PPRE1, PPRE2 in the RCC_CFGR register)
            --  is configured to a division factor of 1, TIMxCLK = PCLKx.
            --  Otherwise, the timer clock frequencies are set to twice to the
            --  frequency of the APB domain to which the timers are connected :
            --  TIMxCLK = 2xPCLKx.
            if PPRE_Presc_Table (PPRE1) = 1 then
               Result.TIMCLK1 := Result.PCLK1;
            else
               Result.TIMCLK1 := Result.PCLK1 * 2;
            end if;
            if PPRE_Presc_Table (PPRE2) = 1 then
               Result.TIMCLK2 := Result.PCLK2;
            else
               Result.TIMCLK2 := Result.PCLK2 * 2;
            end if;
         else
            --  Mpde 1:
            --  If the APB prescaler (PPRE1, PPRE2 in the RCC_CFGR register) is
            --  configured to a division factor of 1, 2 or 4, TIMxCLK = HCLK.
            --  Otherwise, the timer clock frequencies are set to four times
            --  to the frequency of the APB domain to which the timers are
            --  connected : TIMxCLK = 4xPCLKx.
            if PPRE_Presc_Table (PPRE1) in 1 .. 4 then
               Result.TIMCLK1 := Result.HCLK;
            else
               Result.TIMCLK1 := Result.PCLK1 * 4;
            end if;
            if PPRE_Presc_Table (PPRE2) in 1 .. 4 then
               Result.TIMCLK2 := Result.HCLK;
            else
               Result.TIMCLK2 := Result.PCLK1 * 4;
            end if;
         end if;
      end;

      return Result;
   end System_Clock_Frequencies;

   ------------------
   -- PLLSAI_Ready --
   ------------------

   function PLLSAI_Ready return Boolean is
   begin
      --  SHould be PLLSAIRDY, but not binded by the SVD file
      --  PLLSAIRDY: bit 29
      return (RCC_Periph.CR.Reserved_28_31 and 2) /= 0;
   end PLLSAI_Ready;

   -------------------
   -- Enable_PLLSAI --
   -------------------;

   procedure Enable_PLLSAI is
   begin
      --  Should be PLLSAION, but not binded by the SVD file
      --  PLLSAION: bit 28
      RCC_Periph.CR.Reserved_28_31 :=
        RCC_Periph.CR.Reserved_28_31 or 1;

      --  Wait for PLLSAI activation
      loop
         exit when PLLSAI_Ready;
      end loop;
   end Enable_PLLSAI;

   -------------------
   -- Enable_PLLSAI --
   -------------------;

   procedure Disable_PLLSAI is
   begin
      --  Should be PLLSAION, but not binded by the SVD file
      --  PLLSAION: bit 28
      RCC_Periph.CR.Reserved_28_31 :=
        RCC_Periph.CR.Reserved_28_31 and not 1;
   end Disable_PLLSAI;

   ------------------------
   -- Set_PLLSAI_Factors --
   ------------------------

   procedure Set_PLLSAI_Factors (LCD  : UInt3;
                                 VCO  : UInt9;
                                 DivR : PLLSAI_DivR)
   is
      PLLSAICFGR : PLLSAICFGR_Register;
   begin
      PLLSAICFGR.PLLSAIR := LCD;
      PLLSAICFGR.PLLSAIN := VCO;
      RCC_Periph.PLLSAICFGR := PLLSAICFGR;

      --  The exact bit name is device-specific
      RCC_Periph.DCKCFGR.PLLSAIDIVR := UInt2 (DivR);
   end Set_PLLSAI_Factors;

   -----------------------
   -- Enable_DCMI_Clock --
   -----------------------

   procedure Enable_DCMI_Clock is
   begin
      RCC_Periph.AHB2ENR.DCMIEN := True;
   end Enable_DCMI_Clock;

   ----------------
   -- Reset_DCMI --
   ----------------

   procedure Reset_DCMI is
   begin
      RCC_Periph.AHB2RSTR.DCMIRST := True;
      RCC_Periph.AHB2RSTR.DCMIRST := False;
   end Reset_DCMI;

end STM32.Device;
