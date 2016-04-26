with System;        use System;
with STM32_SVD.RCC; use STM32_SVD.RCC;

package body STM32.RCC is

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : STM32_SVD.I2C.I2C_Peripheral)
   is
      Addr : constant System.Address := This'Address;
   begin
      if Addr = STM32_SVD.I2C1_Base then
         RCC_Periph.APB1ENR.I2C1EN := True;
      elsif Addr = STM32_SVD.I2C2_Base then
         RCC_Periph.APB1ENR.I2C2EN := True;
      elsif Addr = STM32_SVD.I2C3_Base then
         RCC_Periph.APB1ENR.I2C3EN := True;
      elsif Addr = STM32_SVD.I2C4_Base then
         RCC_Periph.APB1ENR.I2C4EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : STM32_SVD.I2C.I2C_Peripheral)
   is
      Addr : constant System.Address := This'Address;
   begin
      if Addr = STM32_SVD.I2C1_Base then
         RCC_Periph.APB1RSTR.I2C1RST := True;
         RCC_Periph.APB1RSTR.I2C1RST := False;
      elsif Addr = STM32_SVD.I2C2_Base then
         RCC_Periph.APB1RSTR.I2C2RST := True;
         RCC_Periph.APB1RSTR.I2C2RST := False;
      elsif Addr = STM32_SVD.I2C3_Base then
         RCC_Periph.APB1RSTR.I2C3RST := True;
         RCC_Periph.APB1RSTR.I2C3RST := False;
      elsif Addr = STM32_SVD.I2C4_Base then
         RCC_Periph.APB1RSTR.I2C4RST := True;
         RCC_Periph.APB1RSTR.I2C4RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -------------------------
   -- SYSCFG_Clock_Enable --
   -------------------------

   procedure SYSCFG_Clock_Enable is
   begin
      RCC_Periph.APB2ENR.SYSCFGEN := True;
   end SYSCFG_Clock_Enable;

end STM32.RCC;
