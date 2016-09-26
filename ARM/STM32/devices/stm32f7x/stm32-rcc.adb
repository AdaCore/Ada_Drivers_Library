------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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
