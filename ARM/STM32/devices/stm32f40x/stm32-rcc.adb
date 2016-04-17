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

with Ada.Unchecked_Conversion;
with STM32.Device;             use STM32.Device;
with STM32_SVD.PWR;            use STM32_SVD.PWR;
with STM32_SVD.RCC;            use STM32_SVD.RCC;

package body STM32.RCC is

   function To_AHB1RSTR_T is new Ada.Unchecked_Conversion
     (Word, AHB1RSTR_Register);
   function To_AHB2RSTR_T is new Ada.Unchecked_Conversion
     (Word, AHB2RSTR_Register);
   function To_APB1RSTR_T is new Ada.Unchecked_Conversion
     (Word, APB1RSTR_Register);
   function To_APB2RSTR_T is new Ada.Unchecked_Conversion
     (Word, APB2RSTR_Register);

   ---------------------------------------------------------------------------
   -------  Enable/Disable/Reset Routines  -----------------------------------
   ---------------------------------------------------------------------------

   procedure CRC_Clock_Enable is
   begin
      RCC_Periph.AHB1ENR.CRCEN := True;
   end CRC_Clock_Enable;

   procedure BKPSRAM_Clock_Enable is
   begin
      RCC_Periph.AHB1ENR.BKPSRAMEN := True;
   end BKPSRAM_Clock_Enable;

   procedure WWDG_Clock_Enable is
   begin
      RCC_Periph.APB1ENR.WWDGEN := True;
   end WWDG_Clock_Enable;

   procedure SYSCFG_Clock_Enable is
   begin
      RCC_Periph.APB2ENR.SYSCFGEN := True;
   end SYSCFG_Clock_Enable;

   procedure AHB1_Force_Reset
   is
   begin
      RCC_Periph.AHB1RSTR := To_AHB1RSTR_T (16#FFFF_FFFF#);
   end AHB1_Force_Reset;

   procedure AHB1_Release_Reset is
   begin
      RCC_Periph.AHB1RSTR := To_AHB1RSTR_T (0);
   end AHB1_Release_Reset;

   procedure AHB2_Force_Reset is
   begin
      RCC_Periph.AHB2RSTR := To_AHB2RSTR_T (16#FFFF_FFFF#);
   end AHB2_Force_Reset;

   procedure AHB2_Release_Reset is
   begin
      RCC_Periph.AHB2RSTR := To_AHB2RSTR_T (0);
   end AHB2_Release_Reset;

   procedure APB1_Force_Reset is
   begin
      RCC_Periph.APB1RSTR := To_APB1RSTR_T (16#FFFF_FFFF#);
   end APB1_Force_Reset;

   procedure APB1_Release_Reset is
   begin
      RCC_Periph.APB1RSTR := To_APB1RSTR_T (0);
   end APB1_Release_Reset;

   procedure APB2_Force_Reset is
   begin
      RCC_Periph.APB2RSTR := To_APB2RSTR_T (16#FFFF_FFFF#);
   end APB2_Force_Reset;

   procedure APB2_Release_Reset is
   begin
      RCC_Periph.APB2RSTR := To_APB2RSTR_T (0);
   end APB2_Release_Reset;

   procedure CRC_Force_Reset is
   begin
      RCC_Periph.AHB1RSTR.CRCRST := True;
   end CRC_Force_Reset;

   procedure CRC_Release_Reset is
   begin
      RCC_Periph.AHB1RSTR.CRCRST := False;
   end CRC_Release_Reset;

   procedure OTGFS_Force_Reset is
   begin
      RCC_Periph.AHB2RSTR.OTGFSRST := True;
   end OTGFS_Force_Reset;

   procedure OTGFS_Release_Reset is
   begin
      RCC_Periph.AHB2RSTR.OTGFSRST := False;
   end OTGFS_Release_Reset;

   procedure WWDG_Force_Reset is
   begin
      RCC_Periph.APB1RSTR.WWDGRST := True;
   end WWDG_Force_Reset;

   procedure WWDG_Release_Reset is
   begin
      RCC_Periph.APB1RSTR.WWDGRST := False;
   end WWDG_Release_Reset;

   procedure SYSCFG_Force_Reset is
   begin
      RCC_Periph.APB2RSTR.SYSCFGRST := True;
   end SYSCFG_Force_Reset;

   procedure SYSCFG_Release_Reset is
   begin
      RCC_Periph.APB2RSTR.SYSCFGRST := False;
   end SYSCFG_Release_Reset;

end STM32.RCC;
