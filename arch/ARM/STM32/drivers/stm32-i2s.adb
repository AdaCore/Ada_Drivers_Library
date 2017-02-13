------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_i2s.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.0.0                                                        --
--   @date    18-February-2014                                              --
--   @brief   I2S HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with STM32.Device; use STM32.Device;

package body STM32.I2S is

   function To_UInt16 is new Ada.Unchecked_Conversion (Integer_16,
                                                            UInt16);
   function To_Integer_16 is new Ada.Unchecked_Conversion (UInt16,
                                                           Integer_16);
   ---------------
   -- Configure --
   ---------------

   procedure Configure (This : in out I2S_Port; Conf : I2S_Configuration) is
   begin
      This.Periph.I2SCFGR.I2SCFG := (case Conf.Mode is
                                        when Slave_Transmit  => 2#00#,
                                        when Slave_Receive   => 2#01#,
                                        when Master_Transmit => 2#10#,
                                        when Master_Receive  => 2#11#);

      This.Periph.I2SCFGR.PCMSYNC := Conf.Syncho = Long_Frame_Synchro;

      This.Periph.I2SCFGR.I2SSTD := (case Conf.Standard is
                                        when I2S_Philips_Standard   => 2#00#,
                                        when MSB_Justified_Standard => 2#01#,
                                        when LSB_Justified_Standard => 2#10#,
                                        when PCM_Standard           => 2#11#);

      This.Periph.I2SCFGR.CKPOL := Conf.Clock_Polarity = Steady_State_High;

      This.Periph.I2SCFGR.DATLEN := (case Conf.Data_Length is
                                        when Data_16bits => 2#00#,
                                        when Data_24bits => 2#01#,
                                        when Data_32bits => 2#10#);

      This.Periph.I2SCFGR.CHLEN := Conf.Chan_Length = Channel_32bits;

      This.Periph.I2SPR.MCKOE := Conf.Master_Clock_Out_Enabled;

      This.Periph.CR2.TXDMAEN := Conf.Transmit_DMA_Enabled;
      This.Periph.CR2.RXDMAEN := Conf.Receive_DMA_Enabled;
   end Configure;

   -----------------
   -- In_I2S_Mode --
   -----------------

   function In_I2S_Mode (This : in out I2S_Port) return Boolean is
     (This.Periph.I2SCFGR.I2SMOD);

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out I2S_Port) is
   begin
      if This.Periph.CR1.SPE then
         raise Program_Error with "Device already enabled in SPI mode";
      end if;
      This.Periph.I2SCFGR.I2SMOD := True;
      This.Periph.I2SCFGR.I2SE   := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out I2S_Port) is
   begin
      This.Periph.I2SCFGR.I2SE := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : I2S_Port) return Boolean is
   begin
      return This.Periph.I2SCFGR.I2SE;
   end Enabled;

   ---------------------------
   -- Data_Register_Address --
   ---------------------------

   function Data_Register_Address (This : I2S_Port)
                                   return System.Address
   is
   begin
      return This.Periph.DR'Address;
   end Data_Register_Address;

   -------------------
   -- Set_Frequency --
   -------------------

   overriding
   procedure Set_Frequency (This      : in out I2S_Port;
                            Frequency : Audio_Frequency)
   is
      I2SCLK       : constant UInt32 := System_Clock_Frequencies.I2SCLK;
      Real_Divider : UInt32;
      Packet_Len   : constant UInt32 := (if This.Periph.I2SCFGR.DATLEN = 0
                                         then 1 else 2);
      Is_Odd       : Boolean;
      Divider      : UInt32;
   begin
      if This.Periph.I2SPR.MCKOE then
         Real_Divider := (I2SCLK / 256) * 10;
      else
         Real_Divider := (I2SCLK / (32 * Packet_Len)) * 10;
      end if;

      Real_Divider := ((Real_Divider / Frequency'Enum_Rep) + 5) / 10;

      Is_Odd := Real_Divider mod 2 = 1;

      if Is_Odd then
         Divider := (Real_Divider - 1) / 2;
      else
         Divider := Real_Divider / 2;
      end if;

      if Divider < 2 or else Divider > 255 then
         --  Value out of bounds, use default value
         Divider := 2;
         Is_Odd  := False;
      end if;

      This.Periph.I2SPR.ODD    := Is_Odd;
      This.Periph.I2SPR.I2SDIV := UInt8 (Divider);
   end Set_Frequency;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (This : in out I2S_Port;
      Data : out Audio_Buffer)
   is
   begin
      for Elt of Data loop
         while not This.Periph.SR.RXNE loop
            null;
         end loop;
         Elt := To_Integer_16 (This.Periph.DR.DR);
      end loop;
   end Receive;

   --------------
   -- Transmit --
   --------------

   overriding procedure Transmit
     (This : in out I2S_Port;
      Data : Audio_Buffer)
   is
   begin
      for Elt of Data loop
         while not This.Periph.SR.TXE loop
            null;
         end loop;
         This.Periph.DR.DR := To_UInt16 (Elt);
      end loop;
   end Transmit;

end STM32.I2S;
