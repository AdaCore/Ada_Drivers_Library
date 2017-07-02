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
--   @file    stm32f4xx_hal_i2s.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.0.0                                                        --
--   @date    18-February-2014                                              --
--   @brief   Header file of I2S HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides definitions for the STM32F4 (ARM Cortex M4F
--  from ST Microelectronics) Inter-IC Sound (I2S) facility.

private with STM32_SVD.SPI;
with HAL.Audio; use HAL.Audio;
with System;

package STM32.I2S is

   type I2S_Mode is (Slave_Transmit,
                     Slave_Receive,
                     Master_Transmit,
                     Master_Receive);

   type I2S_Standard is (I2S_Philips_Standard,
                         MSB_Justified_Standard,
                         LSB_Justified_Standard,
                         PCM_Standard);

   type I2S_Synchronization is (Short_Frame_Syncrho,
                                Long_Frame_Synchro);

   type I2S_Clock_Polarity is (Steady_State_Low,
                               Steady_State_High);

   type I2S_Data_Length is (Data_16bits, Data_24bits, Data_32bits);
   type I2S_Channel_Length is (Channel_16bits, Channel_32bits);

   type I2S_Linear_Prescaler is range 2 .. 255;

   type I2S_Configuration is record
      Mode                     : I2S_Mode;
      Standard                 : I2S_Standard;
      Syncho                   : I2S_Synchronization;
      Clock_Polarity           : I2S_Clock_Polarity;
      Data_Length              : I2S_Data_Length;
      Chan_Length              : I2S_Channel_Length;
      Master_Clock_Out_Enabled : Boolean;
      Transmit_DMA_Enabled     : Boolean;
      Receive_DMA_Enabled      : Boolean;
   end record;

   type Internal_I2S_Port is private;

   type I2S_Port (Periph   : not null access Internal_I2S_Port;
                  Extended : Boolean) is
      limited new Audio_Stream with private;

   function In_I2S_Mode (This : in out I2S_Port) return Boolean;
   --  I2S peripherals are shared with SPI, this function returns True only if
   --  the periperal is configure in I2S mode.

   function Valid_Extended_Config (Conf : I2S_Configuration) return Boolean
   is ((Conf.Mode = Slave_Transmit or else Conf.Mode = Slave_Receive)
         and then
       not Conf.Master_Clock_Out_Enabled);

   procedure Configure (This : in out I2S_Port; Conf : I2S_Configuration)
     with Pre  => not This.Enabled
                    and then
                  (not This.Extended or else Valid_Extended_Config (Conf)),
          Post => not This.Enabled;

   procedure Enable (This : in out I2S_Port)
     with Post => This.Enabled and This.In_I2S_Mode;

   procedure Disable (This : in out I2S_Port)
     with Pre => This.In_I2S_Mode;

   function Enabled (This : I2S_Port) return Boolean;

   function Data_Register_Address (This : I2S_Port)
                                   return System.Address;
   --  For DMA transfer

   overriding
   procedure Set_Frequency (This      : in out I2S_Port;
                            Frequency : Audio_Frequency)
     with Pre  => not This.Enabled;

   overriding
   procedure Transmit (This : in out I2S_Port;
                       Data : Audio_Buffer)
     with Pre => This.In_I2S_Mode;

      overriding
   procedure Receive (This : in out I2S_Port;
                      Data : out Audio_Buffer)
     with Pre => This.In_I2S_Mode;

private

   type Internal_I2S_Port is new STM32_SVD.SPI.SPI_Peripheral;

   type I2S_Port (Periph   : not null access Internal_I2S_Port;
                  Extended : Boolean) is
     limited new Audio_Stream with null record;

end STM32.I2S;
