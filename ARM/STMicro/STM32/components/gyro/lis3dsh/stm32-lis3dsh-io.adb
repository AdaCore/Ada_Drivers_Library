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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    lis3dsh.c                                                     --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file provides a set of functions needed to manage the    --
--            LIS3DSH MEMS Accelerometer.                                   --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

<<<<<<< HEAD:ARM/STMicro/STM32/components/gyro/lis3dsh/stm32-lis3dsh-io.adb
with Cortex_M.NVIC;
=======
with STM32F4.NVIC;
with STM32F4.EXTI;
>>>>>>> 885a927... New EXTI package, thus some functionality moved out of SYSCFG package.:ARM/STMicro/STM32/components/stm32f4-lis3dsh-io.adb

package body STM32.LIS3DSH.IO is

   ReadWrite_Cmd_Bit : constant := 16#80#;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is
      (IO_Initialized);

   ----------------------
   -- Chip_Select_High --
   ----------------------

   procedure Chip_Select_High is
   begin
      GPIO.Set (Chip_Select_Pin);
   end Chip_Select_High;

   ---------------------
   -- Chip_Select_Low --
   ---------------------

   procedure Chip_Select_Low is
   begin
      GPIO.Clear (Chip_Select_Pin);
   end Chip_Select_Low;

   -------------------
   -- Init_SPI_GPIO --
   -------------------

   procedure Init_SPI_GPIO is
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (SPIx_SCK_Pin & SPIx_MISO_Pin & SPIx_MOSI_Pin);

      Config.Output_Type := Push_Pull;
      Config.Resistors := Floating;
      Config.Speed := Speed_50MHz;
      Config.Mode := Mode_AF;

      Configure_IO (SPIx_SCK_Pin & SPIx_MISO_Pin & SPIx_MOSI_Pin,
                    Config);

      Configure_Alternate_Function
        (SPIx_SCK_Pin & SPIx_MISO_Pin & SPIx_MOSI_Pin,
         SPIx_AF);
   end Init_SPI_GPIO;

   --------------
   -- Init_SPI --
   --------------

   procedure Init_SPI is
      Config : SPI_Configuration;
   begin
      Enable_Clock (SPIx);

      Config.Mode := Master;
      Config.Baud_Rate_Prescaler := BRP_32;
      Config.Clock_Polarity := Low;
      Config.Clock_Phase := P1Edge;
      Config.First_Bit := MSB;
      Config.CRC_Poly := 7;
      Config.Slave_Management := Software_Managed;  --  essential!!
      Config.Direction := D2Lines_FullDuplex;
      Config.Data_Size := Data_8;

      Disable (SPIx);
      Configure (SPIx, Config);
      Enable (SPIx);
   end Init_SPI;

   ------------------------------
   -- Init_LIS3DSH_Chip_Select --
   ------------------------------

   procedure Init_LIS3DSH_Chip_Select is
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (Chip_Select_Pin);

      Config.Mode := Mode_Out;
      Config.Output_Type := Push_Pull;
      Config.Resistors := Pull_Up;
      Config.Speed := Speed_25MHz;
      Configure_IO (Chip_Select_Pin, Config);

      Chip_Select_High;
   end Init_LIS3DSH_Chip_Select;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not IO_Initialized then
         Init_SPI_GPIO;
         Init_SPI;
         Init_LIS3DSH_Chip_Select;

         IO_Initialized := True;
      end if;
   end Initialize;

   -----------
   -- Write --
   -----------

   procedure Write
     (Value     : Byte;
      WriteAddr : Register_Address)
   is
   begin
      Chip_Select_Low;
      SPI.Transmit (SPIx, Byte (WriteAddr));
      SPI.Transmit (SPIx, Value);
      Chip_Select_High;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (Value    : out Byte;
      ReadAddr : Register_Address)
   is
      Source : constant Register_Address := ReadAddr or ReadWrite_Cmd_Bit;
   begin
      Chip_Select_Low;
      SPI.Transmit (SPIx, Byte (Source));
      SPI.Receive (SPIx, Value);
      Chip_Select_High;
   end Read;

   -------------------------
   -- Configure_Interrupt --
   -------------------------

   procedure Configure_Interrupt is
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (Device_Interrupt1_Pin & Device_Interrupt2_Pin);

      Config.Mode := Mode_In;
      Config.Speed := Speed_50MHz;
      Config.Resistors := Floating;

      Configure_IO (Device_Interrupt2_Pin,
                    Config);

<<<<<<< HEAD:ARM/STMicro/STM32/components/gyro/lis3dsh/stm32-lis3dsh-io.adb
      Configure_Trigger (Device_Interrupt2_Pin,
                         Trigger => Interrupt_Rising_Edge);
=======
      Configure_Trigger (Device_Interrupt_GPIO_Port,
                         Device_Interrupt2_Pin,
                         Trigger => EXTI.Interrupt_Rising_Edge);
>>>>>>> 885a927... New EXTI package, thus some functionality moved out of SYSCFG package.:ARM/STMicro/STM32/components/stm32f4-lis3dsh-io.adb

      Cortex_M.NVIC.Set_Priority (Device_Int2_EXTI_IRQn,
                                  Preempt_Priority => 16#F#,
                                  Subpriority      => 0);

      Cortex_M.NVIC.Enable_Interrupt (Device_Int2_EXTI_IRQn);
   end Configure_Interrupt;

end STM32.LIS3DSH.IO;
