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

--  This private package provides an I/O abstraction layer for the LIS3DSH
--  accelerometer chip.

--  NB: The package uses specific SPI and GPIO ports. See the private part of
--  the package declaration, below, for those used.

with Ada.Interrupts.Names;  use Ada.Interrupts.Names;

with STM32;        use STM32;
with STM32.GPIO;   use STM32.GPIO;
with STM32.SPI;    use STM32.SPI;
with STM32.RCC;
with STM32.Device; use STM32.Device;

private package STM32.LIS3DSH.IO is

   procedure Initialize
     with Post => Initialized;
   --  Initializes the I/O hardware required to communicate with the
   --  accelerometer chip. No effect after first call.

   procedure Write
     (Value     : Byte;
      WriteAddr : Register_Address)
     with Pre => Initialized;

   procedure Read
     (Value    : out Byte;
      ReadAddr : Register_Address)
     with Pre => Initialized;

   procedure Configure_Interrupt;

   function Initialized return Boolean;

private

   IO_Initialized : Boolean := False;

   Chip_Select_Pin  : GPIO_Pin  renames Pin_3;
   Chip_Select_Port : GPIO_Port renames GPIO_E;

   procedure Enable_SPIx_Chip_Select_Clock renames RCC.GPIOE_Clock_Enable;

   Device_Interrupt_GPIO_Port : GPIO_Port renames Chip_Select_Port;
   Device_Interrupt1_Pin      : GPIO_Pin  renames Pin_0;
   Device_Interrupt2_Pin      : GPIO_Pin  renames Pin_1;

   procedure Enable_Device_Interrupt_GPIO_Clock renames RCC.GPIOE_Clock_Enable;

   SPIx    : SPI_Port renames SPI_1;
   SPIx_AF : GPIO_Alternate_Function renames GPIO_AF_SPI1;

   procedure Enable_SPIx_Clock renames RCC.SPI1_Clock_Enable;

   SPIx_GPIO_Port : GPIO_Port renames GPIO_A;
   SPIx_SCK_Pin   : GPIO_Pin renames Pin_5;
   SPIx_MISO_Pin  : GPIO_Pin renames Pin_6;
   SPIx_MOSI_Pin  : GPIO_Pin renames Pin_7;

   procedure Enable_SPIx_GPIO_Clock renames RCC.GPIOA_Clock_Enable;

   use Ada.Interrupts;

   Device_Int1_EXTI_IRQn : Interrupt_Id renames EXTI0_Interrupt;
   Device_Int2_EXTI_IRQn : Interrupt_Id renames EXTI1_Interrupt;

end STM32.LIS3DSH.IO;
