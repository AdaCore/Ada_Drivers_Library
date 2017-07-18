------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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

with HAL.Touch_Panel;
with HAL.Framebuffer;

private with FT6x06;
private with STM32.Device;
private with STM32.I2C.DMA;
private with STM32.DMA;
private with STM32.DMA.Interrupts;
private with STM32.GPIO;
private with Ada.Interrupts.Names;

package Touch_Panel_FT6x06 is

   type Touch_Panel is limited new HAL.Touch_Panel.Touch_Panel_Device
   with private;

   function Initialize
     (This : in out Touch_Panel;
      Orientation : HAL.Framebuffer.Display_Orientation :=
        HAL.Framebuffer.Default) return Boolean;

   procedure Initialize
     (This : in out Touch_Panel;
      Orientation : HAL.Framebuffer.Display_Orientation :=
        HAL.Framebuffer.Default);

   procedure Set_Orientation
     (This        : in out Touch_Panel;
      Orientation : HAL.Framebuffer.Display_Orientation);

private

   TP_I2C        : STM32.I2C.DMA.I2C_Port_DMA renames STM32.Device.I2C_1_DMA;
   TP_I2C_SCL    : STM32.GPIO.GPIO_Point renames STM32.Device.PB8;
   TP_I2C_SDA    : STM32.GPIO.GPIO_Point renames STM32.Device.PB9;
   TP_I2C_AF     : STM32.GPIO_Alternate_Function renames STM32.Device.GPIO_AF_I2C1_4;

   I2C_TX_RX_DMA     : STM32.DMA.DMA_Controller renames STM32.Device.DMA_1;

   I2C_TX_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames STM32.DMA.Channel_1;
   I2C_TX_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames STM32.DMA.Stream_6;
   I2C_TX_DMA_Int    : aliased STM32.DMA.Interrupts.DMA_Interrupt_Controller
     (I2C_TX_RX_DMA'Access,
      I2C_TX_DMA_Stream,
      Ada.Interrupts.Names.DMA1_Stream6_Interrupt);

   I2C_RX_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames STM32.DMA.Channel_1;
   I2C_RX_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames STM32.DMA.Stream_0;
   I2C_RX_DMA_Int    : aliased STM32.DMA.Interrupts.DMA_Interrupt_Controller
     (I2C_TX_RX_DMA'Access,
      I2C_RX_DMA_Stream,
      Ada.Interrupts.Names.DMA1_Stream0_Interrupt);

   type Touch_Panel is limited new FT6x06.FT6x06_Device
     (Port     => TP_I2C'Access,
      I2C_Addr => 16#54#) with null record;

end Touch_Panel_FT6x06;
