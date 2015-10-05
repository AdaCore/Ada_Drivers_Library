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
--   @file    stm32f429i_discovery_ts.h                                     --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file contains all the functions prototypes for the       --
--            stm32f429i_discovery_ts.c driver.                             --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32F4.LTDC;
with STM32F4.I2C;   use STM32F4.I2C;
with STM32F4.GPIO;  use STM32F4.GPIO;

with STM32F429_Discovery;  use STM32F429_Discovery;

package STM32F4.Touch_Panel is

   type TP_State is record
      Touch_Detected : Boolean;
      X : LTDC.Width;
      Y : LTDC.Height;
      Z : Half_Word;
   end record;

   procedure Initialize;

   function Current_State return TP_State;

private

   SCL_GPIO : GPIO_Port renames GPIO_A;
   SCL_Pin  : constant GPIO_Pin := Pin_8;
   SCL_AF   : GPIO_Alternate_Function := GPIO_AF_I2C3;

   SDA_GPIO : GPIO_Port renames GPIO_C;
   SDA_Pin  : constant GPIO_Pin := Pin_9;
   SDA_AF   : constant GPIO_Alternate_Function := GPIO_AF_I2C3;

   TP_I2C   : I2C_Port renames I2C_3;

   IOE_ADDR : constant Byte := 16#82#;

   --  Control Registers
   IOE_REG_SYS_CTRL1 : constant Byte := 16#03#;
   IOE_REG_SYS_CTRL2 : constant Byte := 16#04#;
   IOE_REG_SPI_CFG   : constant Byte := 16#08#;

   --  Touch Panel Registers
   IOE_REG_TP_CTRL      : constant Byte := 16#40#;
   IOE_REG_TP_CFG       : constant Byte := 16#41#;
   IOE_REG_WDM_TR_X     : constant Byte := 16#42#;
   IOE_REG_WDM_TR_Y     : constant Byte := 16#44#;
   IOE_REG_WDM_BL_X     : constant Byte := 16#46#;
   IOE_REG_WDM_BL_Y     : constant Byte := 16#48#;
   IOE_REG_FIFO_TH      : constant Byte := 16#4A#;
   IOE_REG_FIFO_STA     : constant Byte := 16#4B#;
   IOE_REG_FIFO_SIZE    : constant Byte := 16#4C#;
   IOE_REG_TP_DATA_X    : constant Byte := 16#4D#;
   IOE_REG_TP_DATA_Y    : constant Byte := 16#4F#;
   IOE_REG_TP_DATA_Z    : constant Byte := 16#51#;
   IOE_REG_TP_DATA_XYZ  : constant Byte := 16#52#;
   IOE_REG_TP_FRACT_XYZ : constant Byte := 16#56#;
   IOE_REG_TP_DATA      : constant Byte := 16#57#;
   IOE_REG_TP_I_DRIVE   : constant Byte := 16#58#;
   IOE_REG_TP_SHIELD    : constant Byte := 16#59#;

   --  IOE GPIO Registers
   IOE_REG_GPIO_SET_PIN : constant Byte := 16#10#;
   IOE_REG_GPIO_CLR_PIN : constant Byte := 16#11#;
   IOE_REG_GPIO_MP_STA  : constant Byte := 16#12#;
   IOE_REG_GPIO_DIR     : constant Byte := 16#13#;
   IOE_REG_GPIO_ED      : constant Byte := 16#14#;
   IOE_REG_GPIO_RE      : constant Byte := 16#15#;
   IOE_REG_GPIO_FE      : constant Byte := 16#16#;
   IOE_REG_GPIO_AF      : constant Byte := 16#17#;

   --  IOE Functions
   IOE_ADC_FCT : constant Byte := 16#01#;
   IOE_TP_FCT  : constant Byte := 16#02#;
   IOE_IO_FCT  : constant Byte := 16#04#;

   --  ADC Registers
   IOE_REG_ADC_INT_EN   : constant Byte := 16#0E#;
   IOE_REG_ADC_INT_STA  : constant Byte := 16#0F#;
   IOE_REG_ADC_CTRL1    : constant Byte := 16#20#;
   IOE_REG_ADC_CTRL2    : constant Byte := 16#21#;
   IOE_REG_ADC_CAPT     : constant Byte := 16#22#;
   IOE_REG_ADC_DATA_CH0 : constant Byte := 16#30#;
   IOE_REG_ADC_DATA_CH1 : constant Byte := 16#32#;
   IOE_REG_ADC_DATA_CH2 : constant Byte := 16#34#;
   IOE_REG_ADC_DATA_CH3 : constant Byte := 16#36#;
   IOE_REG_ADC_DATA_CH4 : constant Byte := 16#38#;
   IOE_REG_ADC_DATA_CH5 : constant Byte := 16#3A#;
   IOE_REG_ADC_DATA_CH6 : constant Byte := 16#3B#;
   IOE_REG_ADC_DATA_CH7 : constant Byte := 16#3C#;

   --  Interrupt Control Registers
   IOE_REG_INT_CTRL     : constant Byte := 16#09#;
   IOE_REG_INT_EN       : constant Byte := 16#0A#;
   IOE_REG_INT_STA      : constant Byte := 16#0B#;
   IOE_REG_GPIO_INT_EN  : constant Byte := 16#0C#;
   IOE_REG_GPIO_INT_STA : constant Byte := 16#0D#;

   --  touch Panel Pins
   TOUCH_YD     : constant Byte := 16#02#;
   TOUCH_XD     : constant Byte := 16#04#;
   TOUCH_YU     : constant Byte := 16#08#;
   TOUCH_XU     : constant Byte := 16#10#;
   TOUCH_IO_ALL : constant Byte :=
     TOUCH_YD or TOUCH_XD or TOUCH_YU or TOUCH_XU;

end STM32F4.Touch_Panel;
