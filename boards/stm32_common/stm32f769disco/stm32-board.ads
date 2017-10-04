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

--  This file provides declarations for devices on the STM32F7 Discovery kits
--  manufactured by ST Microelectronics.

with Ada.Interrupts.Names; use Ada.Interrupts;

with STM32.Device;         use STM32.Device;
with STM32.DMA;            use STM32.DMA;
with STM32.DMA.Interrupts; use STM32.DMA.Interrupts;
with STM32.FMC;            use STM32.FMC;
with STM32.GPIO;           use STM32.GPIO;
with STM32.I2C;            use STM32.I2C;

use STM32;

with Audio;
with Framebuffer_OTM8009A;
with Touch_Panel_FT6x06;
with SDCard;

package STM32.Board is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Point;

   Red_LED    : User_LED renames PJ13;
   Green_LED  : User_LED renames PJ5;
   Green2_LED : User_LED renames PA12;
   LED1       : User_LED renames Red_LED;
   LED2       : User_LED renames Green_LED;
   LED3       : User_LED renames Green2_LED;
   LCH_LED    : User_LED renames Red_LED;

   All_LEDs : GPIO_Points := (Red_LED, Green_LED, Green2_LED);

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure Turn_On  (This : in out User_LED) renames STM32.GPIO.Set;
   procedure Turn_Off (This : in out User_LED) renames STM32.GPIO.Clear;
   procedure Toggle   (This : in out User_LED) renames STM32.GPIO.Toggle;

   procedure All_LEDs_Off with Inline;
   procedure All_LEDs_On  with Inline;
   procedure Toggle_LEDs (These : in out GPIO_Points)
     renames STM32.GPIO.Toggle;

   --  GPIO Pins for FMC

   FMC_A         : constant GPIO_Points :=
                     (PF0, PF1, PF2, PF3, PF4, PF5, PF12, PF13,
                      PF14, PF15, PG0, PG1, PG2);
   FMC_D         : constant GPIO_Points :=
                     (PD14, PD15, PD0, PD1, PE7, PE8, PE9, PE10,
                      PE11, PE12, PE13, PE14, PE15, PD8, PD9, PD10,
                      PH8, PH9, PH10, PH11, PH12, PH13, PH14, PH15,
                      PI0, PI1, PI2, PI3, PI6, PI7, PI9, PI10);
   FMC_NBL       : constant GPIO_Points :=
                     (PE0, PE1, PI4, PI5);

   FMC_SDNWE     : GPIO_Point renames PH5;
   FMC_SDNRAS    : GPIO_Point renames PF11;
   FMC_SDNCAS    : GPIO_Point renames PG15;
   FMC_SDCLK     : GPIO_Point renames PG8;
   FMC_BA0       : GPIO_Point renames PG4;
   FMC_BA1       : GPIO_Point renames PG5;
   FMC_SDNE0     : GPIO_Point renames PH3;
   FMC_SDCKE0    : GPIO_Point renames PH2;
   FMC_NBL0      : GPIO_Point renames PE0;
   FMC_NBL1      : GPIO_Point renames PE1;
   FMC_NBL2      : GPIO_Point renames PI4;
   FMC_NBL3      : GPIO_Point renames PI5;

   SDRAM_PINS    : constant GPIO_Points :=
                     FMC_A & FMC_D & FMC_SDNWE & FMC_SDNRAS & FMC_SDNCAS &
                     FMC_SDCLK & FMC_BA0 & FMC_BA1 & FMC_SDNE0 & FMC_SDCKE0 &
                     FMC_NBL;

   --  SDRAM CONFIGURATION Parameters
   SDRAM_Base            : constant := 16#C000_0000#;
   SDRAM_Size            : constant := 16#0100_0000#; --  16MB SDRAM
   SDRAM_Bank            : constant STM32.FMC.FMC_SDRAM_Cmd_Target_Bank :=
                             STM32.FMC.FMC_Bank1_SDRAM;
   SDRAM_Mem_Width       : constant STM32.FMC.FMC_SDRAM_Memory_Bus_Width :=
                             STM32.FMC.FMC_SDMemory_Width_32b;
   SDRAM_Row_Bits        : constant STM32.FMC.FMC_SDRAM_Row_Address_Bits :=
                             STM32.FMC.FMC_RowBits_Number_12b;
   SDRAM_CAS_Latency     : constant STM32.FMC.FMC_SDRAM_CAS_Latency :=
                             STM32.FMC.FMC_CAS_Latency_2;
   SDRAM_CLOCK_Period    : constant STM32.FMC.FMC_SDRAM_Clock_Configuration :=
                             STM32.FMC.FMC_SDClock_Period_2;
   SDRAM_Read_Burst      : constant STM32.FMC.FMC_SDRAM_Burst_Read :=
                             STM32.FMC.FMC_Read_Burst_Single;
   SDRAM_Read_Pipe       : constant STM32.FMC.FMC_SDRAM_Read_Pipe_Delay :=
                             STM32.FMC.FMC_ReadPipe_Delay_0;
   SDRAM_Refresh_Cnt     : constant := 16#0603#;
   SDRAM_Min_Delay_In_ns : constant := 60;

   ---------
   -- I2C --
   ---------

   I2C1_SCL : GPIO_Point renames PB8;
   I2C1_SDA : GPIO_Point renames PB9;
   I2C4_SCL : GPIO_Point renames PD12;
   I2C4_SDA : GPIO_Point renames PB7;

   procedure Initialize_I2C_GPIO (Port : in out I2C_Port)
     with
   --  I2C_2 and I2C_4 are not accessible on this board
     Pre => As_Port_Id (Port) = I2C_Id_1
            or else
            As_Port_Id (Port) = I2C_Id_4;

   --------------------------------
   -- Screen/Touch panel devices --
   --------------------------------

   LCD_Natural_Width  : constant := Framebuffer_OTM8009A.LCD_Natural_Width;
   LCD_Natural_Height : constant := Framebuffer_OTM8009A.LCD_Natural_Height;

   Display     : Framebuffer_OTM8009A.Frame_Buffer;
   Touch_Panel : Touch_Panel_FT6x06.Touch_Panel;

   -----------------
   -- Touch Panel --
   -----------------

   TP_INT   : GPIO_Point renames PI13;

   TP_Pins  : constant GPIO_Points :=
                (I2C4_SCL, I2C4_SDA);

   -----------
   -- Audio --
   -----------

   Audio_I2C        : I2C_Port renames I2C_4;
   Audio_I2C_SDA    : STM32.GPIO.GPIO_Point renames STM32.Device.PB7;
   Audio_I2C_SDA_AF : STM32.GPIO_Alternate_Function renames STM32.Device.GPIO_AF_I2C4_4;
   Audio_I2C_SCL    : STM32.GPIO.GPIO_Point renames STM32.Device.PD12;
   Audio_I2C_SCL_AF : STM32.GPIO_Alternate_Function renames STM32.Device.GPIO_AF_I2C4_11;

   --  Audio DMA configuration
   Audio_DMA               : DMA_Controller renames DMA_2;
   Audio_Out_DMA_Interrupt : Ada.Interrupts.Interrupt_ID renames
                               Ada.Interrupts.Names.DMA2_Stream1_Interrupt;
   Audio_DMA_Out_Stream    : DMA_Stream_Selector renames Stream_1;
   Audio_DMA_Out_Channel   : DMA_Channel_Selector renames Channel_0;

   Audio_DMA_In_Sream      : DMA_Stream_Selector renames Stream_7;
   Audio_DMA_In_Channel    : DMA_Channel_Selector renames Channel_0;

   Audio_Device : Audio.WM8994_Audio_Device (Audio_I2C'Access);

   -------------
   -- SD Card --
   -------------

   SD_Detect_Pin     : STM32.GPIO.GPIO_Point renames PI15;

   SD_DMA            : DMA_Controller renames DMA_2;
   SD_DMA_Rx_Stream  : DMA_Stream_Selector renames Stream_0;
   SD_DMA_Rx_Channel : DMA_Channel_Selector renames Channel_11;
   SD_DMA_Tx_Stream  : DMA_Stream_Selector renames Stream_5;
   SD_DMA_Tx_Channel : DMA_Channel_Selector renames Channel_11;
   SD_Pins           : constant GPIO_Points := (PB3, PB4);
   SD_Pins_AF        : constant GPIO_Alternate_Function := GPIO_AF_SDMMC2_10;
   SD_Pins_2         : constant GPIO_Points := (PD6, PD7, PG9, PG10);
   SD_Pins_AF_2      : constant GPIO_Alternate_Function := GPIO_AF_SDMMC2_11;
   SD_Interrupt      : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.SDMMC2_Interrupt;

   SD_Rx_DMA_Int     : DMA_Interrupt_Controller renames DMA2_Stream0;
   SD_Tx_DMA_Int     : DMA_Interrupt_Controller renames DMA2_Stream5;

   SDCard_Device : aliased SDCard.SDCard_Controller (SDMMC_2'Access);

   -----------------
   -- User button --
   -----------------

   User_Button_Point     : GPIO_Point renames PA0;
   User_Button_Interrupt : constant Interrupt_ID := Names.EXTI0_Interrupt;

   procedure Configure_User_Button_GPIO;
   --  Configures the GPIO port/pin for the blue user button. Sufficient
   --  for polling the button, and necessary for having the button generate
   --  interrupts.

end STM32.Board;
