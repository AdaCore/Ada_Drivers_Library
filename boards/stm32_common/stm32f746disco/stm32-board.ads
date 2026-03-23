------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2026, AdaCore                         --
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

with Ada.Interrupts.Names;  use Ada.Interrupts;
with System;

with STM32.Device;          use STM32.Device;

with STM32.DMA;             use STM32.DMA;
with STM32.DMA.Interrupts;  use STM32.DMA.Interrupts;
with STM32.FMC;             use STM32.FMC;
with STM32.GPIO;            use STM32.GPIO;
with STM32.I2C;             use STM32.I2C;

use STM32;

with Audio;
with Touch_Panel_FT5336;
with Framebuffer_RK043FN48H;
with SDCard;

package STM32.Board is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Point;

   Green_LED : User_LED renames PI1;
   LED1      : User_LED renames Green_LED;
   LCH_LED   : User_LED renames Green_LED;

   All_LEDs : GPIO_Points := (1 => Green_LED);

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

   FMC_A : constant GPIO_Points :=
             (PF0, PF1, PF2, PF3, PF4, PF5, PF12, PF13, PF14, PF15, PG0, PG1);

   FMC_D : constant GPIO_Points :=
             (PD14, PD15, PD0, PD1, PE7, PE8, PE9, PE10,
              PE11, PE12, PE13, PE14, PE15, PD8, PD9, PD10);

   FMC_SDNWE     : GPIO_Point renames PH5;
   FMC_SDNRAS    : GPIO_Point renames PF11;
   FMC_SDNCAS    : GPIO_Point renames PG15;
   FMC_SDCLK     : GPIO_Point renames PG8;
   FMC_BA0       : GPIO_Point renames PG4;
   FMC_BA1       : GPIO_Point renames PG5;
   FMC_SDNE0     : GPIO_Point renames PH3;
   FMC_SDCKE0    : GPIO_Point renames PC3;
   FMC_NBL0      : GPIO_Point renames PE0;
   FMC_NBL1      : GPIO_Point renames PE1;

   SDRAM_PINS    : constant GPIO_Points :=
                     FMC_A & FMC_D & FMC_SDNWE & FMC_SDNRAS & FMC_SDNCAS &
                     FMC_SDCLK & FMC_BA0 & FMC_BA1 & FMC_SDNE0 & FMC_SDCKE0 &
                     FMC_NBL0 & FMC_NBL1;

   --  SDRAM CONFIGURATION Parameters
   SDRAM_Base            : constant := 16#C0000000#;
   SDRAM_Size            : constant := 16#800000#;
   SDRAM_Bank            : constant STM32.FMC.FMC_SDRAM_Cmd_Target_Bank :=
                             STM32.FMC.FMC_Bank1_SDRAM;
   SDRAM_Mem_Width       : constant STM32.FMC.FMC_SDRAM_Memory_Bus_Width :=
                             STM32.FMC.FMC_SDMemory_Width_16b;
   SDRAM_Row_Bits        : constant STM32.FMC.FMC_SDRAM_Row_Address_Bits :=
                             STM32.FMC.FMC_RowBits_Number_12b;
   SDRAM_CAS_Latency     : constant STM32.FMC.FMC_SDRAM_CAS_Latency :=
                             STM32.FMC.FMC_CAS_Latency_2;
   SDRAM_CLOCK_Period    : constant STM32.FMC.FMC_SDRAM_Clock_Configuration :=
                             STM32.FMC.FMC_SDClock_Period_2;
   SDRAM_Read_Burst      : constant STM32.FMC.FMC_SDRAM_Burst_Read :=
                             STM32.FMC.FMC_Read_Burst_Disable;
   SDRAM_Read_Pipe       : constant STM32.FMC.FMC_SDRAM_Read_Pipe_Delay :=
                             STM32.FMC.FMC_ReadPipe_Delay_0;
   SDRAM_Refresh_Cnt     : constant := 1539;
   SDRAM_TRAS_In_Ns      : constant := 42;  --  IS42S32400F min self-refresh time
   SDRAM_TRC_In_Ns       : constant := 60;  --  IS42S32400F min row cycle time
   SDRAM_TXSR_In_Ns      : constant := 70;  --  IS42S32400F min exit self-refresh delay

   ---------
   -- I2C --
   ---------

   procedure Initialize_I2C_GPIO (Port : in out I2C_Port)
     with
   --  I2C_2 and I2C_4 are not accessible on this board
     Pre => As_Port_Id (Port) = I2C_Id_1
            or else
            As_Port_Id (Port) = I2C_Id_3;

   --------------------------------
   -- Screen/Touch panel devices --
   --------------------------------

   LCD_Natural_Width  : constant := Framebuffer_RK043FN48H.LCD_Natural_Width;
   LCD_Natural_Height : constant := Framebuffer_RK043FN48H.LCD_Natural_Height;

   Display     : Framebuffer_RK043FN48H.Frame_Buffer;
   Touch_Panel : Touch_Panel_FT5336.Touch_Panel;

   -----------
   -- Audio --
   -----------

   Audio_I2C     : I2C_Port renames I2C_3;
   Audio_I2C_SDA : STM32.GPIO.GPIO_Point renames STM32.Device.PH7;
   Audio_I2C_SCL : STM32.GPIO.GPIO_Point renames STM32.Device.PH8;
   Audio_I2C_AF  : STM32.GPIO_Alternate_Function renames STM32.Device.GPIO_AF_I2C3_4;
   Audio_INT     : GPIO_Point renames PD6;

   --  Audio DMA configuration
   Audio_DMA               : DMA_Controller renames DMA_2;
   Audio_Out_DMA_Interrupt : Ada.Interrupts.Interrupt_ID renames
                               Ada.Interrupts.Names.DMA2_Stream4_Interrupt;
   Audio_DMA_Out_Stream    : DMA_Stream_Selector renames Stream_4;
   Audio_DMA_Out_Channel   : DMA_Channel_Selector renames Channel_3;

   Audio_Device : aliased Audio.WM8994_Audio_CODEC (Audio_I2C'Access);

   --------------------------
   -- micro SD card reader --
   --------------------------

   SD_Detect_Pin     : STM32.GPIO.GPIO_Point renames PC13;

   SD_DMA            : DMA_Controller renames DMA_2;
   SD_DMA_Rx_Stream  : DMA_Stream_Selector renames Stream_3;
   SD_DMA_Rx_Channel : DMA_Channel_Selector renames Channel_4;
   SD_DMA_Tx_Stream  : DMA_Stream_Selector renames Stream_6;
   SD_DMA_Tx_Channel : DMA_Channel_Selector renames Channel_4;
   SD_Pins           : constant GPIO_Points :=
                         (PC8, PC9, PC10, PC11, PC12, PD2);
   SD_Pins_AF        : constant GPIO_Alternate_Function := GPIO_AF_SDMMC1_12;
   SD_Pins_2         : constant GPIO_Points := (1 .. 0 => <>);
   SD_Pins_AF_2      : constant GPIO_Alternate_Function := GPIO_AF_SDMMC1_12;
   SD_Interrupt      : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.SDMMC1_Interrupt;

   DMA2_Stream3 : aliased DMA_Interrupt_Controller
     (DMA_2'Access, Stream_3,
      Ada.Interrupts.Names.DMA2_Stream3_Interrupt,
      System.Interrupt_Priority'Last);

   DMA2_Stream6 : aliased DMA_Interrupt_Controller
     (DMA_2'Access, Stream_6,
      Ada.Interrupts.Names.DMA2_Stream6_Interrupt,
      System.Interrupt_Priority'Last);

   SD_Rx_DMA_Int     : DMA_Interrupt_Controller renames DMA2_Stream3;
   SD_Tx_DMA_Int     : DMA_Interrupt_Controller renames DMA2_Stream6;

   SDCard_Device : aliased SDCard.SDCard_Controller (SDMMC_1'Access);

   ------------------
   --  User button --
   ------------------

   User_Button_Point     : GPIO_Point renames PI11;
   User_Button_Interrupt : constant Interrupt_ID := Names.EXTI15_10_Interrupt;

   procedure Configure_User_Button_GPIO;
   --  Configures the GPIO port/pin for the blue user button. Sufficient
   --  for polling the button, and necessary for having the button generate
   --  interrupts.

  --------------------------------------
  -- Ethernet Physical Layer I/O Pins --
  --------------------------------------

   --  Serial Management Interface (SMI) comm pins for connecting the MAC with
   --  the PHY for RMII. See RM0385 Rev 8, pg 1533.
   RMII_REF_CLK : GPIO_Point renames PA1;
   RMII_MDIO    : GPIO_Point renames PA2;
   RMII_MDC     : GPIO_Point renames PC1;

   --  RMII pins. See RM0385 Rev 8, pp. 1536 and 1538 for overview, and the
   --  actual board's User Manual (eg, UM1907 STM32F746G-DISCO Appendix A:
   --  IO Assignment) for the pins.
   RMII_CRS_DV  : GPIO_Point renames PA7;
   RMII_RXD0    : GPIO_Point renames PC4;
   RMII_RXD1    : GPIO_Point renames PC5;
   RMII_RXER    : GPIO_Point renames PG2;
   RMII_TX_EN   : GPIO_Point renames PG11;
   RMII_TXD0    : GPIO_Point renames PG13;
   RMII_TXD1    : GPIO_Point renames PG14;

   --  Note: On this board, these are the only pins that can perform these
   --  functions.

   All_RMII_Pins : constant GPIO_Points := (RMII_REF_CLK,
                                            RMII_MDIO,
                                            RMII_CRS_DV,
                                            RMII_MDC,
                                            RMII_RXD0,
                                            RMII_RXD1,
                                            RMII_RXER,
                                            RMII_TX_EN,
                                            RMII_TXD0,
                                            RMII_TXD1);

   --  These need to be two separate routines due to hardware initialization
   --  issues with the Ethernet device.
   procedure Enable_RMII_Pins_Clocks;
   procedure Configure_RMII_Pins;

end STM32.Board;
