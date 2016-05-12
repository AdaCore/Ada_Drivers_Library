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

--  This file provides declarations for devices on the STM32F7 Discovery kits
--  manufactured by ST Microelectronics.

with Ada.Interrupts.Names;  use Ada.Interrupts;

with STM32.Device;  use STM32.Device;

with STM32.GPIO;    use STM32.GPIO;
with STM32.FMC;     use STM32.FMC;
with STM32.I2C;     use STM32.I2C;
with STM32.DMA;     use STM32.DMA;

use STM32;

with Touch_Panel_FT5336;
with Framebuffer_RK043FN48H;

package STM32.Board is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Point;

   Green    : User_LED renames PI1;
   LED1     : User_LED renames Green;
   LCH_LED  : User_LED renames Green;

   All_LEDs : GPIO_Points := (1 => Green);

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure Turn_On (This : in out User_LED)
     renames STM32.GPIO.Set;
   procedure Turn_Off (This : in out User_LED)
     renames STM32.GPIO.Clear;

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
   SDRAM_Base         : constant := 16#C0000000#;
   SDRAM_Size         : constant := 16#800000#;
   SDRAM_Bank         : constant STM32.FMC.FMC_SDRAM_Cmd_Target_Bank :=
                          STM32.FMC.FMC_Bank1_SDRAM;
   SDRAM_Mem_Width    : constant STM32.FMC.FMC_SDRAM_Memory_Bus_Width :=
                          STM32.FMC.FMC_SDMemory_Width_16b;
   SDRAM_Row_Bits     : constant STM32.FMC.FMC_SDRAM_Row_Address_Bits :=
                          STM32.FMC.FMC_RowBits_Number_12b;
   SDRAM_CAS_Latency  : constant STM32.FMC.FMC_SDRAM_CAS_Latency :=
                          STM32.FMC.FMC_CAS_Latency_2;
   SDRAM_CLOCK_Period : constant STM32.FMC.FMC_SDRAM_Clock_Configuration :=
                          STM32.FMC.FMC_SDClock_Period_2;
   SDRAM_Read_Burst   : constant STM32.FMC.FMC_SDRAM_Burst_Read :=
                          STM32.FMC.FMC_Read_Burst_Disable;
   SDRAM_Read_Pipe    : constant STM32.FMC.FMC_SDRAM_Read_Pipe_Delay :=
                          STM32.FMC.FMC_ReadPipe_Delay_0;
   SDRAM_Refresh_Cnt  : constant := 1539;

   ---------
   -- I2C --
   ---------

   procedure Initialize_I2C_GPIO (Port : in out I2C_Port)
     with
   --  I2C_2 and I2C_4 are not accessible on this board
     Pre => As_Port_Id (Port) = I2C_Id_1
            or else
            As_Port_Id (Port) = I2C_Id_3;

   procedure Configure_I2C (Port : in out I2C_Port);

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

   --  Audio out
   SAI2_MCLK_A : GPIO_Point renames PI4;
   SAI2_SCK_A  : GPIO_Point renames PI5;
   SAI2_SD_A   : GPIO_Point renames PI6;
   SAI2_FS_A   : GPIO_Point renames PI7;

   --  Audio in
   SAI2_SD_B   : GPIO_Point renames PG10;

   --  Audio interrupts
   TP_PH15     : GPIO_Point renames PH15;

   Audio_I2C   : I2C_Port renames I2C_3;
   Audio_INT   : GPIO_Point renames PD6;

   Audio_DMA   : DMA_Controller renames DMA_2;

   --  User button

   User_Button_Point     : GPIO_Point renames PI11;
   User_Button_Interrupt : constant Interrupt_ID := Names.EXTI15_10_Interrupt;

   procedure Configure_User_Button_GPIO;
   --  Configures the GPIO port/pin for the blue user button. Sufficient
   --  for polling the button, and necessary for having the button generate
   --  interrupts.

end STM32.Board;
