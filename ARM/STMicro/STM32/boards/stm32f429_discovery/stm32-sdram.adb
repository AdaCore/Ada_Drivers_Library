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
--   @file    stm32f4xx_hal_sdram.c                                         --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   SDRAM HAL module driver.                                      --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Real_Time; use Ada.Real_Time;

with STM32;         use STM32;
with STM32.GPIO;    use STM32.GPIO;
with STM32.RCC;     use STM32.RCC;
with STM32.FMC;     use STM32.FMC;

with STM32.Device;  use STM32.Device;

with SDRAM_Reg;     use SDRAM_Reg;

package body STM32.SDRAM is

   --------------------
   -- Configure_GPIO --
   --------------------

   procedure Configure_GPIO is
      Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (GPIO_B);
      Enable_Clock (GPIO_C);
      Enable_Clock (GPIO_D);
      Enable_Clock (GPIO_E);
      Enable_Clock (GPIO_F);
      Enable_Clock (GPIO_G);

      Conf.Speed       := Speed_50MHz;
      Conf.Mode        := Mode_AF;
      Conf.Output_Type := Push_Pull;
      Conf.Resistors   := Floating;

      Configure_Alternate_Function (GPIO_B, (Pin_5, Pin_6), GPIO_AF_FMC);

      Configure_IO (GPIO_B, (Pin_5, Pin_6), Conf);

      Lock (GPIO_B, (Pin_5, Pin_6));

      Configure_Alternate_Function (GPIO_C, Pin_0,  GPIO_AF_FMC);

      Configure_IO (GPIO_C, Pin_0, Conf);

      Lock (GPIO_C, Pin_0);

      Configure_Alternate_Function (GPIO_D,
                                    (Pin_0, Pin_1, Pin_8, Pin_9, Pin_10,
                                     Pin_14, Pin_15),
                                    GPIO_AF_FMC);

      Configure_IO (GPIO_D, (Pin_0, Pin_1, Pin_8, Pin_9, Pin_10, Pin_14, Pin_15), Conf);

      Lock (GPIO_D, (Pin_0, Pin_1, Pin_8, Pin_9, Pin_10, Pin_14, Pin_15));

      Configure_Alternate_Function (GPIO_E, (Pin_0, Pin_1, Pin_7, Pin_8, Pin_9,
                                             Pin_10, Pin_11, Pin_12, Pin_13,
                                             Pin_14, Pin_15), GPIO_AF_FMC);

      Configure_IO (GPIO_E, (Pin_0, Pin_1, Pin_7, Pin_8, Pin_9, Pin_10, Pin_11,
                             Pin_12, Pin_13, Pin_14, Pin_15), Conf);

      Lock (GPIO_E, (Pin_0, Pin_1, Pin_7, Pin_8, Pin_9, Pin_10, Pin_11,
                             Pin_12, Pin_13, Pin_14, Pin_15));

      Configure_Alternate_Function (GPIO_F,
                                    (Pin_0, Pin_1, Pin_2, Pin_3, Pin_4, Pin_5,
                                     Pin_11, Pin_12, Pin_13, Pin_14, Pin_15),
                                    GPIO_AF_FMC);

      Configure_IO (GPIO_F, (Pin_0, Pin_1, Pin_2, Pin_3, Pin_4, Pin_5,
                             Pin_11, Pin_12, Pin_13, Pin_14, Pin_15), Conf);

      Lock (GPIO_F, (Pin_0, Pin_1, Pin_2, Pin_3, Pin_4, Pin_5,
                     Pin_11, Pin_12, Pin_13, Pin_14, Pin_15));

      Configure_Alternate_Function (GPIO_G,
                                    (Pin_0, Pin_1, Pin_4, Pin_5,
                                     Pin_8, Pin_15),
                                    GPIO_AF_FMC);

      Configure_IO (GPIO_G, (Pin_0, Pin_1, Pin_4, Pin_5, Pin_8, Pin_15), Conf);

      Lock (GPIO_G, (Pin_0, Pin_1, Pin_4, Pin_5, Pin_8, Pin_15));
   end Configure_GPIO;

   --------------------
   -- Relative_Delay --
   --------------------

   procedure Relative_Delay (Ms : Integer) is
      Next_Start : Time := Clock;
      Period     : constant Time_Span := Milliseconds (Ms);
   begin
      Next_Start := Next_Start + Period;
      delay until Next_Start;
   end Relative_Delay;

   ------------------------
   -- SDRAM_InitSequence --
   ------------------------

   procedure SDRAM_InitSequence is
      Mode_Reg : SDRAM_Mode_Register;
      function To_UInt10 is new Ada.Unchecked_Conversion
        (SDRAM_Mode_Register, UInt10);
   begin
      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

      FMC_SDRAM_Cmd ((Mode   => FMC_Command_Mode_CLK_Enabled,
                      Target => FMC_Bank2_SDRAM));

      Relative_Delay (100);

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

      FMC_SDRAM_Cmd ((Mode   => FMC_Command_Mode_PALL,
                      Target => FMC_Bank2_SDRAM));

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

      FMC_SDRAM_Cmd ((Mode                => FMC_Command_Mode_AutoRefresh,
                      Target              => FMC_Bank2_SDRAM,
                      Auto_Refresh_Number => 4));

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

      Mode_Reg :=
        (Burst_Length     => Burst_Length_2,
         Burst_Type       => Burst_Sequential,
         Latency_Mode     => CAS_Latency_3,
         Operating_Mode   => <>,
         Write_Burst_Mode => Burst_Mode_Single_Location_Access);

      FMC_SDRAM_Cmd ((Mode                => FMC_Command_Mode_LoadMode,
                      Target              => FMC_Bank2_SDRAM,
                      SDRAM_Mode_Register =>
                        FMC_SDRAM_Mode_Register (To_UInt10 (Mode_Reg))));

      FMC_Set_Refresh_Count (1386);

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;
   end SDRAM_InitSequence;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Timing_Conf : FMC_SDRAM_TimingInit_Config;
      SDRAM_Conf  : FMC_SDRAM_Init_Config;
   begin
      Configure_GPIO;

      FSMC_Clock_Enable;

      Timing_Conf.LoadToActiveDelay    := 2;
      Timing_Conf.ExitSelfRefreshDelay := 7;
      Timing_Conf.SelfRefreshTime      := 4;
      Timing_Conf.RowCycleDelay        := 7;
      Timing_Conf.WriteRecoveryTime    := 2;
      Timing_Conf.RPDelay              := 2;
      Timing_Conf.RCDDelay             := 2;

      SDRAM_Conf.Bank               := FMC_Bank2_SDRAM;
      SDRAM_Conf.ColumnBitsNumber   := FMC_ColumnBits_Number_8b;
      SDRAM_Conf.RowBitsNumber      := FMC_RowBits_Number_12b;
      SDRAM_Conf.SDMemoryDataWidth  := FMC_SDMemory_Width_16b;
      SDRAM_Conf.InternalBankNumber := FMC_InternalBank_Number_4;
      SDRAM_Conf.CASLatency         := FMC_CAS_Latency_3;
      SDRAM_Conf.WriteProtection    := FMC_Write_Protection_Disable;
      SDRAM_Conf.SDClockPeriod      := FMC_SDClock_Period_2;
      SDRAM_Conf.ReadBurst          := FMC_Read_Burst_Disable;
      SDRAM_Conf.ReadPipeDelay      := FMC_ReadPipe_Delay_1;
      SDRAM_Conf.Timing_Conf        := Timing_Conf;

      FMC_SDRAM_Init (SDRAM_Conf);
      SDRAM_InitSequence;
   end Initialize;

end STM32.SDRAM;
