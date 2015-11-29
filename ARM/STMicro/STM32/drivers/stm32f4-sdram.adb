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

with Ada.Real_Time; use Ada.Real_Time;

with STM32F4;      use STM32F4;
with STM32F4.GPIO; use STM32F4.GPIO;
with STM32F4.RCC;  use STM32F4.RCC;

with STM32F429_Discovery;  use STM32F429_Discovery;

package body STM32F4.SDRAM is

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
      Cmd : FMC_SDRAM_Cmd_Conf;
   begin
      Cmd.CommandMode            := FMC_Command_Mode_CLK_Enabled;
      Cmd.CommandTarget          := FMC_Command_Target_bank2;
      Cmd.AutoRefreshNumber      := 1;
      Cmd.ModeRegisterDefinition := 0;

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      Relative_Delay (100);

      Cmd.CommandMode            := FMC_Command_Mode_PALL;
      Cmd.CommandTarget          := FMC_Command_Target_bank2;
      Cmd.AutoRefreshNumber      := 1;
      Cmd.ModeRegisterDefinition := 0;

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      Cmd.CommandMode            := FMC_Command_Mode_AutoRefresh;
      Cmd.CommandTarget          := FMC_Command_Target_bank2;
      Cmd.AutoRefreshNumber      := 4;
      Cmd.ModeRegisterDefinition := 0;

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      Cmd.CommandMode            := FMC_Command_Mode_LoadMode;
      Cmd.CommandTarget          := FMC_Command_Target_bank2;
      Cmd.AutoRefreshNumber      := 1;
      Cmd.ModeRegisterDefinition := SDRAM_MODEREG_BURST_LENGTH_2 or
        SDRAM_MODEREG_BURST_TYPE_SEQUENTIAL or
        SDRAM_MODEREG_CAS_LATENCY_3 or
        SDRAM_MODEREG_OPERATING_MODE_STANDARD or
        SDRAM_MODEREG_WRITEBURST_MODE_SINGLE;

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      FMC_Set_Refresh_Count (1386);

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
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
      SDRAM_Conf.SDMemoryDataWidth  := SDRAM_MEMORY_WIDTH;
      SDRAM_Conf.InternalBankNumber := FMC_InternalBank_Number_4;
      SDRAM_Conf.CASLatency         := SDRAM_CAS_LATENCY;
      SDRAM_Conf.WriteProtection    := FMC_Write_Protection_Disable;
      SDRAM_Conf.SDClockPeriod      := SDCLOCK_PERIOD;
      SDRAM_Conf.ReadBurst          := SDRAM_READBURST;
      SDRAM_Conf.ReadPipeDelay      := FMC_ReadPipe_Delay_1;
      SDRAM_Conf.Timing_Conf        := Timing_Conf;

      FMC_SDRAM_Init (SDRAM_Conf);
      SDRAM_InitSequence;
   end Initialize;

end STM32F4.SDRAM;
