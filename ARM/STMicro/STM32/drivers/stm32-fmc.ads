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
--   @file    stm32f4xx_ll_fmc.h                                            --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of FMC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with System;

package STM32.FMC is
   FMC_Bank1_SDRAM : constant := 16#00000000#;
   FMC_Bank2_SDRAM : constant := 16#00000001#;

   FMC_Bank1_NORSRAM1 : constant := 16#00000000#;
   FMC_Bank1_NORSRAM2 : constant := 16#00000002#;
   FMC_Bank1_NORSRAM3 : constant := 16#00000004#;
   FMC_Bank1_NORSRAM4 : constant := 16#00000006#;
   FMC_Bank2_NAND     : constant := 16#00000010#;
   FMC_Bank3_NAND     : constant := 16#00000100#;
   FMC_Bank4_PCCARD   : constant := 16#00001000#;

   FMC_RowBits_Number_11b : constant := 16#00000000#;
   FMC_RowBits_Number_12b : constant := 16#00000004#;
   FMC_RowBits_Number_13b : constant := 16#00000008#;

   FMC_ColumnBits_Number_8b  : constant := 16#0000_0000#;
   FMC_ColumnBits_Number_9b  : constant := 16#0000_0001#;
   FMC_ColumnBits_Number_10b : constant := 16#0000_0002#;
   FMC_ColumnBits_Number_11b : constant := 16#0000_0003#;

   FMC_SDMemory_Width_8b  : constant := 16#0000_0000#;
   FMC_SDMemory_Width_16b : constant := 16#0000_0010#;
   FMC_SDMemory_Width_32b : constant := 16#0000_0020#;

   FMC_InternalBank_Number_2 : constant := 16#0000_0000#;
   FMC_InternalBank_Number_4 : constant := 16#0000_0040#;

   FMC_CAS_Latency_1 : constant := 16#0000_0080#;
   FMC_CAS_Latency_2 : constant := 16#0000_0100#;
   FMC_CAS_Latency_3 : constant := 16#0000_0180#;

   FMC_Write_Protection_Disable : constant := 16#0000_0000#;
   FMC_Write_Protection_Enable  : constant := 16#0000_0200#;

   FMC_SDClock_Disable  : constant := 16#0000_0000#;
   FMC_SDClock_Period_2 : constant := 16#0000_0800#;
   FMC_SDClock_Period_3 : constant := 16#0000_0C00#;

   FMC_Read_Burst_Disable : constant := 16#0000_0000#;
   FMC_Read_Burst_Enable  : constant := 16#0000_1000#;

   FMC_ReadPipe_Delay_0 : constant := 16#0000_0000#;
   FMC_ReadPipe_Delay_1 : constant := 16#0000_2000#;
   FMC_ReadPipe_Delay_2 : constant := 16#0000_4000#;

   FMC_Command_Mode_Normal      : constant := 16#0000_0000#;
   FMC_Command_Mode_CLK_Enabled : constant := 16#0000_0001#;
   FMC_Command_Mode_PALL        : constant := 16#0000_0002#;
   FMC_Command_Mode_AutoRefresh : constant := 16#0000_0003#;
   FMC_Command_Mode_LoadMode    : constant := 16#0000_0004#;
   FMC_Command_Mode_Selfrefresh : constant := 16#0000_0005#;
   FMC_Command_Mode_PowerDown   : constant := 16#0000_0006#;

   FMC_Command_Target_Bank2   : constant := 16#0000_0008#;
   FMC_Command_Target_Bank1   : constant := 16#0000_0010#;
   FMC_Command_Target_Bank1_2 : constant := 16#0000_0018#;

   FMC_NormalMode_Status      : constant := 16#0000_0000#;
   --  FMC_SelfRefreshMode_Status : constant := FMC_SDSR_MODES1_0;
   --  FMC_PowerDownMode_Status   : constant := FMC_SDSR_MODES1_1;

   FMC_IT_RisingEdge  : constant := 16#0000_0008#;
   FMC_IT_Level       : constant := 16#0000_0010#;
   FMC_IT_FallingEdge : constant := 16#0000_0020#;
   FMC_IT_Refresh     : constant := 16#0000_4000#;

   FMC_FLAG_RisingEdge  : constant := 16#0000_0001#;
   FMC_FLAG_Level       : constant := 16#0000_0002#;
   FMC_FLAG_FallingEdge : constant := 16#0000_0004#;
   FMC_FLAG_FEMPT       : constant := 16#0000_0040#;
   FMC_FLAG_Refresh     : constant := 16#0000_0001#;
   FMC_FLAG_Busy        : constant := 16#0000_0020#;

   type Word_x2  is array (0 .. 1) of Word with Pack, Size => 2 * 32;

   FMC_Base           : constant := 16#A000_0000#;
   FMC_Bank1_R_BASE   : constant := FMC_BASE + 16#0000#;
   FMC_Bank1E_R_BASE  : constant := FMC_BASE + 16#0104#;
   FMC_Bank2_R_BASE   : constant := FMC_BASE + 16#0060#;
   FMC_Bank3_R_BASE   : constant := FMC_BASE + 16#0080#;
   FMC_Bank4_R_BASE   : constant := FMC_BASE + 16#00A0#;
   FMC_Bank5_6_R_BASE : constant := FMC_BASE + 16#0140#;

   type FMC_Bank1_Registers is array (1 .. 8) of Word with Pack;
   Bank1 : FMC_Bank1_Registers
     with Volatile, Address => System'To_Address (FMC_Bank1_R_BASE);

   type FMC_Bank1E_Registers is array (1 .. 7) of Word with Pack;
   Bank1E : FMC_Bank1E_Registers
     with Volatile, Address => System'To_Address (FMC_Bank1E_R_BASE);

   type FMC_Bank2_3_Registers is record
      PCR      : Word;
      SR       : Word;
      PMEM     : Word;
      PATT     : Word;
      Reserved : Word;
      ECCR     : Word;
   end record with Pack, Size => 6 * 32;

   Bank2 : FMC_Bank2_3_Registers
     with Volatile, Address => System'To_Address (FMC_Bank2_R_BASE);
   Bank3 : FMC_Bank2_3_Registers
     with Volatile, Address => System'To_Address (FMC_Bank3_R_BASE);

   type FMC_Bank4_Registers is record
      PCR     : Word;
      SR      : Word;
      PMEM    : Word;
      PATT    : Word;
      PIO     : Word;
   end record with Pack, Size => 5 * 32;

   Bank4 : FMC_Bank4_Registers
     with Volatile, Address => System'To_Address (FMC_Bank4_R_BASE);

   type FMC_Bank5_6_Registers is record
      SDCR  : Word_x2;
      SDTR  : Word_x2;
      SDCMR : Word;
      SDRTR : Word;
      SDSR  : Word;
   end record with Pack, Size => 7 * 32;

   Bank5_6 : FMC_Bank5_6_Registers
     with Volatile, Address => System'To_Address (FMC_Bank5_6_R_BASE);

   type FMC_SDRAM_TimingInit_Config is record
      LoadToActiveDelay    : Word;
      ExitSelfRefreshDelay : Word;
      SelfRefreshTime      : Word;
      RowCycleDelay        : Word;
      WriteRecoveryTime    : Word;
      RPDelay              : Word;
      RCDDelay             : Word;
   end record;

   type FMC_SDRAM_Init_Config is record
      Bank               : Integer;
      ColumnBitsNumber   : Word;
      RowBitsNumber      : Word;
      SDMemoryDataWidth  : Word;
      InternalBankNumber : Word;
      CASLatency         : Word;
      WriteProtection    : Word;
      SDClockPeriod      : Word;
      ReadBurst          : Word;
      ReadPipeDelay      : Word;
      Timing_Conf        : FMC_SDRAM_TimingInit_Config;
   end record;

   type FMC_SDRAM_Cmd_Conf is record
      CommandMode            : Word;
      CommandTarget          : Word;
      AutoRefreshNumber      : Word;
      ModeRegisterDefinition : Word;
   end record;

   procedure FMC_SDRAM_Init (SDRAM_Conf : FMC_SDRAM_Init_Config);

   procedure FMC_SDRAM_Cmd (Cmd : FMC_SDRAM_Cmd_Conf);
   function FMC_Get_Flag (Bank : Word; Flag : Word) return Boolean;
   procedure FMC_Set_Refresh_Count (Cnt : Word);

end STM32.FMC;
