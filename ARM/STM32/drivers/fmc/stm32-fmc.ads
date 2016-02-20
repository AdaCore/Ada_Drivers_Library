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

package STM32.FMC is

   type FMC_SDRAM_Cmd_Target_Bank is
     (FMC_Bank2_SDRAM,
      FMC_Bank1_SDRAM,
      FMC_Banks_1_2_SDRAM)
     with Size => 2;
   for FMC_SDRAM_Cmd_Target_Bank use
     (FMC_Bank2_SDRAM    => 2#01#,
      FMC_Bank1_SDRAM    => 2#10#,
      FMC_Banks_1_2_SDRAM => 2#11#);

   subtype FMC_SDRAM_Bank_Type is FMC_SDRAM_Cmd_Target_Bank range
     FMC_Bank2_SDRAM .. FMC_Bank1_SDRAM;

   FMC_Bank1_NORSRAM1 : constant := 16#00000000#;
   FMC_Bank1_NORSRAM2 : constant := 16#00000002#;
   FMC_Bank1_NORSRAM3 : constant := 16#00000004#;
   FMC_Bank1_NORSRAM4 : constant := 16#00000006#;
   FMC_Bank2_NAND     : constant := 16#00000010#;
   FMC_Bank3_NAND     : constant := 16#00000100#;
   FMC_Bank4_PCCARD   : constant := 16#00001000#;

   type FMC_SDRAM_Row_Address_Bits is
     (FMC_RowBits_Number_11b,
      FMC_RowBits_Number_12b,
      FMC_RowBits_Number_13b)
     with Size => 2;

   type FMC_SDRAM_Column_Address_Bits is
     (FMC_ColumnBits_Number_8b,
      FMC_ColumnBits_Number_9b,
      FMC_ColumnBits_Number_10b,
      FMC_ColumnBits_Number_11b)
     with Size => 2;

   type FMC_SDRAM_Memory_Bus_Width is
     (FMC_SDMemory_Width_8b,
      FMC_SDMemory_Width_16b,
      FMC_SDMemory_Width_32b)
     with Size => 2;

   type FMC_SDRAM_Internal_Banks_Number is
     (FMC_InternalBank_Number_2,
      FMC_InternalBank_Number_4)
     with Size => 1;

   type FMC_SDRAM_CAS_Latency is
     (FMC_CAS_Latency_1,
      FMC_CAS_Latency_2,
      FMC_CAS_Latency_3)
     with Size => 2;
   for FMC_SDRAM_CAS_Latency use
     (FMC_CAS_Latency_1 => 1,
      FMC_CAS_Latency_2 => 2,
      FMC_CAS_Latency_3 => 3);

   type FMC_SDRAM_Write_Protection is
     (FMC_Write_Protection_Disable,
      FMC_Write_Protection_Enable)
     with Size => 1;

   type FMC_SDRAM_Clock_Configuration is
     (FMC_SDClock_Disable,
      FMC_SDClock_Period_2,
      FMC_SDClock_Period_3)
     with Size => 2;
   for FMC_SDRAM_Clock_Configuration use
     (FMC_SDClock_Disable  => 0,
      FMC_SDClock_Period_2 => 2,
      FMC_SDClock_Period_3 => 3);

   type FMC_SDRAM_Burst_Read is
     (FMC_Read_Burst_Disable,
      FMC_Read_Burst_Single)
     with Size => 1;

   type FMC_SDRAM_Read_Pipe_Delay is
     (FMC_ReadPipe_Delay_0,
      FMC_ReadPipe_Delay_1,
      FMC_ReadPipe_Delay_2)
     with Size => 2;

   type FMC_SDRAM_Cmd_Mode is
     (FMC_Command_Mode_Normal,
      FMC_Command_Mode_CLK_Enabled,
      FMC_Command_Mode_PALL,
      FMC_Command_Mode_AutoRefresh,
      FMC_Command_Mode_LoadMode,
      FMC_Command_Mode_Selfrefresh,
      FMC_Command_Mode_PowerDown)
     with Size => 3;

   type FMC_SDRAM_Status_Mode is
     (Normal_Mode,
      Self_Refresh_Mode,
      Power_Down_Mode)
     with Size => 2;

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

   type FMC_SDRAM_Timing is range 1 .. 16;

   type FMC_SDRAM_TimingInit_Config is record
      LoadToActiveDelay    : FMC_SDRAM_Timing;
      ExitSelfRefreshDelay : FMC_SDRAM_Timing;
      SelfRefreshTime      : FMC_SDRAM_Timing;
      RowCycleDelay        : FMC_SDRAM_Timing;
      WriteRecoveryTime    : FMC_SDRAM_Timing;
      RPDelay              : FMC_SDRAM_Timing;
      RCDDelay             : FMC_SDRAM_Timing;
   end record;

   type FMC_SDRAM_Init_Config is record
      Bank               : FMC_SDRAM_Bank_Type;
      ColumnBitsNumber   : FMC_SDRAM_Column_Address_Bits;
      RowBitsNumber      : FMC_SDRAM_Row_Address_Bits;
      SDMemoryDataWidth  : FMC_SDRAM_Memory_Bus_Width;
      InternalBankNumber : FMC_SDRAM_Internal_Banks_Number;
      CASLatency         : FMC_SDRAM_CAS_Latency;
      WriteProtection    : FMC_SDRAM_Write_Protection;
      SDClockPeriod      : FMC_SDRAM_Clock_Configuration;
      ReadBurst          : FMC_SDRAM_Burst_Read;
      ReadPipeDelay      : FMC_SDRAM_Read_Pipe_Delay;
      Timing_Conf        : FMC_SDRAM_TimingInit_Config;
   end record;

   procedure FMC_SDRAM_Init (SDRAM_Conf : FMC_SDRAM_Init_Config);

   --------------------------
   --  SDRAM Mode Register --
   --------------------------

   type SDRAM_Mode_Burst_Length is
     (SDRAM_Mode_Burst_Length_1,
      SDRAM_Mode_Burst_Length_2,
      SDRAM_Mode_Burst_Length_4,
      SDRAM_Mode_Burst_Length_8)
     with Size => 3;
   for SDRAM_Mode_Burst_Length use
     (SDRAM_Mode_Burst_Length_1 => 16#0#,
      SDRAM_Mode_Burst_Length_2 => 16#1#,
      SDRAM_Mode_Burst_Length_4 => 16#2#,
      SDRAM_Mode_Burst_Length_8 => 16#4#);

   type SDRAM_Mode_Burst_Type is
     (SDRAM_Mode_Burst_Sequential,
      SDRAM_Mode_Burst_Interleaved)
     with Size => 1;

   type SDRAM_Mode_CAS_Latency is
     (SDRAM_Mode_CAS_Latency_2,
      SDRAM_Mode_CAS_Latency_3)
     with Size => 2;
   for SDRAM_Mode_CAS_Latency use
     (SDRAM_Mode_CAS_Latency_2 => 2,
      SDRAM_Mode_CAS_Latency_3 => 3);

   type SDRAM_Mode_Operating_Mode is
     (SDRAM_Mode_Writeburst_Mode_Programmed,
      SDRAM_Mode_Writeburst_Mode_Single);

   type SDRAM_Mode_Register is record
      Burst_Length   : SDRAM_Mode_Burst_Length;
      Burst_Type     : SDRAM_Mode_Burst_Type;
      CAS_Latency    : SDRAM_Mode_CAS_Latency;
      Operating_Mode : SDRAM_Mode_Operating_Mode;
   end record with Size => 13;
   for SDRAM_Mode_Register use record
      Burst_Length at 0 range 0 .. 2;
      Burst_Type   at 0 range 3 .. 3;
      CAS_Latency  at 0 range 4 .. 5;
      Operating_Mode at 0 range 9 .. 9;
   end record;

   type SDRAM_Command (Mode : FMC_SDRAM_Cmd_Mode) is record
      Target : FMC_SDRAM_Cmd_Target_Bank;
      case Mode is
         when FMC_Command_Mode_AutoRefresh =>
            Auto_Refresh_Number : FMC_SDRAM_Timing;
         when FMC_Command_Mode_LoadMode =>
            Mode_Register       : SDRAM_Mode_Register;
         when others =>
            null;
      end case;
   end record;

   procedure FMC_SDRAM_Cmd (Cmd : SDRAM_Command);

   function FMC_SDRAM_Busy return Boolean with Inline;

   function FMC_SDRAM_Get_Status
     (Bank : FMC_SDRAM_Bank_Type) return FMC_SDRAM_Status_Mode;

   procedure FMC_Set_Refresh_Count (Cnt : Word)
     with Pre => Cnt < 2 ** 13;

end STM32.FMC;
