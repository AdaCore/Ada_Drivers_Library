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
--   @file    stm32f4xx_ll_fmc.c                                            --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   FMC HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with STM32_SVD.FSMC; use STM32_SVD.FSMC;

package body STM32.FMC is

   --------------------
   -- FMC_SDRAM_Init --
   --------------------

   procedure FMC_SDRAM_Init (SDRAM_Conf : FMC_SDRAM_Init_Config)
   is
      SDCR  : SDCR_Register;
   begin
      SDCR :=
        (RPIPE  =>
           FMC_SDRAM_Read_Pipe_Delay'Enum_Rep (SDRAM_Conf.ReadPipeDelay),
         RBURST => SDRAM_Conf.ReadBurst = FMC_Read_Burst_Single,
         SDCLK  =>
           FMC_SDRAM_Clock_Configuration'Enum_Rep (SDRAM_Conf.SDClockPeriod),
         WP     => SDRAM_Conf.WriteProtection = FMC_Write_Protection_Enable,
         CAS    =>
           FMC_SDRAM_CAS_Latency'Enum_Rep (SDRAM_Conf.CASLatency),
         NB     => SDRAM_Conf.InternalBankNumber = FMC_InternalBank_Number_4,
         MWID   =>
           FMC_SDRAM_Memory_Bus_Width'Enum_Rep (SDRAM_Conf.SDMemoryDataWidth),
         NR     =>
           FMC_SDRAM_Row_Address_Bits'Enum_Rep (SDRAM_Conf.RowBitsNumber),
         NC     =>
           FMC_SDRAM_Column_Address_Bits'Enum_Rep
             (SDRAM_Conf.ColumnBitsNumber),
         others => <>);

      case SDRAM_Conf.Bank is
         when FMC_Bank1_SDRAM =>
            FMC_Periph.SDCR1 := SDCR;
            FMC_Periph.SDTR1 :=
              (TRCD => UInt4 (SDRAM_Conf.Timing_Conf.RCDDelay - 1),
               TRP  => UInt4 (SDRAM_Conf.Timing_Conf.RPDelay - 1),
               TWR  => UInt4 (SDRAM_Conf.Timing_Conf.WriteRecoveryTime - 1),
               TRC  => UInt4 (SDRAM_Conf.Timing_Conf.RowCycleDelay - 1),
               TRAS => UInt4 (SDRAM_Conf.Timing_Conf.SelfRefreshTime - 1),
               TXSR => UInt4 (SDRAM_Conf.Timing_Conf.ExitSelfRefreshDelay - 1),
               TMRD => UInt4 (SDRAM_Conf.Timing_Conf.LoadToActiveDelay - 1),
               others => <>);

         when FMC_Bank2_SDRAM =>
            FMC_Periph.SDCR1 :=
              (RPIPE  =>
                  FMC_SDRAM_Read_Pipe_Delay'Enum_Rep
                 (SDRAM_Conf.ReadPipeDelay),
               RBURST => SDRAM_Conf.ReadBurst = FMC_Read_Burst_Single,
               SDCLK  =>
                  FMC_SDRAM_Clock_Configuration'Enum_Rep
                 (SDRAM_Conf.SDClockPeriod),
               WP     => False,
               CAS    => 0,
               NB     => False,
               MWID   => 0,
               NR     => 0,
               NC     => 0,
               others => <>);
            FMC_Periph.SDCR2 := SDCR;

            --  The TRP and TRC timings are only configured in the FMC_SDTR1
            --  register.
            FMC_Periph.SDTR1 :=
              (TRCD => 0,
               TRP  => UInt4 (SDRAM_Conf.Timing_Conf.RPDelay - 1),
               TWR  => 0,
               TRC  => UInt4 (SDRAM_Conf.Timing_Conf.RowCycleDelay - 1),
               TRAS => 0,
               TXSR => 0,
               TMRD => 0,
               others => <>);
            FMC_Periph.SDTR2 :=
              (TRCD => 0,
               TRP  => 0, --  Don't care
               TWR  => UInt4 (SDRAM_Conf.Timing_Conf.WriteRecoveryTime - 1),
               TRC  => 0, --  Don't care
               TRAS => UInt4 (SDRAM_Conf.Timing_Conf.SelfRefreshTime - 1),
               TXSR => UInt4 (SDRAM_Conf.Timing_Conf.ExitSelfRefreshDelay - 1),
               TMRD => UInt4 (SDRAM_Conf.Timing_Conf.LoadToActiveDelay - 1),
               others => <>);
      end case;
   end FMC_SDRAM_Init;

   -------------------
   -- FMC_SDRAM_Cmd --
   -------------------

   procedure FMC_SDRAM_Cmd (Cmd : SDRAM_Command) is
      SDCMR : SDCMR_Register;
      function To_UInt13 is new Ada.Unchecked_Conversion
        (SDRAM_Mode_Register, UInt13);
   begin
      case Cmd.Mode is
         when FMC_Command_Mode_AutoRefresh =>
            SDCMR.NRFS := FMC_SDRAM_Timing'Enum_Rep (Cmd.Auto_Refresh_Number);
         when FMC_Command_Mode_LoadMode =>
            SDCMR.MRD  := To_UInt13 (Cmd.Mode_Register);
         when others =>
            null;
      end case;

      SDCMR.CTB.Val := FMC_SDRAM_Cmd_Target_Bank'Enum_Rep (Cmd.Target);
      SDCMR.MODE    := FMC_SDRAM_Cmd_Mode'Enum_Rep (Cmd.Mode);

      FMC_Periph.SDCMR := SDCMR;
   end FMC_SDRAM_Cmd;

   --------------------
   -- FMC_SDRAM_Busy --
   --------------------

   function FMC_SDRAM_Busy return Boolean
   is
   begin
      return FMC_Periph.SDSR.BUSY;
   end FMC_SDRAM_Busy;

   --------------------------
   -- FMC_SDRAM_Get_Status --
   --------------------------

   function FMC_SDRAM_Get_Status
     (Bank : FMC_SDRAM_Bank_Type) return FMC_SDRAM_Status_Mode
   is
      Val : UInt2;
   begin
      case Bank is
         when FMC_Bank1_SDRAM =>
            Val := FMC_Periph.SDSR.MODES.Arr (1);
         when FMC_Bank2_SDRAM =>
            Val := FMC_Periph.SDSR.MODES.Arr (2);
      end case;
      return FMC_SDRAM_Status_Mode'Val (Val);
   end FMC_SDRAM_Get_Status;

   ---------------------------
   -- FMC_Set_Refresh_Count --
   ---------------------------

   procedure FMC_Set_Refresh_Count (Cnt : Word) is
   begin
      FMC_Periph.SDRTR.COUNT := UInt13 (Cnt);
   end FMC_Set_Refresh_Count;

end STM32.FMC;
