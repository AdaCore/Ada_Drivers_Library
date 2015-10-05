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

package body STM32F4.FMC is

   procedure FMC_SDRAM_Init (SDRAM_Conf : FMC_SDRAM_Init_Config) is
      Tmpr1, Tmpr2, Tmpr3, Tmpr4 : Word := 0;
   begin
      --  SDRAM bank control register configuration
      Tmpr1 := SDRAM_Conf.ColumnBitsNumber or
        SDRAM_Conf.RowBitsNumber or
        SDRAM_Conf.SDMemoryDataWidth or
        SDRAM_Conf.InternalBankNumber or
        SDRAM_Conf.CASLatency or
        SDRAM_Conf.WriteProtection or
        SDRAM_Conf.SDClockPeriod or
        SDRAM_Conf.ReadBurst or
        SDRAM_Conf.ReadPipeDelay;

      if SDRAM_Conf.Bank = FMC_Bank1_SDRAM then
         Bank5_6.SDCR (SDRAM_Conf.Bank) := tmpr1;
      else   --  SDCR2 "don't care" bits configuration
         Tmpr3 := SDRAM_Conf.SDClockPeriod or
           SDRAM_Conf.ReadBurst or
           SDRAM_Conf.ReadPipeDelay;

         Bank5_6.SDCR (FMC_Bank1_SDRAM) := tmpr3;
         Bank5_6.SDCR (SDRAM_Conf.Bank) := tmpr1;
      end if;

      --  SDRAM bank timing register configuration
      if SDRAM_Conf.Bank = FMC_Bank1_SDRAM then
         Tmpr2 := ((SDRAM_Conf.Timing_Conf.LoadToActiveDelay) - 1) or
           (((SDRAM_Conf.Timing_Conf.ExitSelfRefreshDelay) - 1) * (2**4)) or
           (((SDRAM_Conf.Timing_Conf.SelfRefreshTime) - 1) * (2**8)) or
           (((SDRAM_Conf.Timing_Conf.RowCycleDelay) - 1) * (2**12)) or
           (((SDRAM_Conf.Timing_Conf.WriteRecoveryTime) - 1) * (2**16)) or
           (((SDRAM_Conf.Timing_Conf.RPDelay) - 1) * (2**20)) or
           (((SDRAM_Conf.Timing_Conf.RCDDelay) - 1) * (2**24));

         Bank5_6.SDTR (SDRAM_Conf.Bank) := tmpr2;
      else   --  SDTR "don't care bits configuration
         Tmpr2 := ((SDRAM_Conf.Timing_Conf.LoadToActiveDelay) - 1) or
                ((SDRAM_Conf.Timing_Conf.ExitSelfRefreshDelay - 1) * (2**4)) or
                (((SDRAM_Conf.Timing_Conf.SelfRefreshTime) - 1)  * (2**8)) or
                (((SDRAM_Conf.Timing_Conf.WriteRecoveryTime) - 1) * (2**16));

         Tmpr4 := (((SDRAM_Conf.Timing_Conf.RowCycleDelay)-1) * (2**12)) or
           (((SDRAM_Conf.Timing_Conf.RPDelay)-1) * (2**20));

         Bank5_6.SDTR (FMC_Bank1_SDRAM) := Tmpr4;
         Bank5_6.SDTR (SDRAM_Conf.Bank) := Tmpr2;
      end if;
   end FMC_SDRAM_Init;

   procedure FMC_SDRAM_Cmd (Cmd : FMC_SDRAM_Cmd_Conf) is
   begin
      Bank5_6.SDCMR := Cmd.CommandMode
        or Cmd.CommandTarget
        or ((Cmd.AutoRefreshNumber - 1) * (2**5))
        or (Cmd.ModeRegisterDefinition * (2**9));
   end FMC_SDRAM_Cmd;

   function FMC_Get_Flag (Bank : Word; Flag : Word) return Boolean is
      Reg : Word;
   begin
      case Bank is
         when FMC_Bank2_NAND =>
            Reg := Bank2.SR;
         when FMC_Bank3_NAND =>
            Reg := Bank3.SR;
         when FMC_Bank4_PCCARD =>
            Reg := Bank4.SR;
         when others =>
            Reg := Bank5_6.SDSR;
      end case;

      return (Reg and Flag) /= 0;
   end FMC_Get_Flag;

   procedure FMC_Set_Refresh_Count (Cnt : Word) is
   begin
      Bank5_6.SDRTR := Bank5_6.SDRTR or Cnt * 2;
   end FMC_Set_Refresh_Count;

end STM32F4.FMC;
