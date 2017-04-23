------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

with Ada.Real_Time;   use Ada.Real_Time;
pragma Warnings (Off, "* is an internal GNAT unit*");
with System.BB.Parameters;
pragma Warnings (On, "* is an internal GNAT unit*");

with STM32.Board;     use STM32.Board;
with STM32.Device;    use STM32.Device;
with STM32.FMC;       use STM32.FMC;
with STM32.GPIO;      use STM32.GPIO;

with STM32_SVD.RCC;   use STM32_SVD.RCC;

package body STM32.SDRAM is

   Initialized : Boolean := False;
   G_Base_Addr : UInt32;

   procedure SDRAM_GPIOConfig;
   procedure SDRAM_InitSequence;

   ----------------------
   -- SDRAM_GPIOConfig --
   ----------------------

   procedure SDRAM_GPIOConfig
   is
   begin
      Enable_Clock (SDRAM_PINS);

      Configure_IO (SDRAM_PINS,
                    (Speed       => Speed_50MHz,
                     Mode        => Mode_AF,
                     Output_Type => Push_Pull,
                     Resistors   => Pull_Up));
      Configure_Alternate_Function (SDRAM_PINS, GPIO_AF_FMC_12);
      Lock (SDRAM_PINS);
   end SDRAM_GPIOConfig;

   ------------------------
   -- SDRAM_InitSequence --
   ------------------------

   procedure SDRAM_InitSequence
   is
      CAS : SDRAM_Mode_CAS_Latency;
   begin
      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

      FMC_SDRAM_Cmd
        ((Mode   => FMC_Command_Mode_CLK_Enabled,
          Target => SDRAM_Bank));

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

      delay until Clock + Milliseconds (1);

      FMC_SDRAM_Cmd
        ((Mode   => FMC_Command_Mode_PALL,
          Target => SDRAM_Bank));

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

      FMC_SDRAM_Cmd
        ((Mode                => FMC_Command_Mode_AutoRefresh,
          Target              => SDRAM_Bank,
          Auto_Refresh_Number => 8));

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

      case SDRAM_CAS_Latency is
         when FMC_CAS_Latency_1 | FMC_CAS_Latency_2 =>
            CAS := SDRAM_Mode_CAS_Latency_2;
         when FMC_CAS_Latency_3 =>
            CAS := SDRAM_Mode_CAS_Latency_3;
      end case;

      FMC_SDRAM_Cmd
        ((Mode          => FMC_Command_Mode_LoadMode,
          Target        => SDRAM_Bank,
          Mode_Register =>
           (SDRAM_Mode_Burst_Length_1,
            SDRAM_Mode_Burst_Sequential,
            CAS,
            SDRAM_Mode_Writeburst_Mode_Single)));

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

      FMC_Set_Refresh_Count (SDRAM_Refresh_Cnt);

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;
   end SDRAM_InitSequence;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      Timing_Conf     : FMC_SDRAM_TimingInit_Config;
      SDRAM_Conf      : FMC_SDRAM_Init_Config;
      SDCLK           : constant :=
                          System.BB.Parameters.Clock_Frequency / 2;
      SDPeriod_In_ns  : constant :=
                          1_000_000_000 / SDCLK;
      Refresh_Delay   : Unsigned_32;

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      G_Base_Addr := SDRAM_Base;

      ------------------------
      -- GPIO CONFIGURATION --
      ------------------------

      SDRAM_GPIOConfig;

      --------------
      -- FMC_INIT --
      --------------

      RCC_Periph.AHB3ENR.FMCEN := True;
      RCC_Periph.AHB3RSTR.FMCRST := True;
      RCC_Periph.AHB3RSTR.FMCRST := False;

      --  100 MHz of SD clock frequency (200MHz / 2)
      --  1 Clock cycle = 1 / 100MHz = 10ns

      Refresh_Delay :=
        (SDRAM_Min_Delay_In_ns - SDPeriod_In_ns + 1) / SDPeriod_In_ns;

      Timing_Conf :=
        (
         --  2 Clock cycles for Load to Active delay
         LoadToActiveDelay    => 2,

         --  min = 60ns: 6 * 10.0
         ExitSelfRefreshDelay => FMC_SDRAM_Timing (Refresh_Delay),

         --  in range [42ns, 120k ns] => using 4 * 11.1 ns
         SelfRefreshTime      => 4,

         --  min = 60ns
         RowCycleDelay        => FMC_SDRAM_Timing (Refresh_Delay),

         --  min = 20ns
         WriteRecoveryTime    => 2,
         RPDelay              => 2,
         RCDDelay             => 2);

      SDRAM_Conf :=
        (Bank               => SDRAM_Bank,
         ColumnBitsNumber   => FMC_ColumnBits_Number_8b,
         RowBitsNumber      => SDRAM_Row_Bits,
         SDMemoryDataWidth  => SDRAM_Mem_Width,
         InternalBankNumber => FMC_InternalBank_Number_4,
         CASLatency         => SDRAM_CAS_Latency,
         WriteProtection    => FMC_Write_Protection_Disable,
         SDClockPeriod      => SDRAM_CLOCK_Period,
         ReadBurst          => SDRAM_Read_Burst,
         ReadPipeDelay      => SDRAM_Read_Pipe,
         Timing_Conf        => Timing_Conf);

      FMC_SDRAM_Init (SDRAM_Conf);

      if SDRAM_Conf.Bank /= FMC_Bank1_SDRAM then
         SDRAM_Conf.Bank := FMC_Bank1_SDRAM;
         FMC_SDRAM_Init (SDRAM_Conf);
      end if;

      SDRAM_InitSequence;
   end Initialize;

   ------------------
   -- Base_Address --
   ------------------

   function Base_Address return System.Address
   is
   begin
      return System'To_Address (G_Base_Addr);
   end Base_Address;

   -------------
   -- Reserve --
   -------------

   function Reserve
     (Amount : UInt32;
      Align  : UInt32 := Standard'Maximum_Alignment) return System.Address
   is
      Ret          : constant System.Address :=
                       System'To_Address (G_Base_Addr);
      Rounded_Size : UInt32;

   begin
      Initialize;
      Rounded_Size := Amount + Align;
      Rounded_Size :=
        Rounded_Size - Rounded_Size rem Align;

      G_Base_Addr := G_Base_Addr + Rounded_Size;

      return Ret;
   end Reserve;


end STM32.SDRAM;
