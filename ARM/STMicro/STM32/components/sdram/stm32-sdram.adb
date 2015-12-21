with Ada.Real_Time;   use Ada.Real_Time;

with STM32.Board;     use STM32.Board;
with STM32.Device;    use STM32.Device;
with STM32.FMC;       use STM32.FMC;
with STM32.GPIO;      use STM32.GPIO;

with STM32_SVD.RCC;   use STM32_SVD.RCC;

package body STM32.SDRAM is

   Initialized : Boolean := False;
   G_Base_Addr : Word;

   procedure My_Delay (Ms : Integer);
   procedure SDRAM_GPIOConfig;
   procedure SDRAM_InitSequence;

   --------------
   -- My_Delay --
   --------------

   procedure My_Delay (Ms : Integer) is
      Next_Start : Time := Clock;
      Period    : constant Time_Span := Milliseconds (Ms);
   begin
      Next_Start := Next_Start + Period;
      delay until Next_Start;
   end My_Delay;

   ----------------------
   -- SDRAM_GPIOConfig --
   ----------------------

   procedure SDRAM_GPIOConfig
   is
   begin
      Enable_Clock (SDRAM_PINS);

      Configure_Alternate_Function (SDRAM_Pins, GPIO_AF_FMC);
      Configure_IO (SDRAM_PINS,
                    (Speed       => Speed_50MHz,
                     Mode        => Mode_AF,
                     Output_Type => Push_Pull,
                     Resistors   => Floating));
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

      My_Delay (100);

      loop
         exit when not FMC_SDRAM_Busy;
      end loop;

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
      Timing_Conf : FMC_SDRAM_TimingInit_Config;
      SDRAM_Conf  : FMC_SDRAM_Init_Config;
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

     RCC_Periph.AHB3ENR.FMCEN := 1;

      --  90 MHz of SD clock frequency (180MHz / 2)
      --  1 Clock cycle = 1 / 90MHz = 11.1ns

      --  2 Clock cycles for Load to Active delay
      Timing_Conf.LoadToActiveDelay    := 2;

      --  min = 70ns: 7 * 11.1
      Timing_Conf.ExitSelfRefreshDelay := 7;

      --  in range [42ns, 120k ns] => using 4 * 11.1 ns
      Timing_Conf.SelfRefreshTime      := 4;

      --  min = 70ns
      Timing_Conf.RowCycleDelay        := 7;

      --  min = 20ns
      Timing_Conf.WriteRecoveryTime    := 2;
      Timing_Conf.RPDelay              := 2;
      Timing_Conf.RCDDelay             := 2;

      SDRAM_Conf.Bank               := SDRAM_Bank;
      SDRAM_Conf.ColumnBitsNumber   := FMC_ColumnBits_Number_8b;
      SDRAM_Conf.RowBitsNumber      := FMC_RowBits_Number_12b;
      SDRAM_Conf.SDMemoryDataWidth  := FMC_SDMemory_Width_16b;
      SDRAM_Conf.InternalBankNumber := FMC_InternalBank_Number_4;
      SDRAM_Conf.CASLatency         := SDRAM_CAS_Latency;
      SDRAM_Conf.WriteProtection    := FMC_Write_Protection_Disable;
      SDRAM_Conf.SDClockPeriod      := SDRAM_CLOCK_Period;
      SDRAM_Conf.ReadBurst          := SDRAM_Read_Burst;
      SDRAM_Conf.ReadPipeDelay      := SDRAM_Read_Pipe;
      SDRAM_Conf.Timing_Conf        := Timing_Conf;

      FMC_SDRAM_Init (SDRAM_Conf);

      SDRAM_Conf.Bank := FMC_Bank1_SDRAM;
      FMC_SDRAM_Init (SDRAM_Conf);

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

   function Reserve (Amount : Word) return System.Address
   is
      Ret : constant System.Address := System'To_Address (G_Base_Addr);
   begin
      G_Base_Addr := G_Base_Addr + Amount;

      return Ret;
   end Reserve;


end STM32.SDRAM;
