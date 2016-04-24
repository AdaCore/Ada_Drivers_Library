with Ada.Real_Time;   use Ada.Real_Time;

with STM32.Board;     use STM32.Board;
with STM32.Device;    use STM32.Device;
with STM32.FMC;       use STM32.FMC;
with STM32.GPIO;      use STM32.GPIO;

with STM32_SVD.RCC;   use STM32_SVD.RCC;

package body STM32.SDRAM is

   Initialized : Boolean := False;
   G_Base_Addr : Word;

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
      Configure_Alternate_Function (SDRAM_PINS, GPIO_AF_FMC);
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

      RCC_Periph.AHB3ENR.FMCEN := True;
      RCC_Periph.AHB3RSTR.FMCRST := True;
      RCC_Periph.AHB3RSTR.FMCRST := False;

      --  90 MHz of SD clock frequency (180MHz / 2)
      --  1 Clock cycle = 1 / 90MHz = 11.1ns

      Timing_Conf :=
        (
         --  2 Clock cycles for Load to Active delay
         LoadToActiveDelay    => 2,

         --  min = 70ns: 7 * 11.1
         ExitSelfRefreshDelay => 7,

         --  in range [42ns, 120k ns] => using 4 * 11.1 ns
         SelfRefreshTime      => 4,

         --  min = 70ns
         RowCycleDelay        => 7,

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
     (Amount : Word;
      Align  : Word := Standard'Maximum_Alignment) return System.Address
   is
      Ret          : constant System.Address :=
                       System'To_Address (G_Base_Addr);
      Rounded_Size : Word;

   begin
      Initialize;
      Rounded_Size := Amount + Align;
      Rounded_Size :=
        Rounded_Size - Rounded_Size rem Align;

      G_Base_Addr := G_Base_Addr + Rounded_Size;

      return Ret;
   end Reserve;


end STM32.SDRAM;
