with SAM_SVD.PMC; use SAM_SVD.PMC;
with SAM_SVD.SUPC; use SAM_SVD.SUPC;

package body SAM.PMC is

   procedure Configure_Peripheral (ID     : Natural;
                                   Enable : Boolean)
   is
   begin
      Disable_Peripheral (ID => ID);
      --  TODO: configure gclk or periph_clk
      if Enable then
         Enable_Peripheral (ID => ID);
      end if;
   end Configure_Peripheral;

   procedure Enable_Peripheral (ID : Natural)
   is
      PCR_Reg : PMC_PMC_PCR_Register := PMC_Periph.PMC_PCR;
   begin
      PCR_Reg.PID := UInt7 (ID);
      PCR_Reg.CMD := True;
      PCR_Reg.EN := True;

      PMC_Periph.PMC_PCR := PCR_Reg;
   end Enable_Peripheral;

   procedure Disable_Peripheral (ID : Natural)
   is
      PCR_Reg : PMC_PMC_PCR_Register := PMC_Periph.PMC_PCR;
   begin
      PCR_Reg.PID := UInt7 (ID);
      PCR_Reg.CMD := True;
      PCR_Reg.EN := False;

      PMC_Periph.PMC_PCR := PCR_Reg;
   end Disable_Peripheral;

   procedure Enable_Peripheral_Clock (ID : Natural)
   is
   begin
      if ID < 32 then
         PMC_Periph.PMC_PCER0.PID.Arr (ID) := True;
      elsif ID < 37 then
         PMC_Periph.PMC_PCER1.PID.Arr (ID) := True;
      elsif ID = 37 then
         PMC_Periph.PMC_PCER1.PID37 := True;
      elsif ID < 53 then
         PMC_Periph.PMC_PCER1.PID_1.Arr (ID) := True;
      else
         PMC_Periph.PMC_PCER1.PID_2.Arr (ID) := True;
      end if;
   end Enable_Peripheral_Clock;

   procedure Disable_Peripheral_Clock (ID : Natural)
   is
   begin
      if ID < 32 then
         PMC_Periph.PMC_PCDR0.PID.Arr (ID) := True;
      elsif ID < 37 then
         PMC_Periph.PMC_PCDR1.PID.Arr (ID) := True;
      elsif ID = 37 then
         PMC_Periph.PMC_PCDR1.PID37 := True;
      elsif ID < 53 then
         PMC_Periph.PMC_PCDR1.PID_1.Arr (ID) := True;
      else
         PMC_Periph.PMC_PCDR1.PID_2.Arr (ID) := True;
      end if;
   end Disable_Peripheral_Clock;

   function Get_Peripheral_Clock (ID : Natural) return UInt32
   is
   begin
      if ID = UART0_ID or ID = UART1_ID or ID = UART2_ID or ID = UART3_ID or
        ID = UART4_ID or ID = USART0_ID or ID = USART1_ID or ID = USART2_ID
      then
         return System_Clocks.PCK (4);
      elsif ID = MCAN0_INT0_ID or ID = MCAN0_INT1_ID or ID = MCAN1_INT0_ID or
        ID = MCAN1_INT1_ID
      then
         return System_Clocks.PCK (5);
      elsif ID = TC1_CHANNEL0_ID or ID = TC1_CHANNEL1_ID or
        ID = TC1_CHANNEL2_ID or ID = TC2_CHANNEL0_ID or ID = TC2_CHANNEL1_ID or
        ID = TC2_CHANNEL2_ID or ID = TC3_CHANNEL0_ID or ID = TC3_CHANNEL1_ID or
        ID = TC3_CHANNEL2_ID
      then
         return System_Clocks.PCK (6);
      elsif ID = TC0_CHANNEL0_ID or ID = TC0_CHANNEL1_ID or
        ID = TC0_CHANNEL2_ID
      then
         return System_Clocks.PCK (7);
      elsif ID = I2SC0_ID or ID = I2SC1_ID then
         return Get_GCLK (PID => UInt7 (ID));
      else
         return System_Clocks.MCK;
      end if;

   end Get_Peripheral_Clock;

   function Calc_PLLACK_Div return UInt32
   is
      Div : constant UInt32 := UInt32 (PMC_Periph.CKGR_PLLAR.DIVA);
      Mul : constant UInt32 := UInt32 (PMC_Periph.CKGR_PLLAR.MULA);
   begin
      if Div = 0 or Mul = 0 then
         return 0;
      end if;

      return ((Mul + 1) / Div);
   end Calc_PLLACK_Div;

   function Calc_FCLK (In_Clk : UInt32) return UInt32
   is
      Ret : UInt32;
   begin
      case PMC_Periph.PMC_MCKR.PRES is
         when Clk_1 =>
            Ret := In_Clk / 1;
         when Clk_2 =>
            Ret := In_Clk / 2;
         when Clk_4 =>
            Ret := In_Clk / 4;
         when Clk_8 =>
            Ret := In_Clk / 8;
         when Clk_16 =>
            Ret := In_Clk / 16;
         when Clk_32 =>
            Ret := In_Clk / 32;
         when Clk_64 =>
            Ret := In_Clk / 64;
         when Clk_3 =>
            Ret := In_Clk / 3;
      end case;

      return Ret;
   end Calc_FCLK;

   function Calc_USB_FS (Clocks : System_Clock_Map) return UInt32
   is
      Clk : UInt32;
      Div : constant UInt32 := UInt32 (PMC_Periph.PMC_USB.USBDIV) + 1;
   begin
      if not PMC_Periph.PMC_USB.USBS then
         --  PLLA source
         Clk := Clocks.PLLACK;
      else
         --  UPLL source
         Clk := Clocks.UPLLCKDIV;
      end if;
      return (Clk / Div);
   end Calc_USB_FS;

   function Calc_PCKx (X      : Natural;
                       Clocks : System_Clock_Map)
                       return UInt32
   is
      Clk  : UInt32;
      Pres : constant UInt32 := UInt32 (PMC_Periph.PMC_PCK (X).PRES) + 1;
   begin
      case PMC_Periph.PMC_PCK (X).CSS is
         when Slow_Clk =>
            Clk := Clocks.SLCK;
         when Main_Clk =>
            Clk := Clocks.MAINCK;
         when Plla_Clk =>
            Clk := Clocks.PLLACK;
         when Upll_Clk =>
            Clk := Clocks.UPLLCKDIV;
         when Mck =>
            Clk := Clocks.MCK;
      end case;

      return (Clk / Pres);
   end Calc_PCKx;

   function Get_UPLLDIV2_Value return UInt32
   is (if PMC_Periph.PMC_MCKR.UPLLDIV2 then 2 else 1);

   function System_Clocks return System_Clock_Map
   is
      Clocks : System_Clock_Map;
   begin
      --  calculate SLOW_CLK
      case SUPC_Periph.SUPC_SR.OSCSEL is
         when Rc =>
            Clocks.SLCK := Slow_RC_Osc;
         when Cryst =>
            Clocks.SLCK := Crystal_Osc_32k768;
      end case;

      --  calculate MAINCK
      if not PMC_Periph.CKGR_MOR.MOSCSEL then
         --  main RC osc
         case PMC_Periph.CKGR_MOR.MOSCRCF is
            when Val_4_Mhz =>
               Clocks.MAINCK := 4_000_000;
            when Val_8_Mhz =>
               Clocks.MAINCK := 8_000_000;
            when Val_12_Mhz =>
               Clocks.MAINCK := 12_000_000;
         end case;
      else
         --  main crystal osc
         Clocks.MAINCK := Crystal_Osc_Main;
      end if;

      --  calculate PLLACK
      Clocks.PLLACK := Calc_PLLACK_Div * Clocks.MAINCK;

      --  calculate UTMI PLL
      Clocks.UPLLCK := UPLLCK_480MHz;
      Clocks.UPLLCKDIV := Clocks.UPLLCK /
        (Get_UPLLDIV2_Value + 1);

      --  calc MCK & FCLK
      case PMC_Periph.PMC_MCKR.CSS is
         when Slow_Clk =>
            Clocks.FCLK := Calc_FCLK (In_Clk => Clocks.SLCK);
         when Main_Clk =>
            Clocks.FCLK := Calc_FCLK (In_Clk => Clocks.MAINCK);
         when Plla_Clk =>
            Clocks.FCLK := Calc_FCLK (In_Clk => Clocks.PLLACK);
         when Upll_Clk =>
            Clocks.FCLK := Calc_FCLK (In_Clk => Clocks.UPLLCKDIV);
      end case;

      case PMC_Periph.PMC_MCKR.MDIV is
         when Eq_Pck =>
            Clocks.MCK := Clocks.FCLK / 1;
         when Pck_Div2 =>
            Clocks.MCK := Clocks.FCLK / 2;
         when Pck_Div4 =>
            Clocks.MCK := Clocks.FCLK / 4;
         when Pck_Div3 =>
            Clocks.MCK := Clocks.FCLK / 3;
      end case;

      Clocks.HCLK := Clocks.FCLK;
      Clocks.SysTick := Clocks.FCLK / 8;
      Clocks.USBCLK := Calc_USB_FS (Clocks => Clocks);

      for I in Clocks.PCK'Range loop
         Clocks.PCK (I) := Calc_PCKx (X      => I,
                                      Clocks => Clocks);
      end loop;

      Clocks.USBHS := Clocks.UPLLCK;

      return Clocks;

   end System_Clocks;

   function Get_GCLK (PID : HAL.UInt7) return UInt32
   is
      Div : UInt32;
      Clk : UInt32;
   begin
      PMC_Periph.PMC_PCR := (PID    => PID,
                             CMD    => False,
                             others => <>);

      Div := UInt32 (PMC_Periph.PMC_PCR.GCLKDIV) + 1;
      case PMC_Periph.PMC_PCR.GCLKCSS is
         when Slow_Clk =>
            Clk := System_Clocks.SLCK;
         when Main_Clk =>
            Clk := System_Clocks.MAINCK;
         when Plla_Clk =>
            Clk := System_Clocks.PLLACK;
         when Upll_Clk =>
            Clk := System_Clocks.UPLLCKDIV;
         when Mck_Clk =>
            Clk := System_Clocks.MCK;
      end case;

      return (Clk / Div);
   end Get_GCLK;

end SAM.PMC;
