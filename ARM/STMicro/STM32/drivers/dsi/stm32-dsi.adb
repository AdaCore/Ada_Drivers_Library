with Ada.Real_Time;     use Ada.Real_Time;

pragma Warnings (Off, "* is an internal GNAT unit");
with System.BB.Parameters; use System.BB.Parameters;
with System.STM32F4;       use System.STM32F4;
pragma Warnings (On, "* is an internal GNAT unit");

with STM32_SVD.DSIHOST; use STM32_SVD.DSIHOST;
with STM32_SVD.RCC;     use STM32_SVD.RCC;

with STM32.Board;       use STM32.Board;
with STM32.GPIO;        use STM32.GPIO;

package body STM32.DSI is

   DSI_Data_Type_Encoding : constant array (DSI_Pkt_Data_Type) of Bits_6 :=
                              (DCS_Short_Pkt_Write_P0 => 16#05#,
                               DCS_Short_Pkt_Write_P1 => 16#15#,
                               Gen_Short_Pkt_Write_P0 => 16#03#,
                               Gen_Short_Pkt_Write_P1 => 16#13#,
                               Gen_Short_Pkt_Write_P2 => 16#23#,
                               DCS_Long_Pkt_Write     => 16#39#,
                               Gen_Long_Pkt_Write     => 16#29#,
                               DCS_Short_Pkt_Read     => 16#06#,
                               Gen_Short_Pkg_Read_P0  => 16#04#,
                               Gen_Short_Pkg_Read_P1  => 16#14#,
                               Gen_Short_Pkg_Read_P2  => 16#24#);

   ------------------------------
   -- DSI_Config_Packet_Header --
   ------------------------------

   procedure DSI_Config_Packet_Header
     (Channel_ID : DSI_Virtual_Channel_ID;
      Data_Type  : DSI_Pkt_Data_Type;
      Data0      : Byte;
      Data1      : Byte)
   is
   begin
      DSIHOST_Periph.DSI_GHCR :=
        (DT     => DSI_Data_Type_Encoding (Data_Type),
         VCID   => Channel_Id,
         WCLSB  => Data0,
         WCMSB  => Data1,
         others => <>);
   end DSI_Config_Packet_Header;

   --------------------
   -- DSI_Initialize --
   --------------------

   procedure DSI_Initialize
     (PLL_N_Div                : DSI_PLLN_Div;
      PLL_IN_Div               : DSI_PLL_IDF;
      PLL_OUT_Div              : DSI_PLL_ODF;
      Auto_Clock_Lane_Control  : Boolean;
      TX_Escape_Clock_Division : Byte;
      --  The TX_ESC clock division. 0 or 1 stops the clock.
      Number_Of_Lanes          : DSI_Number_Of_Lanes)
   is
      Start : Time;
   begin
      --  Enable the regulator
      DSIHOST_Periph.DSI_WRPCR.REGEN := 1;

      Start := Clock;
      while DSIHOST_Periph.DSI_WISR.RRS = 0 loop
         if Clock > (Start + Milliseconds (1000)) then
            raise Program_Error with "Timeout during DSI initialisation";
         end if;
      end loop;

      RCC_Periph.APB2ENR.DSIEN := 1;

      Enable_Clock (DSIHOST_TE);
      Configure_Alternate_Function (DSIHOST_TE, GPIO_AF_DSI);
      Configure_IO
        (DSIHOST_TE,
         Config =>
           (Mode        => Mode_AF,
            Output_Type => Push_Pull,
            Speed       => Speed_50MHz,
            Resistors   => Floating));

      ---------------------------
      -- Configure the DSI PLL --
      ---------------------------

      DSIHOST_Periph.DSI_WRPCR.NDIV := PLL_N_Div;
      DSIHOST_Periph.DSI_WRPCR.IDF  := PLL_IN_Div;
      DSIHOST_Periph.DSI_WRPCR.ODF  := DSI_PLL_ODF'Enum_Rep (PLL_OUT_Div);

      --  Enable the DSI PLL
      DSIHOST_Periph.DSI_WRPCR.PLLEN := 1;
      --  Wait for the lock of the PLL

      Start := Clock;
      while DSIHOST_Periph.DSI_WISR.PLLLS = 0 loop
         if Clock > (Start + Milliseconds (1000)) then
            raise Program_Error with "Timeout during DSI PLL setup";
         end if;
      end loop;

      ----------------------------
      -- Set the PHY parameters --
      ----------------------------

      --  Enable D-PHY clock and digital
      DSIHOST_Periph.DSI_PCTLR.CKE := 1;
      DSIHOST_Periph.DSI_PCTLR.DEN := 1;

      --  Clock lane configuration
      DSIHOST_Periph.DSI_CLCR.DPCC := 1;
      DSIHOST_Periph.DSI_CLCR.ACR  :=
        Boolean'Enum_Rep (Auto_Clock_Lane_Control);

      --  Configure the number of active data lanes
      DSIHOST_Periph.DSI_PCCONFR.NL :=
        DSI_Number_Of_Lanes'Enum_Rep (Number_Of_Lanes);

      ----------------------------------
      -- Set the DSI Clock parameters --
      ----------------------------------

      DSIHOST_Periph.DSIHSOT_CCR.TXECKDIV := TX_Escape_Clock_Division;

      --  Calculate the bit period in high-speed mode in unit of 0.25 ns.
      --  The equation is UIX4 = IntegerPart ((1000/F_PHY_Mhz) * 4)
      --  Where F_PHY_Mhz = (PLLNDIV * HSE_MHz) / (IDF * ODF)
      declare
         TempIDF          : constant Word :=
                              (if PLL_IN_Div > 0 then Word (PLL_IN_Div) else 1);
         Unit_Interval_x4 : constant Word :=
                              (4_000_000 * TempIDF
                               * 2 ** DSI_PLL_ODF'Enum_Rep (PLL_OUT_Div))
                              / ((Word (HSE_Clock (MCU_ID.DEV_ID) / 1000)) *
                                   Word (PLL_N_Div));
      begin
         DSIHOST_Periph.DSI_WPCR1.UIX4 := UInt6 (Unit_Interval_x4);
      end;

      ----------------------
      -- Error Management --
      ----------------------

      --  Disable error interrupts
      DSIHOST_Periph.DSI_IER0 := (others => <>);
      DSIHOST_Periph.DSI_IER1 := (others => <>);
   end DSI_Initialize;

   ----------------
   -- DSI_Deinit --
   ----------------

   procedure DSI_Deinit is
   begin
      --  Disable the DSI wrapper
      DSIHOST_Periph.DSI_WCR.DSIEN := 0;

      --  Disable the DSI host
      DSIHOST_Periph.DSI_CR.EN := 0;

      --  D-PHY clock and digital disable
      DSIHOST_Periph.DSI_PCTLR.DEN := 0;
      DSIHOST_Periph.DSI_PCTLR.CKE := 0;

      --  Turn off the DSI PLL
      DSIHOST_Periph.DSI_WRPCR.PLLEN := 0;

      --  Disable the regulator
      DSIHOST_Periph.DSI_WRPCR.REGEN := 0;
   end DSI_Deinit;

   ---------------------
   -- DSI_Setup_Video --
   ---------------------

   procedure DSI_Setup_Video
     (Virtual_Channel             : DSI_Virtual_Channel_ID;
      Color_Coding                : DSI_Color_Mode;
      Loosely_Packed              : Boolean;
      Video_Mode                  : DSI_Video_Mode;
      Packet_Size                 : Bits_15;
      Number_Of_Chunks            : Bits_14;
      Null_Packet_Size            : Bits_14;
      HSync_Polarity              : DSI_Polarity;
      VSync_Polarity              : DSI_Polarity;
      DataEn_Polarity             : DSI_Polarity;
      HSync_Active_Duration       : Bits_13;
      HSync_BackPorch             : Bits_13;
      HLine_Duration              : Bits_15;
      VSync_Active_Duration       : Bits_10;
      VSync_BackPorch             : Bits_10;
      VSync_FrontPorch            : Bits_10;
      Vertical_Active_Duration    : Bits_14;
      LP_Command_Enabled          : Boolean;
      LP_Largest_Packet_Size      : Byte;
      LP_VACT_Largest_Packet_Size : Byte;
      LP_H_Front_Porch_Enable     : Boolean;
      LP_H_Back_Porch_Enable      : Boolean;
      LP_V_Active_Enable          : Boolean;
      LP_V_Front_Porch_Enable     : Boolean;
      LP_V_Back_Porch_Enable      : Boolean;
      LP_V_Sync_Active_Enable     : Boolean;
      Frame_BTA_Ack_Enable        : Boolean)
   is
   begin
      --  Select video mode by resetting CMDM and SDIM bits
      DSIHOST_Periph.DSI_MCR.CMDM := 0;
      DSIHOST_Periph.DSI_WCFGR.DSIM := 0;

      --  Configure the video mode transmission type
      DSIHOST_Periph.DSI_VMCR.VMT     := DSI_Video_Mode'Enum_Rep (Video_Mode);

      --  Configure the video packet size
      DSIHOST_Periph.DSI_VPCR.VPSIZE  := Packet_Size;

      --  Set the chunks number to be transmitted through the DSI link
      DSIHOST_Periph.DSI_VCCR.NUMC    := Number_Of_Chunks;

      --  Set the size of the null packet
      DSIHOST_Periph.DSI_VNPCR.NPSIZE := Null_Packet_Size;

      --  Select the virtual channel for the LTDC interface traffic
      DSIHOST_Periph.DSI_LVCIDR.VCID  := Virtual_Channel;

      --  Configure the polarity of control signals
      DSIHOST_Periph.DSI_LPCR.HSP   := DSI_Polarity'Enum_Rep (HSync_Polarity);
      DSIHOST_Periph.DSI_LPCR.VSP   := DSI_Polarity'Enum_Rep (VSync_Polarity);
      DSIHOST_Periph.DSI_LPCR.DEP   := DSI_Polarity'Enum_Rep (DataEn_Polarity);

      --  Select the color coding for the host
      DSIHOST_Periph.DSI_LCOLCR.COLC := DSI_Color_Mode'Enum_Rep (Color_Coding);
      --  ... and for the wrapper
      DSIHOST_Periph.DSI_WCFGR.COLMUX :=
        DSI_Color_Mode'Enum_Rep (Color_Coding);

      --  Enable/disable the loosely packed variant to 18-bit configuration
      if Color_Coding = RGB666 then
         DSIHOST_Periph.DSI_LCOLCR.LPE := Boolean'Enum_Rep (Loosely_Packed);
      end if;

      --  Set the Horizontal Synchronization Active (HSA) in lane byte clock
      --  cycles
      DSIHOST_Periph.DSI_VHSACR.HSA := HSync_Active_Duration;
      --  Set the Horizontal Back Porch
      DSIHOST_Periph.DSI_VHBPCR.HBP := HSync_BackPorch;
      --  Total line time (HSA+HBP+HACT+HFP
      DSIHOST_Periph.DSI_VLCR.HLINE := HLine_Duration;

      --  Set the Vertical Synchronization Active
      DSIHOST_Periph.DSI_VVSACR.VSA := VSync_Active_Duration;
      --  VBP
      DSIHOST_Periph.DSI_VVBPCR.VBP := VSync_BackPorch;
      --  VFP
      DSIHOST_Periph.DSI_VVFPCR.VFP := VSync_FrontPorch;
      --  Vertical Active Period
      DSIHOST_Periph.DSI_VVACR.VA   := Vertical_Active_Duration;

      --  Configure the command transmission mode
      DSIHOST_Periph.DSI_VMCR.LPCE  := Boolean'Enum_Rep (LP_Command_Enabled);

      --  Low power configuration:
      DSIHOST_Periph.DSI_LPMCR.LPSIZE  := LP_Largest_Packet_Size;
      DSIHOST_Periph.DSI_LPMCR.VLPSIZE := LP_VACT_Largest_Packet_Size;
      DSIHOST_Periph.DSI_VMCR.LPHFE    :=
        Boolean'Enum_Rep (LP_H_Front_Porch_Enable);
      DSIHOST_Periph.DSI_VMCR.LPHBPE   :=
        Boolean'Enum_Rep (LP_H_Back_Porch_Enable);
      DSIHOST_Periph.DSI_VMCR.LVAE     :=
        Boolean'Enum_Rep (LP_V_Active_Enable);
      DSIHOST_Periph.DSI_VMCR.LPVFPE   :=
        Boolean'Enum_Rep (LP_V_Front_Porch_Enable);
      DSIHOST_Periph.DSI_VMCR.LPVBPE   :=
        Boolean'Enum_Rep (LP_V_Back_Porch_Enable);
      DSIHOST_Periph.DSI_VMCR.LPVSAE   :=
        Boolean'Enum_Rep (LP_V_Sync_Active_Enable);

      DSIHOST_Periph.DSI_VMCR.FBTAAE :=
        Boolean'Enum_Rep (Frame_BTA_Ack_Enable);
   end DSI_Setup_Video;

   ---------------
   -- DSI_Start --
   ---------------

   procedure DSI_Start is
   begin
      --  Enable the DSI Host
      DSIHOST_Periph.DSI_CR.EN := 1;
      --  Enable the DSI wrapper
      DSIHOST_Periph.DSI_WCR.DSIEN := 1;
   end DSI_Start;

   --------------
   -- DSI_Stop --
   --------------

   procedure DSI_Stop is
   begin
      --  Disable the DSI Host
      DSIHOST_Periph.DSI_CR.EN := 0;
      --  Disable the DSI wrapper
      DSIHOST_Periph.DSI_WCR.DSIEN := 0;
   end DSI_Stop;

   ---------------------
   -- DSI_Short_Write --
   ---------------------

   procedure DSI_Short_Write
     (Channel_ID : DSI_Virtual_Channel_ID;
      Mode       : DSI_Short_Write_Packet_Data_Type;
      Param1     : Byte;
      Param2     : Byte)
   is
      Start : Time;
   begin
      --  Wait for FIFO empty
      Start := Clock;
      while DSIHOST_Periph.DSI_GPSR.CMDFE = 0 loop
         if Clock > Start + Milliseconds (1000) then
            raise Program_Error with
              "Timeout while waiting for DSI FIFO empty";
         end if;
      end loop;

      --  Configure the packet to send a short DCS command with 0 or 1
      --  parameter
      DSI_Config_Packet_Header (Channel_ID, Mode, Param1, Param2);
   end DSI_Short_Write;

   --------------------
   -- DSI_Long_Write --
   --------------------

   procedure DSI_Long_Write
     (Channel_Id : DSI_Virtual_Channel_ID;
      Mode       : DSI_Long_Write_Packet_Data_Type;
      Param1     : Byte;
      Parameters : DSI_Data)
   is
      Start : Time;
      Off   : Natural := 0;
      Value : DSI_GPDR_Register;
      Val1  : Word;
      Val2  : Word;

   begin
      --  Wait for FIFO empty
      Start := Clock;
      while DSIHOST_Periph.DSI_GPSR.CMDFE = 0 loop
         if Clock > Start + Milliseconds (1000) then
            raise Program_Error with
              "Timeout while waiting for DSI FIFO empty";
         end if;
      end loop;

      Value.DATA.Arr (0) := Param1;
      Off   := 1;

      for Param of Parameters loop
         Value.DATA.Arr (Off) := Param;
         Off := Off + 1;
         if Off > Value.DATA.Arr'Last then
            DSIHOST_Periph.DSI_GPDR := Value;
            Value.DATA.Val := 0;
            Off            := 0;
         end if;
      end loop;

      if Off /= 0 then
         DSIHOST_Periph.DSI_GPDR := Value;
      end if;

      Val1 := Word (Parameters'Length + 1) and 16#FF#;
      Val2 := Shift_Right (Word (Parameters'Length + 1) and 16#FF00#, 8);
      DSI_Config_Packet_Header
        (Channel_Id, Mode, Byte (Val1), Byte (Val2));
   end DSI_Long_Write;

end STM32.DSI;
