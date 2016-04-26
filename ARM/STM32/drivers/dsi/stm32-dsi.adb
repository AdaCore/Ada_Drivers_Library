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
--   @file    stm32f4xx_hal_dsi.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.4.2                                                        --
--   @date    10-November-2015                                              --
--                                                                          --
--   COPYRIGHT(c) 2015 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Real_Time;     use Ada.Real_Time;
with Ada.Unchecked_Conversion;

pragma Warnings (Off, "* is an internal GNAT unit");
with System.BB.Parameters; use System.BB.Parameters;
pragma Warnings (On, "* is an internal GNAT unit");

with HAL.DSI;           use HAL.DSI;
with STM32_SVD.DSIHOST; use STM32_SVD.DSIHOST;
with STM32_SVD.RCC;     use STM32_SVD.RCC;

package body STM32.DSI is

   DSI_Data_Type_Encoding : constant array (DSI_Pkt_Data_Type) of UInt6 :=
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

   procedure DSI_Config_Packet_Header
     (This       : in out DSI_Host;
      Channel_ID : DSI_Virtual_Channel_ID;
      Data_Type  : DSI_Pkt_Data_Type;
      Data0      : Byte;
      Data1      : Byte);

   ------------------------------
   -- DSI_Config_Packet_Header --
   ------------------------------

   procedure DSI_Config_Packet_Header
     (This       : in out DSI_Host;
      Channel_ID : DSI_Virtual_Channel_ID;
      Data_Type  : DSI_Pkt_Data_Type;
      Data0      : Byte;
      Data1      : Byte)
   is
   begin
      This.Periph.DSI_GHCR :=
        (DT     => DSI_Data_Type_Encoding (Data_Type),
         VCID   => Channel_ID,
         WCLSB  => Data0,
         WCMSB  => Data1,
         others => <>);
   end DSI_Config_Packet_Header;

   --------------------
   -- DSI_Initialize --
   --------------------

   procedure DSI_Initialize
     (This                     : in out DSI_Host;
      PLL_N_Div                : DSI_PLLN_Div;
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
      This.Periph.DSI_WRPCR.REGEN := True;

      Start := Clock;
      --  Wait for the Regulator Ready Status
      while not This.Periph.DSI_WISR.RRS loop
         if Clock > (Start + Milliseconds (1000)) then
            raise Program_Error with "Timeout during DSI initialisation";
         end if;
      end loop;

      --  Enable the DSI clock
      RCC_Periph.APB2ENR.DSIEN := True;

      --  Make sure the DSI peripheral is OFF
      This.DSI_Stop;

      ---------------------------
      -- Configure the DSI PLL --
      ---------------------------

      This.Periph.DSI_WRPCR.IDF  := PLL_IN_Div;
      This.Periph.DSI_WRPCR.NDIV := PLL_N_Div;
      This.Periph.DSI_WRPCR.ODF  := DSI_PLL_ODF'Enum_Rep (PLL_OUT_Div);

      --  Enable the DSI PLL
      This.Periph.DSI_WRPCR.PLLEN := True;
      --  Wait for the lock of the PLL

      Start := Clock;
      while not This.Periph.DSI_WISR.PLLLS loop
         if Clock > (Start + Milliseconds (1000)) then
            raise Program_Error with "Timeout during DSI PLL setup";
         end if;
      end loop;

      ----------------------------
      -- Set the PHY parameters --
      ----------------------------

      --  Enable D-PHY clock and digital
      This.Periph.DSI_PCTLR.CKE := True;
      This.Periph.DSI_PCTLR.DEN := True;

      --  Clock lane configuration
      This.Periph.DSI_CLCR.DPCC := True;
      This.Periph.DSI_CLCR.ACR  := Auto_Clock_Lane_Control;

      --  Configure the number of active data lanes
      This.Periph.DSI_PCCONFR.NL :=
        DSI_Number_Of_Lanes'Enum_Rep (Number_Of_Lanes);

      ----------------------------------
      -- Set the DSI Clock parameters --
      ----------------------------------

      This.Periph.DSIHSOT_CCR.TXECKDIV := TX_Escape_Clock_Division;

      --  Calculate the bit period in high-speed mode in unit of 0.25 ns.
      --  The equation is UIX4 = IntegerPart ((1000/F_PHY_Mhz) * 4)
      --  Where F_PHY_Mhz = (PLLNDIV * HSE_MHz) / (IDF * ODF)
      --  => UIX4 = 4_000 * IDF * ODV / (PLLNDIV * HSE_MHz)
      declare
         HSE_MHz          : constant Word := HSE_Clock / 1_000_000;
         IDF              : constant Word :=
                              (if PLL_IN_Div > 0
                               then Word (PLL_IN_Div) else 1);
         ODF              : constant Word :=
                              Shift_Left
                                (1, DSI_PLL_ODF'Enum_Rep (PLL_OUT_Div));
         PLLN             : constant Word := Word (PLL_N_Div);
         Unit_Interval_x4 : constant Word :=
                              ((4_000 * IDF * ODF) / (PLLN * HSE_MHz));
      begin
         This.Periph.DSI_WPCR1.UIX4 := UInt6 (Unit_Interval_x4);
      end;

      ----------------------
      -- Error Management --
      ----------------------

      --  Disable error interrupts
      This.Periph.DSI_IER0 := (others => <>);
      This.Periph.DSI_IER1 := (others => <>);
   end DSI_Initialize;

   ----------------
   -- DSI_Deinit --
   ----------------

   procedure DSI_Deinit (This : in out DSI_Host) is
   begin
      --  Disable the DSI wrapper and host
      This.DSI_Stop;

      --  D-PHY clock and digital disable
      This.Periph.DSI_PCTLR.DEN := False;
      This.Periph.DSI_PCTLR.CKE := False;

      --  Turn off the DSI PLL
      This.Periph.DSI_WRPCR.PLLEN := False;

      --  Disable the regulator
      This.Periph.DSI_WRPCR.REGEN := False;
   end DSI_Deinit;

   --------------------------
   -- DSI_Setup_Video_Mode --
   --------------------------

   procedure DSI_Setup_Video_Mode
     (This                        : in out DSI_Host;
      Virtual_Channel             : DSI_Virtual_Channel_ID;
      Color_Coding                : DSI_Color_Mode;
      Loosely_Packed              : Boolean;
      Video_Mode                  : DSI_Video_Mode;
      Packet_Size                 : UInt15;
      Number_Of_Chunks            : UInt14;
      Null_Packet_Size            : UInt14;
      HSync_Polarity              : DSI_Polarity;
      VSync_Polarity              : DSI_Polarity;
      DataEn_Polarity             : DSI_Polarity;
      HSync_Active_Duration       : UInt13;
      Horizontal_BackPorch        : UInt13;
      Horizontal_Line             : UInt15;
      VSync_Active_Duration       : UInt10;
      Vertical_BackPorch          : UInt10;
      Vertical_FrontPorch         : UInt10;
      Vertical_Active             : UInt14;
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
      function To_Bool is new Ada.Unchecked_Conversion
        (DSI_Polarity, Boolean);
   begin
      --  Select video mode by resetting CMDM and SDIM bits
      This.Periph.DSI_MCR.CMDM := False;
      This.Periph.DSI_WCFGR.DSIM := False;

      --  Configure the video mode transmission type
      This.Periph.DSI_VMCR.VMT     := DSI_Video_Mode'Enum_Rep (Video_Mode);

      --  Configure the video packet size
      This.Periph.DSI_VPCR.VPSIZE  := Packet_Size;

      --  Set the chunks number to be transmitted through the DSI link
      This.Periph.DSI_VCCR.NUMC    := Number_Of_Chunks;

      --  Set the size of the null packet
      This.Periph.DSI_VNPCR.NPSIZE := Null_Packet_Size;

      --  Select the virtual channel for the LTDC interface traffic
      This.Periph.DSI_LVCIDR.VCID  := Virtual_Channel;

      --  Configure the polarity of control signals
      This.Periph.DSI_LPCR.HSP   := To_Bool (HSync_Polarity);
      This.Periph.DSI_LPCR.VSP   := To_Bool (VSync_Polarity);
      This.Periph.DSI_LPCR.DEP   := To_Bool (DataEn_Polarity);

      --  Select the color coding for the host
      This.Periph.DSI_LCOLCR.COLC := DSI_Color_Mode'Enum_Rep (Color_Coding);
      --  ... and for the wrapper
      This.Periph.DSI_WCFGR.COLMUX := DSI_Color_Mode'Enum_Rep (Color_Coding);

      --  Enable/disable the loosely packed variant to 18-bit configuration
      if Color_Coding = RGB666 then
         This.Periph.DSI_LCOLCR.LPE := Loosely_Packed;
      end if;

      --  Set the Horizontal Synchronization Active (HSA) in lane byte clock
      --  cycles
      This.Periph.DSI_VHSACR.HSA := HSync_Active_Duration;
      --  Set the Horizontal Back Porch
      This.Periph.DSI_VHBPCR.HBP := Horizontal_BackPorch;
      --  Total line time (HSA+HBP+HACT+HFP
      This.Periph.DSI_VLCR.HLINE := Horizontal_Line;

      --  Set the Vertical Synchronization Active
      This.Periph.DSI_VVSACR.VSA := VSync_Active_Duration;
      --  VBP
      This.Periph.DSI_VVBPCR.VBP := Vertical_BackPorch;
      --  VFP
      This.Periph.DSI_VVFPCR.VFP := Vertical_FrontPorch;
      --  Vertical Active Period
      This.Periph.DSI_VVACR.VA   := Vertical_Active;

      --  Configure the command transmission mode
      This.Periph.DSI_VMCR.LPCE  := LP_Command_Enabled;

      --  Low power configuration:
      This.Periph.DSI_LPMCR.LPSIZE  := LP_Largest_Packet_Size;
      This.Periph.DSI_LPMCR.VLPSIZE := LP_VACT_Largest_Packet_Size;
      This.Periph.DSI_VMCR.LPHFE    := LP_H_Front_Porch_Enable;
      This.Periph.DSI_VMCR.LPHBPE   := LP_H_Back_Porch_Enable;
      This.Periph.DSI_VMCR.LVAE     := LP_V_Active_Enable;
      This.Periph.DSI_VMCR.LPVFPE   := LP_V_Front_Porch_Enable;
      This.Periph.DSI_VMCR.LPVBPE   := LP_V_Back_Porch_Enable;
      This.Periph.DSI_VMCR.LPVSAE   := LP_V_Sync_Active_Enable;

      This.Periph.DSI_VMCR.FBTAAE := Frame_BTA_Ack_Enable;
   end DSI_Setup_Video_Mode;

   ------------------------------------
   -- DSI_Setup_Adapted_Command_Mode --
   ------------------------------------

   procedure DSI_Setup_Adapted_Command_Mode
     (This                        : in out DSI_Host;
      Virtual_Channel             : DSI_Virtual_Channel_ID;
      Color_Coding                : DSI_Color_Mode;
      Command_Size                : Short;
      Tearing_Effect_Source       : DSI_Tearing_Effect_Source;
      Tearing_Effect_Polarity     : DSI_TE_Polarity;
      HSync_Polarity              : DSI_Polarity;
      VSync_Polarity              : DSI_Polarity;
      DataEn_Polarity             : DSI_Polarity;
      VSync_Edge                  : DSI_Edge;
      Automatic_Refresh           : Boolean;
      TE_Acknowledge_Request      : Boolean)
   is
      function To_Bool is new Ada.Unchecked_Conversion
        (DSI_Polarity, Boolean);
      function To_Bool is new Ada.Unchecked_Conversion
        (DSI_TE_Polarity, Boolean);
      function To_Bool is new Ada.Unchecked_Conversion
        (DSI_Edge, Boolean);
   begin
      --  Select the command mode by setting CMDM and DSIM bits
      This.Periph.DSI_MCR.CMDM := True;
      This.Periph.DSI_WCFGR.DSIM := False;
      This.Periph.DSI_WCFGR.DSIM := True;

      --  Select the virtual channel for the LTDC interface traffic
      This.Periph.DSI_LVCIDR.VCID := Virtual_Channel;

      --  Configure the polarity of control signals
      This.Periph.DSI_LPCR.HSP   := To_Bool (HSync_Polarity);
      This.Periph.DSI_LPCR.VSP   := To_Bool (VSync_Polarity);
      This.Periph.DSI_LPCR.DEP   := To_Bool (DataEn_Polarity);

      --  Select the color coding for the host
      This.Periph.DSI_LCOLCR.COLC := DSI_Color_Mode'Enum_Rep (Color_Coding);
      --  ... and for the wrapper
      This.Periph.DSI_WCFGR.COLMUX :=
        DSI_Color_Mode'Enum_Rep (Color_Coding);

      --  Configure the maximum allowed size for write memory command
      This.Periph.DSI_LCCR.CMDSIZE := Command_Size;

      --  Configure the tearing effect source and polarity
      This.Periph.DSI_WCFGR.TESRC := Tearing_Effect_Source = TE_External;
      This.Periph.DSI_WCFGR.TEPOL := To_Bool (Tearing_Effect_Polarity);
      This.Periph.DSI_WCFGR.AR    := Automatic_Refresh;
      This.Periph.DSI_WCFGR.VSPOL := To_Bool (VSync_Edge);

      --  Tearing effect acknowledge request
      This.Periph.DSI_CMCR.TEARE := TE_Acknowledge_Request;

      --  Enable the TE interrupt
      This.Periph.DSI_WIER.TEIE := True;

      --  Enable the End-of-refresh interrupt
      This.Periph.DSI_WIER.ERIE := True;
   end DSI_Setup_Adapted_Command_Mode;

   -----------------------
   -- DSI_Setup_Command --
   -----------------------

   procedure DSI_Setup_Command
     (This                     : in out DSI_Host;
      LP_Gen_Short_Write_No_P  : Boolean := True;
      LP_Gen_Short_Write_One_P : Boolean := True;
      LP_Gen_Short_Write_Two_P : Boolean := True;
      LP_Gen_Short_Read_No_P   : Boolean := True;
      LP_Gen_Short_Read_One_P  : Boolean := True;
      LP_Gen_Short_Read_Two_P  : Boolean := True;
      LP_Gen_Long_Write        : Boolean := True;
      LP_DCS_Short_Write_No_P  : Boolean := True;
      LP_DCS_Short_Write_One_P : Boolean := True;
      LP_DCS_Short_Read_No_P   : Boolean := True;
      LP_DCS_Long_Write        : Boolean := True;
      LP_Max_Read_Packet       : Boolean := False;
      Acknowledge_Request      : Boolean := False)
   is
   begin
      This.Periph.DSI_CMCR.GSW0TX := LP_Gen_Short_Write_No_P;
      This.Periph.DSI_CMCR.GSW1TX := LP_Gen_Short_Write_One_P;
      This.Periph.DSI_CMCR.GSW2TX := LP_Gen_Short_Write_Two_P;
      This.Periph.DSI_CMCR.GSR0TX := LP_Gen_Short_Read_No_P;
      This.Periph.DSI_CMCR.GSR1TX := LP_Gen_Short_Read_One_P;
      This.Periph.DSI_CMCR.GSR2TX := LP_Gen_Short_Read_Two_P;
      This.Periph.DSI_CMCR.GLWTX := LP_Gen_Long_Write;
      This.Periph.DSI_CMCR.DSW0TX := LP_DCS_Short_Write_No_P;
      This.Periph.DSI_CMCR.DSW1TX := LP_DCS_Short_Write_One_P;
      This.Periph.DSI_CMCR.DSR0TX := LP_DCS_Short_Read_No_P;
      This.Periph.DSI_CMCR.DLWTX := LP_DCS_Long_Write;
      This.Periph.DSI_CMCR.MRDPS := LP_Max_Read_Packet;
      This.Periph.DSI_CMCR.ARE := Acknowledge_Request;
   end DSI_Setup_Command;

   ----------------------------
   -- DSI_Setup_Flow_Control --
   ----------------------------

   procedure DSI_Setup_Flow_Control
     (This        : in out DSI_Host;
      Flow_Control : DSI_Flow_Control)
   is
   begin
      This.Periph.DSI_PCR := (others =>  <>);
      case Flow_Control is
         when Flow_Control_CRC_RX =>
            This.Periph.DSI_PCR.CRCRXE := True;
         when Flow_Control_ECC_RX =>
            This.Periph.DSI_PCR.ECCRXE := True;
         when Flow_Control_BTA =>
            This.Periph.DSI_PCR.BTAE := True;
         when Flow_Control_EOTP_RX =>
            This.Periph.DSI_PCR.ETRXE := True;
         when Flow_Control_EOTP_TX =>
            This.Periph.DSI_PCR.ETTXE := True;
      end case;
   end DSI_Setup_Flow_Control;

   ---------------
   -- DSI_Start --
   ---------------

   procedure DSI_Start (This : in out DSI_Host) is
   begin
      --  Enable the DSI Host
      This.Periph.DSI_CR.EN := True;
      --  Enable the DSI wrapper
      This.DSI_Wrapper_Enable;
   end DSI_Start;

   --------------
   -- DSI_Stop --
   --------------

   procedure DSI_Stop (This : in out DSI_Host) is
   begin
      --  Disable the DSI Host
      This.Periph.DSI_CR.EN := False;
      --  Disable the DSI wrapper
      This.Periph.DSI_WCR.DSIEN := False;
   end DSI_Stop;

   ------------------------
   -- DSI_Wrapper_Enable --
   ------------------------

   procedure DSI_Wrapper_Enable (This : in out DSI_Host) is
   begin
      This.Periph.DSI_WCR.DSIEN := True;
   end DSI_Wrapper_Enable;

   -------------------------
   -- DSI_Wrapper_Disable --
   -------------------------

   procedure DSI_Wrapper_Disable (This : in out DSI_Host) is
   begin
      This.Periph.DSI_WCR.DSIEN := False;
   end DSI_Wrapper_Disable;

   -----------------
   -- DSI_Refresh --
   -----------------

   procedure DSI_Refresh (This : in out DSI_Host) is
   begin
      This.Periph.DSI_WCR.LTDCEN := True;
   end DSI_Refresh;

   ---------------------
   -- DSI_Short_Write --
   ---------------------

   overriding
   procedure DSI_Short_Write
     (This       : in out DSI_Host;
      Channel_ID : DSI_Virtual_Channel_ID;
      Mode       : DSI_Short_Write_Packet_Data_Type;
      Param1     : Byte;
      Param2     : Byte)
   is
      Start : Time;
   begin
      --  Wait for FIFO empty
      Start := Clock;
      while not This.Periph.DSI_GPSR.CMDFE loop
         if Clock > Start + Milliseconds (1000) then
            raise Program_Error with
              "Timeout while waiting for DSI FIFO empty";
         end if;
      end loop;

      --  Configure the packet to send a short DCS command with 0 or 1
      --  parameter
      This.DSI_Config_Packet_Header (Channel_ID, Mode, Param1, Param2);
   end DSI_Short_Write;

   --------------------
   -- DSI_Long_Write --
   --------------------

   overriding
   procedure DSI_Long_Write
     (This       : in out DSI_Host;
      Channel_Id : DSI_Virtual_Channel_ID;
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
      while not This.Periph.DSI_GPSR.CMDFE loop
         if Clock > Start + Milliseconds (1000) then
            raise Program_Error with
              "Timeout while waiting for DSI FIFO empty";
         end if;
      end loop;

      Value.Arr (Value.Arr'First) := Param1;
      Off   := Value.Arr'First + 1;

      for Param of Parameters loop
         Value.Arr (Off) := Param;
         Off := Off + 1;
         if Off > Value.Arr'Last then
            This.Periph.DSI_GPDR := Value;
            Value.Val := 0;
            Off       := Value.Arr'First;
         end if;
      end loop;

      if Off /= Value.Arr'First then
         This.Periph.DSI_GPDR := Value;
      end if;

      Val1 := Word (Parameters'Length + 1) and 16#FF#;
      Val2 := Shift_Right (Word (Parameters'Length + 1) and 16#FF00#, 8);
      This.DSI_Config_Packet_Header
        (Channel_Id, Mode, Byte (Val1), Byte (Val2));
   end DSI_Long_Write;

end STM32.DSI;
