with SAM_SVD.SPI; use SAM_SVD.SPI;

with SAM.PMC;

with System;

package body SAM.SPI is

   procedure Configure (This : in out SPI_Port;
                        Cfg  : Configuration)
   is
   begin
      Enable_Clock (This => This);
      Unlock_Write_Protection (This => This);
      Disable (This => This);
      Reset (This => This);

      Set_Mode (This => This,
                Md   => Cfg.Md);

      Set_Variable_Peripheral_Select_Mode (This   => This,
                                           Enable => Cfg.Variable_Peripheral);
      Set_Peripheral_Select_Decode (This   => This,
                                    Enable => Cfg.Select_Decode);
      if not Cfg.Variable_Peripheral then
         if not Cfg.Select_Decode then
            Set_Peripheral_Chip_Select_Value (This  => This,
                                              Value =>
                                                Get_PCS (NPCS => This.CS));
         else
            Set_Peripheral_Chip_Select_Value (This  => This,
                                              Value => UInt4 (This.CS));
         end if;
      end if;
      Set_Fault_Detect_Mode (This   => This,
                             Enable => Cfg.Fault_Detect);
      Set_Tx_On_Rx_Empty (This   => This,
                          Enable => Cfg.Tx_On_Rx_Empty);
      Set_Delay_Between_Chip_Select (This  => This,
                                     Value => Cfg.Dly_Btw_Cs);
      Set_Clock_Polarity (This => This,
                          Pol  => Cfg.Clk_Pol);
      Set_Clock_Phase (This  => This,
                       Phase => Cfg.Clk_Phase);
      Configure_Cs_Behavior (This     => This,
                             Behavior => Cfg.Cs_Beh);
      Set_Bits_Per_Transfer (This => This,
                             Size => Cfg.BPT);
      Set_Transfer_Delay (This                    => This,
                          Delay_Before_SPCK       => Cfg.Dly_SPCK,
                          Delay_Between_Transfers => Cfg.Dly_Btw_Transfers);
      Set_Baudrate (This    => This,
                    Bitrate => Cfg.Baud);
      Enable (This => This);

   end Configure;

   overriding
   function Data_Size (This : SPI_Port) return HAL.SPI.SPI_Data_Size
   is
      Size : constant SPI_CSR_BITS_Field :=
               This.Periph.SPI_CSR (Integer (This.CS)).BITS;
   begin
      case Size is
         when SAM_SVD.SPI.Val_8_Bit =>
            return HAL.SPI.Data_Size_8b;
         when SAM_SVD.SPI.Val_16_Bit =>
            return HAL.SPI.Data_Size_16b;
         when others =>
            null;
      end case;

      --  EXECUTION FLOW CAN'T REACH HERE
      raise Program_Error with "Unsupported Data Size";
      return HAL.SPI.Data_Size_8b;
   end Data_Size;

   function Poll_For_TX_Ready (This    : SPI_Port;
                               Timeout : Natural)
                               return Boolean
   is
   begin
      for I in 1 .. Timeout loop
         if Is_Tx_Ready (This => This) then
            return True;
         end if;
      end loop;

      --  if execution gets here there was a timeout
      return False;
   end Poll_For_TX_Ready;

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_8b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := SPI_Timeout)
   is
      PCS : UInt4;
   begin
      if not This.Periph.SPI_MR.PCSDEC then
         PCS := Get_PCS (NPCS => This.CS);
      else
         PCS := UInt4 (This.CS);
      end if;

      for I in Data'Range loop
         if not Poll_For_TX_Ready (This    => This,
                                   Timeout => Timeout)
         then
            Status := HAL.SPI.Err_Timeout;
            return;
         end if;

         This.Periph.SPI_TDR := (TD => SPI_SPI_TDR_TD_Field (Data (I)),
                                 PCS => PCS,
                                 others => <>);
      end loop;
      This.Periph.SPI_CR.LASTXFER := True;
      Status := HAL.SPI.Ok;
   end Transmit;

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := SPI_Timeout)
   is
      PCS : UInt4;
   begin
      if not This.Periph.SPI_MR.PCSDEC then
         PCS := Get_PCS (NPCS => This.CS);
      else
         PCS := UInt4 (This.CS);
      end if;

      for I in Data'Range loop
         if not Poll_For_TX_Ready (This    => This,
                                   Timeout => Timeout)
         then
            Status := HAL.SPI.Err_Timeout;
            return;
         end if;

         This.Periph.SPI_TDR := (TD => SPI_SPI_TDR_TD_Field (Data (I)),
                                 PCS => PCS,
                                 others => <>);
      end loop;
      This.Periph.SPI_CR.LASTXFER := True;
      Status := HAL.SPI.Ok;
   end Transmit;

   function Poll_For_RX_Ready (This    : SPI_Port;
                               Timeout : Natural)
                               return Boolean
   is
   begin
      for I in 1 .. Timeout loop
         if Is_Rx_Full (This => This) then
            return True;
         end if;
      end loop;

      --  if execution gets here there was a timeout
      return False;
   end Poll_For_RX_Ready;

   overriding
   procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := SPI_Timeout)
   is
   begin
      for I in Data'Range loop
         if not Poll_For_RX_Ready (This    => This,
                                   Timeout => Timeout)
         then
            Status := HAL.SPI.Err_Timeout;
            return;
         end if;
         Data (I) := HAL.UInt8 (This.Periph.SPI_RDR.RD);
      end loop;

      Status := HAL.SPI.Ok;
   end Receive;

   overriding
   procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := SPI_Timeout)
   is
   begin
      for I in Data'Range loop
         if not Poll_For_RX_Ready (This    => This,
                                   Timeout => Timeout)
         then
            Status := HAL.SPI.Err_Timeout;
            return;
         end if;
         Data (I) := HAL.UInt16 (This.Periph.SPI_RDR.RD);
      end loop;

      Status := HAL.SPI.Ok;
   end Receive;

   overriding
   procedure Transfer
     (This    : in out SPI_Port;
      Tx_Data    : HAL.SPI.SPI_Data_8b;
      Rx_Data    : out HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout    : Natural := SPI_Timeout)
   is
      PCS : UInt4;
   begin
      if not This.Periph.SPI_MR.PCSDEC then
         PCS := Get_PCS (NPCS => This.CS);
      else
         PCS := UInt4 (This.CS);
      end if;

      --  Clear any pending RX's
      if This.Poll_For_RX_Ready (Timeout => 10) then
         Rx_Data (Rx_Data'First) := HAL.UInt8 (This.Periph.SPI_RDR.RD);
      end if;

      for I in Tx_Data'Range loop
         declare
            Tx : HAL.SPI.SPI_Data_8b (1 .. 1)
              with Address => Tx_Data (I)'Address;
            Rx : HAL.SPI.SPI_Data_8b (1 .. 1)
              with Address => Rx_Data (I)'Address;

            use HAL.SPI;
         begin
--              This.Transmit (Data    => Tx,
--                             Status  => Status,
--                             Timeout => Timeout);
--              if Status /= Ok then
--                 return;
--              end if;

            if not Poll_For_TX_Ready (This    => This,
                                      Timeout => Timeout)
            then
               Status := HAL.SPI.Err_Timeout;
               return;
            end if;

            This.Periph.SPI_TDR := (TD     => SPI_SPI_TDR_TD_Field (Tx (1)),
                                    PCS    => PCS,
                                    others => <>);

            This.Receive (Data    => Rx,
                          Status  => Status,
                          Timeout => Timeout);
            if Status /= Ok then
               return;
            end if;
         end;
      end loop;
      This.Periph.SPI_CR.LASTXFER := True;
   end Transfer;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out SPI_Port)
   is
   begin
      This.Periph.SPI_CR.SPIEN := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out SPI_Port)
   is
   begin
      This.Periph.SPI_CR.SPIDIS := True;
   end Disable;

   function Enabled (This : SPI_Port) return Boolean
   is
   begin
      return (This.Periph.SPI_SR.SPIENS);
   end Enabled;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out SPI_Port)
   is
   begin
      This.Periph.SPI_CR.SWRST := True;
   end Reset;

   procedure Set_Mode (This : in out SPI_Port;
                       Md : Mode)
   is
   begin
      case Md is
         when Master =>
            Set_Last_Xfer (This => This);
            This.Periph.SPI_MR.MSTR := True;
         when Slave =>
            This.Periph.SPI_MR.MSTR := False;
      end case;
   end Set_Mode;

   function Get_Mode (This : SPI_Port) return Mode
   is
      Ret : Mode;
   begin
      if This.Periph.SPI_MR.MSTR then
         Ret := Master;
      else
         Ret := Slave;
      end if;

      return Ret;
   end Get_Mode;

   procedure Set_Variable_Peripheral_Select_Mode (This   : in out SPI_Port;
                                                  Enable : Boolean)
   is
   begin
      This.Periph.SPI_MR.PS := Enable;
   end Set_Variable_Peripheral_Select_Mode;

   function Get_Variable_Peripheral_Select_Mode (This : SPI_Port)
                                                 return Boolean
   is
   begin
      return (This.Periph.SPI_MR.PS);
   end Get_Variable_Peripheral_Select_Mode;

   procedure Set_Peripheral_Select_Decode (This   : in out SPI_Port;
                                           Enable : Boolean)
   is
   begin
      if Enable then
         raise Program_Error with "Select decoding is not currently supported";
--         This.Periph.SPI_MR.PCSDEC := 1;
      else
         This.Periph.SPI_MR.PCSDEC := False;
      end if;
   end Set_Peripheral_Select_Decode;

   function Get_Peripheral_Select_Decode (This : SPI_Port)
                                          return Boolean
   is
   begin
      return (This.Periph.SPI_MR.PCSDEC);
   end Get_Peripheral_Select_Decode;

   procedure Set_Fault_Detect_Mode (This   : in out SPI_Port;
                                    Enable : Boolean)
   is
   begin
      This.Periph.SPI_MR.MODFDIS := Enable;
   end Set_Fault_Detect_Mode;

   function Get_Fault_Detect_Mode (This : SPI_Port) return Boolean
   is
   begin
      return (This.Periph.SPI_MR.MODFDIS);
   end Get_Fault_Detect_Mode;

   procedure Set_Tx_On_Rx_Empty (This   : in out SPI_Port;
                                 Enable : Boolean)
   is
   begin
      This.Periph.SPI_MR.WDRBT := Enable;
   end Set_Tx_On_Rx_Empty;

   function Get_Tx_In_Rx_Empty (This : SPI_Port) return Boolean
   is
   begin
      return (This.Periph.SPI_MR.WDRBT);
   end Get_Tx_In_Rx_Empty;

   procedure Set_Loopback (This   : in out SPI_Port;
                           Enable : Boolean)
   is
   begin
      This.Periph.SPI_MR.LLB := Enable;
   end Set_Loopback;

   procedure Set_Clock_Polarity (This : in out SPI_Port;
                                 Pol  : Clock_Polarity)
   is
   begin
      case Pol is
         when Default_Hi =>
            This.Periph.SPI_CSR (Integer (This.CS)).CPOL := True;
         when Default_Lo =>
            This.Periph.SPI_CSR (Integer (This.CS)).CPOL := False;
      end case;
   end Set_Clock_Polarity;

   procedure Set_Clock_Phase (This  : in out SPI_Port;
                              Phase : Clock_Phase)
   is
   begin
      case Phase is
         when Cap_Rising =>
            This.Periph.SPI_CSR (Integer (This.CS)).NCPHA := True;
         when Cap_Falling =>
            This.Periph.SPI_CSR (Integer (This.CS)).NCPHA := False;
      end case;
   end Set_Clock_Phase;

   procedure Configure_Cs_Behavior (This     : in out SPI_Port;
                                    Behavior : Cs_Behavior)
   is
   begin
      case Behavior is
         when Keep_Low =>
            This.Periph.SPI_CSR (Integer (This.CS)).CSAAT := True;
         when Rise_No_Tx =>
            This.Periph.SPI_CSR (Integer (This.CS)).CSAAT := False;
            This.Periph.SPI_CSR (Integer (This.CS)).CSNAAT := False;
         when Rise_Forced =>
            This.Periph.SPI_CSR (Integer (This.CS)).CSAAT := False;
            This.Periph.SPI_CSR (Integer (This.CS)).CSNAAT := True;
      end case;
   end Configure_Cs_Behavior;

   procedure Set_Bits_Per_Transfer (This : in out SPI_Port;
                                    Size : HAL.SPI.SPI_Data_Size)
   is
   begin
      case Size is
         when HAL.SPI.Data_Size_8b =>
            This.Periph.SPI_CSR (Integer (This.CS)).BITS := Val_8_Bit;
         when HAL.SPI.Data_Size_16b =>
            This.Periph.SPI_CSR (Integer (This.CS)).BITS := Val_16_Bit;
      end case;
   end Set_Bits_Per_Transfer;

   procedure Set_Transfer_Delay (This                    : in out SPI_Port;
                                 Delay_Before_SPCK       : Delay_Type;
                                 Delay_Between_Transfers : Delay_Type)
   is
   begin
      This.Periph.SPI_CSR (Integer (This.CS)).DLYBS :=
        SPI_SPI_CSR_DLYBS_Field (Delay_Before_SPCK);
      This.Periph.SPI_CSR (Integer (This.CS)).DLYBCT :=
        SPI_SPI_CSR_DLYBCT_Field (Delay_Between_Transfers);
   end Set_Transfer_Delay;

   procedure Set_Baudrate (This     : in out SPI_Port;
                           Bitrate  : UInt32)
   is
      MCK : constant UInt32 := SAM.PMC.System_Clocks.MCK;
      SCBR : constant UInt32 := MCK / Bitrate;
   begin
      if SCBR = 0 then
         raise Program_Error;
      end if;

      if SCBR > UInt32 (UInt8'Last) then
         raise Program_Error;
      end if;

      This.Periph.SPI_CSR (Integer (This.CS)).SCBR :=
        UInt8 (SCBR);
   end Set_Baudrate;

   procedure Set_Last_Xfer (This : in out SPI_Port)
   is
   begin
      This.Periph.SPI_CR.LASTXFER := True;
   end Set_Last_Xfer;

   procedure Set_Peripheral_Chip_Select_Value (This  : in out SPI_Port;
                                               Value : UInt4)
   is
   begin
      This.Periph.SPI_MR.PCS := Value;
   end Set_Peripheral_Chip_Select_Value;

   procedure Set_Delay_Between_Chip_Select (This  : in out SPI_Port;
                                            Value : Delay_Type)
   is
   begin
      This.Periph.SPI_MR.DLYBCS := SPI_SPI_MR_DLYBCS_Field (Value);
   end Set_Delay_Between_Chip_Select;

   function Calculate_Baud_Divider (Baud : Natural;
                                    Mck  : Natural)
                                    return Divider_Type
   is
      Ret_Pre : Natural;
   begin
      Ret_Pre := Div_Ceil (Lhs => Mck,
                           Rhs => Baud);
      if Ret_Pre > Natural (Divider_Type'Last) then
         return Divider_Type'Last;
      end if;

      return Divider_Type (Ret_Pre);
   end Calculate_Baud_Divider;

   function Div_Ceil (Lhs, Rhs : Natural) return Natural
   is
   begin
      return (1 + ((Lhs - 1) / Rhs));
   end Div_Ceil;

   procedure Enable_Clock (This : SPI_Port)
   is
      use System;
   begin
      if This.Periph.all'Address = SPI0_Base then
         SAM.PMC.Enable_Peripheral_Clock (ID => SPI0_ID);
      elsif This.Periph.all'Address = SPI1_Base then
         SAM.PMC.Enable_Peripheral_Clock (ID => SPI1_ID);
      else
         raise Unknown_Port;
      end if;
   end Enable_Clock;


   procedure Disable_Clock (This : SPI_Port)
   is
      use System;
   begin
      if This.Periph.all'Address = SPI0_Base then
         SAM.PMC.Disable_Peripheral_Clock (ID => SPI0_ID);
      elsif This.Periph.all'Address = SPI1_Base then
         SAM.PMC.Disable_Peripheral_Clock (ID => SPI1_ID);
      else
         raise Unknown_Port;
      end if;
   end Disable_Clock;

   procedure Unlock_Write_Protection (This : in out SPI_Port)
   is
   begin
      This.Periph.SPI_WPMR := (WPEN => False,
                               WPITEN => False,
                               WPCREN => False,
                               WPKEY  => Passwd,
                               others => <>);
   end Unlock_Write_Protection;

   procedure Lock_Write_Protection (This : in out SPI_Port)
   is
   begin
      This.Periph.SPI_WPMR := (WPEN   => True,
                               WPITEN => True,
                               WPCREN => True,
                               WPKEY  => Passwd,
                               others => <>);
   end Lock_Write_Protection;

end SAM.SPI;
