------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

package body NRF24L01P is

   procedure Set_Power
     (RF_Setup : in out RF_SETUP_Register;
      Power    : Amplifier_Power)
     with Inline;

   procedure Set_Rate
     (RF_Setup : in out RF_SETUP_Register;
      Rate     : Air_Data_Rate)
     with Inline;

   procedure Set_En_Rxaddr
     (Reg  : in out EN_RXADDR_Register;
      Pipe : RX_Pipe;
      Data : Bit)
     with Inline;

   function To_RX_ADDR (Pipe : RX_Pipe) return Register_Address
     with Inline;

   function Pipe_To_RX_PW (Pipe : RX_Pipe) return Register_Name
     with Inline;

   function Boolean_To_Bit (Value : Boolean) return Bit
     with Inline;

   function Payload_To_Bit (Value : Payload_Length_Type) return Bit
     with Inline;

   --------------------
   -- Boolean_To_Bit --
   --------------------

   function Boolean_To_Bit (Value : Boolean) return Bit is
   begin
      return (if Value then 1 else 0);
   end Boolean_To_Bit;

   -------------
   -- CE_High --
   -------------

   procedure CE_High (This : NRF24L01P_Driver) is
   begin
      This.Holder.CE_Pin.Set;
   end CE_High;

   ------------
   -- CE_Low --
   ------------

   procedure CE_Low (This : NRF24L01P_Driver) is
   begin
      This.Holder.CE_Pin.Clear;
   end CE_Low;

   --------------
   -- CSN_High --
   --------------

   procedure CSN_High (This : NRF24L01P_Driver) is
   begin
      if This.Holder.Kind = Software then
         This.Holder.CSN_Pin.Set;
      end if;
   end CSN_High;

   -------------
   -- CSN_Low --
   -------------

   procedure CSN_Low (This : NRF24L01P_Driver) is
   begin
      if This.Holder.Kind = Software then
         This.Holder.CSN_Pin.Clear;
      end if;
   end CSN_Low;

   -----------------------
   -- Can_Be_Configured --
   -----------------------

   function Can_Be_Configured (This : NRF24L01P_Driver) return Boolean is
   begin
      return not Is_Power_On (This) or else not Is_Wating_RX (This);
   end Can_Be_Configured;

   ---------------------
   -- Clear_RX_Buffer --
   ---------------------

   procedure Clear_RX_Buffer (This : in out NRF24L01P_Driver) is
   begin
      Write_Command (This, Cmd_FLUSH_RX);
   end Clear_RX_Buffer;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This               : in out NRF24L01P_Driver;
      RX_Resived         : Boolean := True;
      TX_Sent            : Boolean := True;
      Max_TX_Retransmits : Boolean := True)
   is
      Data     : Register := Read_Register (This, STATUS);
      STATUS_R : STATUS_Register
        with Import, Address => Data'Address;
   begin
      STATUS_R.RX_DR_RW  := Boolean_To_Bit (RX_Resived);
      STATUS_R.TX_DS_RW  := Boolean_To_Bit (TX_Sent);
      STATUS_R.MAX_RT_RW := Boolean_To_Bit (Max_TX_Retransmits);

      Write_Register (This, STATUS, Data);
   end Clear_Status;

   ---------------------
   -- Clear_TX_Buffer --
   ---------------------

   procedure Clear_TX_Buffer (This : in out NRF24L01P_Driver) is
   begin
      Write_Command (This, Cmd_FLUSH_TX);
   end Clear_TX_Buffer;

   -----------------------
   -- Config_Power_Rate --
   -----------------------

   procedure Config_Power_Rate
     (This  : in out NRF24L01P_Driver;
      Power : Amplifier_Power;
      Rate  : Air_Data_Rate)
   is
      Data       : Register := Read_Register (This, RF_SETUP);
      RF_Setup_R : RF_SETUP_Register
        with Import, Address => Data'Address;
   begin
      Set_Power (RF_Setup_R, Power);
      Set_Rate (RF_Setup_R, Rate);
      Write_Register (This, RF_SETUP, Data);
   end Config_Power_Rate;

   ----------------------------------
   -- Configure_And_Enable_RX_Pipe --
   ----------------------------------

   procedure Configure_And_Enable_RX_Pipe
     (This    : in out NRF24L01P_Driver;
      Pipe    : RX_Pipe;
      Addr    : Pipe_Address;
      ACK     : Boolean;
      Payload : Pipe_Payload)
   is
      procedure Set_Address;
      procedure Set_ACK;
      procedure Set_Payload;

      -- Set_ACK --

      procedure Set_ACK
      is
         Data    : Register := Read_Register (This, EN_AA);
         EN_AA_R : EN_AA_Register
           with Import, Address => Data'Address;
      begin
         case Pipe is
            when 0 => EN_AA_R.ENAA_P0_RW := Boolean_To_Bit (ACK);
            when 1 => EN_AA_R.ENAA_P1_RW := Boolean_To_Bit (ACK);
            when 2 => EN_AA_R.ENAA_P2_RW := Boolean_To_Bit (ACK);
            when 3 => EN_AA_R.ENAA_P3_RW := Boolean_To_Bit (ACK);
            when 4 => EN_AA_R.ENAA_P4_RW := Boolean_To_Bit (ACK);
            when 5 => EN_AA_R.ENAA_P5_RW := Boolean_To_Bit (ACK);
         end case;

         Write_Register (This, EN_AA, Data);
      end Set_ACK;

      -- Set_Address --

      procedure Set_Address
      is
         Size : Pipe_Address_Size renames This.Holder.Addr_Size;
      begin
         case Pipe is
            when 0 .. 1 =>
               declare
                  Data  : HAL.SPI.SPI_Data_8b (1 .. 1 + Positive (Size));
                  CMD   : Command_W_REGISTER :=
                    (Address => To_RX_ADDR (Pipe),
                     Command => <>)
                    with Address => Data (Data'First)'Address;
                  Local : Pipe_Address (1 .. Pipe_Address_Index (Size)) :=
                    Addr (1 .. Pipe_Address_Index (Size))
                    with Address => Data (Data'First + 1)'Address;
               begin
                  Write (This, Data);
               end;

            when 2 .. 5 =>
               declare
                  Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
                  CMD   : Command_W_REGISTER :=
                    (Address => To_RX_ADDR (Pipe),
                     Command => <>)
                    with Address => Data (Data'First)'Address;
                  Local : Pipe_Address (1 .. 1) := Addr (1 .. 1)
                    with Address => Data (Data'First + 1)'Address;
               begin
                  Write (This, Data);
               end;
         end case;
      end Set_Address;

      -- Set_Payload --

      procedure Set_Payload
      is
         Data    : Register := Read_Register (This, DYNPD);
         DYNPD_R : DYNPD_Register
           with Import, Address => Data'Address;
      begin
         --  Set Static/Dynamic payload
         case Pipe is
            when 0 => DYNPD_R.DPL_P0_RW := Payload_To_Bit (Payload.Payload);
            when 1 => DYNPD_R.DPL_P1_RW := Payload_To_Bit (Payload.Payload);
            when 2 => DYNPD_R.DPL_P2_RW := Payload_To_Bit (Payload.Payload);
            when 3 => DYNPD_R.DPL_P3_RW := Payload_To_Bit (Payload.Payload);
            when 4 => DYNPD_R.DPL_P4_RW := Payload_To_Bit (Payload.Payload);
            when 5 => DYNPD_R.DPL_P5_RW := Payload_To_Bit (Payload.Payload);
         end case;

         Write_Register (This, DYNPD, Data);

         if Payload.Payload = Dynamic_Payload then
            This.Holder.Payloads (Pipe) := (Payload => Dynamic_Payload);
            return;
         end if;

         --  Set static lenght
         declare
            Name       : constant Register_Name := Pipe_To_RX_PW (Pipe);
            Data       : Register := Read_Register (This, Name);
            RX_PW_PX_R : RX_PW_PX_Register
              with Import, Address => Data'Address;
         begin
            RX_PW_PX_R.RX_PW_PX_RW := UInt6 (Payload.Size);
            Write_Register (This, Name, Data);

            This.Holder.Payloads (Pipe) :=
              (Static_Payload, Payload.Size);
         end;
      end Set_Payload;

   begin
      Set_Address;
      Set_ACK;
      Set_Payload;
      Enable_RX_Pipe (This, Pipe);
   end Configure_And_Enable_RX_Pipe;

   --------------------
   -- Configure_Chip --
   --------------------

   procedure Configure_Chip
     (This   : in out NRF24L01P_Driver;
      Config : Configuration) is
   begin
      CE_Low (This);
      CSN_High (This);

      Set_Mode (This, Config.Mode);
      Set_RF_Channel_Frequency (This, Config.Chanel);
      Config_Power_Rate (This, Config.Power, Config.Rate);
      Set_CRC (This, Config.CRC);

      Configure_IRQ
        (This,
         Config.RX_IRQ,
         Config.TX_IRQ,
         Config.MAX_TR_IRQ);

      Clear_RX_Buffer (This);
      Clear_TX_Buffer (This);
      Clear_Status (This);
   end Configure_Chip;

   -------------------
   -- Configure_IRQ --
   -------------------

   procedure Configure_IRQ
     (This       : in out NRF24L01P_Driver;
      RX_IRQ     : Boolean;
      TX_IRQ     : Boolean;
      MAX_TR_IRQ : Boolean)
   is
      Data     : Register := Read_Register (This, CONFIG);
      CONFIG_R : CONFIG_Register
        with Import, Address => Data'Address;
   begin
      CONFIG_R.MASK_RX_DR_RW  := (if RX_IRQ then 0 else 1);
      CONFIG_R.MASK_TX_DS_RW  := (if TX_IRQ then 0 else 1);
      CONFIG_R.MASK_MAX_RT_RW := (if MAX_TR_IRQ then 0 else 1);
      Write_Register (This, CONFIG, Data);
   end Configure_IRQ;

   ---------------------
   -- Disable_RX_Pipe --
   ---------------------

   procedure Disable_RX_Pipe
     (This : in out NRF24L01P_Driver;
      Pipe : RX_Pipe)
   is
      Data        : Register := Read_Register (This, EN_RXADDR);
      En_Rxaddr_R : EN_RXADDR_Register
        with Import, Address => Data'Address;
   begin
      Set_En_Rxaddr (En_Rxaddr_R, Pipe, 0);
      Write_Register (This, EN_RXADDR, Data);
   end Disable_RX_Pipe;

   --------------------
   -- Enable_RX_Pipe --
   --------------------

   procedure Enable_RX_Pipe
     (This : in out NRF24L01P_Driver;
      Pipe : RX_Pipe)
   is
      Data        : Register := Read_Register (This, EN_RXADDR);
      En_Rxaddr_R : EN_RXADDR_Register
        with Import, Address => Data'Address;
   begin
      Set_En_Rxaddr (En_Rxaddr_R, Pipe, 1);
      Write_Register (This, EN_RXADDR, Data);
   end Enable_RX_Pipe;

   ----------------------------
   -- Get_First_RX_Data_Size --
   ----------------------------

   function Get_First_RX_Data_Size
     (This : in out NRF24L01P_Driver)
      return Payload_Length
   is
      Data : HAL.SPI.SPI_Data_8b (1 .. 1);
   begin
      Write_Command_And_Read (This, Cmd_R_RX_PL_WID, Data);
      return Payload_Length (Data (Data'First));
   end Get_First_RX_Data_Size;


   ----------------------
   -- Get_RX_Data_Size --
   ----------------------

   function Get_RX_Data_Size
     (This : in out NRF24L01P_Driver;
      Pipe : RX_Pipe)
      return Payload_Length
   is
      Data       : Register := Read_Register (This, Pipe_To_RX_PW (Pipe));
      RX_PW_PX_R : RX_PW_PX_Register
        with Import, Address => Data'Address;
   begin
      return Payload_Length (RX_PW_PX_R.RX_PW_PX_RW);
   end Get_RX_Data_Size;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (This               : in out NRF24L01P_Driver;
      TX_Full            : out Boolean;
      Max_TX_Retransmits : out Boolean;
      TX_Sent            : out Boolean;
      RX_Resived         : out Boolean;
      Pipe               : out RX_Pipe)
   is
      Data     : Register := Read_Register (This, STATUS);
      STATUS_R : STATUS_Register
        with Import, Address => Data'Address;
   begin
      TX_Full            := STATUS_R.TX_FULL_RO = 1;
      Max_TX_Retransmits := STATUS_R.MAX_RT_RW  = 1;
      TX_Sent            := STATUS_R.TX_DS_RW   = 1;
      RX_Resived         := STATUS_R.RX_DR_RW   = 1;

      Pipe := (if STATUS_R.RX_P_NO_RO <= UInt3 (RX_Pipe'Last)
               then RX_Pipe (STATUS_R.RX_P_NO_RO)
               else 0);
   end Get_Status;

   ----------------------
   -- Have_ACK_Payload --
   ----------------------

   function Have_ACK_Payload (This : in out NRF24L01P_Driver) return Boolean is
   begin
      return Is_Received (This);
   end Have_ACK_Payload;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This    : out NRF24L01P_Driver;
      CE_Pin  : HAL.GPIO.Any_GPIO_Point;
      CSN_Pin : HAL.GPIO.Any_GPIO_Point;
      SPI     : HAL.SPI.Any_SPI_Port;
      Config  : Configuration) is
   begin
      This.Holder := Holder_Type'
        (Kind      => Software,
         SPI       => SPI,
         CE_Pin    => CE_Pin,
         CSN_Pin   => CSN_Pin,
         Power_On  => <>,
         Mode      => <>,
         NO_ACK    => <>,
         Addr_Size => <>,
         Payloads  => <>);

      Configure_Chip (This, Config);
   end Initialize;

   --------------------
   -- Is_ACK_Allowed --
   --------------------

   function Is_ACK_Allowed
     (ACK             : Boolean;
      Dynamic_Payload : Boolean)
      return Boolean is
   begin
      return (if ACK then Dynamic_Payload else True);
   end Is_ACK_Allowed;

   -------------------
   -- Is_In_RX_Mode --
   -------------------

   function Is_In_RX_Mode (This : NRF24L01P_Driver) return Boolean is
   begin
      return This.Holder.Mode = PRX;
   end Is_In_RX_Mode;

   -----------------------
   -- Is_No_ACK_Allowed --
   -----------------------

   function Is_No_ACK_Allowed
     (This   : NRF24L01P_Driver;
      No_ACK : Boolean) return Boolean is
   begin
      return (if No_ACK then This.Holder.NO_ACK else True);
   end Is_No_ACK_Allowed;

   -----------------
   -- Is_Power_On --
   -----------------

   function Is_Power_On (This : NRF24L01P_Driver) return Boolean is
   begin
      return This.Holder.Power_On;
   end Is_Power_On;

   -----------------
   -- Is_Received --
   -----------------

   function Is_Received (This : in out NRF24L01P_Driver) return Boolean
   is
      TX_Full    : Boolean;
      Max_TX     : Boolean;
      TX_Sent    : Boolean;
      RX_Resived : Boolean;
      Pipe       : RX_Pipe;
   begin
      Get_Status (This, TX_Full, Max_TX, TX_Sent, RX_Resived, Pipe);
      return RX_Resived;
   end Is_Received;

   --------------------
   -- Is_Transmitted --
   --------------------

   function Is_Transmitted (This : in out NRF24L01P_Driver) return Boolean
   is
      TX_Full    : Boolean;
      Max_TX     : Boolean;
      TX_Sent    : Boolean;
      RX_Resived : Boolean;
      Pipe       : RX_Pipe;
   begin
      Get_Status (This, TX_Full, Max_TX, TX_Sent, RX_Resived, Pipe);
      return TX_Sent;
   end Is_Transmitted;

   ---------------------------
   -- Is_Valid_Address_Size --
   ---------------------------

   function Is_Valid_Address_Size
     (This    : NRF24L01P_Driver;
      Pipe    : RX_Pipe;
      Address : Pipe_Address)
      return Boolean is
   begin
      case Pipe is
         when 0 .. 1 =>
            return Address'Length = This.Holder.Addr_Size;

         when 2 .. 5 =>
            return Address'Length = 1;
      end case;
   end Is_Valid_Address_Size;

   ------------------------------
   -- Is_Valid_TX_Address_Size --
   ------------------------------

   function Is_Valid_TX_Address_Size
     (This    : NRF24L01P_Driver;
      Address : Pipe_Address)
      return Boolean is
   begin
      return Address'Length = This.Holder.Addr_Size;
   end Is_Valid_TX_Address_Size;

   ------------------
   -- Is_Wating_RX --
   ------------------

   function Is_Wating_RX (This : NRF24L01P_Driver) return Boolean is
   begin
      return Is_In_RX_Mode (This) and then This.Holder.CE_Pin.Set;
   end Is_Wating_RX;

   --------------------
   -- Payload_To_Bit --
   --------------------

   function Payload_To_Bit (Value : Payload_Length_Type) return Bit is
   begin
      return (if Value = Static_Payload then 0 else 1);
   end Payload_To_Bit;

   -------------------
   -- Pipe_To_RX_PW --
   -------------------

   function Pipe_To_RX_PW (Pipe : RX_Pipe) return Register_Name is
   begin
      case Pipe is
         when 0 => return RX_PW_P0;
         when 1 => return RX_PW_P1;
         when 2 => return RX_PW_P2;
         when 3 => return RX_PW_P3;
         when 4 => return RX_PW_P4;
         when 5 => return RX_PW_P5;
      end case;
   end Pipe_To_RX_PW;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down (This : in out NRF24L01P_Driver) is
   begin
      Stand_By (This);  --  Appendix A, 6

      declare
         Data        : Register := Read_Register (This, CONFIG);
         RF_Config_R : CONFIG_Register
           with Import, Address => Data'Address;
      begin
         RF_Config_R.PWR_UP_RW := 0;
         Write_Register (This, CONFIG, Data);
      end;

      This.Holder.Power_On := False;
   end Power_Down;

   --------------
   -- Power_Up --
   --------------

   procedure Power_Up (This : in out NRF24L01P_Driver)
   is
      Data        : Register := Read_Register (This, CONFIG);
      RF_Config_R : CONFIG_Register
        with Import, Address => Data'Address;
   begin
      RF_Config_R.PWR_UP_RW := 1;
      Write_Register (This, CONFIG, Data);
      This.Holder.Power_On := True;
   end Power_Up;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register
     (This : in out NRF24L01P_Driver;
      Name : Register_Name)
      return Register
   is
      Command : HAL.SPI.SPI_Data_8b (1 .. 1);
      Cmd     : Command_R_REGISTER :=
        (Address => Registers_Addressses (Name),
         Command => <>)
        with Address => Command (Command'First)'Address;

      Data   : HAL.SPI.SPI_Data_8b (1 .. 1);
      Result : Register
        with Import, Address => Data (Data'First)'Address;
   begin
      Write_And_Read (This, Command, Data);
      return Result;
   end Read_Register;

   -------------
   -- Receive --
   -------------

   function Receive
     (This      : in out NRF24L01P_Driver;
      Pipe      : out RX_Pipe;
      Have_More : out Boolean)
      return TX_RX_Data
   is
      TX_Full : Boolean;
      Max_TX  : Boolean;
      TX_Data : Boolean;

      Payloads : Pipes_Payloads renames This.Holder.Payloads;
   begin
      Get_Status (This, TX_Full, Max_TX, TX_Data, Have_More, Pipe);

      if Have_More then
         declare
            Buffer : TX_RX_Data
              (1 ..
                 (if Payloads (Pipe).Payload = Dynamic_Payload
                  then Get_First_RX_Data_Size (This)
                  else Payloads (Pipe).Size));
         begin
            Receive (This, Buffer);
            Get_Status (This, TX_Full, Max_TX, TX_Data, Have_More, Pipe);
            return Buffer;
         end;

      else
         declare
            Empty : TX_RX_Data (1 .. 0);
         begin
            return Empty;
         end;
      end if;
   end Receive;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (This : in out NRF24L01P_Driver;
      Data : out TX_RX_Data)
   is
      Buf : SPI_Data_8b (Integer (Data'First) .. Integer (Data'Last));
   begin
      Write_Command_And_Read (This, Cmd_R_RX_PAYLOAD, Buf);
      Data := TX_RX_Data (Buf);

      Clear_Status
        (This               => This,
         RX_Resived         => True,
         TX_Sent            => False,
         Max_TX_Retransmits => False);
   end Receive;

   -----------------------
   -- Set_Air_Data_Rate --
   -----------------------

   procedure Set_Air_Data_Rate
     (This : in out NRF24L01P_Driver;
      Rate : Air_Data_Rate)
   is
      Data       : Register := Read_Register (This, RF_SETUP);
      RF_Setup_R : RF_SETUP_Register
        with Import, Address => Data'Address;
   begin
      Set_Rate (RF_Setup_R, Rate);
      Write_Register (This, RF_SETUP, Data);
   end Set_Air_Data_Rate;

   -------------------------
   -- Set_Amplifier_Power --
   -------------------------

   procedure Set_Amplifier_Power
     (This  : in out NRF24L01P_Driver;
      Power : Amplifier_Power)
   is
      Data       : Register := Read_Register (This, RF_SETUP);
      RF_Setup_R : RF_SETUP_Register
        with Import, Address => Data'Address;
   begin
      Set_Power (RF_Setup_R, Power);
      Write_Register (This, RF_SETUP, Data);
   end Set_Amplifier_Power;

   -------------------------
   -- Set_Auto_Retransmit --
   -------------------------

   procedure Set_Auto_Retransmit
     (This    : in out NRF24L01P_Driver;
      Count   : Auto_Retransmit_Count;
      A_Delay : Auto_Retransmit_Delay)
   is
      Data         : Register := Read_Register (This, SETUP_RETR);
      Setup_Retr_R : SETUP_RETR_Register
        with Import, Address => Data'Address;
   begin
      Setup_Retr_R.ARC_RW := Count;
      Setup_Retr_R.ARD_RW := A_Delay;
      Write_Register (This, SETUP_RETR, Data);
   end Set_Auto_Retransmit;

   -------------------------------
   -- Set_Auto_Retransmit_Count --
   -------------------------------

   procedure Set_Auto_Retransmit_Count
     (This  : in out NRF24L01P_Driver;
      Count : Auto_Retransmit_Count)
   is
      Data         : Register := Read_Register (This, SETUP_RETR);
      Setup_Retr_R : SETUP_RETR_Register
        with Import, Address => Data'Address;
   begin
      Setup_Retr_R.ARC_RW := Count;
      Write_Register (This, SETUP_RETR, Data);
   end Set_Auto_Retransmit_Count;

   -------------------------------
   -- Set_Auto_Retransmit_Delay --
   -------------------------------

   procedure Set_Auto_Retransmit_Delay
     (This    : in out NRF24L01P_Driver;
      A_Delay : Auto_Retransmit_Delay)
   is
      Data         : Register := Read_Register (This, SETUP_RETR);
      Setup_Retr_R : SETUP_RETR_Register
        with Import, Address => Data'Address;
   begin
      Setup_Retr_R.ARD_RW := A_Delay;
      Write_Register (This, SETUP_RETR, Data);
   end Set_Auto_Retransmit_Delay;

   -------------
   -- Set_CRC --
   -------------

   procedure Set_CRC
     (This : in out NRF24L01P_Driver;
      CRC  : CRC_Length)
   is
      Data        : Register := Read_Register (This, CONFIG);
      RF_Config_R : CONFIG_Register
        with Import, Address => Data'Address;
   begin
      case CRC is
         when Disabled =>
            RF_Config_R.EN_CRC_RW := 0;
            RF_Config_R.CRCO_RW   := 0;

         when Enabled_1byte =>
            RF_Config_R.EN_CRC_RW := 1;
            RF_Config_R.CRCO_RW   := 0;

         when Enabled_2byte =>
            RF_Config_R.EN_CRC_RW := 1;
            RF_Config_R.CRCO_RW   := 1;
      end case;

      Write_Register (This, CONFIG, Data);
   end Set_CRC;

   ------------------
   -- Set_Features --
   ------------------

   procedure Set_Features
     (This            : in out NRF24L01P_Driver;
      Dynamic_Payload : Boolean;
      ACK_Payload     : Boolean;
      NO_ACK_Allowed  : Boolean)
   is
      Data      : Register := Read_Register (This, FEATURE);
      Feature_R : FEATURE_Register
        with Import, Address => Data'Address;
   begin
      Feature_R.EN_DPL_RW     := Boolean_To_Bit (Dynamic_Payload);
      Feature_R.EN_ACK_PAY_RW := Boolean_To_Bit (ACK_Payload);
      Feature_R.EN_DYN_ACK_RW := Boolean_To_Bit (NO_ACK_Allowed);

      Write_Register (This, FEATURE, Data);
      This.Holder.NO_ACK := NO_ACK_Allowed;
   end Set_Features;

   -------------------------------
   -- Set_Max_Pipe_Address_Size --
   -------------------------------

   procedure Set_Max_Pipe_Address_Size
     (This : in out NRF24L01P_Driver;
      Size : Pipe_Address_Size)
   is
      Data       : Register := Read_Register (This, SETUP_AW);
      Setup_Aw_R : SETUP_AW_Register
        with Import, Address => Data'Address;
   begin
      case Size is
         when 3 =>
            Setup_Aw_R.AW_RW := 2#01#;
         when 4 =>
            Setup_Aw_R.AW_RW := 2#10#;
         when 5 =>
            Setup_Aw_R.AW_RW := 2#11#;
      end case;

      Write_Register (This, SETUP_AW, Data);
      This.Holder.Addr_Size := Size;
   end Set_Max_Pipe_Address_Size;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (This : in out NRF24L01P_Driver;
      Mode : PX_Mode)
   is
      Data     : Register := Read_Register (This, CONFIG);
      CONFIG_R : CONFIG_Register
        with Import, Address => Data'Address;
   begin
      CONFIG_R.PRIM_RX_RW := (if Mode = PRX then 1 else 0);
      Write_Register (This, CONFIG, Data);
      This.Holder.Mode := Mode;
   end Set_Mode;

   --------------------
   -- Enable_RX_Pipe --
   --------------------

   procedure Set_En_Rxaddr
     (Reg  : in out EN_RXADDR_Register;
      Pipe : RX_Pipe;
      Data : Bit) is
   begin
      case Pipe is
         when 0 =>
            Reg.ERX_P0_RW := Data;
         when 1 =>
            Reg.ERX_P1_RW := Data;
         when 2 =>
            Reg.ERX_P2_RW := Data;
         when 3 =>
            Reg.ERX_P3_RW := Data;
         when 4 =>
            Reg.ERX_P4_RW := Data;
         when 5 =>
            Reg.ERX_P5_RW := Data;
      end case;
   end Set_En_Rxaddr;

   ------------------------------
   -- Set_RF_Channel_Frequency --
   ------------------------------

   procedure Set_RF_Channel_Frequency
     (This   : in out NRF24L01P_Driver;
      Chanel : RF_Channel_Frequency)
   is
      Data    : Register := Read_Register (This, RF_CH);
      RF_CH_R : RF_CH_Register
        with Import, Address => Data'Address;
   begin
      RF_CH_R.RF_CH_RW := Chanel;

      Write_Register (This, RF_CH, Data);
   end Set_RF_Channel_Frequency;

   --------------------
   -- Set_TX_Address --
   --------------------

   procedure Set_TX_Address
     (This : in out NRF24L01P_Driver;
      Addr : Pipe_Address)
   is
      Size  : Pipe_Address_Size renames This.Holder.Addr_Size;
      Data  : HAL.SPI.SPI_Data_8b (1 .. 1 + Positive (Size));
      CMD   : Command_W_REGISTER :=
        (Address => Registers_Addressses (TX_ADDR),
         Command => <>)
        with Address => Data (Data'First)'Address;
      Local : Pipe_Address (1 .. Pipe_Address_Index (Size)) :=
        Addr (1 .. Pipe_Address_Index (Size))
        with Address => Data (Data'First + 1)'Address;
   begin
      Write (This, Data);
   end Set_TX_Address;

   ---------------------
   -- Start_Wating_RX --
   ---------------------

   procedure Start_Wating_RX (This : in out NRF24L01P_Driver) is
   begin
      Clear_RX_Buffer (This);
      Clear_TX_Buffer (This);
      Clear_Status (This);

      This.Holder.CE_Pin.Set;
   end Start_Wating_RX;

   --------------
   -- Transmit --
   --------------

   procedure Transmit
     (This   : in out NRF24L01P_Driver;
      Data   : TX_RX_Data;
      No_ACK : Boolean := False)
   is
      Buffer : HAL.SPI.SPI_Data_8b (1 .. 1 + Data'Length);

   begin
      Buffer (Buffer'First) := UInt8
        (if No_ACK then Cmd_W_TX_PAYLOAD_NO_ACK else Cmd_W_TX_PAYLOAD);
      Buffer (Buffer'First + 1 .. Buffer'Last) := HAL.SPI.SPI_Data_8b (Data);
      Write (This, Buffer);

      This.Holder.CE_Pin.Set;
   end Transmit;

   --------------
   -- Stand_By --
   --------------

   procedure Stand_By (This : in out NRF24L01P_Driver) is
   begin
      This.Holder.CE_Pin.Clear;
   end Stand_By;

   ---------------
   -- Set_Power --
   ---------------

   procedure Set_Power
     (RF_Setup : in out RF_SETUP_Register;
      Power    : Amplifier_Power) is
   begin
      RF_Setup.RF_PWR_RW := Power;
   end Set_Power;

   --------------
   -- Set_Rate --
   --------------

   procedure Set_Rate
     (RF_Setup : in out RF_SETUP_Register;
      Rate     : Air_Data_Rate) is
   begin
      case Rate is
         when Rate_250kbps =>
            RF_Setup.RF_DR_LOW_RW  := 1;
            RF_Setup.RF_DR_HIGH_RW := 0;

         when Rate_1Mbps =>
            RF_Setup.RF_DR_LOW_RW  := 0;
            RF_Setup.RF_DR_HIGH_RW := 0;

         when Rate_2Mbps =>
            RF_Setup.RF_DR_LOW_RW  := 0;
            RF_Setup.RF_DR_HIGH_RW := 1;
      end case;
   end Set_Rate;

   ----------------
   -- To_RX_ADDR --
   ----------------

   function To_RX_ADDR (Pipe : RX_Pipe) return Register_Address is
   begin
      case Pipe is
         when 0 => return Registers_Addressses (RX_ADDR_P0);
         when 1 => return Registers_Addressses (RX_ADDR_P1);
         when 2 => return Registers_Addressses (RX_ADDR_P2);
         when 3 => return Registers_Addressses (RX_ADDR_P3);
         when 4 => return Registers_Addressses (RX_ADDR_P4);
         when 5 => return Registers_Addressses (RX_ADDR_P5);
      end case;
   end To_RX_ADDR;

   -----------
   -- Write --
   -----------

   procedure Write
     (This  : in out NRF24L01P_Driver;
      Data  : HAL.SPI.SPI_Data_8b)
   is
      Status : HAL.SPI.SPI_Status;
   begin
      CSN_Low (This);
      This.Holder.SPI.Transmit (Data, Status);
      CSN_High (This);

      if Status /= HAL.SPI.Ok then
         raise Program_Error;
      end if;
   end Write;

   -----------------------
   -- Write_ACK_Payload --
   -----------------------

   procedure Write_ACK_Payload
     (This : in out NRF24L01P_Driver;
      Pipe : RX_Pipe;
      Data : TX_RX_Data)
   is
      Buffer  : HAL.SPI.SPI_Data_8b (1 .. Data'Length + 1);
      Command : Command_W_ACK_PAYLOAD := (Pipe => Pipe, Command => <>)
        with Address => Buffer (Buffer'First)'Address;
      Local   : TX_RX_Data := Data
        with Address => Buffer (Buffer'First + 1)'Address;
   begin
      Command.Pipe := Pipe;
      Write (This, Buffer);
   end Write_ACK_Payload;

   --------------------
   -- Write_And_Read --
   --------------------

   procedure Write_And_Read
     (This    : in out NRF24L01P_Driver;
      Command : HAL.SPI.SPI_Data_8b;
      Data    : out HAL.SPI.SPI_Data_8b)
   is
      Status : HAL.SPI.SPI_Status;
   begin
      CSN_Low (This);
      This.Holder.SPI.Transmit (Command, Status);
      if Status /= HAL.SPI.Ok then
         CSN_High (This);
         raise Program_Error;
      end if;

      This.Holder.SPI.Receive (Data, Status);
      CSN_High (This);

      if Status /= HAL.SPI.Ok then
         raise Program_Error;
      end if;
   end Write_And_Read;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command
     (This : in out NRF24L01P_Driver;
      Cmd  : Command)
   is
      Buffer : HAL.SPI.SPI_Data_8b (1 .. 1);
      Local  : Command := Cmd
        with Address => Buffer (Buffer'First)'Address;
   begin
      Write (This, Buffer);
   end Write_Command;

   ----------------------------
   -- Write_Command_And_Read --
   ----------------------------

   procedure Write_Command_And_Read
     (This : in out NRF24L01P_Driver;
      Cmd  : Command;
      Data : out HAL.SPI.SPI_Data_8b)
   is
      Buffer : HAL.SPI.SPI_Data_8b (1 .. 1);
      Local  : Command := Cmd
        with Address => Buffer (Buffer'First)'Address;
   begin
      Write_And_Read (This, Buffer, Data);
   end Write_Command_And_Read;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register
     (This : in out NRF24L01P_Driver;
      Name : Register_Name;
      Data : Register)
   is
      Buffer : HAL.SPI.SPI_Data_8b (1 .. 2);
      Cmd    : Command_W_REGISTER :=
        (Address => Registers_Addressses (Name),
         Command => <>)
        with Address => Buffer (Buffer'First)'Address;

      Local  : Register := Data
        with Address => Buffer (Buffer'First + 1)'Address;
   begin
      Write (This, Buffer);
   end Write_Register;

end NRF24L01P;
