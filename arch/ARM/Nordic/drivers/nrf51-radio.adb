------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with NRF51_SVD.RADIO; use NRF51_SVD.RADIO;
with System.Storage_Elements; use System.Storage_Elements;

package body nRF51.Radio is

   ------------------------------------
   -- Setup_For_Bluetooth_Low_Energy --
   ------------------------------------

   procedure Setup_For_Bluetooth_Low_Energy is
   begin
      Set_Mode (BLE_1MBIT);

      Configure_Packet (S0_Field_Size_In_Byte        => 1,
                        S1_Field_Size_In_Bit         => 0,
                        Length_Field_Size_In_Bit     => 8,
                        Max_Packet_Length_In_Byte    => 1 + 1 + 37,
                        Static_Packet_Length_In_Byte => 0,
                        On_Air_Endianness            => Little_Endian);

      Configure_CRC (Enable        => True,
                     Length        => 3,
                     Skip_Address  => True,
                     Polynomial    => 16#00_065B#,
                     Initial_Value => 16#55_5555#);

      Configure_Whitening (True);
   end Setup_For_Bluetooth_Low_Energy;

   -----------
   -- State --
   -----------

   function State return Radio_State is
   begin
      --  return Radio_State'Enum_Val (RADIO_Periph.STATE.STATE'Enum_Rep);
      case RADIO_Periph.STATE.STATE is
         when Disabled => return Disabled;
         when Rxru => return Rx_Ramp_Up;
         when Rxidle => return Rx_Idle;
         when Rx => return Rx_State;
         when Rxdisable => return Rx_Disabled;
         when Txru => return Tx_Ramp_Up;
         when Txidle => return Tx_Idle;
         when Tx => return Tx_State;
         when Txdisable => return Tx_Disabled;
      end case;
   end State;

   ---------------------
   -- Enable_Shortcut --
   ---------------------

   procedure Enable_Shortcut (Short : Shortcut) is
   begin
      case Short is
         when Ready_To_Start =>
            RADIO_Periph.SHORTS.READY_START := Enabled;
         when End_To_Disable =>
            RADIO_Periph.SHORTS.END_DISABLE := Enabled;
         when Disabled_To_TXen =>
            RADIO_Periph.SHORTS.DISABLED_TXEN := Enabled;
         when Disabled_To_RXen =>
            RADIO_Periph.SHORTS.DISABLED_RXEN := Enabled;
         when Address_To_RSSIstart =>
            RADIO_Periph.SHORTS.ADDRESS_RSSISTART := Enabled;
         when End_To_Start =>
            RADIO_Periph.SHORTS.END_START := Enabled;
         when Address_To_BCstart =>
            RADIO_Periph.SHORTS.ADDRESS_BCSTART := Enabled;
      end case;
   end Enable_Shortcut;

   ----------------------
   -- Disable_Shortcut --
   ----------------------

   procedure Disable_Shortcut (Short : Shortcut) is
   begin
      case Short is
         when Ready_To_Start =>
            RADIO_Periph.SHORTS.READY_START := Disabled;
         when End_To_Disable =>
            RADIO_Periph.SHORTS.END_DISABLE := Disabled;
         when Disabled_To_TXen =>
            RADIO_Periph.SHORTS.DISABLED_TXEN := Disabled;
         when Disabled_To_RXen =>
            RADIO_Periph.SHORTS.DISABLED_RXEN := Disabled;
         when Address_To_RSSIstart =>
            RADIO_Periph.SHORTS.ADDRESS_RSSISTART := Disabled;
         when End_To_Start =>
            RADIO_Periph.SHORTS.END_START := Disabled;
         when Address_To_BCstart =>
            RADIO_Periph.SHORTS.ADDRESS_BCSTART := Disabled;
      end case;
   end Disable_Shortcut;

   ----------------
   -- Set_Packet --
   ----------------

   procedure Set_Packet
     (Address : System.Address)
   is
   begin
      RADIO_Periph.PACKETPTR := UInt32 (To_Integer (Address));
   end Set_Packet;

   -------------------
   -- Set_Frequency --
   -------------------

   procedure Set_Frequency (F : Radio_Frequency_MHz) is
   begin
      RADIO_Periph.FREQUENCY.FREQUENCY :=
        UInt7 (F - Radio_Frequency_MHz'First);
   end Set_Frequency;

   ---------------
   -- Set_Power --
   ---------------

   procedure Set_Power (P : Radio_Power) is
   begin
      RADIO_Periph.TXPOWER.TXPOWER :=
        TXPOWER_TXPOWER_Field'Enum_Val (P'Enum_Rep);
   end Set_Power;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (Mode : Radio_Mode) is
   begin
      RADIO_Periph.MODE.MODE := (case Mode is
                                    when Nordic_1MBIT => Nrf_1Mbit,
                                    when Nordic_2MBIT => Nrf_2Mbit,
                                    when Nordic_250KBIT => Nrf_250Kbit,
                                    when BLE_1MBIT => Ble_1Mbit);
   end Set_Mode;

   -------------------------
   -- Set_Logic_Addresses --
   -------------------------

   procedure Set_Logic_Addresses
     (Base0, Base1 : HAL.UInt32;
      Base_Length_In_Byte : Base_Address_Lenght;
      AP0, AP1, AP2, AP3, AP4, AP5, AP6, AP7 : HAL.UInt8)
   is
   begin
      RADIO_Periph.BASE0 := Base0;
      RADIO_Periph.BASE1 := Base1;
      RADIO_Periph.PCNF1.BALEN := UInt3 (Base_Length_In_Byte);
      RADIO_Periph.PREFIX0.Arr := (AP0, AP1, AP2, AP3);
      RADIO_Periph.PREFIX1.Arr := (AP4, AP5, AP6, AP7);
   end Set_Logic_Addresses;

   -----------------------------
   -- Translate_Logic_Address --
   -----------------------------

   procedure Translate_Logic_Address (Logic_Addr : Radio_Logic_Address;
                                      Base       : out HAL.UInt32;
                                      Prefix     : out HAL.UInt8)
   is
   begin
      case Logic_Addr is
         when 0 =>
            Base := RADIO_Periph.BASE0;
         when 1 .. 7 =>
            Base := RADIO_Periph.BASE1;
      end case;
      case Logic_Addr is
         when 0 .. 3 =>
            Prefix := RADIO_Periph.PREFIX0.Arr (Integer (Logic_Addr));
         when 4 .. 7 =>
            Prefix := RADIO_Periph.PREFIX1.Arr (Integer (Logic_Addr));
      end case;
   end Translate_Logic_Address;

   --------------------
   -- Set_TX_Address --
   --------------------

   procedure Set_TX_Address (Logic_Addr : Radio_Logic_Address) is
   begin
      RADIO_Periph.TXADDRESS.TXADDRESS := UInt3 (Logic_Addr);
   end Set_TX_Address;

   --------------------------
   -- Get_RX_Match_Address --
   --------------------------

   function RX_Match_Address return Radio_Logic_Address is
   begin
      return Radio_Logic_Address (RADIO_Periph.RXMATCH.RXMATCH);
   end RX_Match_Address;

   ----------------------
   -- Set_RX_Addresses --
   ----------------------

   procedure Set_RX_Addresses (Enable_Mask : Logic_Address_Mask) is
   begin
      for Index in Enable_Mask'Range loop
         RADIO_Periph.RXADDRESSES.ADDR.Arr (Integer (Index)) :=
           (if Enable_Mask (Index) then Enabled else Disabled);
      end loop;
   end Set_RX_Addresses;

   -------------------
   -- Configure_CRC --
   -------------------

   procedure Configure_CRC (Enable        : Boolean;
                            Length        : UInt2;
                            Skip_Address  : Boolean;
                            Polynomial    : UInt24;
                            Initial_Value : UInt24)
   is
   begin
      if Enable then
         case Length is
            when 0 =>
               RADIO_Periph.CRCCNF.LEN := Disabled;
            when 1 =>
               RADIO_Periph.CRCCNF.LEN := One;
            when 2 =>
               RADIO_Periph.CRCCNF.LEN := Two;
            when 3 =>
               RADIO_Periph.CRCCNF.LEN := Three;
         end case;
      else
         RADIO_Periph.CRCCNF.LEN := Disabled;
      end if;

      RADIO_Periph.CRCCNF.SKIPADDR := (if Skip_Address then Skip else Include);
      RADIO_Periph.CRCPOLY.CRCPOLY := Polynomial;
      RADIO_Periph.CRCINIT.CRCINIT := Initial_Value;
   end Configure_CRC;

   ---------------
   -- CRC_Error --
   ---------------

   function CRC_Error return Boolean is
   begin
      return RADIO_Periph.CRCSTATUS.CRCSTATUS = Crcerror;
   end CRC_Error;

   -------------------------
   -- Configure_Whitening --
   -------------------------

   procedure Configure_Whitening (Enable        : Boolean;
                                  Initial_Value : UInt6 := 0)
   is
   begin
      RADIO_Periph.PCNF1.WHITEEN := (if Enable then Enabled else Disabled);
      RADIO_Periph.DATAWHITEIV.DATAWHITEIV :=
        2#0100_0000# or UInt7 (Initial_Value);
   end Configure_Whitening;

   ----------------------
   -- Configure_Packet --
   ----------------------

   procedure Configure_Packet
     (S0_Field_Size_In_Byte        : Bit;
      S1_Field_Size_In_Bit         : UInt4;
      Length_Field_Size_In_Bit     : UInt4;
      Max_Packet_Length_In_Byte    : Packet_Len;
      Static_Packet_Length_In_Byte : Packet_Len;
      On_Air_Endianness            : Length_Field_Endianness)
   is
   begin
      RADIO_Periph.PCNF0.LFLEN := Length_Field_Size_In_Bit;
      RADIO_Periph.PCNF0.S0LEN := S0_Field_Size_In_Byte = 1;
      RADIO_Periph.PCNF0.S1LEN := S1_Field_Size_In_Bit;
      RADIO_Periph.PCNF1.MAXLEN := Max_Packet_Length_In_Byte;
      RADIO_Periph.PCNF1.STATLEN := Static_Packet_Length_In_Byte;
      RADIO_Periph.PCNF1.ENDIAN := (if On_Air_Endianness = Little_Endian then
                                       Little
                                    else
                                       Big);
   end Configure_Packet;
end nRF51.Radio;
