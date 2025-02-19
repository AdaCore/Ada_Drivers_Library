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

--  Driver for NRF24L01P chip

with HAL;       use HAL;
with HAL.GPIO;  use HAL.GPIO;
with HAL.SPI;   use HAL.SPI;

package NRF24L01P is
   pragma Extensions_Allowed (On);

   type NRF24L01P_Driver is limited private;

   type PX_Mode is (PTX, PRX);
   --  Transiver or responser

   type Amplifier_Power is
     (Power_Minimum, Power_Low, Power_High, Power_Maximum);
   --  PA control is used to set the output power from the amplifier.

   type Air_Data_Rate is (Rate_250kbps, Rate_1Mbps, Rate_2Mbps);
   --  The air data rate. A transmitter and a receiver must be
   --  configured with the same air data rate to communicate with each other.
   --  Should be set to 1Mbps or 250kbps for backward compatibility with the
   --  nRF2401 nRF2401A, nRF2402, nRF24E1 and nRF24E2.

   type CRC_Length is (Disabled, Enabled_1byte, Enabled_2byte);
   --  The CRC is the mandatory error detection mechanism in the packet.
   --  It is either 1 or 2 bytes and is calculated over the address,
   --  Packet Control Field and Payload.

   type RF_Channel_Frequency is range 0 .. 63 with Size => 7;
   --  The RF channel frequency determines the center of the channel used by
   --  the nRF24L01+ F0= 2400 + RF_CH [MHz]

   type Configuration is record
      Mode       : PX_Mode;
      Power      : Amplifier_Power;
      Rate       : Air_Data_Rate;
      CRC        : CRC_Length;
      Chanel     : RF_Channel_Frequency;
      RX_IRQ     : Boolean := True;
      TX_IRQ     : Boolean := True;
      MAX_TR_IRQ : Boolean := True;
   end record;
   --  Holds configuration data which is used for initial chip configuraton.
   --  RX_IRQ, TX_IRQ, MAX_TR_IRQ controls whether IRQ should be generated.

   procedure Initialize
     (This    : out NRF24L01P_Driver;
      CE_Pin  : HAL.GPIO.Any_GPIO_Point;
      CSN_Pin : HAL.GPIO.Any_GPIO_Point;
      SPI     : HAL.SPI.Any_SPI_Port;
      Config  : Configuration)
     with Pre => (CE_Pin.Mode = Output and then
                    CSN_Pin.Mode = Output and then
                      SPI.Data_Size = Data_Size_8b);
   --  Creates a NRF24L01P_Driver instance for SPI with SOFTWARE slave
   --  selection. Only 3 SPI wires will be used. NSS pin from board's SPI
   --  is not used. CSN_Pin is used instead. It should be attached to
   --  CSN (pin 2) on NRF24L01P. CSN_Pin should be initialized for OUTPUT
   --  mode. CE_Pin will be used to control TX/RX and should be connected
   --  to CE (pin 1) on NRF24L01P. Should be in OUTPUT mode. Config is used
   --  for the initial configuration. SPI should have the following
   --  configuration: Master, FullDuplex, Data_Size_8b, Low/P1Edge,
   --    Software_Managed, 0-10Mbps, MSB.
   --  IRQ pin should be configured as:
   --    Mode_In, Floating, Interrupt_Falling_Edge.

   function Can_Be_Configured (This : NRF24L01P_Driver) return Boolean;
   --  Returns True when the chip can be configured (power off or standby mode)

   procedure Set_Mode
     (This : in out NRF24L01P_Driver;
      Mode : PX_Mode)
     with Pre => (Can_Be_Configured (This));
   --  Sets transiver or responser mode

   procedure Set_Amplifier_Power
     (This  : in out NRF24L01P_Driver;
      Power : Amplifier_Power)
     with Pre => (Can_Be_Configured (This));
   --  Sets the output power of the amplifier

   procedure Set_Air_Data_Rate
     (This : in out NRF24L01P_Driver;
      Rate : Air_Data_Rate)
     with Pre => (Can_Be_Configured (This));
   --  Sets the air data rate.
   --  Should be set to 1Mbps or 250kbps for backward compatibility with the
   --  nRF2401 nRF2401A, nRF2402, nRF24E1 and nRF24E2.

   procedure Set_CRC
     (This : in out NRF24L01P_Driver;
      CRC  : CRC_Length)
     with Pre => (Can_Be_Configured (This));
   --  Enable/disable CRC.
   --  Forced enabled when Enhanced ShockBurst is enabled.

   procedure Set_RF_Channel_Frequency
     (This   : in out NRF24L01P_Driver;
      Chanel : RF_Channel_Frequency)
     with Pre => (Can_Be_Configured (This));
   --  Sets the RF channel frequency.

   type Auto_Retransmit_Count is range 0 .. 15 with Size => 4;
   --  Count of attempts to retransmit data when ACK packet is not recived back

   procedure Set_Auto_Retransmit_Count
     (This  : in out NRF24L01P_Driver;
      Count : Auto_Retransmit_Count)
     with Pre => (Can_Be_Configured (This));
   --  Sets count of attempts to retransmit data when ACK packet
   --  is not recived back. Should be 0 for backward compatibility with the
   --  nRF2401 nRF2401A, nRF2402, nRF24E1 and nRF24E2.

   type Auto_Retransmit_Delay is range 0 .. 15 with Size => 4;
   --  Waiting for ACK packet time before retransmission

   procedure Set_Auto_Retransmit_Delay
     (This    : in out NRF24L01P_Driver;
      A_Delay : Auto_Retransmit_Delay)
     with Pre => (Can_Be_Configured (This));
   --  Set waiting for ACK packet time before retransmission
   --  Please take care when setting this parameter. If the ACK payload
   --  is more than 15 byte in 2Mbps mode the ARD must be 500µS or more,
   --  if the ACK payload is more than 5byte in 1Mbps mode the ARD must be
   --  500µS or more. In 250kbps mode (even when the payload is not in ACK)
   --  the ARD must be 500µS or more.

   procedure Set_Auto_Retransmit
     (This    : in out NRF24L01P_Driver;
      Count   : Auto_Retransmit_Count;
      A_Delay : Auto_Retransmit_Delay)
     with Pre => (Can_Be_Configured (This));
   --  The same as two above.

   type Pipe_Address_Index is range 1 .. 5;
   --  Used as an index for address arrays

   type Pipe_Address_Size is range 3 .. 5;
   --  Used to define address size for RX0, RX1 and TX pipes

   procedure Set_Max_Pipe_Address_Size
     (This : in out NRF24L01P_Driver;
      Size : Pipe_Address_Size)
     with Pre => (Can_Be_Configured (This));
   --  Sets the address sizes for RX pipes 0, 1 and TX 0

   type RX_Pipe is range 0 .. 5;
   --  Number of the RX pipe

   type Pipe_Address is array (Pipe_Address_Index range <>) of UInt8;
   --  Used to set address for pipes

   function Is_Valid_Address_Size
     (This    : NRF24L01P_Driver;
      Pipe    : RX_Pipe;
      Address : Pipe_Address)
      return Boolean;
   --  Checks that Address'Length = Max_Pipe_Address_Size for RX0, RX1
   --  and 1 for RX2-5

   type Payload_Length_Type is (Static_Payload, Dynamic_Payload);

   type Payload_Length is range 0 .. 32;

   type Pipe_Payload
     (Payload : Payload_Length_Type := Static_Payload)
   is record
      case Payload is
         when Static_Payload =>
            Size : Payload_Length := 0;
         when Dynamic_Payload =>
            null;
      end case;
   end record;

   function Is_ACK_Allowed
     (ACK             : Boolean;
      Dynamic_Payload : Boolean)
      return Boolean;
   --  Return True if ACK=False or both ACK & Dynamic_Payload are True

   procedure Configure_And_Enable_RX_Pipe
     (This    : in out NRF24L01P_Driver;
      Pipe    : RX_Pipe;
      Addr    : Pipe_Address; --  Address that will be used for pipe
      ACK     : Boolean;      --  Enable ACK for pipe
      Payload : Pipe_Payload) --  Payload size
     with Pre => (Can_Be_Configured (This)
                  and then Is_Valid_Address_Size (This, Pipe, Addr)
                  and then Is_ACK_Allowed
                    (ACK, Payload.Payload = Dynamic_Payload));
   --  Configures and enables the RX pipe.
   --  For PRX each pipe should have different address to accept a data from
   --  different transmitters.
   --  ACK_Payload should be enabled with Set_Features if ACK is True.
   --
   --  !!! For PTX:
   --    TX0 and RX0 pipes should have the same addresses.
   --    RX0 pipe should have DPL enabled if receiver uses DPL/ACK

   function Is_Valid_TX_Address_Size
     (This    : NRF24L01P_Driver;
      Address : Pipe_Address)
      return Boolean;
   --  Checks that Address'Length = Max_Pipe_Address_Size

   procedure Set_TX_Address
     (This : in out NRF24L01P_Driver;
      Addr : Pipe_Address)
     with Pre => (Can_Be_Configured (This) and then
                    Is_Valid_TX_Address_Size (This, Addr));
   --  Sets address which will be used for TX pipe in PTX mode.
   --  !!! The same address should be set for the TX(n) and RX0 pipe.

   procedure Enable_RX_Pipe
     (This : in out NRF24L01P_Driver;
      Pipe : RX_Pipe)
     with Pre => (Can_Be_Configured (This));
   --  Enables the RX pipe, does not change its configuration

   procedure Disable_RX_Pipe
     (This : in out NRF24L01P_Driver;
      Pipe : RX_Pipe)
     with Pre => (Can_Be_Configured (This));
   --  Disables the RX pipe

   procedure Set_Features
     (This            : in out NRF24L01P_Driver;
      Dynamic_Payload : Boolean;  --  Enables Dynamic Payload Length (DPL)
      ACK_Payload     : Boolean;  --  Enables Payload with ACK
      NO_ACK_Allowed  : Boolean)  --  Enables the W_TX_PAYLOAD_NOACK command
     with Pre => (Can_Be_Configured (This) and then
                    Is_ACK_Allowed (ACK_Payload, Dynamic_Payload));
   --  Sets chip's features
   --
   --  A PTX that transmits to a PRX with DPL enabled must have the DPL
   --  enabled for RX0.
   --  DPL should be off for backward compatibility with the nRF2401 nRF2401A,
   --  nRF2402, nRF24E1 and nRF24E2.
   --
   --  If ACK packet payload is activated the DPL feature should be enabled
   --  for PTX and RTX.
   --  On PTX side RX0 pipe should have DPL.
   --  On PRX side DPL should be enabled for the pipe that will accept data
   --  from transmitter with DPL.
   --
   --  The PTX can send NO_ACK for PRX if NO_ACK_Allowed is True.
   --  The PRX does not transmit an ACK packet back in this case.

   procedure Configure_IRQ
     (This       : in out NRF24L01P_Driver;
      RX_IRQ     : Boolean;
      TX_IRQ     : Boolean;
      MAX_TR_IRQ : Boolean);
   --  Enables/Disables IRQs

   function Is_Power_On (This : NRF24L01P_Driver) return Boolean;
   --  Returns True when the chip is powered on

   procedure Power_Up (This : in out NRF24L01P_Driver)
     with Pre => (not Is_Power_On (This));

   procedure Power_Down (This : in out NRF24L01P_Driver)
     with Pre => (Is_Power_On (This));

   type TX_RX_Data is array (Payload_Length range 1 .. <>) of UInt8;

   function Is_No_ACK_Allowed
     (This   : NRF24L01P_Driver;
      No_ACK : Boolean) return Boolean;
   --  Returns True when No_ACK=False or FEATURE.EN_DYN_ACK is set

   procedure Transmit
     (This   : in out NRF24L01P_Driver;
      Data   : TX_RX_Data;
      No_ACK : Boolean := False)
     with Pre => (Is_Power_On (This) and then
                    not Is_In_RX_Mode (This) and then
                      Is_No_ACK_Allowed (This, No_ACK));
   --  Trasmits the data. No ACK will be send back if No_ACK=True

   function Is_Transmitted (This : in out NRF24L01P_Driver) return Boolean;
   --  Returns True if TX has sent data.
   --    Technically returns Status.TX_Sent

   function Have_ACK_Payload (This : in out NRF24L01P_Driver) return Boolean;
   --  Returns True if RX side has sent data back with ACK.
   --    Technically returns Status.RX_Resived

   function Is_In_RX_Mode (This : NRF24L01P_Driver) return Boolean;
   --  Returns True when the chip is in RX mode

   function Is_Wating_RX (This : NRF24L01P_Driver) return Boolean;
   --  Returns True when the chip is wait for RX data

   procedure Start_Wating_RX (This : in out NRF24L01P_Driver)
     with Pre => (Is_Power_On (This) and then
                    Is_In_RX_Mode (This) and then
                      not Is_Wating_RX (This));
   --  Device will wait for data

   function Is_Received (This : in out NRF24L01P_Driver) return Boolean;
   --  Returns True if RX has got data.
   --    Technically returns Status.RX_Resived

   function Receive
     (This      : in out NRF24L01P_Driver;
      Pipe      : out RX_Pipe;
      Have_More : out Boolean)
      return TX_RX_Data;
   --  Gets RX data size, read data from the buffer, clear RX_Resived status
   --  flag. Fills Pipe with the pipe number where data was.
   --  Set Have_More = True if RX buffer contains more data recived.
   --  RX_Resived flag should not be cleared (by Clear_Status) before this
   --  method called.

   procedure Write_ACK_Payload
     (This : in out NRF24L01P_Driver;
      Pipe : RX_Pipe;
      Data : TX_RX_Data);
   --  Upload data that will be send back to the TX when the next
   --  packet is transived.

   procedure Stand_By (This : in out NRF24L01P_Driver)
     with Pre => (Is_Power_On (This));
   --  Stops waiting for data or transmitting it

   -------------------------
   -- "Low level" methods --
   -------------------------

   procedure Get_Status
     (This               : in out NRF24L01P_Driver;
      TX_Full            : out Boolean;
      Max_TX_Retransmits : out Boolean;
      TX_Sent            : out Boolean;
      RX_Resived         : out Boolean;
      Pipe               : out RX_Pipe);
   --  Returns current chip status

   procedure Clear_Status
     (This               : in out NRF24L01P_Driver;
      RX_Resived         : Boolean := True;
      TX_Sent            : Boolean := True;
      Max_TX_Retransmits : Boolean := True);

   function Get_First_RX_Data_Size
     (This : in out NRF24L01P_Driver)
      return Payload_Length;
   --  Returns the data's size which is the first in the buffer's queue

   procedure Receive
     (This : in out NRF24L01P_Driver;
      Data : out TX_RX_Data);
   --  Gets data from the RX buffer and clear RX_Resived status flag

   procedure Clear_RX_Buffer (This : in out NRF24L01P_Driver);
   --  Deletes all data from the RX buffer

   procedure Clear_TX_Buffer (This : in out NRF24L01P_Driver);
   --  Deletes all data from the TX buffer

private

   type Register is new UInt8;
   type Register_Address is new UInt5;

   for Amplifier_Power use
     (Power_Minimum => 2#00#,  -- Power -18dBm, DC 7.0mA
      Power_Low     => 2#01#,  -- Power -12dBm, DC 7.5mA
      Power_High    => 2#10#,  -- Power  -6dBm, DC 9.0mA
      Power_Maximum => 2#11#); -- Power   0dBm, DC 11.3mA
   for Amplifier_Power'Size use 2;

   ----------------
   --  Registers --
   ----------------

   type CONFIG_Register is record
      PRIM_RX_RW     : Bit;
      --  RX/TX control 1: PRX, 0: PTX

      PWR_UP_RW      : Bit;
      --  1: POWER UP, 0:POWER DOWN

      CRCO_RW        : Bit;
      --  CRC encoding scheme.
      --  '0' - 1 byte
      --  '1' - 2 bytes

      EN_CRC_RW      : Bit;
      --  Enable CRC.
      --  Forced high if one of the bits in the EN_AA is high

      MASK_MAX_RT_RW : Bit;
      --  Mask interrupt caused by MAX_RT.
      --  1: Interrupt not reflected on the IRQ pin
      --  0: Reflect MAX_RT as active low interrupt on the IRQ pin

      MASK_TX_DS_RW  : Bit;
      --  Mask interrupt caused by TX_DS
      --  1: Interrupt not reflected on the IRQ pin
      --  0: Reflect TX_DS as active low interrupt on the IRQ pin

      MASK_RX_DR_RW  : Bit;
      --  Mask interrupt caused by RX_DR
      --  1: Interrupt not reflected on the IRQ pin
      --  0: Reflect RX_DR as active low interrupt on the pin

      Reserved_RW    : Bit := 0;
   end record with Size => Register'Size;

   for CONFIG_Register use record
      PRIM_RX_RW     at 0 range 0 .. 0;
      PWR_UP_RW      at 0 range 1 .. 1;
      CRCO_RW        at 0 range 2 .. 2;
      EN_CRC_RW      at 0 range 3 .. 3;
      MASK_MAX_RT_RW at 0 range 4 .. 4;
      MASK_TX_DS_RW  at 0 range 5 .. 5;
      MASK_RX_DR_RW  at 0 range 6 .. 6;
      Reserved_RW    at 0 range 7 .. 7;
   end record;

   -- EN_AA_Register --

   type EN_AA_Register is record
      ENAA_P0_RW  : Bit;
      ENAA_P1_RW  : Bit;
      ENAA_P2_RW  : Bit;
      ENAA_P3_RW  : Bit;
      ENAA_P4_RW  : Bit;
      ENAA_P5_RW  : Bit;
      Reserved_RW : UInt2 := 0;
   end record with Size => Register'Size;

   for EN_AA_Register use record
      ENAA_P0_RW  at 0 range 0 .. 0;
      ENAA_P1_RW  at 0 range 1 .. 1;
      ENAA_P2_RW  at 0 range 2 .. 2;
      ENAA_P3_RW  at 0 range 3 .. 3;
      ENAA_P4_RW  at 0 range 4 .. 4;
      ENAA_P5_RW  at 0 range 5 .. 5;
      Reserved_RW at 0 range 6 .. 7;
   end record;

   -- EN_RXADDR_Register --

   type EN_RXADDR_Register is record
      ERX_P0_RW   : Bit;
      ERX_P1_RW   : Bit;
      ERX_P2_RW   : Bit;
      ERX_P3_RW   : Bit;
      ERX_P4_RW   : Bit;
      ERX_P5_RW   : Bit;
      Reserved_RW : UInt2 := 0;
   end record with Size => Register'Size;

   for EN_RXADDR_Register use record
      ERX_P0_RW   at 0 range 0 .. 0;
      ERX_P1_RW   at 0 range 1 .. 1;
      ERX_P2_RW   at 0 range 2 .. 2;
      ERX_P3_RW   at 0 range 3 .. 3;
      ERX_P4_RW   at 0 range 4 .. 4;
      ERX_P5_RW   at 0 range 5 .. 5;
      Reserved_RW at 0 range 6 .. 7;
   end record;

   -- SETUP_AW_Register --

   type SETUP_AW_Register is record
      AW_RW       : UInt2;
      --  RX/TX Address field width

      Reserved_RW : UInt6 := 0;
   end record with Size => Register'Size;

   for SETUP_AW_Register use record
      AW_RW       at 0 range 0 .. 1;
      Reserved_RW at 0 range 2 .. 7;
   end record;

   -- SETUP_RETR_Register --

   type SETUP_RETR_Register is record
      ARC_RW : Auto_Retransmit_Count;
      --  Auto Retransmit Count
      ARD_RW : Auto_Retransmit_Delay;
      --  Auto Retransmit Delay
   end record with Size => Register'Size;

   for SETUP_RETR_Register use record
      ARC_RW     at 0 range 0 .. 3;
      ARD_RW     at 0 range 4 .. 7;
   end record;

   --  RF_CH_Register --

   type RF_CH_Register is record
      RF_CH_RW    : RF_Channel_Frequency;
      --  Sets the frequency channel device operates

      Reserved_RW : Bit := 0;
   end record with Size => Register'Size;

   for RF_CH_Register use record
      RF_CH_RW    at 0 range 0 .. 6;
      Reserved_RW at 0 range 7 .. 7;
   end record;

   -- RF_SETUP_Register --

   type RF_SETUP_Register is record
      Obsolete_NA   : Bit := 0;
      --  Don't care

      RF_PWR_RW     : Amplifier_Power;
      --  Set RF output power in TX mode

      RF_DR_HIGH_RW : Bit;
      --  Select between the high speed data rates. This bit is don't
      --  care if RF_DR_LOW is set.

      PLL_LOCK_RW   : Bit;
      --  Force PLL lock signal. Only used in test

      RF_DR_LOW_RW  : Bit;
      --  Set RF Data Rate to 250kbps.

      Reserved_RW   : Bit := 0;
      --  Only '0' allowed

      CONT_WAVE_RW  : Bit;
      --  Enables continuous carrier transmit when high.
   end record with Size => Register'Size;

   for RF_SETUP_Register use record
      Obsolete_NA   at 0 range 0 .. 0;
      RF_PWR_RW     at 0 range 1 .. 2;
      RF_DR_HIGH_RW at 0 range 3 .. 3;
      PLL_LOCK_RW   at 0 range 4 .. 4;
      RF_DR_LOW_RW  at 0 range 5 .. 5;
      Reserved_RW   at 0 range 6 .. 6;
      CONT_WAVE_RW  at 0 range 7 .. 7;
   end record;

   -- STATUS_Register --

   type STATUS_Register is record
      TX_FULL_RO : Bit;
      --  TX FIFO full flag

      RX_P_NO_RO : UInt3;
      --  Data pipe number for the payload available for reading from RX_FIFO
      --  000-101: Data Pipe Number
      --  110: Not Used
      --  111: RX FIFO Empty

      MAX_RT_RW  : Bit;
      --  Maximum number of TX retransmits interrupt. Write 1 to clear bit.
      --  If is asserted it must be cleared to enable further communication.

      TX_DS_RW   : Bit;
      --  Data Sent TX FIFO interrupt. Asserted when packet transmitted on TX.
      --  If AUTO_ACK is activated, this bit is set high only when ACK is
      --  received. Write 1 to clear bit.

      RX_DR_RW   : Bit;
      --  Data Ready RX FIFO interrupt. Asserted when new data arrives RX
      --  FIFOc. Write 1 to clear bit.

      Reserved_RW : Bit := 0;
      --  Only '0' allowed.
   end record with Size => Register'Size;

   for STATUS_Register use record
      TX_FULL_RO  at 0 range 0 .. 0;
      RX_P_NO_RO  at 0 range 1 .. 3;
      MAX_RT_RW   at 0 range 4 .. 4;
      TX_DS_RW    at 0 range 5 .. 5;
      RX_DR_RW    at 0 range 6 .. 6;
      Reserved_RW at 0 range 7 .. 7;
   end record;

   -- OBSERVE_TX_Register --

   type OBSERVE_TX_Register is record
      ARC_CNT_RO  : UInt4;
      --  Count retransmitted packets. The counter is reset
      --  when transmission of a new packet starts

      PLOS_CNT_RO : UInt4;
      --  Count lost packets. The counter is overflow protected to 15,
      --  and discontinues at max until reset. The counter is reset by
      --  writing to RF_CH.
   end record with Size => Register'Size;

   for OBSERVE_TX_Register use record
      ARC_CNT_RO  at 0 range 0 .. 3;
      PLOS_CNT_RO at 0 range 4 .. 7;
   end record;

   -- RPD_Register --

   type RPD_Register is record
      RPD_RO  : Bit;
      --  Received Power Detector. This register is called CD (Carrier Detect)
      --  in the nRF24L01. The name is different in nRF24L01+ due to the
      --  different input power level threshold for this bit.

      Reserved_RO : UInt7 := 0;
   end record with Size => Register'Size;

   for RPD_Register use record
      RPD_RO      at 0 range 0 .. 0;
      Reserved_RO at 0 range 1 .. 7;
   end record;

   -- RX_PW_PX_Register --

   type RX_PW_PX_Register is record
      RX_PW_PX_RW : UInt6;
      --  Number of bytes in RX payload in data pipe X
      Reserved_RW : UInt2 := 0;
   end record with Size => Register'Size;

   for RX_PW_PX_Register use record
      RX_PW_PX_RW at 0 range 0 .. 5;
      Reserved_RW at 0 range 6 .. 7;
   end record;

   -- FIFO_STATUS_Register --

   type FIFO_STATUS_Register is record
      RX_EMPTY_RO  : Bit; --  1: RX FIFO empty.
      RX_FULL_RO   : Bit; --  1: RX FIFO full.
      Reserved_RW  : UInt2 := 0;
      TX_EMPTY_RO  : Bit; --  1: TX FIFO empty.
      TX_FULL_RO   : Bit; --  1: TX FIFO full.
      TX_REUSE_RO  : Bit; --  Reuse last transmitted payload
      Reserved_RW2 : Bit := 0;
   end record with Size => Register'Size;

   for FIFO_STATUS_Register use record
      RX_EMPTY_RO  at 0 range 0 .. 0;
      RX_FULL_RO   at 0 range 1 .. 1;
      Reserved_RW  at 0 range 2 .. 3;
      TX_EMPTY_RO  at 0 range 4 .. 4;
      TX_FULL_RO   at 0 range 5 .. 5;
      TX_REUSE_RO  at 0 range 6 .. 6;
      Reserved_RW2 at 0 range 7 .. 7;
   end record;

   -- DYNPD_Register --

   type DYNPD_Register is record
      DPL_P0_RW   : Bit;
      DPL_P1_RW   : Bit;
      DPL_P2_RW   : Bit;
      DPL_P3_RW   : Bit;
      DPL_P4_RW   : Bit;
      DPL_P5_RW   : Bit;
      Reserved_RW : UInt2 := 0;
   end record with Size => Register'Size;

   for DYNPD_Register use record
      DPL_P0_RW   at 0 range 0 .. 0;
      DPL_P1_RW   at 0 range 1 .. 1;
      DPL_P2_RW   at 0 range 2 .. 2;
      DPL_P3_RW   at 0 range 3 .. 3;
      DPL_P4_RW   at 0 range 4 .. 4;
      DPL_P5_RW   at 0 range 5 .. 5;
      Reserved_RW at 0 range 6 .. 7;
   end record;

   -- FEATURE_Register --

   type FEATURE_Register is record
      EN_DYN_ACK_RW : Bit;
      --  Enables the W_TX_PAYLOAD_NOACK command

      EN_ACK_PAY_RW : Bit;
      --  Enables Payload with ACK

      EN_DPL_RW     : Bit;
      --  Enables Dynamic Payload Length

      Reserved_RW   : UInt5 := 0;
   end record with Size => Register'Size;

   for FEATURE_Register use record
      EN_DYN_ACK_RW at 0 range 0 .. 0;
      EN_ACK_PAY_RW at 0 range 1 .. 1;
      EN_DPL_RW     at 0 range 2 .. 2;
      Reserved_RW   at 0 range 3 .. 7;
   end record;

   -- Register_Name --

   type Register_Name is
     (CONFIG, EN_AA, EN_RXADDR, SETUP_AW, SETUP_RETR, RF_CH, RF_SETUP, STATUS,
      OBSERVE_TX, RPD,
      RX_ADDR_P0, RX_ADDR_P1, RX_ADDR_P2, RX_ADDR_P3, RX_ADDR_P4, RX_ADDR_P5,
      TX_ADDR,
      RX_PW_P0, RX_PW_P1, RX_PW_P2, RX_PW_P3, RX_PW_P4, RX_PW_P5,
      FIFO_STATUS, DYNPD, FEATURE);

   Registers_Addressses : constant array (Register_Name) of
     Register_Address :=
       [CONFIG      => 16#00#,
        EN_AA       => 16#01#,
        EN_RXADDR   => 16#02#,
        SETUP_AW    => 16#03#,
        SETUP_RETR  => 16#04#,
        RF_CH       => 16#05#,
        RF_SETUP    => 16#06#,
        STATUS      => 16#07#,
        OBSERVE_TX  => 16#08#,
        RPD         => 16#09#,
        RX_ADDR_P0  => 16#0A#,
        RX_ADDR_P1  => 16#0B#,
        RX_ADDR_P2  => 16#0C#,
        RX_ADDR_P3  => 16#0D#,
        RX_ADDR_P4  => 16#0E#,
        RX_ADDR_P5  => 16#0F#,
        TX_ADDR     => 16#10#,
        RX_PW_P0    => 16#11#,
        RX_PW_P1    => 16#12#,
        RX_PW_P2    => 16#13#,
        RX_PW_P3    => 16#14#,
        RX_PW_P4    => 16#15#,
        RX_PW_P5    => 16#16#,
        FIFO_STATUS => 16#17#,
        DYNPD       => 16#1C#,
        FEATURE     => 16#1D#];

   --------------
   -- Commands --
   --------------

   type Command is new UInt8;
   --  Base type for all commands

   type Command_R_REGISTER is record
      Address : Register_Address;
      Command : UInt3 := 0;
   end record with Size => Command'Size;
   --  Read registers command

   for Command_R_REGISTER use record
      Address at 0 range 0 .. 4;
      Command at 0 range 5 .. 7;
   end record;

   type Command_W_REGISTER is record
      Address : Register_Address;
      Command : UInt3 := 1;
   end record with Size => Command'Size;
   --  Write registers command

   for Command_W_REGISTER use record
      Address at 0 range 0 .. 4;
      Command at 0 range 5 .. 7;
   end record;

   type Command_W_ACK_PAYLOAD is record
      Pipe    : RX_Pipe;
      Command : UInt5 := 2#10101#;
   end record with Size => Command'Size;
   --  Write ACK payload to be trasfered back to TX

   for Command_W_ACK_PAYLOAD use record
      Pipe    at 0 range 0 .. 2;
      Command at 0 range 3 .. 7;
   end record;

   Cmd_R_RX_PAYLOAD        : constant Command := 2#0110_0001#;
   Cmd_W_TX_PAYLOAD        : constant Command := 2#1010_0000#;
   Cmd_FLUSH_RX            : constant Command := 2#1110_0010#;
   Cmd_FLUSH_TX            : constant Command := 2#1110_0001#;
   Cmd_REUSE_TX_PL         : constant Command := 2#1110_0011#;
   Cmd_R_RX_PL_WID         : constant Command := 2#0110_0000#;
   Cmd_W_TX_PAYLOAD_NO_ACK : constant Command := 2#1011_0000#;
   Cmd_NOP                 : constant Command := Command'Last;

   ----------------------
   -- NRF24L01P_Driver --
   ----------------------

   type Pipes_Payloads is array (RX_Pipe) of Pipe_Payload;

   type Holder_Type_Kind is (Hardware, Software);

   type Holder_Type
     (Kind : Holder_Type_Kind := Software) is
      record
         SPI       : HAL.SPI.Any_SPI_Port;
         CE_Pin    : HAL.GPIO.Any_GPIO_Point;
         --  Used to Enable/Disable chip

         --  Some current configuration data
         Power_On  : Boolean := False;
         Mode      : PX_Mode := PTX;
         NO_ACK    : Boolean := False;
         Addr_Size : Pipe_Address_Size := 5;
         Payloads  : Pipes_Payloads :=
           [0 => (Payload => Dynamic_Payload),
            1 => (Payload => Dynamic_Payload),
            2 => <>,
            3 => <>,
            4 => <>,
            5 => <>];

         case Kind is
            when Hardware =>
               null;
            when Software =>
               CSN_Pin : HAL.GPIO.Any_GPIO_Point;
               --  Used for "selecting" chip vis SPI
         end case;
      end record;

   type NRF24L01P_Driver is limited record
      Holder : Holder_Type;
   end record;

   procedure Configure_Chip
     (This   : in out NRF24L01P_Driver;
      Config : Configuration);
   --  Configures microcircuit chip.

   function Read_Register
     (This : in out NRF24L01P_Driver;
      Name : Register_Name)
      return Register;
   --  Reads register from NRF24L01P_Driver chip by SPI

   procedure Write
     (This : in out NRF24L01P_Driver;
      Data : HAL.SPI.SPI_Data_8b);
   --  Write data to chip

   procedure Write_And_Read
     (This    : in out NRF24L01P_Driver;
      Command : HAL.SPI.SPI_Data_8b;
      Data    : out HAL.SPI.SPI_Data_8b);
   --  Read data from chip

   procedure Write_Register
     (This : in out NRF24L01P_Driver;
      Name : Register_Name;
      Data : Register);
   --  Writes register to NRF24L01P_Driver chip by SPI

   procedure Write_Command
     (This : in out NRF24L01P_Driver;
      Cmd  : Command);
   --  Writes command to NRF24L01P_Driver chip by SPI

   procedure Write_Command_And_Read
     (This : in out NRF24L01P_Driver;
      Cmd  : Command;
      Data : out HAL.SPI.SPI_Data_8b);
   --  Writes command to NRF24L01P_Driver chip by SPI and get the response

   procedure Config_Power_Rate
     (This  : in out NRF24L01P_Driver;
      Power : Amplifier_Power;
      Rate  : Air_Data_Rate);
   --  Sets amplifier power and air data rate

   function Get_RX_Data_Size
     (This : in out NRF24L01P_Driver;
      Pipe : RX_Pipe)
      return Payload_Length;
   --  Returns size we set for the pipe in non DPL mode

   procedure CE_High (This : NRF24L01P_Driver);
   procedure CE_Low  (This : NRF24L01P_Driver);
   --  High/Low CE pin to Activate/Deactivate TX/RX operations

   procedure CSN_High (This : NRF24L01P_Driver);
   procedure CSN_Low  (This : NRF24L01P_Driver);
   --  High/Low CSN pin in SOFTWARE slave selection mode.

end NRF24L01P;
