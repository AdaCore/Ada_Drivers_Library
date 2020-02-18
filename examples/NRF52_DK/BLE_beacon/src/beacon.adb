------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2016-2020, AdaCore                     --
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

with nRF.Tasks;
with nRF.Events;
with nRF.Radio;     use nRF.Radio;
with nRF.Clock;     use nRF.Clock;

with Bluetooth_Low_Energy.Packets; use Bluetooth_Low_Energy.Packets;
with Bluetooth_Low_Energy;         use Bluetooth_Low_Energy;
with Bluetooth_Low_Energy.Beacon;  use Bluetooth_Low_Energy.Beacon;

with HAL; use HAL;

package body Beacon is

   Current_Adv_Channel : BLE_Advertising_Channel_Number := 37;
   Beacon_Packet       : BLE_Packet;

   ----------------------
   -- Initialize_Radio --
   ----------------------

   procedure Initialize_Radio is
      Beacon_UUID : constant BLE_UUID :=
        Make_UUID ((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16));
   begin

      Beacon_Packet := Make_Beacon_Packet
        (MAC   => (16#A1#, 16#AD#, 16#A1#, 16#AD#, 16#A1#, 16#AD#),
         UUID  => Beacon_UUID,
         Major => 0,
         Minor => 0,
         Power => 0);

      --  Setup high frequency clock for BLE transmission
      Set_High_Freq_Source (HFCLK_RC);
      Start_High_Freq;
      while not High_Freq_Running loop
         null;
      end loop;

      --  Setup radio module for BLE
      Setup_For_Bluetooth_Low_Energy;

      --  Set BLE advertising address
      Set_Logic_Addresses (Base0 => 16#89_BE_D6_00#,
                           Base1 => 16#00_00_00_00#,
                           Base_Length_In_Byte => 3,
                           AP0   => 16#8E#,
                           AP1   => 16#00#,
                           AP2   => 16#00#,
                           AP3   => 16#00#,
                           AP4   => 16#00#,
                           AP5   => 16#00#,
                           AP6   => 16#00#,
                           AP7   => 16#00#);

      --  Select logic address
      Set_TX_Address (0);

      --  Transmission power
      Set_Power (Zero_Dbm);

      --  Enable shortcuts for easier radio operation
      Enable_Shortcut (Ready_To_Start);
      Enable_Shortcut (End_To_Disable);
   end Initialize_Radio;

   ------------------------
   -- Send_Beacon_Packet --
   ------------------------

   procedure Send_Beacon_Packet is
   begin
      Configure_Whitening (True, UInt6 (Current_Adv_Channel));

      Set_Frequency
        (Radio_Frequency_MHz (Channel_Frequency (Current_Adv_Channel)));

      if Current_Adv_Channel /= BLE_Advertising_Channel_Number'Last then
         Current_Adv_Channel := Current_Adv_Channel + 1;
      else
         Current_Adv_Channel := BLE_Advertising_Channel_Number'First;
      end if;

      --  Set TX packet address
      Set_Packet (Memory_Address (Beacon_Packet));

      --  Clear all events
      nRF.Events.Clear (nRF.Events.Radio_DISABLED);
      nRF.Events.Clear (nRF.Events.Radio_ADDRESS);
      nRF.Events.Clear (nRF.Events.Radio_PAYLOAD);
      nRF.Events.Clear (nRF.Events.Radio_END);

      --  Start transmission
      nRF.Tasks.Trigger (nRF.Tasks.Radio_TXEN);

      --  Wait for end of transmission
      while not nRF.Events.Triggered (nRF.Events.Radio_DISABLED) loop
         null;
      end loop;
   end Send_Beacon_Packet;

end Beacon;
