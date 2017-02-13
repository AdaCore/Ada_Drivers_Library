------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

with HAL;        use HAL;

package Bluetooth_Low_Energy is

   --------------
   -- Channels --
   --------------

   type BLE_Channel_Number is range 0 .. 39;
   subtype BLE_Data_Channel_Number is BLE_Channel_Number range 0 .. 36;
   subtype BLE_Advertising_Channel_Number is BLE_Channel_Number range 37 .. 39;

   type BLE_Frequency_MHz is range 2402 .. 2480;

   Channel_Frequency : array (BLE_Channel_Number) of BLE_Frequency_MHz :=
     (0 => 2404,  1 => 2406,  2 => 2408,  3 => 2410,  4 => 2412,
      5 => 2414,  6 => 2416,  7 => 2418,  8 => 2420,  9 => 2422,
     10 => 2424, 11 => 2428, 12 => 2430, 13 => 2432, 14 => 2434,
     15 => 2436, 16 => 2438, 17 => 2440, 18 => 2442, 19 => 2444,
     20 => 2446, 21 => 2448, 22 => 2450, 23 => 2452, 24 => 2454,
     25 => 2456, 26 => 2458, 27 => 2460, 28 => 2462, 29 => 2464,
     30 => 2466, 31 => 2468, 32 => 2470, 33 => 2472, 34 => 2474,
     35 => 2476, 36 => 2478, 37 => 2402, 38 => 2426, 39 => 2480);

   ----------
   -- UUID --
   ----------

   type BLE_UUID_Kind is (UUID_16bits, UUID_32bits, UUID_16UInt8s);

   subtype BLE_16UInt8s_UUID is UInt8_Array (1 .. 16);

   type BLE_UUID (Kind : BLE_UUID_Kind) is record
      case Kind is
         when UUID_16bits =>
            UUID_16 : UInt16;
         when UUID_32bits =>
            UUID_32 : UInt32;
         when UUID_16UInt8s =>
            UUID_16_UInt8s : BLE_16UInt8s_UUID;
      end case;
   end record;

   function Make_UUID (UUID : UInt16) return BLE_UUID;
   function Make_UUID (UUID : UInt32) return BLE_UUID;
   function Make_UUID (UUID : BLE_16UInt8s_UUID) return BLE_UUID;

end Bluetooth_Low_Energy;
