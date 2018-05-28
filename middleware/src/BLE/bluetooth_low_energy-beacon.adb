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


package body Bluetooth_Low_Energy.Beacon is

   function Make_Beacon_Packet (MAC          : UInt8_Array;
                                UUID         : BLE_UUID;
                                Major, Minor : UInt16;
                                Power        : Integer_8)
                                return BLE_Packet
   is
      Pck : BLE_Packet;
   begin
      Set_Header (Pck, 16#42#);
      --  MAC
      Push (Pck, MAC);
      --  Flag length
      Push (Pck, UInt8 (2));
      --  Flag type
      Push (Pck, UInt8 (1));
      --  Flag Content
      Push (Pck, UInt8 (6));
      --  Data length
      Push (Pck, UInt8 (16#1A#));
      --  Data type
      Push (Pck, UInt8 (16#FF#));
      --  Data header
      Push (Pck, (16#4C#, 16#00#, 16#02#, 16#15#));
      --  UUID
      Push_UUID (Pck, UUID);
      --  Major
      Push (Pck, Major);
      --  Minor
      Push (Pck, Minor);
      --  Power
      Push (Pck, Power);
      return Pck;
   end Make_Beacon_Packet;

end Bluetooth_Low_Energy.Beacon;
