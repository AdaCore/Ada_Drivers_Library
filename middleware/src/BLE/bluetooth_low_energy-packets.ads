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

with System;
with Interfaces;

package Bluetooth_Low_Energy.Packets is

   type BLE_Packet is private;

   function Memory_Address (This : BLE_Packet) return System.Address;
   --  Return memory address of the radio data to be transmitted

   procedure Set_Header (This   : in out BLE_Packet;
                         Header : UInt8);

   procedure Push (This : in out BLE_Packet;
                   Data : UInt8);

   procedure Push (This : in out BLE_Packet;
                   Data : Interfaces.Integer_8);

   procedure Push (This : in out BLE_Packet;
                   Data : UInt16);

   procedure Push (This : in out BLE_Packet;
                   Data : UInt32);

   procedure Push (This : in out BLE_Packet;
                   Data : UInt8_Array);

   procedure Push_UUID (This : in out BLE_Packet;
                        UUID : BLE_UUID);
private

   BLE_PACKET_MIC_SIZE    : constant := 4;
   --  Size of Message integrity check (MIC) field

   BLE_PACKET_MAX_PAYLOAD : constant := 37;
   --  Maximum size of BLE payload (without header, MIC or CRC)

   type BLE_Data is array (1 .. BLE_PACKET_MAX_PAYLOAD + BLE_PACKET_MIC_SIZE)
     of UInt8 with Pack;
   --  BLE Payload plus optional MIC field

   type BLE_Packet is record
      Header        : UInt8;
      Packet_Length : UInt8 := 0;
      Data          : BLE_Data;
   end record with Pack;


end Bluetooth_Low_Energy.Packets;
