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

with Ada.Unchecked_Conversion;

package body Bluetooth_Low_Energy.Packets is

   function As_UInt8 is new Ada.Unchecked_Conversion (Interfaces.Integer_8,
                                                      UInt8);
   -----------------
   -- Get_Address --
   -----------------

   function Memory_Address (This : BLE_Packet) return System.Address is
   begin
      return This'Address;
   end Memory_Address;

   ----------------
   -- Set_Header --
   ----------------

   procedure Set_Header (This   : in out BLE_Packet;
                         Header : UInt8)
   is
   begin
      This.Header := Header;
   end Set_Header;

   ----------
   -- Push --
   ----------

   procedure Push
     (This : in out BLE_Packet;
      Data : UInt8)
   is
      Next_Index : constant Integer :=
        This.Data'First + Integer (This.Packet_Length);
   begin
      if This.Packet_Length < BLE_PACKET_MAX_PAYLOAD then
         This.Data (Next_Index) := Data;
         This.Packet_Length := This.Packet_Length + 1;
      end if;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out BLE_Packet;
                   Data : Interfaces.Integer_8)
   is
   begin
      Push (This, As_UInt8 (Data));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (This : in out BLE_Packet;
      Data : UInt16)
   is
      type As_Array is array (1 .. 2) of UInt8 with Pack, Size => 16;

      Data_As_Array : As_Array;
      for Data_As_Array'Address use Data'Address;
   begin
      Push (This, Data_As_Array (1));
      Push (This, Data_As_Array (2));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (This : in out BLE_Packet;
      Data : UInt32)
   is
      type As_Array is array (1 .. 4) of UInt8 with Pack, Size => 32;

      Data_As_Array : As_Array;
      for Data_As_Array'Address use Data'Address;
   begin
      Push (This, Data_As_Array (1));
      Push (This, Data_As_Array (2));
      Push (This, Data_As_Array (3));
      Push (This, Data_As_Array (4));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (This : in out BLE_Packet;
      Data : UInt8_Array)
   is
   begin
      for Elt of Data loop
         Push (This, Elt);
      end loop;
   end Push;

   ---------------
   -- Push_UUID --
   ---------------

   procedure Push_UUID
     (This : in out BLE_Packet;
      UUID : BLE_UUID)
   is
   begin
      case UUID.Kind is
         when UUID_16bits =>
            Push (This, UUID.UUID_16);
         when UUID_32bits =>
            Push (This, UUID.UUID_32);
         when UUID_16UInt8s =>
            Push (This, UUID.UUID_16_UInt8s);
      end case;
   end Push_UUID;

end Bluetooth_Low_Energy.Packets;
