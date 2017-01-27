package body Bluetooth_Low_Energy is

   ---------------
   -- Make_UUID --
   ---------------

   function Make_UUID (UUID : UInt16) return BLE_UUID is
   begin
      return (Kind    => UUID_16bits,
              UUID_16 => UUID);
   end Make_UUID;

   ---------------
   -- Make_UUID --
   ---------------

   function Make_UUID (UUID : UInt32) return BLE_UUID is
   begin
      return (Kind    => UUID_32bits,
              UUID_32 => UUID);
   end Make_UUID;

   ---------------
   -- Make_UUID --
   ---------------

   function Make_UUID (UUID : BLE_16bytes_UUID) return BLE_UUID is
   begin
      return (Kind          => UUID_16bytes,
              UUID_16_Bytes => UUID);
   end Make_UUID;

end Bluetooth_Low_Energy;
