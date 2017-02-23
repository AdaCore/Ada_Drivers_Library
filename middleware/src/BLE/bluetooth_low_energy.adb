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

   function Make_UUID (UUID : BLE_16UInt8s_UUID) return BLE_UUID is
   begin
      return (Kind          => UUID_16UInt8s,
              UUID_16_UInt8s => UUID);
   end Make_UUID;

end Bluetooth_Low_Energy;
