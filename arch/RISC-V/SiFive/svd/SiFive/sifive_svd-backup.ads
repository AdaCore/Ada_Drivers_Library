--  This spec has been automatically generated from SiFive.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SiFive_SVD.BACKUP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  

   type Backup_Registers is array (0 .. 31) of HAL.UInt32;

   -----------------
   -- Peripherals --
   -----------------

   --  Backup Registers.
   type BACKUP_Peripheral is record
      Backup : aliased Backup_Registers;
   end record
     with Volatile;

   for BACKUP_Peripheral use record
      Backup at 0 range 0 .. 1023;
   end record;

   --  Backup Registers.
   BACKUP_Periph : aliased BACKUP_Peripheral
     with Import, Address => BACKUP_Base;

end SiFive_SVD.BACKUP;
