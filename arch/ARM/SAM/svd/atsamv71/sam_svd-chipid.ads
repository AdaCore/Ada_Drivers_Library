--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.CHIPID is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CHIPID_CHIPID_CIDR_VERSION_Field is HAL.UInt5;

   --  Embedded Processor
   type CHIPID_CIDR_EPROC_Field is
     (
      --  Cortex-M7
      Samx7,
      --  ARM946ES
      Arm946Es,
      --  ARM7TDMI
      Arm7Tdmi,
      --  Cortex-M3
      Cm3,
      --  ARM920T
      Arm920T,
      --  ARM926EJS
      Arm926Ejs,
      --  Cortex-A5
      Ca5,
      --  Cortex-M4
      Cm4)
     with Size => 3;
   for CHIPID_CIDR_EPROC_Field use
     (Samx7 => 0,
      Arm946Es => 1,
      Arm7Tdmi => 2,
      Cm3 => 3,
      Arm920T => 4,
      Arm926Ejs => 5,
      Ca5 => 6,
      Cm4 => 7);

   --  Nonvolatile Program Memory Size
   type CHIPID_CIDR_NVPSIZ_Field is
     (
      --  None
      None,
      --  8 Kbytes
      Val_8K,
      --  16 Kbytes
      Val_16K,
      --  32 Kbytes
      Val_32K,
      --  64 Kbytes
      Val_64K,
      --  128 Kbytes
      Val_128K,
      --  160 Kbytes
      Val_160K,
      --  256 Kbytes
      Val_256K,
      --  512 Kbytes
      Val_512K,
      --  1024 Kbytes
      Val_1024K,
      --  2048 Kbytes
      Val_2048K)
     with Size => 4;
   for CHIPID_CIDR_NVPSIZ_Field use
     (None => 0,
      Val_8K => 1,
      Val_16K => 2,
      Val_32K => 3,
      Val_64K => 5,
      Val_128K => 7,
      Val_160K => 8,
      Val_256K => 9,
      Val_512K => 10,
      Val_1024K => 12,
      Val_2048K => 14);

   --  Second Nonvolatile Program Memory Size
   type CHIPID_CIDR_NVPSIZ2_Field is
     (
      --  None
      None,
      --  8 Kbytes
      Val_8K,
      --  16 Kbytes
      Val_16K,
      --  32 Kbytes
      Val_32K,
      --  64 Kbytes
      Val_64K,
      --  128 Kbytes
      Val_128K,
      --  256 Kbytes
      Val_256K,
      --  512 Kbytes
      Val_512K,
      --  1024 Kbytes
      Val_1024K,
      --  2048 Kbytes
      Val_2048K)
     with Size => 4;
   for CHIPID_CIDR_NVPSIZ2_Field use
     (None => 0,
      Val_8K => 1,
      Val_16K => 2,
      Val_32K => 3,
      Val_64K => 5,
      Val_128K => 7,
      Val_256K => 9,
      Val_512K => 10,
      Val_1024K => 12,
      Val_2048K => 14);

   --  Internal SRAM Size
   type CHIPID_CIDR_SRAMSIZ_Field is
     (
      --  48 Kbytes
      Val_48K,
      --  192 Kbytes
      Val_192K,
      --  384 Kbytes
      Val_384K,
      --  6 Kbytes
      Val_6K,
      --  24 Kbytes
      Val_24K,
      --  4 Kbytes
      Val_4K,
      --  80 Kbytes
      Val_80K,
      --  160 Kbytes
      Val_160K,
      --  8 Kbytes
      Val_8K,
      --  16 Kbytes
      Val_16K,
      --  32 Kbytes
      Val_32K,
      --  64 Kbytes
      Val_64K,
      --  128 Kbytes
      Val_128K,
      --  256 Kbytes
      Val_256K,
      --  96 Kbytes
      Val_96K,
      --  512 Kbytes
      Val_512K)
     with Size => 4;
   for CHIPID_CIDR_SRAMSIZ_Field use
     (Val_48K => 0,
      Val_192K => 1,
      Val_384K => 2,
      Val_6K => 3,
      Val_24K => 4,
      Val_4K => 5,
      Val_80K => 6,
      Val_160K => 7,
      Val_8K => 8,
      Val_16K => 9,
      Val_32K => 10,
      Val_64K => 11,
      Val_128K => 12,
      Val_256K => 13,
      Val_96K => 14,
      Val_512K => 15);

   --  Architecture Identifier
   type CHIPID_CIDR_ARCH_Field is
     (
      --  SAM V71
      Samv71)
     with Size => 8;
   for CHIPID_CIDR_ARCH_Field use
     (Samv71 => 18);

   --  Nonvolatile Program Memory Type
   type CHIPID_CIDR_NVPTYP_Field is
     (
      --  ROM
      Rom,
      --  ROMless or on-chip Flash
      Romless,
      --  Embedded Flash Memory
      Flash,
      --  ROM and Embedded Flash Memory- NVPSIZ is ROM size- NVPSIZ2 is Flash
      --  size
      Rom_Flash,
      --  SRAM emulating ROM
      Sram)
     with Size => 3;
   for CHIPID_CIDR_NVPTYP_Field use
     (Rom => 0,
      Romless => 1,
      Flash => 2,
      Rom_Flash => 3,
      Sram => 4);

   --  Chip ID Register
   type CHIPID_CHIPID_CIDR_Register is record
      --  Read-only. Version of the Device
      VERSION : CHIPID_CHIPID_CIDR_VERSION_Field;
      --  Read-only. Embedded Processor
      EPROC   : CHIPID_CIDR_EPROC_Field;
      --  Read-only. Nonvolatile Program Memory Size
      NVPSIZ  : CHIPID_CIDR_NVPSIZ_Field;
      --  Read-only. Second Nonvolatile Program Memory Size
      NVPSIZ2 : CHIPID_CIDR_NVPSIZ2_Field;
      --  Read-only. Internal SRAM Size
      SRAMSIZ : CHIPID_CIDR_SRAMSIZ_Field;
      --  Read-only. Architecture Identifier
      ARCH    : CHIPID_CIDR_ARCH_Field;
      --  Read-only. Nonvolatile Program Memory Type
      NVPTYP  : CHIPID_CIDR_NVPTYP_Field;
      --  Read-only. Extension Flag
      EXT     : Boolean;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHIPID_CHIPID_CIDR_Register use record
      VERSION at 0 range 0 .. 4;
      EPROC   at 0 range 5 .. 7;
      NVPSIZ  at 0 range 8 .. 11;
      NVPSIZ2 at 0 range 12 .. 15;
      SRAMSIZ at 0 range 16 .. 19;
      ARCH    at 0 range 20 .. 27;
      NVPTYP  at 0 range 28 .. 30;
      EXT     at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Chip Identifier
   type CHIPID_Peripheral is record
      --  Chip ID Register
      CHIPID_CIDR : aliased CHIPID_CHIPID_CIDR_Register;
      --  Chip ID Extension Register
      CHIPID_EXID : aliased HAL.UInt32;
   end record
     with Volatile;

   for CHIPID_Peripheral use record
      CHIPID_CIDR at 16#0# range 0 .. 31;
      CHIPID_EXID at 16#4# range 0 .. 31;
   end record;

   --  Chip Identifier
   CHIPID_Periph : aliased CHIPID_Peripheral
     with Import, Address => System'To_Address (16#400E0940#);

end SAM_SVD.CHIPID;
