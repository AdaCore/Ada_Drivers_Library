--  This spec has been automatically generated from STM32F7x9.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.MDIOS is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype MDIOS_CR_PORT_ADDRESS_Field is HAL.UInt5;

   --  MDIOS configuration register
   type MDIOS_CR_Register is record
      --  Peripheral enable
      EN             : Boolean := False;
      --  Register write interrupt enable
      WRIE           : Boolean := False;
      --  Register Read Interrupt Enable
      RDIE           : Boolean := False;
      --  Error interrupt enable
      EIE            : Boolean := False;
      --  unspecified
      Reserved_4_6   : HAL.UInt3 := 16#0#;
      --  Disable Preamble Check
      DPC            : Boolean := False;
      --  Slaves's address
      PORT_ADDRESS   : MDIOS_CR_PORT_ADDRESS_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_CR_Register use record
      EN             at 0 range 0 .. 0;
      WRIE           at 0 range 1 .. 1;
      RDIE           at 0 range 2 .. 2;
      EIE            at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      DPC            at 0 range 7 .. 7;
      PORT_ADDRESS   at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  MDIOS status register
   type MDIOS_SR_Register is record
      --  Read-only. Preamble error flag
      PERF          : Boolean;
      --  Read-only. Start error flag
      SERF          : Boolean;
      --  Read-only. Turnaround error flag
      TERF          : Boolean;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_SR_Register use record
      PERF          at 0 range 0 .. 0;
      SERF          at 0 range 1 .. 1;
      TERF          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  MDIOS clear flag register
   type MDIOS_CLRFR_Register is record
      --  Clear the preamble error flag
      CPERF         : Boolean := False;
      --  Clear the start error flag
      CSERF         : Boolean := False;
      --  Clear the turnaround error flag
      CTERF         : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_CLRFR_Register use record
      CPERF         at 0 range 0 .. 0;
      CSERF         at 0 range 1 .. 1;
      CTERF         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype MDIOS_DINR0_DIN0_Field is HAL.UInt16;

   --  MDIOS input data register 0
   type MDIOS_DINR0_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN0           : MDIOS_DINR0_DIN0_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR0_Register use record
      DIN0           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR1_DIN1_Field is HAL.UInt16;

   --  MDIOS input data register 1
   type MDIOS_DINR1_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN1           : MDIOS_DINR1_DIN1_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR1_Register use record
      DIN1           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR2_DIN2_Field is HAL.UInt16;

   --  MDIOS input data register 2
   type MDIOS_DINR2_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN2           : MDIOS_DINR2_DIN2_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR2_Register use record
      DIN2           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR3_DIN3_Field is HAL.UInt16;

   --  MDIOS input data register 3
   type MDIOS_DINR3_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN3           : MDIOS_DINR3_DIN3_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR3_Register use record
      DIN3           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR4_DIN4_Field is HAL.UInt16;

   --  MDIOS input data register 4
   type MDIOS_DINR4_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN4           : MDIOS_DINR4_DIN4_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR4_Register use record
      DIN4           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR5_DIN5_Field is HAL.UInt16;

   --  MDIOS input data register 5
   type MDIOS_DINR5_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN5           : MDIOS_DINR5_DIN5_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR5_Register use record
      DIN5           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR6_DIN6_Field is HAL.UInt16;

   --  MDIOS input data register 6
   type MDIOS_DINR6_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN6           : MDIOS_DINR6_DIN6_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR6_Register use record
      DIN6           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR7_DIN7_Field is HAL.UInt16;

   --  MDIOS input data register 7
   type MDIOS_DINR7_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN7           : MDIOS_DINR7_DIN7_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR7_Register use record
      DIN7           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR8_DIN8_Field is HAL.UInt16;

   --  MDIOS input data register 8
   type MDIOS_DINR8_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN8           : MDIOS_DINR8_DIN8_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR8_Register use record
      DIN8           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR9_DIN9_Field is HAL.UInt16;

   --  MDIOS input data register 9
   type MDIOS_DINR9_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN9           : MDIOS_DINR9_DIN9_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR9_Register use record
      DIN9           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR10_DIN10_Field is HAL.UInt16;

   --  MDIOS input data register 10
   type MDIOS_DINR10_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN10          : MDIOS_DINR10_DIN10_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR10_Register use record
      DIN10          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR11_DIN11_Field is HAL.UInt16;

   --  MDIOS input data register 11
   type MDIOS_DINR11_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN11          : MDIOS_DINR11_DIN11_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR11_Register use record
      DIN11          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR12_DIN12_Field is HAL.UInt16;

   --  MDIOS input data register 12
   type MDIOS_DINR12_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN12          : MDIOS_DINR12_DIN12_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR12_Register use record
      DIN12          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR13_DIN13_Field is HAL.UInt16;

   --  MDIOS input data register 13
   type MDIOS_DINR13_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN13          : MDIOS_DINR13_DIN13_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR13_Register use record
      DIN13          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR14_DIN14_Field is HAL.UInt16;

   --  MDIOS input data register 14
   type MDIOS_DINR14_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN14          : MDIOS_DINR14_DIN14_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR14_Register use record
      DIN14          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR15_DIN15_Field is HAL.UInt16;

   --  MDIOS input data register 15
   type MDIOS_DINR15_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN15          : MDIOS_DINR15_DIN15_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR15_Register use record
      DIN15          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR16_DIN16_Field is HAL.UInt16;

   --  MDIOS input data register 16
   type MDIOS_DINR16_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN16          : MDIOS_DINR16_DIN16_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR16_Register use record
      DIN16          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR17_DIN17_Field is HAL.UInt16;

   --  MDIOS input data register 17
   type MDIOS_DINR17_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN17          : MDIOS_DINR17_DIN17_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR17_Register use record
      DIN17          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR18_DIN18_Field is HAL.UInt16;

   --  MDIOS input data register 18
   type MDIOS_DINR18_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN18          : MDIOS_DINR18_DIN18_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR18_Register use record
      DIN18          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR19_DIN19_Field is HAL.UInt16;

   --  MDIOS input data register 19
   type MDIOS_DINR19_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN19          : MDIOS_DINR19_DIN19_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR19_Register use record
      DIN19          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR20_DIN20_Field is HAL.UInt16;

   --  MDIOS input data register 20
   type MDIOS_DINR20_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN20          : MDIOS_DINR20_DIN20_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR20_Register use record
      DIN20          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR21_DIN21_Field is HAL.UInt16;

   --  MDIOS input data register 21
   type MDIOS_DINR21_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN21          : MDIOS_DINR21_DIN21_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR21_Register use record
      DIN21          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR22_DIN22_Field is HAL.UInt16;

   --  MDIOS input data register 22
   type MDIOS_DINR22_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN22          : MDIOS_DINR22_DIN22_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR22_Register use record
      DIN22          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR23_DIN23_Field is HAL.UInt16;

   --  MDIOS input data register 23
   type MDIOS_DINR23_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN23          : MDIOS_DINR23_DIN23_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR23_Register use record
      DIN23          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR24_DIN24_Field is HAL.UInt16;

   --  MDIOS input data register 24
   type MDIOS_DINR24_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN24          : MDIOS_DINR24_DIN24_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR24_Register use record
      DIN24          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR25_DIN25_Field is HAL.UInt16;

   --  MDIOS input data register 25
   type MDIOS_DINR25_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN25          : MDIOS_DINR25_DIN25_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR25_Register use record
      DIN25          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR26_DIN26_Field is HAL.UInt16;

   --  MDIOS input data register 26
   type MDIOS_DINR26_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN26          : MDIOS_DINR26_DIN26_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR26_Register use record
      DIN26          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR27_DIN27_Field is HAL.UInt16;

   --  MDIOS input data register 27
   type MDIOS_DINR27_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN27          : MDIOS_DINR27_DIN27_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR27_Register use record
      DIN27          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR28_DIN28_Field is HAL.UInt16;

   --  MDIOS input data register 28
   type MDIOS_DINR28_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN28          : MDIOS_DINR28_DIN28_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR28_Register use record
      DIN28          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR29_DIN29_Field is HAL.UInt16;

   --  MDIOS input data register 29
   type MDIOS_DINR29_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN29          : MDIOS_DINR29_DIN29_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR29_Register use record
      DIN29          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR30_DIN30_Field is HAL.UInt16;

   --  MDIOS input data register 30
   type MDIOS_DINR30_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN30          : MDIOS_DINR30_DIN30_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR30_Register use record
      DIN30          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DINR31_DIN31_Field is HAL.UInt16;

   --  MDIOS input data register 31
   type MDIOS_DINR31_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN31          : MDIOS_DINR31_DIN31_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DINR31_Register use record
      DIN31          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR0_DOUT0_Field is HAL.UInt16;

   --  MDIOS output data register 0
   type MDIOS_DOUTR0_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT0          : MDIOS_DOUTR0_DOUT0_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR0_Register use record
      DOUT0          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR1_DOUT1_Field is HAL.UInt16;

   --  MDIOS output data register 1
   type MDIOS_DOUTR1_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT1          : MDIOS_DOUTR1_DOUT1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR1_Register use record
      DOUT1          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR2_DOUT2_Field is HAL.UInt16;

   --  MDIOS output data register 2
   type MDIOS_DOUTR2_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT2          : MDIOS_DOUTR2_DOUT2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR2_Register use record
      DOUT2          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR3_DOUT3_Field is HAL.UInt16;

   --  MDIOS output data register 3
   type MDIOS_DOUTR3_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT3          : MDIOS_DOUTR3_DOUT3_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR3_Register use record
      DOUT3          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR4_DOUT4_Field is HAL.UInt16;

   --  MDIOS output data register 4
   type MDIOS_DOUTR4_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT4          : MDIOS_DOUTR4_DOUT4_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR4_Register use record
      DOUT4          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR5_DOUT5_Field is HAL.UInt16;

   --  MDIOS output data register 5
   type MDIOS_DOUTR5_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT5          : MDIOS_DOUTR5_DOUT5_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR5_Register use record
      DOUT5          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR6_DOUT6_Field is HAL.UInt16;

   --  MDIOS output data register 6
   type MDIOS_DOUTR6_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT6          : MDIOS_DOUTR6_DOUT6_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR6_Register use record
      DOUT6          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR7_DOUT7_Field is HAL.UInt16;

   --  MDIOS output data register 7
   type MDIOS_DOUTR7_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT7          : MDIOS_DOUTR7_DOUT7_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR7_Register use record
      DOUT7          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR8_DOUT8_Field is HAL.UInt16;

   --  MDIOS output data register 8
   type MDIOS_DOUTR8_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT8          : MDIOS_DOUTR8_DOUT8_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR8_Register use record
      DOUT8          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR9_DOUT9_Field is HAL.UInt16;

   --  MDIOS output data register 9
   type MDIOS_DOUTR9_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT9          : MDIOS_DOUTR9_DOUT9_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR9_Register use record
      DOUT9          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR10_DOUT10_Field is HAL.UInt16;

   --  MDIOS output data register 10
   type MDIOS_DOUTR10_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT10         : MDIOS_DOUTR10_DOUT10_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR10_Register use record
      DOUT10         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR11_DOUT11_Field is HAL.UInt16;

   --  MDIOS output data register 11
   type MDIOS_DOUTR11_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT11         : MDIOS_DOUTR11_DOUT11_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR11_Register use record
      DOUT11         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR12_DOUT12_Field is HAL.UInt16;

   --  MDIOS output data register 12
   type MDIOS_DOUTR12_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT12         : MDIOS_DOUTR12_DOUT12_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR12_Register use record
      DOUT12         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR13_DOUT13_Field is HAL.UInt16;

   --  MDIOS output data register 13
   type MDIOS_DOUTR13_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT13         : MDIOS_DOUTR13_DOUT13_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR13_Register use record
      DOUT13         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR14_DOUT14_Field is HAL.UInt16;

   --  MDIOS output data register 14
   type MDIOS_DOUTR14_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT14         : MDIOS_DOUTR14_DOUT14_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR14_Register use record
      DOUT14         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR15_DOUT15_Field is HAL.UInt16;

   --  MDIOS output data register 15
   type MDIOS_DOUTR15_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT15         : MDIOS_DOUTR15_DOUT15_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR15_Register use record
      DOUT15         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR16_DOUT16_Field is HAL.UInt16;

   --  MDIOS output data register 16
   type MDIOS_DOUTR16_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT16         : MDIOS_DOUTR16_DOUT16_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR16_Register use record
      DOUT16         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR17_DOUT17_Field is HAL.UInt16;

   --  MDIOS output data register 17
   type MDIOS_DOUTR17_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT17         : MDIOS_DOUTR17_DOUT17_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR17_Register use record
      DOUT17         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR18_DOUT18_Field is HAL.UInt16;

   --  MDIOS output data register 18
   type MDIOS_DOUTR18_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT18         : MDIOS_DOUTR18_DOUT18_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR18_Register use record
      DOUT18         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR19_DOUT19_Field is HAL.UInt16;

   --  MDIOS output data register 19
   type MDIOS_DOUTR19_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT19         : MDIOS_DOUTR19_DOUT19_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR19_Register use record
      DOUT19         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR20_DOUT20_Field is HAL.UInt16;

   --  MDIOS output data register 20
   type MDIOS_DOUTR20_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT20         : MDIOS_DOUTR20_DOUT20_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR20_Register use record
      DOUT20         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR21_DOUT21_Field is HAL.UInt16;

   --  MDIOS output data register 21
   type MDIOS_DOUTR21_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT21         : MDIOS_DOUTR21_DOUT21_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR21_Register use record
      DOUT21         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR22_DOUT22_Field is HAL.UInt16;

   --  MDIOS output data register 22
   type MDIOS_DOUTR22_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT22         : MDIOS_DOUTR22_DOUT22_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR22_Register use record
      DOUT22         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR23_DOUT23_Field is HAL.UInt16;

   --  MDIOS output data register 23
   type MDIOS_DOUTR23_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT23         : MDIOS_DOUTR23_DOUT23_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR23_Register use record
      DOUT23         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR24_DOUT24_Field is HAL.UInt16;

   --  MDIOS output data register 24
   type MDIOS_DOUTR24_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT24         : MDIOS_DOUTR24_DOUT24_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR24_Register use record
      DOUT24         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR25_DOUT25_Field is HAL.UInt16;

   --  MDIOS output data register 25
   type MDIOS_DOUTR25_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT25         : MDIOS_DOUTR25_DOUT25_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR25_Register use record
      DOUT25         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR26_DOUT26_Field is HAL.UInt16;

   --  MDIOS output data register 26
   type MDIOS_DOUTR26_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT26         : MDIOS_DOUTR26_DOUT26_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR26_Register use record
      DOUT26         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR27_DOUT27_Field is HAL.UInt16;

   --  MDIOS output data register 27
   type MDIOS_DOUTR27_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT27         : MDIOS_DOUTR27_DOUT27_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR27_Register use record
      DOUT27         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR28_DOUT28_Field is HAL.UInt16;

   --  MDIOS output data register 28
   type MDIOS_DOUTR28_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT28         : MDIOS_DOUTR28_DOUT28_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR28_Register use record
      DOUT28         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR29_DOUT29_Field is HAL.UInt16;

   --  MDIOS output data register 29
   type MDIOS_DOUTR29_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT29         : MDIOS_DOUTR29_DOUT29_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR29_Register use record
      DOUT29         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR30_DOUT30_Field is HAL.UInt16;

   --  MDIOS output data register 30
   type MDIOS_DOUTR30_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT30         : MDIOS_DOUTR30_DOUT30_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR30_Register use record
      DOUT30         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDIOS_DOUTR31_DOUT31_Field is HAL.UInt16;

   --  MDIOS output data register 31
   type MDIOS_DOUTR31_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT31         : MDIOS_DOUTR31_DOUT31_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIOS_DOUTR31_Register use record
      DOUT31         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Management data input/output slave
   type MDIOS_Peripheral is record
      --  MDIOS configuration register
      MDIOS_CR      : aliased MDIOS_CR_Register;
      --  MDIOS write flag register
      MDIOS_WRFR    : aliased HAL.UInt32;
      --  MDIOS clear write flag register
      MDIOS_CWRFR   : aliased HAL.UInt32;
      --  MDIOS read flag register
      MDIOS_RDFR    : aliased HAL.UInt32;
      --  MDIOS clear read flag register
      MDIOS_CRDFR   : aliased HAL.UInt32;
      --  MDIOS status register
      MDIOS_SR      : aliased MDIOS_SR_Register;
      --  MDIOS clear flag register
      MDIOS_CLRFR   : aliased MDIOS_CLRFR_Register;
      --  MDIOS input data register 0
      MDIOS_DINR0   : aliased MDIOS_DINR0_Register;
      --  MDIOS input data register 1
      MDIOS_DINR1   : aliased MDIOS_DINR1_Register;
      --  MDIOS input data register 2
      MDIOS_DINR2   : aliased MDIOS_DINR2_Register;
      --  MDIOS input data register 3
      MDIOS_DINR3   : aliased MDIOS_DINR3_Register;
      --  MDIOS input data register 4
      MDIOS_DINR4   : aliased MDIOS_DINR4_Register;
      --  MDIOS input data register 5
      MDIOS_DINR5   : aliased MDIOS_DINR5_Register;
      --  MDIOS input data register 6
      MDIOS_DINR6   : aliased MDIOS_DINR6_Register;
      --  MDIOS input data register 7
      MDIOS_DINR7   : aliased MDIOS_DINR7_Register;
      --  MDIOS input data register 8
      MDIOS_DINR8   : aliased MDIOS_DINR8_Register;
      --  MDIOS input data register 9
      MDIOS_DINR9   : aliased MDIOS_DINR9_Register;
      --  MDIOS input data register 10
      MDIOS_DINR10  : aliased MDIOS_DINR10_Register;
      --  MDIOS input data register 11
      MDIOS_DINR11  : aliased MDIOS_DINR11_Register;
      --  MDIOS input data register 12
      MDIOS_DINR12  : aliased MDIOS_DINR12_Register;
      --  MDIOS input data register 13
      MDIOS_DINR13  : aliased MDIOS_DINR13_Register;
      --  MDIOS input data register 14
      MDIOS_DINR14  : aliased MDIOS_DINR14_Register;
      --  MDIOS input data register 15
      MDIOS_DINR15  : aliased MDIOS_DINR15_Register;
      --  MDIOS input data register 16
      MDIOS_DINR16  : aliased MDIOS_DINR16_Register;
      --  MDIOS input data register 17
      MDIOS_DINR17  : aliased MDIOS_DINR17_Register;
      --  MDIOS input data register 18
      MDIOS_DINR18  : aliased MDIOS_DINR18_Register;
      --  MDIOS input data register 19
      MDIOS_DINR19  : aliased MDIOS_DINR19_Register;
      --  MDIOS input data register 20
      MDIOS_DINR20  : aliased MDIOS_DINR20_Register;
      --  MDIOS input data register 21
      MDIOS_DINR21  : aliased MDIOS_DINR21_Register;
      --  MDIOS input data register 22
      MDIOS_DINR22  : aliased MDIOS_DINR22_Register;
      --  MDIOS input data register 23
      MDIOS_DINR23  : aliased MDIOS_DINR23_Register;
      --  MDIOS input data register 24
      MDIOS_DINR24  : aliased MDIOS_DINR24_Register;
      --  MDIOS input data register 25
      MDIOS_DINR25  : aliased MDIOS_DINR25_Register;
      --  MDIOS input data register 26
      MDIOS_DINR26  : aliased MDIOS_DINR26_Register;
      --  MDIOS input data register 27
      MDIOS_DINR27  : aliased MDIOS_DINR27_Register;
      --  MDIOS input data register 28
      MDIOS_DINR28  : aliased MDIOS_DINR28_Register;
      --  MDIOS input data register 29
      MDIOS_DINR29  : aliased MDIOS_DINR29_Register;
      --  MDIOS input data register 30
      MDIOS_DINR30  : aliased MDIOS_DINR30_Register;
      --  MDIOS input data register 31
      MDIOS_DINR31  : aliased MDIOS_DINR31_Register;
      --  MDIOS output data register 0
      MDIOS_DOUTR0  : aliased MDIOS_DOUTR0_Register;
      --  MDIOS output data register 1
      MDIOS_DOUTR1  : aliased MDIOS_DOUTR1_Register;
      --  MDIOS output data register 2
      MDIOS_DOUTR2  : aliased MDIOS_DOUTR2_Register;
      --  MDIOS output data register 3
      MDIOS_DOUTR3  : aliased MDIOS_DOUTR3_Register;
      --  MDIOS output data register 4
      MDIOS_DOUTR4  : aliased MDIOS_DOUTR4_Register;
      --  MDIOS output data register 5
      MDIOS_DOUTR5  : aliased MDIOS_DOUTR5_Register;
      --  MDIOS output data register 6
      MDIOS_DOUTR6  : aliased MDIOS_DOUTR6_Register;
      --  MDIOS output data register 7
      MDIOS_DOUTR7  : aliased MDIOS_DOUTR7_Register;
      --  MDIOS output data register 8
      MDIOS_DOUTR8  : aliased MDIOS_DOUTR8_Register;
      --  MDIOS output data register 9
      MDIOS_DOUTR9  : aliased MDIOS_DOUTR9_Register;
      --  MDIOS output data register 10
      MDIOS_DOUTR10 : aliased MDIOS_DOUTR10_Register;
      --  MDIOS output data register 11
      MDIOS_DOUTR11 : aliased MDIOS_DOUTR11_Register;
      --  MDIOS output data register 12
      MDIOS_DOUTR12 : aliased MDIOS_DOUTR12_Register;
      --  MDIOS output data register 13
      MDIOS_DOUTR13 : aliased MDIOS_DOUTR13_Register;
      --  MDIOS output data register 14
      MDIOS_DOUTR14 : aliased MDIOS_DOUTR14_Register;
      --  MDIOS output data register 15
      MDIOS_DOUTR15 : aliased MDIOS_DOUTR15_Register;
      --  MDIOS output data register 16
      MDIOS_DOUTR16 : aliased MDIOS_DOUTR16_Register;
      --  MDIOS output data register 17
      MDIOS_DOUTR17 : aliased MDIOS_DOUTR17_Register;
      --  MDIOS output data register 18
      MDIOS_DOUTR18 : aliased MDIOS_DOUTR18_Register;
      --  MDIOS output data register 19
      MDIOS_DOUTR19 : aliased MDIOS_DOUTR19_Register;
      --  MDIOS output data register 20
      MDIOS_DOUTR20 : aliased MDIOS_DOUTR20_Register;
      --  MDIOS output data register 21
      MDIOS_DOUTR21 : aliased MDIOS_DOUTR21_Register;
      --  MDIOS output data register 22
      MDIOS_DOUTR22 : aliased MDIOS_DOUTR22_Register;
      --  MDIOS output data register 23
      MDIOS_DOUTR23 : aliased MDIOS_DOUTR23_Register;
      --  MDIOS output data register 24
      MDIOS_DOUTR24 : aliased MDIOS_DOUTR24_Register;
      --  MDIOS output data register 25
      MDIOS_DOUTR25 : aliased MDIOS_DOUTR25_Register;
      --  MDIOS output data register 26
      MDIOS_DOUTR26 : aliased MDIOS_DOUTR26_Register;
      --  MDIOS output data register 27
      MDIOS_DOUTR27 : aliased MDIOS_DOUTR27_Register;
      --  MDIOS output data register 28
      MDIOS_DOUTR28 : aliased MDIOS_DOUTR28_Register;
      --  MDIOS output data register 29
      MDIOS_DOUTR29 : aliased MDIOS_DOUTR29_Register;
      --  MDIOS output data register 30
      MDIOS_DOUTR30 : aliased MDIOS_DOUTR30_Register;
      --  MDIOS output data register 31
      MDIOS_DOUTR31 : aliased MDIOS_DOUTR31_Register;
   end record
     with Volatile;

   for MDIOS_Peripheral use record
      MDIOS_CR      at 16#0# range 0 .. 31;
      MDIOS_WRFR    at 16#4# range 0 .. 31;
      MDIOS_CWRFR   at 16#8# range 0 .. 31;
      MDIOS_RDFR    at 16#C# range 0 .. 31;
      MDIOS_CRDFR   at 16#10# range 0 .. 31;
      MDIOS_SR      at 16#14# range 0 .. 31;
      MDIOS_CLRFR   at 16#18# range 0 .. 31;
      MDIOS_DINR0   at 16#1C# range 0 .. 31;
      MDIOS_DINR1   at 16#20# range 0 .. 31;
      MDIOS_DINR2   at 16#24# range 0 .. 31;
      MDIOS_DINR3   at 16#28# range 0 .. 31;
      MDIOS_DINR4   at 16#2C# range 0 .. 31;
      MDIOS_DINR5   at 16#30# range 0 .. 31;
      MDIOS_DINR6   at 16#34# range 0 .. 31;
      MDIOS_DINR7   at 16#38# range 0 .. 31;
      MDIOS_DINR8   at 16#3C# range 0 .. 31;
      MDIOS_DINR9   at 16#40# range 0 .. 31;
      MDIOS_DINR10  at 16#44# range 0 .. 31;
      MDIOS_DINR11  at 16#48# range 0 .. 31;
      MDIOS_DINR12  at 16#4C# range 0 .. 31;
      MDIOS_DINR13  at 16#50# range 0 .. 31;
      MDIOS_DINR14  at 16#54# range 0 .. 31;
      MDIOS_DINR15  at 16#58# range 0 .. 31;
      MDIOS_DINR16  at 16#5C# range 0 .. 31;
      MDIOS_DINR17  at 16#60# range 0 .. 31;
      MDIOS_DINR18  at 16#64# range 0 .. 31;
      MDIOS_DINR19  at 16#68# range 0 .. 31;
      MDIOS_DINR20  at 16#6C# range 0 .. 31;
      MDIOS_DINR21  at 16#70# range 0 .. 31;
      MDIOS_DINR22  at 16#74# range 0 .. 31;
      MDIOS_DINR23  at 16#78# range 0 .. 31;
      MDIOS_DINR24  at 16#7C# range 0 .. 31;
      MDIOS_DINR25  at 16#80# range 0 .. 31;
      MDIOS_DINR26  at 16#84# range 0 .. 31;
      MDIOS_DINR27  at 16#88# range 0 .. 31;
      MDIOS_DINR28  at 16#8C# range 0 .. 31;
      MDIOS_DINR29  at 16#90# range 0 .. 31;
      MDIOS_DINR30  at 16#94# range 0 .. 31;
      MDIOS_DINR31  at 16#98# range 0 .. 31;
      MDIOS_DOUTR0  at 16#9C# range 0 .. 31;
      MDIOS_DOUTR1  at 16#A0# range 0 .. 31;
      MDIOS_DOUTR2  at 16#A4# range 0 .. 31;
      MDIOS_DOUTR3  at 16#A8# range 0 .. 31;
      MDIOS_DOUTR4  at 16#AC# range 0 .. 31;
      MDIOS_DOUTR5  at 16#B0# range 0 .. 31;
      MDIOS_DOUTR6  at 16#B4# range 0 .. 31;
      MDIOS_DOUTR7  at 16#B8# range 0 .. 31;
      MDIOS_DOUTR8  at 16#BC# range 0 .. 31;
      MDIOS_DOUTR9  at 16#C0# range 0 .. 31;
      MDIOS_DOUTR10 at 16#C4# range 0 .. 31;
      MDIOS_DOUTR11 at 16#C8# range 0 .. 31;
      MDIOS_DOUTR12 at 16#CC# range 0 .. 31;
      MDIOS_DOUTR13 at 16#D0# range 0 .. 31;
      MDIOS_DOUTR14 at 16#D4# range 0 .. 31;
      MDIOS_DOUTR15 at 16#D8# range 0 .. 31;
      MDIOS_DOUTR16 at 16#DC# range 0 .. 31;
      MDIOS_DOUTR17 at 16#E0# range 0 .. 31;
      MDIOS_DOUTR18 at 16#E4# range 0 .. 31;
      MDIOS_DOUTR19 at 16#E8# range 0 .. 31;
      MDIOS_DOUTR20 at 16#EC# range 0 .. 31;
      MDIOS_DOUTR21 at 16#F0# range 0 .. 31;
      MDIOS_DOUTR22 at 16#F4# range 0 .. 31;
      MDIOS_DOUTR23 at 16#F8# range 0 .. 31;
      MDIOS_DOUTR24 at 16#FC# range 0 .. 31;
      MDIOS_DOUTR25 at 16#100# range 0 .. 31;
      MDIOS_DOUTR26 at 16#104# range 0 .. 31;
      MDIOS_DOUTR27 at 16#108# range 0 .. 31;
      MDIOS_DOUTR28 at 16#10C# range 0 .. 31;
      MDIOS_DOUTR29 at 16#110# range 0 .. 31;
      MDIOS_DOUTR30 at 16#114# range 0 .. 31;
      MDIOS_DOUTR31 at 16#118# range 0 .. 31;
   end record;

   --  Management data input/output slave
   MDIOS_Periph : aliased MDIOS_Peripheral
     with Import, Address => System'To_Address (16#40017800#);

end STM32_SVD.MDIOS;
