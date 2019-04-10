--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.NVIC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype NVIC_NVICSTIR_INTID_Field is HAL.UInt9;

   --  Software Trigger Interrupt Register
   type NVIC_NVICSTIR_Register is record
      --  Interrupt ID of the interrupt to trigger, in the range 0-239. For
      --  example, a value of 0x03 specifies interrupt IRQ3.
      INTID         : NVIC_NVICSTIR_INTID_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for NVIC_NVICSTIR_Register use record
      INTID         at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Nested Vectored Interrupt Controller
   type NVIC_Peripheral is record
      --  Interrupt Set Enable Register n
      NVICISER0 : aliased HAL.UInt32;
      --  Interrupt Set Enable Register n
      NVICISER1 : aliased HAL.UInt32;
      --  Interrupt Set Enable Register n
      NVICISER2 : aliased HAL.UInt32;
      --  Interrupt Set Enable Register n
      NVICISER3 : aliased HAL.UInt32;
      --  Interrupt Clear Enable Register n
      NVICICER0 : aliased HAL.UInt32;
      --  Interrupt Clear Enable Register n
      NVICICER1 : aliased HAL.UInt32;
      --  Interrupt Clear Enable Register n
      NVICICER2 : aliased HAL.UInt32;
      --  Interrupt Clear Enable Register n
      NVICICER3 : aliased HAL.UInt32;
      --  Interrupt Set Pending Register n
      NVICISPR0 : aliased HAL.UInt32;
      --  Interrupt Set Pending Register n
      NVICISPR1 : aliased HAL.UInt32;
      --  Interrupt Set Pending Register n
      NVICISPR2 : aliased HAL.UInt32;
      --  Interrupt Set Pending Register n
      NVICISPR3 : aliased HAL.UInt32;
      --  Interrupt Clear Pending Register n
      NVICICPR0 : aliased HAL.UInt32;
      --  Interrupt Clear Pending Register n
      NVICICPR1 : aliased HAL.UInt32;
      --  Interrupt Clear Pending Register n
      NVICICPR2 : aliased HAL.UInt32;
      --  Interrupt Clear Pending Register n
      NVICICPR3 : aliased HAL.UInt32;
      --  Interrupt Active bit Register n
      NVICIABR0 : aliased HAL.UInt32;
      --  Interrupt Active bit Register n
      NVICIABR1 : aliased HAL.UInt32;
      --  Interrupt Active bit Register n
      NVICIABR2 : aliased HAL.UInt32;
      --  Interrupt Active bit Register n
      NVICIABR3 : aliased HAL.UInt32;
      --  Interrupt Priority Register n
      NVICIP0   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP1   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP2   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP3   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP4   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP5   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP6   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP7   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP8   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP9   : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP10  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP11  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP12  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP13  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP14  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP15  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP16  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP17  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP18  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP19  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP20  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP21  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP22  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP23  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP24  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP25  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP26  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP27  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP28  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP29  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP30  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP31  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP32  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP33  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP34  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP35  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP36  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP37  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP38  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP39  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP40  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP41  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP42  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP43  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP44  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP45  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP46  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP47  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP48  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP49  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP50  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP51  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP52  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP53  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP54  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP55  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP56  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP57  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP58  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP59  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP60  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP61  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP62  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP63  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP64  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP65  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP66  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP67  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP68  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP69  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP70  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP71  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP72  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP73  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP74  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP75  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP76  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP77  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP78  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP79  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP80  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP81  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP82  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP83  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP84  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP85  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP86  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP87  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP88  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP89  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP90  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP91  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP92  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP93  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP94  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP95  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP96  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP97  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP98  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP99  : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP100 : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP101 : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP102 : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP103 : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP104 : aliased HAL.UInt8;
      --  Interrupt Priority Register n
      NVICIP105 : aliased HAL.UInt8;
      --  Software Trigger Interrupt Register
      NVICSTIR  : aliased NVIC_NVICSTIR_Register;
   end record
     with Volatile;

   for NVIC_Peripheral use record
      NVICISER0 at 16#0# range 0 .. 31;
      NVICISER1 at 16#4# range 0 .. 31;
      NVICISER2 at 16#8# range 0 .. 31;
      NVICISER3 at 16#C# range 0 .. 31;
      NVICICER0 at 16#80# range 0 .. 31;
      NVICICER1 at 16#84# range 0 .. 31;
      NVICICER2 at 16#88# range 0 .. 31;
      NVICICER3 at 16#8C# range 0 .. 31;
      NVICISPR0 at 16#100# range 0 .. 31;
      NVICISPR1 at 16#104# range 0 .. 31;
      NVICISPR2 at 16#108# range 0 .. 31;
      NVICISPR3 at 16#10C# range 0 .. 31;
      NVICICPR0 at 16#180# range 0 .. 31;
      NVICICPR1 at 16#184# range 0 .. 31;
      NVICICPR2 at 16#188# range 0 .. 31;
      NVICICPR3 at 16#18C# range 0 .. 31;
      NVICIABR0 at 16#200# range 0 .. 31;
      NVICIABR1 at 16#204# range 0 .. 31;
      NVICIABR2 at 16#208# range 0 .. 31;
      NVICIABR3 at 16#20C# range 0 .. 31;
      NVICIP0   at 16#300# range 0 .. 7;
      NVICIP1   at 16#301# range 0 .. 7;
      NVICIP2   at 16#302# range 0 .. 7;
      NVICIP3   at 16#303# range 0 .. 7;
      NVICIP4   at 16#304# range 0 .. 7;
      NVICIP5   at 16#305# range 0 .. 7;
      NVICIP6   at 16#306# range 0 .. 7;
      NVICIP7   at 16#307# range 0 .. 7;
      NVICIP8   at 16#308# range 0 .. 7;
      NVICIP9   at 16#309# range 0 .. 7;
      NVICIP10  at 16#30A# range 0 .. 7;
      NVICIP11  at 16#30B# range 0 .. 7;
      NVICIP12  at 16#30C# range 0 .. 7;
      NVICIP13  at 16#30D# range 0 .. 7;
      NVICIP14  at 16#30E# range 0 .. 7;
      NVICIP15  at 16#30F# range 0 .. 7;
      NVICIP16  at 16#310# range 0 .. 7;
      NVICIP17  at 16#311# range 0 .. 7;
      NVICIP18  at 16#312# range 0 .. 7;
      NVICIP19  at 16#313# range 0 .. 7;
      NVICIP20  at 16#314# range 0 .. 7;
      NVICIP21  at 16#315# range 0 .. 7;
      NVICIP22  at 16#316# range 0 .. 7;
      NVICIP23  at 16#317# range 0 .. 7;
      NVICIP24  at 16#318# range 0 .. 7;
      NVICIP25  at 16#319# range 0 .. 7;
      NVICIP26  at 16#31A# range 0 .. 7;
      NVICIP27  at 16#31B# range 0 .. 7;
      NVICIP28  at 16#31C# range 0 .. 7;
      NVICIP29  at 16#31D# range 0 .. 7;
      NVICIP30  at 16#31E# range 0 .. 7;
      NVICIP31  at 16#31F# range 0 .. 7;
      NVICIP32  at 16#320# range 0 .. 7;
      NVICIP33  at 16#321# range 0 .. 7;
      NVICIP34  at 16#322# range 0 .. 7;
      NVICIP35  at 16#323# range 0 .. 7;
      NVICIP36  at 16#324# range 0 .. 7;
      NVICIP37  at 16#325# range 0 .. 7;
      NVICIP38  at 16#326# range 0 .. 7;
      NVICIP39  at 16#327# range 0 .. 7;
      NVICIP40  at 16#328# range 0 .. 7;
      NVICIP41  at 16#329# range 0 .. 7;
      NVICIP42  at 16#32A# range 0 .. 7;
      NVICIP43  at 16#32B# range 0 .. 7;
      NVICIP44  at 16#32C# range 0 .. 7;
      NVICIP45  at 16#32D# range 0 .. 7;
      NVICIP46  at 16#32E# range 0 .. 7;
      NVICIP47  at 16#32F# range 0 .. 7;
      NVICIP48  at 16#330# range 0 .. 7;
      NVICIP49  at 16#331# range 0 .. 7;
      NVICIP50  at 16#332# range 0 .. 7;
      NVICIP51  at 16#333# range 0 .. 7;
      NVICIP52  at 16#334# range 0 .. 7;
      NVICIP53  at 16#335# range 0 .. 7;
      NVICIP54  at 16#336# range 0 .. 7;
      NVICIP55  at 16#337# range 0 .. 7;
      NVICIP56  at 16#338# range 0 .. 7;
      NVICIP57  at 16#339# range 0 .. 7;
      NVICIP58  at 16#33A# range 0 .. 7;
      NVICIP59  at 16#33B# range 0 .. 7;
      NVICIP60  at 16#33C# range 0 .. 7;
      NVICIP61  at 16#33D# range 0 .. 7;
      NVICIP62  at 16#33E# range 0 .. 7;
      NVICIP63  at 16#33F# range 0 .. 7;
      NVICIP64  at 16#340# range 0 .. 7;
      NVICIP65  at 16#341# range 0 .. 7;
      NVICIP66  at 16#342# range 0 .. 7;
      NVICIP67  at 16#343# range 0 .. 7;
      NVICIP68  at 16#344# range 0 .. 7;
      NVICIP69  at 16#345# range 0 .. 7;
      NVICIP70  at 16#346# range 0 .. 7;
      NVICIP71  at 16#347# range 0 .. 7;
      NVICIP72  at 16#348# range 0 .. 7;
      NVICIP73  at 16#349# range 0 .. 7;
      NVICIP74  at 16#34A# range 0 .. 7;
      NVICIP75  at 16#34B# range 0 .. 7;
      NVICIP76  at 16#34C# range 0 .. 7;
      NVICIP77  at 16#34D# range 0 .. 7;
      NVICIP78  at 16#34E# range 0 .. 7;
      NVICIP79  at 16#34F# range 0 .. 7;
      NVICIP80  at 16#350# range 0 .. 7;
      NVICIP81  at 16#351# range 0 .. 7;
      NVICIP82  at 16#352# range 0 .. 7;
      NVICIP83  at 16#353# range 0 .. 7;
      NVICIP84  at 16#354# range 0 .. 7;
      NVICIP85  at 16#355# range 0 .. 7;
      NVICIP86  at 16#356# range 0 .. 7;
      NVICIP87  at 16#357# range 0 .. 7;
      NVICIP88  at 16#358# range 0 .. 7;
      NVICIP89  at 16#359# range 0 .. 7;
      NVICIP90  at 16#35A# range 0 .. 7;
      NVICIP91  at 16#35B# range 0 .. 7;
      NVICIP92  at 16#35C# range 0 .. 7;
      NVICIP93  at 16#35D# range 0 .. 7;
      NVICIP94  at 16#35E# range 0 .. 7;
      NVICIP95  at 16#35F# range 0 .. 7;
      NVICIP96  at 16#360# range 0 .. 7;
      NVICIP97  at 16#361# range 0 .. 7;
      NVICIP98  at 16#362# range 0 .. 7;
      NVICIP99  at 16#363# range 0 .. 7;
      NVICIP100 at 16#364# range 0 .. 7;
      NVICIP101 at 16#365# range 0 .. 7;
      NVICIP102 at 16#366# range 0 .. 7;
      NVICIP103 at 16#367# range 0 .. 7;
      NVICIP104 at 16#368# range 0 .. 7;
      NVICIP105 at 16#369# range 0 .. 7;
      NVICSTIR  at 16#E00# range 0 .. 31;
   end record;

   --  Nested Vectored Interrupt Controller
   NVIC_Periph : aliased NVIC_Peripheral
     with Import, Address => System'To_Address (16#E000E100#);

end SAM_SVD.NVIC;
