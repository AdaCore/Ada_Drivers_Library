--  This spec has been automatically generated from STM32F429x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.WWDG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- CR_Register --
   -----------------

   subtype CR_T_Field is HAL.UInt7;

   --  Control register
   type CR_Register is record
      --  7-bit counter (MSB to LSB)
      T             : CR_T_Field := 16#7F#;
      --  Activation bit
      WDGA          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      T             at 0 range 0 .. 6;
      WDGA          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   ------------------
   -- CFR_Register --
   ------------------

   subtype CFR_W_Field is HAL.UInt7;

   ---------------
   -- CFR.WDGTB --
   ---------------

   --  CFR_WDGTB array
   type CFR_WDGTB_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CFR_WDGTB
   type CFR_WDGTB_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WDGTB as a value
            Val : HAL.UInt2;
         when True =>
            --  WDGTB as an array
            Arr : CFR_WDGTB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CFR_WDGTB_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Configuration register
   type CFR_Register is record
      --  7-bit window value
      W              : CFR_W_Field := 16#7F#;
      --  Timer base
      WDGTB          : CFR_WDGTB_Field := (As_Array => False, Val => 16#0#);
      --  Early wakeup interrupt
      EWI            : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFR_Register use record
      W              at 0 range 0 .. 6;
      WDGTB          at 0 range 7 .. 8;
      EWI            at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   --  Status register
   type SR_Register is record
      --  Early wakeup interrupt flag
      EWIF          : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      EWIF          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Window watchdog
   type WWDG_Peripheral is record
      --  Control register
      CR  : CR_Register;
      --  Configuration register
      CFR : CFR_Register;
      --  Status register
      SR  : SR_Register;
   end record
     with Volatile;

   for WWDG_Peripheral use record
      CR  at 0 range 0 .. 31;
      CFR at 4 range 0 .. 31;
      SR  at 8 range 0 .. 31;
   end record;

   --  Window watchdog
   WWDG_Periph : aliased WWDG_Peripheral
     with Import, Address => WWDG_Base;

end STM32_SVD.WWDG;
