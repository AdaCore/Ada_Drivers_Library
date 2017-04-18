--  This spec has been automatically generated from FE310.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package FE310_SVD.PWM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CONFIG_SCALE_Field is HAL.UInt4;

   --  PWM Configuration Register.
   type CONFIG_Register is record
      SCALE          : CONFIG_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      STICKY         : Boolean := False;
      ZEROCMP        : Boolean := False;
      DEGLITCH       : Boolean := False;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      ENALWAYS       : Boolean := False;
      ENONESHOT      : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      CMP_CENTER_0   : Boolean := False;
      CMP_CENTER_1   : Boolean := False;
      CMP_CENTER_2   : Boolean := False;
      CMP_CENTER_3   : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      CMP_GANG_0     : Boolean := False;
      CMP_GANG_1     : Boolean := False;
      CMP_GANG_2     : Boolean := False;
      CMP_GANG_3     : Boolean := False;
      CMP_IP_0       : Boolean := False;
      CMP_IP_1       : Boolean := False;
      CMP_IP_2       : Boolean := False;
      CMP_IP_3       : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      SCALE          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      STICKY         at 0 range 8 .. 8;
      ZEROCMP        at 0 range 9 .. 9;
      DEGLITCH       at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      ENALWAYS       at 0 range 12 .. 12;
      ENONESHOT      at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      CMP_CENTER_0   at 0 range 16 .. 16;
      CMP_CENTER_1   at 0 range 17 .. 17;
      CMP_CENTER_2   at 0 range 18 .. 18;
      CMP_CENTER_3   at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      CMP_GANG_0     at 0 range 24 .. 24;
      CMP_GANG_1     at 0 range 25 .. 25;
      CMP_GANG_2     at 0 range 26 .. 26;
      CMP_GANG_3     at 0 range 27 .. 27;
      CMP_IP_0       at 0 range 28 .. 28;
      CMP_IP_1       at 0 range 29 .. 29;
      CMP_IP_2       at 0 range 30 .. 30;
      CMP_IP_3       at 0 range 31 .. 31;
   end record;

   subtype COUNT_16_CNT_Field is HAL.UInt31;

   --  PWM Count Register.
   type COUNT_16_Register is record
      CNT            : COUNT_16_CNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNT_16_Register use record
      CNT            at 0 range 0 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype COUNT_8_CNT_Field is HAL.UInt23;

   --  PWM Count Register.
   type COUNT_8_Register is record
      CNT            : COUNT_8_CNT_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNT_8_Register use record
      CNT            at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype SCALE_COUNT_16_CNT_Field is HAL.UInt16;

   --  PWM Scaled Conunter Register.
   type SCALE_COUNT_16_Register is record
      CNT            : SCALE_COUNT_16_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCALE_COUNT_16_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype SCALE_COUNT_8_CNT_Field is HAL.UInt9;

   --  PWM Scaled Conunter Register.
   type SCALE_COUNT_8_Register is record
      CNT           : SCALE_COUNT_8_CNT_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCALE_COUNT_8_Register use record
      CNT           at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype COMPARE_0_16_COMPARE_Field is HAL.UInt16;

   --  PWM Compare Register.
   type COMPARE_0_16_Register is record
      COMPARE        : COMPARE_0_16_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_0_16_Register use record
      COMPARE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype COMPARE_0_8_COMPARE_Field is HAL.UInt16;

   --  PWM Compare Register.
   type COMPARE_0_8_Register is record
      COMPARE        : COMPARE_0_8_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_0_8_Register use record
      COMPARE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype COMPARE_1_16_COMPARE_Field is HAL.UInt16;

   --  PWM Compare Register.
   type COMPARE_1_16_Register is record
      COMPARE        : COMPARE_1_16_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_1_16_Register use record
      COMPARE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype COMPARE_1_8_COMPARE_Field is HAL.UInt16;

   --  PWM Compare Register.
   type COMPARE_1_8_Register is record
      COMPARE        : COMPARE_1_8_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_1_8_Register use record
      COMPARE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype COMPARE_2_16_COMPARE_Field is HAL.UInt16;

   --  PWM Compare Register.
   type COMPARE_2_16_Register is record
      COMPARE        : COMPARE_2_16_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_2_16_Register use record
      COMPARE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype COMPARE_2_8_COMPARE_Field is HAL.UInt16;

   --  PWM Compare Register.
   type COMPARE_2_8_Register is record
      COMPARE        : COMPARE_2_8_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_2_8_Register use record
      COMPARE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype COMPARE_3_16_COMPARE_Field is HAL.UInt16;

   --  PWM Compare Register.
   type COMPARE_3_16_Register is record
      COMPARE        : COMPARE_3_16_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_3_16_Register use record
      COMPARE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype COMPARE_3_8_COMPARE_Field is HAL.UInt16;

   --  PWM Compare Register.
   type COMPARE_3_8_Register is record
      COMPARE        : COMPARE_3_8_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_3_8_Register use record
      COMPARE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type PWM0_Disc is
     (
      PWM0_Disc_16,
      PWM0_Disc_8);

   --  Pulse-Width Modulation.
   type PWM_Peripheral
     (Discriminent : PWM0_Disc := PWM0_Disc_16)
   is record
      --  PWM Configuration Register.
      CONFIG         : aliased CONFIG_Register;
      case Discriminent is
         when PWM0_Disc_16 =>
            --  PWM Count Register.
            COUNT_16 : aliased COUNT_16_Register;
            --  PWM Scaled Conunter Register.
            SCALE_COUNT_16 : aliased SCALE_COUNT_16_Register;
            --  PWM Compare Register.
            COMPARE_0_16 : aliased COMPARE_0_16_Register;
            --  PWM Compare Register.
            COMPARE_1_16 : aliased COMPARE_1_16_Register;
            --  PWM Compare Register.
            COMPARE_2_16 : aliased COMPARE_2_16_Register;
            --  PWM Compare Register.
            COMPARE_3_16 : aliased COMPARE_3_16_Register;
         when PWM0_Disc_8 =>
            --  PWM Count Register.
            COUNT_8 : aliased COUNT_8_Register;
            --  PWM Scaled Conunter Register.
            SCALE_COUNT_8 : aliased SCALE_COUNT_8_Register;
            --  PWM Compare Register.
            COMPARE_0_8 : aliased COMPARE_0_8_Register;
            --  PWM Compare Register.
            COMPARE_1_8 : aliased COMPARE_1_8_Register;
            --  PWM Compare Register.
            COMPARE_2_8 : aliased COMPARE_2_8_Register;
            --  PWM Compare Register.
            COMPARE_3_8 : aliased COMPARE_3_8_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for PWM_Peripheral use record
      CONFIG         at 16#0# range 0 .. 31;
      COUNT_16       at 16#8# range 0 .. 31;
      SCALE_COUNT_16 at 16#10# range 0 .. 31;
      COMPARE_0_16   at 16#20# range 0 .. 31;
      COMPARE_1_16   at 16#24# range 0 .. 31;
      COMPARE_2_16   at 16#28# range 0 .. 31;
      COMPARE_3_16   at 16#2C# range 0 .. 31;
      COUNT_8        at 16#8# range 0 .. 31;
      SCALE_COUNT_8  at 16#10# range 0 .. 31;
      COMPARE_0_8    at 16#20# range 0 .. 31;
      COMPARE_1_8    at 16#24# range 0 .. 31;
      COMPARE_2_8    at 16#28# range 0 .. 31;
      COMPARE_3_8    at 16#2C# range 0 .. 31;
   end record;

   --  Pulse-Width Modulation.
   PWM0_Periph : aliased PWM_Peripheral
     with Import, Address => System'To_Address (16#10015000#);

   --  Pulse-Width Modulation.
   PWM1_Periph : aliased PWM_Peripheral
     with Import, Address => System'To_Address (16#10025000#);

   --  Pulse-Width Modulation.
   PWM2_Periph : aliased PWM_Peripheral
     with Import, Address => System'To_Address (16#10035000#);

end FE310_SVD.PWM;
