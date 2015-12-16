--  Automatically generated from CMSIS-SVD description file
pragma Restrictions (No_Elaboration_Code);

with System;

package STM32_SVD.EXTI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype IMR_MR0_Field is STM32_SVD.Bit;

   type MR_Field_Array is array (0 .. 22) of IMR_MR0_Field
     with Component_Size => 1, Size => 23;

   --  Type definition for MR
   type MR_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of MR0
            Val : STM32_SVD.UInt23;
         when True =>
            --  Array vision of MR0
            Arr : MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for MR_Union use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  Interrupt mask register (EXTI_IMR)
   type IMR_Register is record
      --  Interrupt Mask on line 0
      MR             : MR_Union := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : STM32_SVD.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR_Register use record
      MR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype EMR_MR0_Field is STM32_SVD.Bit;

   type MR_Field_Array_1 is array (0 .. 22) of EMR_MR0_Field
     with Component_Size => 1, Size => 23;

   --  Type definition for MR
   type MR_Union_1 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of MR0
            Val : STM32_SVD.UInt23;
         when True =>
            --  Array vision of MR0
            Arr : MR_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for MR_Union_1 use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  Event mask register (EXTI_EMR)
   type EMR_Register is record
      --  Event Mask on line 0
      MR             : MR_Union_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : STM32_SVD.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EMR_Register use record
      MR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype RTSR_TR0_Field is STM32_SVD.Bit;

   type TR_Field_Array is array (0 .. 22) of RTSR_TR0_Field
     with Component_Size => 1, Size => 23;

   --  Type definition for TR
   type TR_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of TR0
            Val : STM32_SVD.UInt23;
         when True =>
            --  Array vision of TR0
            Arr : TR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for TR_Union use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  Rising Trigger selection register (EXTI_RTSR)
   type RTSR_Register is record
      --  Rising trigger event configuration of line 0
      TR             : TR_Union := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : STM32_SVD.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR_Register use record
      TR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype FTSR_TR0_Field is STM32_SVD.Bit;

   type TR_Field_Array_1 is array (0 .. 22) of FTSR_TR0_Field
     with Component_Size => 1, Size => 23;

   --  Type definition for TR
   type TR_Union_1 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of TR0
            Val : STM32_SVD.UInt23;
         when True =>
            --  Array vision of TR0
            Arr : TR_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for TR_Union_1 use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  Falling Trigger selection register (EXTI_FTSR)
   type FTSR_Register is record
      --  Falling trigger event configuration of line 0
      TR             : TR_Union_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : STM32_SVD.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR_Register use record
      TR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype SWIER_SWIER0_Field is STM32_SVD.Bit;

   type SWIER_Field_Array is array (0 .. 22) of SWIER_SWIER0_Field
     with Component_Size => 1, Size => 23;

   --  Type definition for SWIER
   type SWIER_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of SWIER0
            Val : STM32_SVD.UInt23;
         when True =>
            --  Array vision of SWIER0
            Arr : SWIER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for SWIER_Union use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  Software interrupt event register (EXTI_SWIER)
   type SWIER_Register is record
      --  Software Interrupt on line 0
      SWIER          : SWIER_Union := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : STM32_SVD.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER_Register use record
      SWIER          at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype PR_PR0_Field is STM32_SVD.Bit;

   type PR_Field_Array is array (0 .. 22) of PR_PR0_Field
     with Component_Size => 1, Size => 23;

   --  Type definition for PR
   type PR_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of PR0
            Val : STM32_SVD.UInt23;
         when True =>
            --  Array vision of PR0
            Arr : PR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for PR_Union use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  Pending register (EXTI_PR)
   type PR_Register is record
      --  Pending bit 0
      PR             : PR_Union := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : STM32_SVD.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PR_Register use record
      PR             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  External interrupt/event controller
   type EXTI_Peripheral is record
      --  Interrupt mask register (EXTI_IMR)
      IMR   : IMR_Register;
      --  Event mask register (EXTI_EMR)
      EMR   : EMR_Register;
      --  Rising Trigger selection register (EXTI_RTSR)
      RTSR  : RTSR_Register;
      --  Falling Trigger selection register (EXTI_FTSR)
      FTSR  : FTSR_Register;
      --  Software interrupt event register (EXTI_SWIER)
      SWIER : SWIER_Register;
      --  Pending register (EXTI_PR)
      PR    : PR_Register;
   end record
     with Volatile;

   for EXTI_Peripheral use record
      IMR   at 0 range 0 .. 31;
      EMR   at 4 range 0 .. 31;
      RTSR  at 8 range 0 .. 31;
      FTSR  at 12 range 0 .. 31;
      SWIER at 16 range 0 .. 31;
      PR    at 20 range 0 .. 31;
   end record;

   --  External interrupt/event controller
   EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => System'To_Address(16#40013C00#);

end STM32_SVD.EXTI;
