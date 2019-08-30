------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with HAL; use HAL;

private with System;

package SiFive.PWM is

   type Internal_PWM is limited private;

   type PWM_Device (Periph : not null access Internal_PWM) is
     limited private;

   subtype Count_Value is UInt31;

   function Count (This : PWM_Device) return Count_Value;

   procedure Set_Count (This  : in out PWM_Device;
                        Value : Count_Value);

   subtype Scaled_Value is UInt16;

   function Scaled_Counter (This : PWM_Device)
                            return Scaled_Value;

   -- Enable --

   procedure Enable_Continous (This : in out PWM_Device);
   procedure Enable_One_Shot (This : in out PWM_Device);

   procedure Disable (This : in out PWM_Device);

   -- Configuration --


   procedure Configure (This          : in out PWM_Device;
                        Scale         : UInt4;
                        Sticky        : Boolean;
                        Reset_To_Zero : Boolean;
                        Deglitch      : Boolean);

   -- Comparators --

   subtype Comparator_ID is Natural range 0 .. 3;

   procedure Configure (This           : in out PWM_Device;
                        ID             : Comparator_ID;
                        Compare_Center : Boolean;
                        Compare_Gang   : Boolean);

   -- Compare Value --

   subtype Compare_Value is UInt16;

   procedure Set_Compare (This  : in out PWM_Device;
                          ID    : Comparator_ID;
                          Value : Compare_Value);

   function Compare (This : PWM_Device;
                     ID   : Comparator_ID)
                     return Compare_Value;

   -- Interrupts --

   function Interrupt_Pending (This : PWM_Device;
                               ID   : Comparator_ID)
                               return Boolean;

private

   ---------------
   -- Registers --
   ---------------

   subtype CONFIG_SCALE_Field is HAL.UInt4;

   --  CONFIG_CMP_CENTER array
   type CONFIG_CMP_CENTER_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CONFIG_CMP_CENTER
   type CONFIG_CMP_CENTER_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP_CENTER as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP_CENTER as an array
            Arr : CONFIG_CMP_CENTER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CONFIG_CMP_CENTER_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  CONFIG_CMP_GANG array
   type CONFIG_CMP_GANG_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CONFIG_CMP_GANG
   type CONFIG_CMP_GANG_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP_GANG as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP_GANG as an array
            Arr : CONFIG_CMP_GANG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CONFIG_CMP_GANG_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  CONFIG_CMP_IP array
   type CONFIG_CMP_IP_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CONFIG_CMP_IP
   type CONFIG_CMP_IP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP_IP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP_IP as an array
            Arr : CONFIG_CMP_IP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CONFIG_CMP_IP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

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
      CMP_CENTER     : CONFIG_CMP_CENTER_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      CMP_GANG       : CONFIG_CMP_GANG_Field :=
                        (As_Array => False, Val => 16#0#);
      CMP_IP         : CONFIG_CMP_IP_Field :=
                        (As_Array => False, Val => 16#0#);
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
      CMP_CENTER     at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      CMP_GANG       at 0 range 24 .. 27;
      CMP_IP         at 0 range 28 .. 31;
   end record;

   subtype COUNT_CNT_Field is HAL.UInt31;

   --  PWM Count Register.
   type COUNT_Register is record
      CNT            : COUNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNT_Register use record
      CNT            at 0 range 0 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SCALE_COUNT_CNT_Field is HAL.UInt16;

   --  PWM Scaled Counter Register.
   type SCALE_COUNT_Register is record
      CNT            : SCALE_COUNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCALE_COUNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype COMPARE_COMPARE_Field is HAL.UInt16;

   --  PWM Compare Register.
   type COMPARE_Register is record
      COMPARE        : COMPARE_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_Register use record
      COMPARE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Pulse-Width Modulation.
   type Internal_PWM is record
      --  PWM Configuration Register.
      CONFIG      : aliased CONFIG_Register;
      --  PWM Count Register.
      COUNT       : aliased COUNT_Register;
      --  PWM Scaled Counter Register.
      SCALE_COUNT : aliased SCALE_COUNT_Register;
      --  PWM Compare Register.
      COMPARE0    : aliased COMPARE_Register;
      --  PWM Compare Register.
      COMPARE1    : aliased COMPARE_Register;
      --  PWM Compare Register.
      COMPARE2    : aliased COMPARE_Register;
      --  PWM Compare Register.
      COMPARE3    : aliased COMPARE_Register;
   end record
     with Volatile;

   for Internal_PWM use record
      CONFIG      at 16#0# range 0 .. 31;
      COUNT       at 16#8# range 0 .. 31;
      SCALE_COUNT at 16#10# range 0 .. 31;
      COMPARE0    at 16#20# range 0 .. 31;
      COMPARE1    at 16#24# range 0 .. 31;
      COMPARE2    at 16#28# range 0 .. 31;
      COMPARE3    at 16#2C# range 0 .. 31;
   end record;

   type PWM_Device (Periph : not null access Internal_PWM) is
     limited null record;

end SiFive.PWM;
