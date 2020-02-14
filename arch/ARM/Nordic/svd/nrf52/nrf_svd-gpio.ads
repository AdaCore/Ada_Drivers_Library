--  Copyright (c) 2010 - 2018, Nordic Semiconductor ASA
--
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without modification,
--  are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form, except as embedded into a Nordic
--  Semiconductor ASA integrated circuit in a product or a software update for
--  such product, must reproduce the above copyright notice, this list of
--  conditions and the following disclaimer in the documentation and/or other
--  materials provided with the distribution.
--
--  3. Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from this
--  software without specific prior written permission.
--
--  4. This software, with or without modification, must only be used with a
--  Nordic Semiconductor ASA integrated circuit.
--
--  5. Any software provided in binary form under this license must not be reverse
--  engineered, decompiled, modified and/or disassembled.
--
--  THIS SOFTWARE IS PROVIDED BY NORDIC SEMICONDUCTOR ASA "AS IS" AND ANY EXPRESS
--  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL NORDIC SEMICONDUCTOR ASA OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
--  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
--  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf52.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF_SVD.GPIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Pin 0
   type OUT_PIN0_Field is
     (--  Pin driver is low
      Low,
      --  Pin driver is high
      High)
     with Size => 1;
   for OUT_PIN0_Field use
     (Low => 0,
      High => 1);

   --  OUT_PIN array
   type OUT_PIN_Field_Array is array (0 .. 31) of OUT_PIN0_Field
     with Component_Size => 1, Size => 32;

   --  Write GPIO port
   type OUT_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : OUT_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUT_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Pin 0
   type OUTSET_PIN0_Field is
     (--  Read: pin driver is low
      Low,
      --  Read: pin driver is high
      High)
     with Size => 1;
   for OUTSET_PIN0_Field use
     (Low => 0,
      High => 1);

   --  Pin 0
   type OUTSET_PIN0_Field_1 is
     (--  Reset value for the field
      Outset_Pin0_Field_Reset,
      --  Write: writing a '1' sets the pin high; writing a '0' has no effect
      Set)
     with Size => 1;
   for OUTSET_PIN0_Field_1 use
     (Outset_Pin0_Field_Reset => 0,
      Set => 1);

   --  OUTSET_PIN array
   type OUTSET_PIN_Field_Array is array (0 .. 31) of OUTSET_PIN0_Field_1
     with Component_Size => 1, Size => 32;

   --  Set individual bits in GPIO port
   type OUTSET_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : OUTSET_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTSET_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Pin 0
   type OUTCLR_PIN0_Field is
     (--  Read: pin driver is low
      Low,
      --  Read: pin driver is high
      High)
     with Size => 1;
   for OUTCLR_PIN0_Field use
     (Low => 0,
      High => 1);

   --  Pin 0
   type OUTCLR_PIN0_Field_1 is
     (--  Reset value for the field
      Outclr_Pin0_Field_Reset,
      --  Write: writing a '1' sets the pin low; writing a '0' has no effect
      Clear)
     with Size => 1;
   for OUTCLR_PIN0_Field_1 use
     (Outclr_Pin0_Field_Reset => 0,
      Clear => 1);

   --  OUTCLR_PIN array
   type OUTCLR_PIN_Field_Array is array (0 .. 31) of OUTCLR_PIN0_Field_1
     with Component_Size => 1, Size => 32;

   --  Clear individual bits in GPIO port
   type OUTCLR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : OUTCLR_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTCLR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Pin 0
   type IN_PIN0_Field is
     (--  Pin input is low
      Low,
      --  Pin input is high
      High)
     with Size => 1;
   for IN_PIN0_Field use
     (Low => 0,
      High => 1);

   --  IN_PIN array
   type IN_PIN_Field_Array is array (0 .. 31) of IN_PIN0_Field
     with Component_Size => 1, Size => 32;

   --  Read GPIO port
   type IN_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : IN_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IN_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Pin 0
   type DIR_PIN0_Field is
     (--  Pin set as input
      Input,
      --  Pin set as output
      Output)
     with Size => 1;
   for DIR_PIN0_Field use
     (Input => 0,
      Output => 1);

   --  DIR_PIN array
   type DIR_PIN_Field_Array is array (0 .. 31) of DIR_PIN0_Field
     with Component_Size => 1, Size => 32;

   --  Direction of GPIO pins
   type DIR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : DIR_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Set as output pin 0
   type DIRSET_PIN0_Field is
     (--  Read: pin set as input
      Input,
      --  Read: pin set as output
      Output)
     with Size => 1;
   for DIRSET_PIN0_Field use
     (Input => 0,
      Output => 1);

   --  Set as output pin 0
   type DIRSET_PIN0_Field_1 is
     (--  Reset value for the field
      Dirset_Pin0_Field_Reset,
      --  Write: writing a '1' sets pin to output; writing a '0' has no effect
      Set)
     with Size => 1;
   for DIRSET_PIN0_Field_1 use
     (Dirset_Pin0_Field_Reset => 0,
      Set => 1);

   --  DIRSET_PIN array
   type DIRSET_PIN_Field_Array is array (0 .. 31) of DIRSET_PIN0_Field_1
     with Component_Size => 1, Size => 32;

   --  DIR set register
   type DIRSET_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : DIRSET_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIRSET_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Set as input pin 0
   type DIRCLR_PIN0_Field is
     (--  Read: pin set as input
      Input,
      --  Read: pin set as output
      Output)
     with Size => 1;
   for DIRCLR_PIN0_Field use
     (Input => 0,
      Output => 1);

   --  Set as input pin 0
   type DIRCLR_PIN0_Field_1 is
     (--  Reset value for the field
      Dirclr_Pin0_Field_Reset,
      --  Write: writing a '1' sets pin to input; writing a '0' has no effect
      Clear)
     with Size => 1;
   for DIRCLR_PIN0_Field_1 use
     (Dirclr_Pin0_Field_Reset => 0,
      Clear => 1);

   --  DIRCLR_PIN array
   type DIRCLR_PIN_Field_Array is array (0 .. 31) of DIRCLR_PIN0_Field_1
     with Component_Size => 1, Size => 32;

   --  DIR clear register
   type DIRCLR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : DIRCLR_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIRCLR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Status on whether PIN0 has met criteria set in PIN_CNF0.SENSE register.
   --  Write '1' to clear.
   type LATCH_PIN0_Field is
     (--  Criteria has not been met
      Notlatched,
      --  Criteria has been met
      Latched)
     with Size => 1;
   for LATCH_PIN0_Field use
     (Notlatched => 0,
      Latched => 1);

   --  LATCH_PIN array
   type LATCH_PIN_Field_Array is array (0 .. 31) of LATCH_PIN0_Field
     with Component_Size => 1, Size => 32;

   --  Latch register indicating what GPIO pins that have met the criteria set
   --  in the PIN_CNF[n].SENSE registers
   type LATCH_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : LATCH_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LATCH_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Select between default DETECT signal behaviour and LDETECT mode
   type DETECTMODE_DETECTMODE_Field is
     (--  DETECT directly connected to PIN DETECT signals
      Default,
      --  Use the latched LDETECT behaviour
      Ldetect)
     with Size => 1;
   for DETECTMODE_DETECTMODE_Field use
     (Default => 0,
      Ldetect => 1);

   --  Select between default DETECT signal behaviour and LDETECT mode
   type DETECTMODE_Register is record
      --  Select between default DETECT signal behaviour and LDETECT mode
      DETECTMODE    : DETECTMODE_DETECTMODE_Field := NRF_SVD.GPIO.Default;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DETECTMODE_Register use record
      DETECTMODE    at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Pin direction. Same physical register as DIR register
   type PIN_CNF_DIR_Field is
     (--  Configure pin as an input pin
      Input,
      --  Configure pin as an output pin
      Output)
     with Size => 1;
   for PIN_CNF_DIR_Field use
     (Input => 0,
      Output => 1);

   --  Connect or disconnect input buffer
   type PIN_CNF_INPUT_Field is
     (--  Connect input buffer
      Connect,
      --  Disconnect input buffer
      Disconnect)
     with Size => 1;
   for PIN_CNF_INPUT_Field use
     (Connect => 0,
      Disconnect => 1);

   --  Pull configuration
   type PIN_CNF_PULL_Field is
     (--  No pull
      Disabled,
      --  Pull down on pin
      Pulldown,
      --  Pull up on pin
      Pullup)
     with Size => 2;
   for PIN_CNF_PULL_Field use
     (Disabled => 0,
      Pulldown => 1,
      Pullup => 3);

   --  Drive configuration
   type PIN_CNF_DRIVE_Field is
     (--  Standard '0', standard '1'
      S0S1,
      --  High drive '0', standard '1'
      H0S1,
      --  Standard '0', high drive '1'
      S0H1,
      --  High drive '0', high 'drive '1''
      H0H1,
      --  Disconnect '0' standard '1' (normally used for wired-or connections)
      D0S1,
      --  Disconnect '0', high drive '1' (normally used for wired-or connections)
      D0H1,
      --  Standard '0'. disconnect '1' (normally used for wired-and connections)
      S0D1,
      --  High drive '0', disconnect '1' (normally used for wired-and connections)
      H0D1)
     with Size => 3;
   for PIN_CNF_DRIVE_Field use
     (S0S1 => 0,
      H0S1 => 1,
      S0H1 => 2,
      H0H1 => 3,
      D0S1 => 4,
      D0H1 => 5,
      S0D1 => 6,
      H0D1 => 7);

   --  Pin sensing mechanism
   type PIN_CNF_SENSE_Field is
     (--  Disabled
      Disabled,
      --  Sense for high level
      High,
      --  Sense for low level
      Low)
     with Size => 2;
   for PIN_CNF_SENSE_Field use
     (Disabled => 0,
      High => 2,
      Low => 3);

   --  Description collection[0]: Configuration of GPIO pins
   type PIN_CNF_Register is record
      --  Pin direction. Same physical register as DIR register
      DIR            : PIN_CNF_DIR_Field := NRF_SVD.GPIO.Input;
      --  Connect or disconnect input buffer
      INPUT          : PIN_CNF_INPUT_Field := NRF_SVD.GPIO.Disconnect;
      --  Pull configuration
      PULL           : PIN_CNF_PULL_Field := NRF_SVD.GPIO.Disabled;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CNF_DRIVE_Field := NRF_SVD.GPIO.S0S1;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Pin sensing mechanism
      SENSE          : PIN_CNF_SENSE_Field := NRF_SVD.GPIO.Disabled;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CNF_Register use record
      DIR            at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Description collection[0]: Configuration of GPIO pins
   type PIN_CNF_Registers is array (0 .. 31) of PIN_CNF_Register;

   -----------------
   -- Peripherals --
   -----------------

   --  GPIO Port 1
   type GPIO_Peripheral is record
      --  Write GPIO port
      OUT_k      : aliased OUT_Register;
      --  Set individual bits in GPIO port
      OUTSET     : aliased OUTSET_Register;
      --  Clear individual bits in GPIO port
      OUTCLR     : aliased OUTCLR_Register;
      --  Read GPIO port
      IN_k       : aliased IN_Register;
      --  Direction of GPIO pins
      DIR        : aliased DIR_Register;
      --  DIR set register
      DIRSET     : aliased DIRSET_Register;
      --  DIR clear register
      DIRCLR     : aliased DIRCLR_Register;
      --  Latch register indicating what GPIO pins that have met the criteria
      --  set in the PIN_CNF[n].SENSE registers
      LATCH      : aliased LATCH_Register;
      --  Select between default DETECT signal behaviour and LDETECT mode
      DETECTMODE : aliased DETECTMODE_Register;
      --  Description collection[0]: Configuration of GPIO pins
      PIN_CNF    : aliased PIN_CNF_Registers;
   end record
     with Volatile;

   for GPIO_Peripheral use record
      OUT_k      at 16#504# range 0 .. 31;
      OUTSET     at 16#508# range 0 .. 31;
      OUTCLR     at 16#50C# range 0 .. 31;
      IN_k       at 16#510# range 0 .. 31;
      DIR        at 16#514# range 0 .. 31;
      DIRSET     at 16#518# range 0 .. 31;
      DIRCLR     at 16#51C# range 0 .. 31;
      LATCH      at 16#520# range 0 .. 31;
      DETECTMODE at 16#524# range 0 .. 31;
      PIN_CNF    at 16#700# range 0 .. 1023;
   end record;

   --  GPIO Port 1
   GPIO_Periph : aliased GPIO_Peripheral
     with Import, Address => P0_Base;

end NRF_SVD.GPIO;
