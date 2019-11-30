------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with HAL.GPIO;

private with System;

package SAM.Port is

   type GPIO_Pin_Index is range 0 .. 31;

   type Peripheral_Function is (A, B, C, D, E, F, G, H, I, J, K, L, M, N,
                                Disabled);

   type Port_Internal is private;

   type Port_Controller
     (Periph : not null access Port_Internal)
   is private;

   type Any_GPIO_Controller is access all Port_Controller;

   type GPIO_Point (Controller : not null Any_GPIO_Controller;
                    Pin        : GPIO_Pin_Index)
   is new HAL.GPIO.GPIO_Point
     with
     private;


   procedure Set_Function (This : in out GPIO_Point;
                           Func :        Peripheral_Function);
   --  Set the peripheral function for the given pin. The function is disabled
   --  by default.
   --
   --  See Multiplexed Peripheral Signals table of your device reference manual
   --  for more information about the available functions.

   ---------------
   --  HAL.GPIO --
   ---------------

   overriding
   function Support (This   : GPIO_Point;
                     Unused : HAL.GPIO.Capability)
                     return Boolean
   is (True);

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode;

   overriding
   procedure Set_Mode (This : in out GPIO_Point;
                       Mode : HAL.GPIO.GPIO_Config_Mode);

   overriding
   function Pull_Resistor (This : GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor (This : in out GPIO_Point;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor);

   overriding
   function Set (This : GPIO_Point) return Boolean;

   overriding
   procedure Set (This : in out GPIO_Point);

   overriding
   procedure Clear (This : in out GPIO_Point);

   overriding
   procedure Toggle (This : in out GPIO_Point);

private

   for Peripheral_Function use (A        => 0,
                                B        => 1,
                                C        => 2,
                                D        => 3,
                                E        => 4,
                                F        => 5,
                                G        => 6,
                                H        => 7,
                                I        => 8,
                                J        => 9,
                                K        => 10,
                                L        => 11,
                                M        => 12,
                                N        => 13,
                                Disabled => 17);

   type Port_Controller
     (Periph : not null access Port_Internal)
   is null record;

   type GPIO_Point (Controller : not null Any_GPIO_Controller;
                    Pin        : GPIO_Pin_Index)
   is new HAL.GPIO.GPIO_Point
     with null record;


      ---------------
   -- Registers --
   ---------------

   subtype WRCONFIG_PINMASK_Field is HAL.UInt16;
   subtype WRCONFIG_PMUX_Field is HAL.UInt4;

   --  Write Configuration
   type WRCONFIG_Register is record
      --  Write-only. Pin Mask for Multiple Pin Configuration
      PINMASK        : WRCONFIG_PINMASK_Field := 16#0#;
      --  Write-only. Peripheral Multiplexer Enable
      PMUXEN         : Boolean := False;
      --  Write-only. Input Enable
      INEN           : Boolean := False;
      --  Write-only. Pull Enable
      PULLEN         : Boolean := False;
      --  unspecified
      Reserved_19_21 : HAL.UInt3 := 16#0#;
      --  Write-only. Output Driver Strength Selection
      DRVSTR         : Boolean := False;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Write-only. Peripheral Multiplexing
      PMUX           : WRCONFIG_PMUX_Field := 16#0#;
      --  Write-only. Write PMUX
      WRPMUX         : Boolean := False;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  Write-only. Write PINCFG
      WRPINCFG       : Boolean := False;
      --  Write-only. Half-Word Select
      HWSEL          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WRCONFIG_Register use record
      PINMASK        at 0 range 0 .. 15;
      PMUXEN         at 0 range 16 .. 16;
      INEN           at 0 range 17 .. 17;
      PULLEN         at 0 range 18 .. 18;
      Reserved_19_21 at 0 range 19 .. 21;
      DRVSTR         at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      PMUX           at 0 range 24 .. 27;
      WRPMUX         at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      WRPINCFG       at 0 range 30 .. 30;
      HWSEL          at 0 range 31 .. 31;
   end record;

   subtype EVCTRL_PID0_Field is HAL.UInt5;

   --  PORT Event Action 0
   type EVCTRL0_EVACT0Select is
     ( --  Event output to pin
      Out_k,
      --  Set output register of pin on event
      Set,
      --  Clear output register of pin on event
      Clr,
      --  Toggle output register of pin on event
      Tgl)
     with Size => 2;
   for EVCTRL0_EVACT0Select use
     (Out_k => 0,
      Set => 1,
      Clr => 2,
      Tgl => 3);

   subtype EVCTRL_PID1_Field is HAL.UInt5;
   subtype EVCTRL_EVACT1_Field is HAL.UInt2;
   subtype EVCTRL_PID2_Field is HAL.UInt5;
   subtype EVCTRL_EVACT2_Field is HAL.UInt2;
   subtype EVCTRL_PID3_Field is HAL.UInt5;
   subtype EVCTRL_EVACT3_Field is HAL.UInt2;

   --  Event Input Control
   type EVCTRL_Register is record
      --  PORT Event Pin Identifier 0
      PID0    : EVCTRL_PID0_Field := 16#0#;
      --  PORT Event Action 0
      EVACT0  : EVCTRL0_EVACT0Select := Out_k;
      --  PORT Event Input Enable 0
      PORTEI0 : Boolean := False;
      --  PORT Event Pin Identifier 1
      PID1    : EVCTRL_PID1_Field := 16#0#;
      --  PORT Event Action 1
      EVACT1  : EVCTRL_EVACT1_Field := 16#0#;
      --  PORT Event Input Enable 1
      PORTEI1 : Boolean := False;
      --  PORT Event Pin Identifier 2
      PID2    : EVCTRL_PID2_Field := 16#0#;
      --  PORT Event Action 2
      EVACT2  : EVCTRL_EVACT2_Field := 16#0#;
      --  PORT Event Input Enable 2
      PORTEI2 : Boolean := False;
      --  PORT Event Pin Identifier 3
      PID3    : EVCTRL_PID3_Field := 16#0#;
      --  PORT Event Action 3
      EVACT3  : EVCTRL_EVACT3_Field := 16#0#;
      --  PORT Event Input Enable 3
      PORTEI3 : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVCTRL_Register use record
      PID0    at 0 range 0 .. 4;
      EVACT0  at 0 range 5 .. 6;
      PORTEI0 at 0 range 7 .. 7;
      PID1    at 0 range 8 .. 12;
      EVACT1  at 0 range 13 .. 14;
      PORTEI1 at 0 range 15 .. 15;
      PID2    at 0 range 16 .. 20;
      EVACT2  at 0 range 21 .. 22;
      PORTEI2 at 0 range 23 .. 23;
      PID3    at 0 range 24 .. 28;
      EVACT3  at 0 range 29 .. 30;
      PORTEI3 at 0 range 31 .. 31;
   end record;

   subtype PMUX_PMUXE_Field is HAL.UInt4;
   subtype PMUX_PMUXO_Field is HAL.UInt4;

   --  Peripheral Multiplexing - Group 0
   type PMUX_Register is record
      --  Peripheral Multiplexing for Even-Numbered Pin
      PMUXE : PMUX_PMUXE_Field := 16#0#;
      --  Peripheral Multiplexing for Odd-Numbered Pin
      PMUXO : PMUX_PMUXO_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for PMUX_Register use record
      PMUXE at 0 range 0 .. 3;
      PMUXO at 0 range 4 .. 7;
   end record;

   --  Peripheral Multiplexing - Group 0
   type PMUX_Registers is array (0 .. 15) of PMUX_Register;

   --  Pin Configuration - Group 0
   type PINCFG_Register is record
      --  Peripheral Multiplexer Enable
      PMUXEN       : Boolean := False;
      --  Input Enable
      INEN         : Boolean := False;
      --  Pull Enable
      PULLEN       : Boolean := False;
      --  unspecified
      Reserved_3_5 : HAL.UInt3 := 16#0#;
      --  Output Driver Strength Selection
      DRVSTR       : Boolean := False;
      --  unspecified
      Reserved_7_7 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for PINCFG_Register use record
      PMUXEN       at 0 range 0 .. 0;
      INEN         at 0 range 1 .. 1;
      PULLEN       at 0 range 2 .. 2;
      Reserved_3_5 at 0 range 3 .. 5;
      DRVSTR       at 0 range 6 .. 6;
      Reserved_7_7 at 0 range 7 .. 7;
   end record;

   --  Pin Configuration - Group 0
   type PINCFG_Registers is array (GPIO_Pin_Index) of PINCFG_Register;

   -----------------
   -- Peripherals --
   -----------------

   --  Port Module
   type PORT_Internal is record
      --  Data Direction
      DIR      : aliased HAL.UInt32;
      --  Data Direction Clear
      DIRCLR   : aliased HAL.UInt32;
      --  Data Direction Set
      DIRSET   : aliased HAL.UInt32;
      --  Data Direction Toggle
      DIRTGL   : aliased HAL.UInt32;
      --  Data Output Value
      DATA_OUT : aliased HAL.UInt32;
      --  Data Output Value Clear
      OUTCLR   : aliased HAL.UInt32;
      --  Data Output Value Set
      OUTSET   : aliased HAL.UInt32;
      --  Data Output Value Toggle
      OUTTGL   : aliased HAL.UInt32;
      --  Data Input Value
      DATA_IN  : aliased HAL.UInt32;
      --  Control
      CTRL     : aliased HAL.UInt32;
      --  Write Configuration
      WRCONFIG : aliased WRCONFIG_Register;
      --  Event Input Control
      EVCTRL   : aliased EVCTRL_Register;
      --  Peripheral Multiplexing - Group 0
      PMUX     : aliased PMUX_Registers;
      --  Pin Configuration - Group 0
      PINCFG   : aliased PINCFG_Registers;
   end record
     with Volatile;

   for PORT_Internal use record
      DIR      at 16#0# range 0 .. 31;
      DIRCLR   at 16#4# range 0 .. 31;
      DIRSET   at 16#8# range 0 .. 31;
      DIRTGL   at 16#C# range 0 .. 31;
      DATA_OUT at 16#10# range 0 .. 31;
      OUTCLR   at 16#14# range 0 .. 31;
      OUTSET   at 16#18# range 0 .. 31;
      OUTTGL   at 16#1C# range 0 .. 31;
      DATA_IN  at 16#20# range 0 .. 31;
      CTRL     at 16#24# range 0 .. 31;
      WRCONFIG at 16#28# range 0 .. 31;
      EVCTRL   at 16#2C# range 0 .. 31;
      PMUX     at 16#30# range 0 .. 127;
      PINCFG   at 16#40# range 0 .. 255;
   end record;

end SAM.Port;
