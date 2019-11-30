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

with System;
with HAL;

package SAM.SERCOM is

   type Pad_Id is new HAL.UInt2;
   --  Identifier for the pads (IO line) of SERCOM devices

   type SERCOM_Internal is private;
   --  Register mapping for internal use only

   type SERCOM_Device
     (Periph : not null access SERCOM_Internal)
   is abstract tagged limited
   private;

   function Configured (This : SERCOM_Device) return Boolean;
   --  Return True if the device is configure for one of its operation mode
   --  (I2C, SPI, UART).
   --
   --  Configuration can only be done by one of the specific mode drivers, see
   --  SAM.SERCOM.SPI, SAM.SERCOM.I2C, etc.

   function Enabled (This : SERCOM_Device) return Boolean;
   --  Return True if the device is enableds

   procedure Enable (This : in out SERCOM_Device)
     with Pre  => This.Configured and then not This.Enabled,
          Post => This.Configured and then This.Enabled;
   --  Enable the device that was previously configured for a specific mode

   procedure Disable (This : in out SERCOM_Device)
     with Pre  => This.Enabled,
          Post => not This.Enabled;
   --  Disable the device

   procedure Reset (This : in out SERCOM_Device)
     with Post => not This.Configured;
   --  Reset the device and its configuration

   procedure Debug_Stop_Mode (This    : in out SERCOM_Device;
                              Enabled :        Boolean := True);
   --  Stop the device when the CPU is halted by an external debugger.
   --  This mode is disabled by default.
   --
   --  When this mdoe is disabled, the device will continue to send and receive
   --  data when the CPU is halted which may lead to data loss.

private

   type SERCOM_Device
     (Periph : not null access SERCOM_Internal)
   is abstract tagged limited
      record
         Config_Done : Boolean := False;
      end record;

   ---------------
   -- Registers --
   ---------------

   ------------------------------------
   -- SercomI2cm cluster's Registers --
   ------------------------------------

   subtype SERCOM_CTRLA_SERCOM_I2CM_MODE_Field is HAL.UInt3;
   subtype SERCOM_CTRLA_SERCOM_I2CM_SDAHOLD_Field is HAL.UInt2;
   subtype SERCOM_CTRLA_SERCOM_I2CM_SPEED_Field is HAL.UInt2;
   subtype SERCOM_CTRLA_SERCOM_I2CM_INACTOUT_Field is HAL.UInt2;

   --  I2CM Control A
   type SERCOM_CTRLA_SERCOM_I2CM_Register is record
      --  Software Reset
      SWRST          : Boolean := False;
      --  Enable
      ENABLE         : Boolean := False;
      --  Operating Mode
      MODE           : SERCOM_CTRLA_SERCOM_I2CM_MODE_Field := 16#0#;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Run in Standby
      RUNSTDBY       : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Pin Usage
      PINOUT         : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  SDA Hold Time
      SDAHOLD        : SERCOM_CTRLA_SERCOM_I2CM_SDAHOLD_Field := 16#0#;
      --  Master SCL Low Extend Timeout
      MEXTTOEN       : Boolean := False;
      --  Slave SCL Low Extend Timeout
      SEXTTOEN       : Boolean := False;
      --  Transfer Speed
      SPEED          : SERCOM_CTRLA_SERCOM_I2CM_SPEED_Field := 16#0#;
      --  unspecified
      Reserved_26_26 : HAL.Bit := 16#0#;
      --  SCL Clock Stretch Mode
      SCLSM          : Boolean := False;
      --  Inactive Time-Out
      INACTOUT       : SERCOM_CTRLA_SERCOM_I2CM_INACTOUT_Field := 16#0#;
      --  SCL Low Timeout Enable
      LOWTOUTEN      : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLA_SERCOM_I2CM_Register use record
      SWRST          at 0 range 0 .. 0;
      ENABLE         at 0 range 1 .. 1;
      MODE           at 0 range 2 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      RUNSTDBY       at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      PINOUT         at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SDAHOLD        at 0 range 20 .. 21;
      MEXTTOEN       at 0 range 22 .. 22;
      SEXTTOEN       at 0 range 23 .. 23;
      SPEED          at 0 range 24 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      SCLSM          at 0 range 27 .. 27;
      INACTOUT       at 0 range 28 .. 29;
      LOWTOUTEN      at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SERCOM_CTRLB_SERCOM_I2CM_CMD_Field is HAL.UInt2;

   --  I2CM Control B
   type SERCOM_CTRLB_SERCOM_I2CM_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Smart Mode Enable
      SMEN           : Boolean := False;
      --  Quick Command Enable
      QCEN           : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Write-only. Command
      CMD            : SERCOM_CTRLB_SERCOM_I2CM_CMD_Field := 16#0#;
      --  Acknowledge Action
      ACKACT         : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLB_SERCOM_I2CM_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      SMEN           at 0 range 8 .. 8;
      QCEN           at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      CMD            at 0 range 16 .. 17;
      ACKACT         at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  I2CM Control C
   type SERCOM_CTRLC_SERCOM_I2CM_Register is record
      --  unspecified
      Reserved_0_23  : HAL.UInt24 := 16#0#;
      --  Data 32 Bit
      DATA32B        : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLC_SERCOM_I2CM_Register use record
      Reserved_0_23  at 0 range 0 .. 23;
      DATA32B        at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype SERCOM_BAUD_SERCOM_I2CM_BAUD_Field is HAL.UInt8;
   subtype SERCOM_BAUD_SERCOM_I2CM_BAUDLOW_Field is HAL.UInt8;
   subtype SERCOM_BAUD_SERCOM_I2CM_HSBAUD_Field is HAL.UInt8;
   subtype SERCOM_BAUD_SERCOM_I2CM_HSBAUDLOW_Field is HAL.UInt8;

   --  I2CM Baud Rate
   type SERCOM_BAUD_SERCOM_I2CM_Register is record
      --  Baud Rate Value
      BAUD      : SERCOM_BAUD_SERCOM_I2CM_BAUD_Field := 16#0#;
      --  Baud Rate Value Low
      BAUDLOW   : SERCOM_BAUD_SERCOM_I2CM_BAUDLOW_Field := 16#0#;
      --  High Speed Baud Rate Value
      HSBAUD    : SERCOM_BAUD_SERCOM_I2CM_HSBAUD_Field := 16#0#;
      --  High Speed Baud Rate Value Low
      HSBAUDLOW : SERCOM_BAUD_SERCOM_I2CM_HSBAUDLOW_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_BAUD_SERCOM_I2CM_Register use record
      BAUD      at 0 range 0 .. 7;
      BAUDLOW   at 0 range 8 .. 15;
      HSBAUD    at 0 range 16 .. 23;
      HSBAUDLOW at 0 range 24 .. 31;
   end record;

   --  I2CM Interrupt Enable Clear
   type SERCOM_INTENCLR_SERCOM_I2CM_Register is record
      --  Master On Bus Interrupt Disable
      MB           : Boolean := False;
      --  Slave On Bus Interrupt Disable
      SB           : Boolean := False;
      --  unspecified
      Reserved_2_6 : HAL.UInt5 := 16#0#;
      --  Combined Error Interrupt Disable
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTENCLR_SERCOM_I2CM_Register use record
      MB           at 0 range 0 .. 0;
      SB           at 0 range 1 .. 1;
      Reserved_2_6 at 0 range 2 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  I2CM Interrupt Enable Set
   type SERCOM_INTENSET_SERCOM_I2CM_Register is record
      --  Master On Bus Interrupt Enable
      MB           : Boolean := False;
      --  Slave On Bus Interrupt Enable
      SB           : Boolean := False;
      --  unspecified
      Reserved_2_6 : HAL.UInt5 := 16#0#;
      --  Combined Error Interrupt Enable
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTENSET_SERCOM_I2CM_Register use record
      MB           at 0 range 0 .. 0;
      SB           at 0 range 1 .. 1;
      Reserved_2_6 at 0 range 2 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  I2CM Interrupt Flag Status and Clear
   type SERCOM_INTFLAG_SERCOM_I2CM_Register is record
      --  Master On Bus Interrupt
      MB           : Boolean := False;
      --  Slave On Bus Interrupt
      SB           : Boolean := False;
      --  unspecified
      Reserved_2_6 : HAL.UInt5 := 16#0#;
      --  Combined Error Interrupt
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTFLAG_SERCOM_I2CM_Register use record
      MB           at 0 range 0 .. 0;
      SB           at 0 range 1 .. 1;
      Reserved_2_6 at 0 range 2 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_STATUS_SERCOM_I2CM_BUSSTATE_Field is HAL.UInt2;

   --  I2CM Status
   type SERCOM_STATUS_SERCOM_I2CM_Register is record
      --  Bus Error
      BUSERR         : Boolean := False;
      --  Arbitration Lost
      ARBLOST        : Boolean := False;
      --  Read-only. Received Not Acknowledge
      RXNACK         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Bus State
      BUSSTATE       : SERCOM_STATUS_SERCOM_I2CM_BUSSTATE_Field := 16#0#;
      --  SCL Low Timeout
      LOWTOUT        : Boolean := False;
      --  Read-only. Clock Hold
      CLKHOLD        : Boolean := False;
      --  Master SCL Low Extend Timeout
      MEXTTOUT       : Boolean := False;
      --  Slave SCL Low Extend Timeout
      SEXTTOUT       : Boolean := False;
      --  Length Error
      LENERR         : Boolean := False;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_STATUS_SERCOM_I2CM_Register use record
      BUSERR         at 0 range 0 .. 0;
      ARBLOST        at 0 range 1 .. 1;
      RXNACK         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      BUSSTATE       at 0 range 4 .. 5;
      LOWTOUT        at 0 range 6 .. 6;
      CLKHOLD        at 0 range 7 .. 7;
      MEXTTOUT       at 0 range 8 .. 8;
      SEXTTOUT       at 0 range 9 .. 9;
      LENERR         at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
   end record;

   --  I2CM Synchronization Busy
   type SERCOM_SYNCBUSY_SERCOM_I2CM_Register is record
      --  Read-only. Software Reset Synchronization Busy
      SWRST         : Boolean;
      --  Read-only. SERCOM Enable Synchronization Busy
      ENABLE        : Boolean;
      --  Read-only. System Operation Synchronization Busy
      SYSOP         : Boolean;
      --  unspecified
      Reserved_3_3  : HAL.Bit;
      --  Read-only. Length Synchronization Busy
      LENGTH        : Boolean;
      --  unspecified
      Reserved_5_31 : HAL.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_SYNCBUSY_SERCOM_I2CM_Register use record
      SWRST         at 0 range 0 .. 0;
      ENABLE        at 0 range 1 .. 1;
      SYSOP         at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      LENGTH        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype SERCOM_ADDR_SERCOM_I2CM_ADDR_Field is HAL.UInt11;
   subtype SERCOM_ADDR_SERCOM_I2CM_LEN_Field is HAL.UInt8;

   --  I2CM Address
   type SERCOM_ADDR_SERCOM_I2CM_Register is record
      --  Address Value
      ADDR           : SERCOM_ADDR_SERCOM_I2CM_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : HAL.UInt2 := 16#0#;
      --  Length Enable
      LENEN          : Boolean := False;
      --  High Speed Mode
      HS             : Boolean := False;
      --  Ten Bit Addressing Enable
      TENBITEN       : Boolean := False;
      --  Length
      LEN            : SERCOM_ADDR_SERCOM_I2CM_LEN_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_ADDR_SERCOM_I2CM_Register use record
      ADDR           at 0 range 0 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      LENEN          at 0 range 13 .. 13;
      HS             at 0 range 14 .. 14;
      TENBITEN       at 0 range 15 .. 15;
      LEN            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  I2CM Debug Control
   type SERCOM_DBGCTRL_SERCOM_I2CM_Register is record
      --  Debug Mode
      DBGSTOP      : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_DBGCTRL_SERCOM_I2CM_Register use record
      DBGSTOP      at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
   end record;

   --  I2C Master Mode
   type SercomI2cm_Cluster is record
      --  I2CM Control A
      CTRLA    : aliased SERCOM_CTRLA_SERCOM_I2CM_Register;
      --  I2CM Control B
      CTRLB    : aliased SERCOM_CTRLB_SERCOM_I2CM_Register;
      --  I2CM Control C
      CTRLC    : aliased SERCOM_CTRLC_SERCOM_I2CM_Register;
      --  I2CM Baud Rate
      BAUD     : aliased SERCOM_BAUD_SERCOM_I2CM_Register;
      --  I2CM Interrupt Enable Clear
      INTENCLR : aliased SERCOM_INTENCLR_SERCOM_I2CM_Register;
      --  I2CM Interrupt Enable Set
      INTENSET : aliased SERCOM_INTENSET_SERCOM_I2CM_Register;
      --  I2CM Interrupt Flag Status and Clear
      INTFLAG  : aliased SERCOM_INTFLAG_SERCOM_I2CM_Register;
      --  I2CM Status
      STATUS   : aliased SERCOM_STATUS_SERCOM_I2CM_Register;
      --  I2CM Synchronization Busy
      SYNCBUSY : aliased SERCOM_SYNCBUSY_SERCOM_I2CM_Register;
      --  I2CM Address
      ADDR     : aliased SERCOM_ADDR_SERCOM_I2CM_Register;
      --  I2CM Data
      DATA     : aliased HAL.UInt32;
      --  I2CM Debug Control
      DBGCTRL  : aliased SERCOM_DBGCTRL_SERCOM_I2CM_Register;
   end record
     with Size => 416;

   for SercomI2cm_Cluster use record
      CTRLA    at 16#0# range 0 .. 31;
      CTRLB    at 16#4# range 0 .. 31;
      CTRLC    at 16#8# range 0 .. 31;
      BAUD     at 16#C# range 0 .. 31;
      INTENCLR at 16#14# range 0 .. 7;
      INTENSET at 16#16# range 0 .. 7;
      INTFLAG  at 16#18# range 0 .. 7;
      STATUS   at 16#1A# range 0 .. 15;
      SYNCBUSY at 16#1C# range 0 .. 31;
      ADDR     at 16#24# range 0 .. 31;
      DATA     at 16#28# range 0 .. 31;
      DBGCTRL  at 16#30# range 0 .. 7;
   end record;

   ------------------------------------
   -- SercomI2cs cluster's Registers --
   ------------------------------------

   subtype SERCOM_CTRLA_SERCOM_I2CS_MODE_Field is HAL.UInt3;
   subtype SERCOM_CTRLA_SERCOM_I2CS_SDAHOLD_Field is HAL.UInt2;
   subtype SERCOM_CTRLA_SERCOM_I2CS_SPEED_Field is HAL.UInt2;

   --  I2CS Control A
   type SERCOM_CTRLA_SERCOM_I2CS_Register is record
      --  Software Reset
      SWRST          : Boolean := False;
      --  Enable
      ENABLE         : Boolean := False;
      --  Operating Mode
      MODE           : SERCOM_CTRLA_SERCOM_I2CS_MODE_Field := 16#0#;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Run during Standby
      RUNSTDBY       : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Pin Usage
      PINOUT         : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  SDA Hold Time
      SDAHOLD        : SERCOM_CTRLA_SERCOM_I2CS_SDAHOLD_Field := 16#0#;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#0#;
      --  Slave SCL Low Extend Timeout
      SEXTTOEN       : Boolean := False;
      --  Transfer Speed
      SPEED          : SERCOM_CTRLA_SERCOM_I2CS_SPEED_Field := 16#0#;
      --  unspecified
      Reserved_26_26 : HAL.Bit := 16#0#;
      --  SCL Clock Stretch Mode
      SCLSM          : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  SCL Low Timeout Enable
      LOWTOUTEN      : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLA_SERCOM_I2CS_Register use record
      SWRST          at 0 range 0 .. 0;
      ENABLE         at 0 range 1 .. 1;
      MODE           at 0 range 2 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      RUNSTDBY       at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      PINOUT         at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SDAHOLD        at 0 range 20 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      SEXTTOEN       at 0 range 23 .. 23;
      SPEED          at 0 range 24 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      SCLSM          at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      LOWTOUTEN      at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SERCOM_CTRLB_SERCOM_I2CS_AMODE_Field is HAL.UInt2;
   subtype SERCOM_CTRLB_SERCOM_I2CS_CMD_Field is HAL.UInt2;

   --  I2CS Control B
   type SERCOM_CTRLB_SERCOM_I2CS_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Smart Mode Enable
      SMEN           : Boolean := False;
      --  PMBus Group Command
      GCMD           : Boolean := False;
      --  Automatic Address Acknowledge
      AACKEN         : Boolean := False;
      --  unspecified
      Reserved_11_13 : HAL.UInt3 := 16#0#;
      --  Address Mode
      AMODE          : SERCOM_CTRLB_SERCOM_I2CS_AMODE_Field := 16#0#;
      --  Write-only. Command
      CMD            : SERCOM_CTRLB_SERCOM_I2CS_CMD_Field := 16#0#;
      --  Acknowledge Action
      ACKACT         : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLB_SERCOM_I2CS_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      SMEN           at 0 range 8 .. 8;
      GCMD           at 0 range 9 .. 9;
      AACKEN         at 0 range 10 .. 10;
      Reserved_11_13 at 0 range 11 .. 13;
      AMODE          at 0 range 14 .. 15;
      CMD            at 0 range 16 .. 17;
      ACKACT         at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype SERCOM_CTRLC_SERCOM_I2CS_SDASETUP_Field is HAL.UInt4;

   --  I2CS Control C
   type SERCOM_CTRLC_SERCOM_I2CS_Register is record
      --  SDA Setup Time
      SDASETUP       : SERCOM_CTRLC_SERCOM_I2CS_SDASETUP_Field := 16#0#;
      --  unspecified
      Reserved_4_23  : HAL.UInt20 := 16#0#;
      --  Data 32 Bit
      DATA32B        : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLC_SERCOM_I2CS_Register use record
      SDASETUP       at 0 range 0 .. 3;
      Reserved_4_23  at 0 range 4 .. 23;
      DATA32B        at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  I2CS Interrupt Enable Clear
   type SERCOM_INTENCLR_SERCOM_I2CS_Register is record
      --  Stop Received Interrupt Disable
      PREC         : Boolean := False;
      --  Address Match Interrupt Disable
      AMATCH       : Boolean := False;
      --  Data Interrupt Disable
      DRDY         : Boolean := False;
      --  unspecified
      Reserved_3_6 : HAL.UInt4 := 16#0#;
      --  Combined Error Interrupt Disable
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTENCLR_SERCOM_I2CS_Register use record
      PREC         at 0 range 0 .. 0;
      AMATCH       at 0 range 1 .. 1;
      DRDY         at 0 range 2 .. 2;
      Reserved_3_6 at 0 range 3 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  I2CS Interrupt Enable Set
   type SERCOM_INTENSET_SERCOM_I2CS_Register is record
      --  Stop Received Interrupt Enable
      PREC         : Boolean := False;
      --  Address Match Interrupt Enable
      AMATCH       : Boolean := False;
      --  Data Interrupt Enable
      DRDY         : Boolean := False;
      --  unspecified
      Reserved_3_6 : HAL.UInt4 := 16#0#;
      --  Combined Error Interrupt Enable
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTENSET_SERCOM_I2CS_Register use record
      PREC         at 0 range 0 .. 0;
      AMATCH       at 0 range 1 .. 1;
      DRDY         at 0 range 2 .. 2;
      Reserved_3_6 at 0 range 3 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  I2CS Interrupt Flag Status and Clear
   type SERCOM_INTFLAG_SERCOM_I2CS_Register is record
      --  Stop Received Interrupt
      PREC         : Boolean := False;
      --  Address Match Interrupt
      AMATCH       : Boolean := False;
      --  Data Interrupt
      DRDY         : Boolean := False;
      --  unspecified
      Reserved_3_6 : HAL.UInt4 := 16#0#;
      --  Combined Error Interrupt
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTFLAG_SERCOM_I2CS_Register use record
      PREC         at 0 range 0 .. 0;
      AMATCH       at 0 range 1 .. 1;
      DRDY         at 0 range 2 .. 2;
      Reserved_3_6 at 0 range 3 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  I2CS Status
   type SERCOM_STATUS_SERCOM_I2CS_Register is record
      --  Bus Error
      BUSERR         : Boolean := False;
      --  Transmit Collision
      COLL           : Boolean := False;
      --  Read-only. Received Not Acknowledge
      RXNACK         : Boolean := False;
      --  Read-only. Read/Write Direction
      DIR            : Boolean := False;
      --  Read-only. Repeated Start
      SR             : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  SCL Low Timeout
      LOWTOUT        : Boolean := False;
      --  Read-only. Clock Hold
      CLKHOLD        : Boolean := False;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Slave SCL Low Extend Timeout
      SEXTTOUT       : Boolean := False;
      --  High Speed
      HS             : Boolean := False;
      --  Transaction Length Error
      LENERR         : Boolean := False;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_STATUS_SERCOM_I2CS_Register use record
      BUSERR         at 0 range 0 .. 0;
      COLL           at 0 range 1 .. 1;
      RXNACK         at 0 range 2 .. 2;
      DIR            at 0 range 3 .. 3;
      SR             at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      LOWTOUT        at 0 range 6 .. 6;
      CLKHOLD        at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      SEXTTOUT       at 0 range 9 .. 9;
      HS             at 0 range 10 .. 10;
      LENERR         at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
   end record;

   --  I2CS Synchronization Busy
   type SERCOM_SYNCBUSY_SERCOM_I2CS_Register is record
      --  Read-only. Software Reset Synchronization Busy
      SWRST         : Boolean;
      --  Read-only. SERCOM Enable Synchronization Busy
      ENABLE        : Boolean;
      --  unspecified
      Reserved_2_3  : HAL.UInt2;
      --  Read-only. Length Synchronization Busy
      LENGTH        : Boolean;
      --  unspecified
      Reserved_5_31 : HAL.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_SYNCBUSY_SERCOM_I2CS_Register use record
      SWRST         at 0 range 0 .. 0;
      ENABLE        at 0 range 1 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      LENGTH        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype SERCOM_LENGTH_SERCOM_I2CS_LEN_Field is HAL.UInt8;

   --  I2CS Length
   type SERCOM_LENGTH_SERCOM_I2CS_Register is record
      --  Data Length
      LEN           : SERCOM_LENGTH_SERCOM_I2CS_LEN_Field := 16#0#;
      --  Data Length Enable
      LENEN         : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_LENGTH_SERCOM_I2CS_Register use record
      LEN           at 0 range 0 .. 7;
      LENEN         at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
   end record;

   subtype SERCOM_ADDR_SERCOM_I2CS_ADDR_Field is HAL.UInt10;
   subtype SERCOM_ADDR_SERCOM_I2CS_ADDRMASK_Field is HAL.UInt10;

   --  I2CS Address
   type SERCOM_ADDR_SERCOM_I2CS_Register is record
      --  General Call Address Enable
      GENCEN         : Boolean := False;
      --  Address Value
      ADDR           : SERCOM_ADDR_SERCOM_I2CS_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  Ten Bit Addressing Enable
      TENBITEN       : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Address Mask
      ADDRMASK       : SERCOM_ADDR_SERCOM_I2CS_ADDRMASK_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_ADDR_SERCOM_I2CS_Register use record
      GENCEN         at 0 range 0 .. 0;
      ADDR           at 0 range 1 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      TENBITEN       at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      ADDRMASK       at 0 range 17 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   --  I2C Slave Mode
   type SercomI2cs_Cluster is record
      --  I2CS Control A
      CTRLA    : aliased SERCOM_CTRLA_SERCOM_I2CS_Register;
      --  I2CS Control B
      CTRLB    : aliased SERCOM_CTRLB_SERCOM_I2CS_Register;
      --  I2CS Control C
      CTRLC    : aliased SERCOM_CTRLC_SERCOM_I2CS_Register;
      --  I2CS Interrupt Enable Clear
      INTENCLR : aliased SERCOM_INTENCLR_SERCOM_I2CS_Register;
      --  I2CS Interrupt Enable Set
      INTENSET : aliased SERCOM_INTENSET_SERCOM_I2CS_Register;
      --  I2CS Interrupt Flag Status and Clear
      INTFLAG  : aliased SERCOM_INTFLAG_SERCOM_I2CS_Register;
      --  I2CS Status
      STATUS   : aliased SERCOM_STATUS_SERCOM_I2CS_Register;
      --  I2CS Synchronization Busy
      SYNCBUSY : aliased SERCOM_SYNCBUSY_SERCOM_I2CS_Register;
      --  I2CS Length
      LENGTH   : aliased SERCOM_LENGTH_SERCOM_I2CS_Register;
      --  I2CS Address
      ADDR     : aliased SERCOM_ADDR_SERCOM_I2CS_Register;
      --  I2CS Data
      DATA     : aliased HAL.UInt32;
   end record
     with Size => 352;

   for SercomI2cs_Cluster use record
      CTRLA    at 16#0# range 0 .. 31;
      CTRLB    at 16#4# range 0 .. 31;
      CTRLC    at 16#8# range 0 .. 31;
      INTENCLR at 16#14# range 0 .. 7;
      INTENSET at 16#16# range 0 .. 7;
      INTFLAG  at 16#18# range 0 .. 7;
      STATUS   at 16#1A# range 0 .. 15;
      SYNCBUSY at 16#1C# range 0 .. 31;
      LENGTH   at 16#22# range 0 .. 15;
      ADDR     at 16#24# range 0 .. 31;
      DATA     at 16#28# range 0 .. 31;
   end record;

   -----------------------------------
   -- SercomSpi cluster's Registers --
   -----------------------------------

   subtype SERCOM_CTRLA_SERCOM_SPI_MODE_Field is HAL.UInt3;
   subtype SERCOM_CTRLA_SERCOM_SPI_DOPO_Field is HAL.UInt2;
   subtype SERCOM_CTRLA_SERCOM_SPI_DIPO_Field is HAL.UInt2;
   subtype SERCOM_CTRLA_SERCOM_SPI_FORM_Field is HAL.UInt4;

   --  SPI Control A
   type SERCOM_CTRLA_SERCOM_SPI_Register is record
      --  Software Reset
      SWRST          : Boolean := False;
      --  Enable
      ENABLE         : Boolean := False;
      --  Operating Mode
      MODE           : SERCOM_CTRLA_SERCOM_SPI_MODE_Field := 16#0#;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Run during Standby
      RUNSTDBY       : Boolean := False;
      --  Immediate Buffer Overflow Notification
      IBON           : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Data Out Pinout
      DOPO           : SERCOM_CTRLA_SERCOM_SPI_DOPO_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Data In Pinout
      DIPO           : SERCOM_CTRLA_SERCOM_SPI_DIPO_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Frame Format
      FORM           : SERCOM_CTRLA_SERCOM_SPI_FORM_Field := 16#0#;
      --  Clock Phase
      CPHA           : Boolean := False;
      --  Clock Polarity
      CPOL           : Boolean := False;
      --  Data Order
      DORD           : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLA_SERCOM_SPI_Register use record
      SWRST          at 0 range 0 .. 0;
      ENABLE         at 0 range 1 .. 1;
      MODE           at 0 range 2 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      RUNSTDBY       at 0 range 7 .. 7;
      IBON           at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      DOPO           at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      DIPO           at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      FORM           at 0 range 24 .. 27;
      CPHA           at 0 range 28 .. 28;
      CPOL           at 0 range 29 .. 29;
      DORD           at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SERCOM_CTRLB_SERCOM_SPI_CHSIZE_Field is HAL.UInt3;
   subtype SERCOM_CTRLB_SERCOM_SPI_AMODE_Field is HAL.UInt2;

   --  SPI Control B
   type SERCOM_CTRLB_SERCOM_SPI_Register is record
      --  Character Size
      CHSIZE         : SERCOM_CTRLB_SERCOM_SPI_CHSIZE_Field := 16#0#;
      --  unspecified
      Reserved_3_5   : HAL.UInt3 := 16#0#;
      --  Data Preload Enable
      PLOADEN        : Boolean := False;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  Slave Select Low Detect Enable
      SSDE           : Boolean := False;
      --  unspecified
      Reserved_10_12 : HAL.UInt3 := 16#0#;
      --  Master Slave Select Enable
      MSSEN          : Boolean := False;
      --  Address Mode
      AMODE          : SERCOM_CTRLB_SERCOM_SPI_AMODE_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Receiver Enable
      RXEN           : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLB_SERCOM_SPI_Register use record
      CHSIZE         at 0 range 0 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      PLOADEN        at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SSDE           at 0 range 9 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      MSSEN          at 0 range 13 .. 13;
      AMODE          at 0 range 14 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      RXEN           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype SERCOM_CTRLC_SERCOM_SPI_ICSPACE_Field is HAL.UInt6;

   --  SPI Control C
   type SERCOM_CTRLC_SERCOM_SPI_Register is record
      --  Inter-Character Spacing
      ICSPACE        : SERCOM_CTRLC_SERCOM_SPI_ICSPACE_Field := 16#0#;
      --  unspecified
      Reserved_6_23  : HAL.UInt18 := 16#0#;
      --  Data 32 Bit
      DATA32B        : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLC_SERCOM_SPI_Register use record
      ICSPACE        at 0 range 0 .. 5;
      Reserved_6_23  at 0 range 6 .. 23;
      DATA32B        at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  SPI Interrupt Enable Clear
   type SERCOM_INTENCLR_SERCOM_SPI_Register is record
      --  Data Register Empty Interrupt Disable
      DRE          : Boolean := False;
      --  Transmit Complete Interrupt Disable
      TXC          : Boolean := False;
      --  Receive Complete Interrupt Disable
      RXC          : Boolean := False;
      --  Slave Select Low Interrupt Disable
      SSL          : Boolean := False;
      --  unspecified
      Reserved_4_6 : HAL.UInt3 := 16#0#;
      --  Combined Error Interrupt Disable
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTENCLR_SERCOM_SPI_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      SSL          at 0 range 3 .. 3;
      Reserved_4_6 at 0 range 4 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  SPI Interrupt Enable Set
   type SERCOM_INTENSET_SERCOM_SPI_Register is record
      --  Data Register Empty Interrupt Enable
      DRE          : Boolean := False;
      --  Transmit Complete Interrupt Enable
      TXC          : Boolean := False;
      --  Receive Complete Interrupt Enable
      RXC          : Boolean := False;
      --  Slave Select Low Interrupt Enable
      SSL          : Boolean := False;
      --  unspecified
      Reserved_4_6 : HAL.UInt3 := 16#0#;
      --  Combined Error Interrupt Enable
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTENSET_SERCOM_SPI_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      SSL          at 0 range 3 .. 3;
      Reserved_4_6 at 0 range 4 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  SPI Interrupt Flag Status and Clear
   type SERCOM_INTFLAG_SERCOM_SPI_Register is record
      --  Read-only. Data Register Empty Interrupt
      DRE          : Boolean := False;
      --  Transmit Complete Interrupt
      TXC          : Boolean := False;
      --  Read-only. Receive Complete Interrupt
      RXC          : Boolean := False;
      --  Slave Select Low Interrupt Flag
      SSL          : Boolean := False;
      --  unspecified
      Reserved_4_6 : HAL.UInt3 := 16#0#;
      --  Combined Error Interrupt
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTFLAG_SERCOM_SPI_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      SSL          at 0 range 3 .. 3;
      Reserved_4_6 at 0 range 4 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  SPI Status
   type SERCOM_STATUS_SERCOM_SPI_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Buffer Overflow
      BUFOVF         : Boolean := False;
      --  unspecified
      Reserved_3_10  : HAL.UInt8 := 16#0#;
      --  Transaction Length Error
      LENERR         : Boolean := False;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_STATUS_SERCOM_SPI_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      BUFOVF         at 0 range 2 .. 2;
      Reserved_3_10  at 0 range 3 .. 10;
      LENERR         at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
   end record;

   --  SPI Synchronization Busy
   type SERCOM_SYNCBUSY_SERCOM_SPI_Register is record
      --  Read-only. Software Reset Synchronization Busy
      SWRST         : Boolean;
      --  Read-only. SERCOM Enable Synchronization Busy
      ENABLE        : Boolean;
      --  Read-only. CTRLB Synchronization Busy
      CTRLB         : Boolean;
      --  unspecified
      Reserved_3_3  : HAL.Bit;
      --  Read-only. LENGTH Synchronization Busy
      LENGTH        : Boolean;
      --  unspecified
      Reserved_5_31 : HAL.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_SYNCBUSY_SERCOM_SPI_Register use record
      SWRST         at 0 range 0 .. 0;
      ENABLE        at 0 range 1 .. 1;
      CTRLB         at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      LENGTH        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype SERCOM_LENGTH_SERCOM_SPI_LEN_Field is HAL.UInt8;

   --  SPI Length
   type SERCOM_LENGTH_SERCOM_SPI_Register is record
      --  Data Length
      LEN           : SERCOM_LENGTH_SERCOM_SPI_LEN_Field := 16#0#;
      --  Data Length Enable
      LENEN         : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_LENGTH_SERCOM_SPI_Register use record
      LEN           at 0 range 0 .. 7;
      LENEN         at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
   end record;

   subtype SERCOM_ADDR_SERCOM_SPI_ADDR_Field is HAL.UInt8;
   subtype SERCOM_ADDR_SERCOM_SPI_ADDRMASK_Field is HAL.UInt8;

   --  SPI Address
   type SERCOM_ADDR_SERCOM_SPI_Register is record
      --  Address Value
      ADDR           : SERCOM_ADDR_SERCOM_SPI_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Address Mask
      ADDRMASK       : SERCOM_ADDR_SERCOM_SPI_ADDRMASK_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_ADDR_SERCOM_SPI_Register use record
      ADDR           at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      ADDRMASK       at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  SPI Debug Control
   type SERCOM_DBGCTRL_SERCOM_SPI_Register is record
      --  Debug Mode
      DBGSTOP      : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_DBGCTRL_SERCOM_SPI_Register use record
      DBGSTOP      at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
   end record;

   --  SPI Mode
   type SercomSpi_Cluster is record
      --  SPI Control A
      CTRLA    : aliased SERCOM_CTRLA_SERCOM_SPI_Register;
      --  SPI Control B
      CTRLB    : aliased SERCOM_CTRLB_SERCOM_SPI_Register;
      --  SPI Control C
      CTRLC    : aliased SERCOM_CTRLC_SERCOM_SPI_Register;
      --  SPI Baud Rate
      BAUD     : aliased HAL.UInt8;
      --  SPI Interrupt Enable Clear
      INTENCLR : aliased SERCOM_INTENCLR_SERCOM_SPI_Register;
      --  SPI Interrupt Enable Set
      INTENSET : aliased SERCOM_INTENSET_SERCOM_SPI_Register;
      --  SPI Interrupt Flag Status and Clear
      INTFLAG  : aliased SERCOM_INTFLAG_SERCOM_SPI_Register;
      --  SPI Status
      STATUS   : aliased SERCOM_STATUS_SERCOM_SPI_Register;
      --  SPI Synchronization Busy
      SYNCBUSY : aliased SERCOM_SYNCBUSY_SERCOM_SPI_Register;
      --  SPI Length
      LENGTH   : aliased SERCOM_LENGTH_SERCOM_SPI_Register;
      --  SPI Address
      ADDR     : aliased SERCOM_ADDR_SERCOM_SPI_Register;
      --  SPI Data
      DATA     : aliased HAL.UInt32;
      --  SPI Debug Control
      DBGCTRL  : aliased SERCOM_DBGCTRL_SERCOM_SPI_Register;
   end record
     with Size => 416;

   for SercomSpi_Cluster use record
      CTRLA    at 16#0# range 0 .. 31;
      CTRLB    at 16#4# range 0 .. 31;
      CTRLC    at 16#8# range 0 .. 31;
      BAUD     at 16#C# range 0 .. 7;
      INTENCLR at 16#14# range 0 .. 7;
      INTENSET at 16#16# range 0 .. 7;
      INTFLAG  at 16#18# range 0 .. 7;
      STATUS   at 16#1A# range 0 .. 15;
      SYNCBUSY at 16#1C# range 0 .. 31;
      LENGTH   at 16#22# range 0 .. 15;
      ADDR     at 16#24# range 0 .. 31;
      DATA     at 16#28# range 0 .. 31;
      DBGCTRL  at 16#30# range 0 .. 7;
   end record;

   -------------------------------------
   -- SercomUsart cluster's Registers --
   -------------------------------------

   subtype SERCOM_CTRLA_SERCOM_USART_MODE_Field is HAL.UInt3;
   subtype SERCOM_CTRLA_SERCOM_USART_SAMPR_Field is HAL.UInt3;
   subtype SERCOM_CTRLA_SERCOM_USART_TXPO_Field is HAL.UInt2;
   subtype SERCOM_CTRLA_SERCOM_USART_RXPO_Field is HAL.UInt2;
   subtype SERCOM_CTRLA_SERCOM_USART_SAMPA_Field is HAL.UInt2;
   subtype SERCOM_CTRLA_SERCOM_USART_FORM_Field is HAL.UInt4;

   --  USART Control A
   type SERCOM_CTRLA_SERCOM_USART_Register is record
      --  Software Reset
      SWRST          : Boolean := False;
      --  Enable
      ENABLE         : Boolean := False;
      --  Operating Mode
      MODE           : SERCOM_CTRLA_SERCOM_USART_MODE_Field := 16#0#;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Run during Standby
      RUNSTDBY       : Boolean := False;
      --  Immediate Buffer Overflow Notification
      IBON           : Boolean := False;
      --  Transmit Data Invert
      TXINV          : Boolean := False;
      --  Receive Data Invert
      RXINV          : Boolean := False;
      --  unspecified
      Reserved_11_12 : HAL.UInt2 := 16#0#;
      --  Sample
      SAMPR          : SERCOM_CTRLA_SERCOM_USART_SAMPR_Field := 16#0#;
      --  Transmit Data Pinout
      TXPO           : SERCOM_CTRLA_SERCOM_USART_TXPO_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Receive Data Pinout
      RXPO           : SERCOM_CTRLA_SERCOM_USART_RXPO_Field := 16#0#;
      --  Sample Adjustment
      SAMPA          : SERCOM_CTRLA_SERCOM_USART_SAMPA_Field := 16#0#;
      --  Frame Format
      FORM           : SERCOM_CTRLA_SERCOM_USART_FORM_Field := 16#0#;
      --  Communication Mode
      CMODE          : Boolean := False;
      --  Clock Polarity
      CPOL           : Boolean := False;
      --  Data Order
      DORD           : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLA_SERCOM_USART_Register use record
      SWRST          at 0 range 0 .. 0;
      ENABLE         at 0 range 1 .. 1;
      MODE           at 0 range 2 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      RUNSTDBY       at 0 range 7 .. 7;
      IBON           at 0 range 8 .. 8;
      TXINV          at 0 range 9 .. 9;
      RXINV          at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      SAMPR          at 0 range 13 .. 15;
      TXPO           at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      RXPO           at 0 range 20 .. 21;
      SAMPA          at 0 range 22 .. 23;
      FORM           at 0 range 24 .. 27;
      CMODE          at 0 range 28 .. 28;
      CPOL           at 0 range 29 .. 29;
      DORD           at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SERCOM_CTRLB_SERCOM_USART_CHSIZE_Field is HAL.UInt3;
   subtype SERCOM_CTRLB_SERCOM_USART_LINCMD_Field is HAL.UInt2;

   --  USART Control B
   type SERCOM_CTRLB_SERCOM_USART_Register is record
      --  Character Size
      CHSIZE         : SERCOM_CTRLB_SERCOM_USART_CHSIZE_Field := 16#0#;
      --  unspecified
      Reserved_3_5   : HAL.UInt3 := 16#0#;
      --  Stop Bit Mode
      SBMODE         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Collision Detection Enable
      COLDEN         : Boolean := False;
      --  Start of Frame Detection Enable
      SFDE           : Boolean := False;
      --  Encoding Format
      ENC            : Boolean := False;
      --  unspecified
      Reserved_11_12 : HAL.UInt2 := 16#0#;
      --  Parity Mode
      PMODE          : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Transmitter Enable
      TXEN           : Boolean := False;
      --  Receiver Enable
      RXEN           : Boolean := False;
      --  unspecified
      Reserved_18_23 : HAL.UInt6 := 16#0#;
      --  Write-only. LIN Command
      LINCMD         : SERCOM_CTRLB_SERCOM_USART_LINCMD_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLB_SERCOM_USART_Register use record
      CHSIZE         at 0 range 0 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      SBMODE         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      COLDEN         at 0 range 8 .. 8;
      SFDE           at 0 range 9 .. 9;
      ENC            at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      PMODE          at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      TXEN           at 0 range 16 .. 16;
      RXEN           at 0 range 17 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      LINCMD         at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype SERCOM_CTRLC_SERCOM_USART_GTIME_Field is HAL.UInt3;
   subtype SERCOM_CTRLC_SERCOM_USART_BRKLEN_Field is HAL.UInt2;
   subtype SERCOM_CTRLC_SERCOM_USART_HDRDLY_Field is HAL.UInt2;
   subtype SERCOM_CTRLC_SERCOM_USART_MAXITER_Field is HAL.UInt3;
   subtype SERCOM_CTRLC_SERCOM_USART_DATA32B_Field is HAL.UInt2;

   --  USART Control C
   type SERCOM_CTRLC_SERCOM_USART_Register is record
      --  Guard Time
      GTIME          : SERCOM_CTRLC_SERCOM_USART_GTIME_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  LIN Master Break Length
      BRKLEN         : SERCOM_CTRLC_SERCOM_USART_BRKLEN_Field := 16#0#;
      --  LIN Master Header Delay
      HDRDLY         : SERCOM_CTRLC_SERCOM_USART_HDRDLY_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Inhibit Not Acknowledge
      INACK          : Boolean := False;
      --  Disable Successive NACK
      DSNACK         : Boolean := False;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Maximum Iterations
      MAXITER        : SERCOM_CTRLC_SERCOM_USART_MAXITER_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Data 32 Bit
      DATA32B        : SERCOM_CTRLC_SERCOM_USART_DATA32B_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLC_SERCOM_USART_Register use record
      GTIME          at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      BRKLEN         at 0 range 8 .. 9;
      HDRDLY         at 0 range 10 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      INACK          at 0 range 16 .. 16;
      DSNACK         at 0 range 17 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      MAXITER        at 0 range 20 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      DATA32B        at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype SERCOM_BAUD_FRAC_MODE_SERCOM_USART_BAUD_Field is HAL.UInt13;
   subtype SERCOM_BAUD_FRAC_MODE_SERCOM_USART_FP_Field is HAL.UInt3;

   --  USART Baud Rate
   type SERCOM_BAUD_FRAC_MODE_SERCOM_USART_Register is record
      --  Baud Rate Value
      BAUD : SERCOM_BAUD_FRAC_MODE_SERCOM_USART_BAUD_Field := 16#0#;
      --  Fractional Part
      FP   : SERCOM_BAUD_FRAC_MODE_SERCOM_USART_FP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_BAUD_FRAC_MODE_SERCOM_USART_Register use record
      BAUD at 0 range 0 .. 12;
      FP   at 0 range 13 .. 15;
   end record;

   subtype SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_BAUD_Field is HAL.UInt13;
   subtype SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_FP_Field is HAL.UInt3;

   --  USART Baud Rate
   type SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_Register is record
      --  Baud Rate Value
      BAUD : SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_BAUD_Field := 16#0#;
      --  Fractional Part
      FP   : SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_FP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_Register use record
      BAUD at 0 range 0 .. 12;
      FP   at 0 range 13 .. 15;
   end record;

   --  USART Interrupt Enable Clear
   type SERCOM_INTENCLR_SERCOM_USART_Register is record
      --  Data Register Empty Interrupt Disable
      DRE          : Boolean := False;
      --  Transmit Complete Interrupt Disable
      TXC          : Boolean := False;
      --  Receive Complete Interrupt Disable
      RXC          : Boolean := False;
      --  Receive Start Interrupt Disable
      RXS          : Boolean := False;
      --  Clear To Send Input Change Interrupt Disable
      CTSIC        : Boolean := False;
      --  Break Received Interrupt Disable
      RXBRK        : Boolean := False;
      --  unspecified
      Reserved_6_6 : HAL.Bit := 16#0#;
      --  Combined Error Interrupt Disable
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTENCLR_SERCOM_USART_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      RXS          at 0 range 3 .. 3;
      CTSIC        at 0 range 4 .. 4;
      RXBRK        at 0 range 5 .. 5;
      Reserved_6_6 at 0 range 6 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  USART Interrupt Enable Set
   type SERCOM_INTENSET_SERCOM_USART_Register is record
      --  Data Register Empty Interrupt Enable
      DRE          : Boolean := False;
      --  Transmit Complete Interrupt Enable
      TXC          : Boolean := False;
      --  Receive Complete Interrupt Enable
      RXC          : Boolean := False;
      --  Receive Start Interrupt Enable
      RXS          : Boolean := False;
      --  Clear To Send Input Change Interrupt Enable
      CTSIC        : Boolean := False;
      --  Break Received Interrupt Enable
      RXBRK        : Boolean := False;
      --  unspecified
      Reserved_6_6 : HAL.Bit := 16#0#;
      --  Combined Error Interrupt Enable
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTENSET_SERCOM_USART_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      RXS          at 0 range 3 .. 3;
      CTSIC        at 0 range 4 .. 4;
      RXBRK        at 0 range 5 .. 5;
      Reserved_6_6 at 0 range 6 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  USART Interrupt Flag Status and Clear
   type SERCOM_INTFLAG_SERCOM_USART_Register is record
      --  Read-only. Data Register Empty Interrupt
      DRE          : Boolean := False;
      --  Transmit Complete Interrupt
      TXC          : Boolean := False;
      --  Read-only. Receive Complete Interrupt
      RXC          : Boolean := False;
      --  Write-only. Receive Start Interrupt
      RXS          : Boolean := False;
      --  Clear To Send Input Change Interrupt
      CTSIC        : Boolean := False;
      --  Break Received Interrupt
      RXBRK        : Boolean := False;
      --  unspecified
      Reserved_6_6 : HAL.Bit := 16#0#;
      --  Combined Error Interrupt
      ERROR        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_INTFLAG_SERCOM_USART_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      RXS          at 0 range 3 .. 3;
      CTSIC        at 0 range 4 .. 4;
      RXBRK        at 0 range 5 .. 5;
      Reserved_6_6 at 0 range 6 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   --  USART Status
   type SERCOM_STATUS_SERCOM_USART_Register is record
      --  Parity Error
      PERR          : Boolean := False;
      --  Frame Error
      FERR          : Boolean := False;
      --  Buffer Overflow
      BUFOVF        : Boolean := False;
      --  Read-only. Clear To Send
      CTS           : Boolean := False;
      --  Inconsistent Sync Field
      ISF           : Boolean := False;
      --  Collision Detected
      COLL          : Boolean := False;
      --  Read-only. Transmitter Empty
      TXE           : Boolean := False;
      --  Maximum Number of Repetitions Reached
      ITER          : Boolean := False;
      --  unspecified
      Reserved_8_15 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_STATUS_SERCOM_USART_Register use record
      PERR          at 0 range 0 .. 0;
      FERR          at 0 range 1 .. 1;
      BUFOVF        at 0 range 2 .. 2;
      CTS           at 0 range 3 .. 3;
      ISF           at 0 range 4 .. 4;
      COLL          at 0 range 5 .. 5;
      TXE           at 0 range 6 .. 6;
      ITER          at 0 range 7 .. 7;
      Reserved_8_15 at 0 range 8 .. 15;
   end record;

   --  USART Synchronization Busy
   type SERCOM_SYNCBUSY_SERCOM_USART_Register is record
      --  Read-only. Software Reset Synchronization Busy
      SWRST         : Boolean;
      --  Read-only. SERCOM Enable Synchronization Busy
      ENABLE        : Boolean;
      --  Read-only. CTRLB Synchronization Busy
      CTRLB         : Boolean;
      --  Read-only. RXERRCNT Synchronization Busy
      RXERRCNT      : Boolean;
      --  Read-only. LENGTH Synchronization Busy
      LENGTH        : Boolean;
      --  unspecified
      Reserved_5_31 : HAL.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_SYNCBUSY_SERCOM_USART_Register use record
      SWRST         at 0 range 0 .. 0;
      ENABLE        at 0 range 1 .. 1;
      CTRLB         at 0 range 2 .. 2;
      RXERRCNT      at 0 range 3 .. 3;
      LENGTH        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype SERCOM_LENGTH_SERCOM_USART_LEN_Field is HAL.UInt8;
   subtype SERCOM_LENGTH_SERCOM_USART_LENEN_Field is HAL.UInt2;

   --  USART Length
   type SERCOM_LENGTH_SERCOM_USART_Register is record
      --  Data Length
      LEN            : SERCOM_LENGTH_SERCOM_USART_LEN_Field := 16#0#;
      --  Data Length Enable
      LENEN          : SERCOM_LENGTH_SERCOM_USART_LENEN_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_LENGTH_SERCOM_USART_Register use record
      LEN            at 0 range 0 .. 7;
      LENEN          at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
   end record;

   --  USART Debug Control
   type SERCOM_DBGCTRL_SERCOM_USART_Register is record
      --  Debug Mode
      DBGSTOP      : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for SERCOM_DBGCTRL_SERCOM_USART_Register use record
      DBGSTOP      at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
   end record;

   type SercomUsart_Disc is
     (Default,
      Frac_Mode,
      Fracfp_Mode,
      Usartfp_Mode);

   pragma Warnings (Off, "bits of * unused");
   --  USART Mode
   type SercomUsart_Cluster
     (Discriminent : SercomUsart_Disc := Default)
   is record
      --  USART Control A
      CTRLA             : aliased SERCOM_CTRLA_SERCOM_USART_Register;
      --  USART Control B
      CTRLB             : aliased SERCOM_CTRLB_SERCOM_USART_Register;
      --  USART Control C
      CTRLC             : aliased SERCOM_CTRLC_SERCOM_USART_Register;
      --  USART Receive Pulse Length
      RXPL              : aliased HAL.UInt8;
      --  USART Interrupt Enable Clear
      INTENCLR          : aliased SERCOM_INTENCLR_SERCOM_USART_Register;
      --  USART Interrupt Enable Set
      INTENSET          : aliased SERCOM_INTENSET_SERCOM_USART_Register;
      --  USART Interrupt Flag Status and Clear
      INTFLAG           : aliased SERCOM_INTFLAG_SERCOM_USART_Register;
      --  USART Status
      STATUS            : aliased SERCOM_STATUS_SERCOM_USART_Register;
      --  USART Synchronization Busy
      SYNCBUSY          : aliased SERCOM_SYNCBUSY_SERCOM_USART_Register;
      --  USART Receive Error Count
      RXERRCNT          : aliased HAL.UInt8;
      --  USART Length
      LENGTH            : aliased SERCOM_LENGTH_SERCOM_USART_Register;
      --  USART Data
      DATA              : aliased HAL.UInt32;
      --  USART Debug Control
      DBGCTRL           : aliased SERCOM_DBGCTRL_SERCOM_USART_Register;
      case Discriminent is
         when Default =>
            --  USART Baud Rate
            BAUD : aliased HAL.UInt16;
         when Frac_Mode =>
            --  USART Baud Rate
            BAUD_FRAC_MODE : aliased SERCOM_BAUD_FRAC_MODE_SERCOM_USART_Register;
         when Fracfp_Mode =>
            --  USART Baud Rate
            BAUD_FRACFP_MODE : aliased SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_Register;
         when Usartfp_Mode =>
            --  USART Baud Rate
            BAUD_USARTFP_MODE : aliased HAL.UInt16;
      end case;
   end record
     with Unchecked_Union, Size => 416;
   pragma Warnings (On, "bits of * unused");

   for SercomUsart_Cluster use record
      CTRLA             at 16#0# range 0 .. 31;
      CTRLB             at 16#4# range 0 .. 31;
      CTRLC             at 16#8# range 0 .. 31;
      RXPL              at 16#E# range 0 .. 7;
      INTENCLR          at 16#14# range 0 .. 7;
      INTENSET          at 16#16# range 0 .. 7;
      INTFLAG           at 16#18# range 0 .. 7;
      STATUS            at 16#1A# range 0 .. 15;
      SYNCBUSY          at 16#1C# range 0 .. 31;
      RXERRCNT          at 16#20# range 0 .. 7;
      LENGTH            at 16#22# range 0 .. 15;
      DATA              at 16#28# range 0 .. 31;
      DBGCTRL           at 16#30# range 0 .. 7;
      BAUD              at 16#C# range 0 .. 15;
      BAUD_FRAC_MODE    at 16#C# range 0 .. 15;
      BAUD_FRACFP_MODE  at 16#C# range 0 .. 15;
      BAUD_USARTFP_MODE at 16#C# range 0 .. 15;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type SERCOM0_Disc is
     (I2Cm,
      I2Cs,
      Spi_Mode,
      Usart);

   --  Serial Communication Interface 0
   type SERCOM_Internal
     (Discriminent : SERCOM0_Disc := I2Cm)
   is record
      case Discriminent is
         when I2Cm =>
            --  I2C Master Mode
            SERCOM_I2CM : aliased SercomI2cm_Cluster;
         when I2Cs =>
            --  I2C Slave Mode
            SERCOM_I2CS : aliased SercomI2cs_Cluster;
         when Spi_Mode =>
            --  SPI Mode
            SERCOM_SPI : aliased SercomSpi_Cluster;
         when Usart =>
            --  USART Mode
            SERCOM_USART : aliased SercomUsart_Cluster;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for SERCOM_Internal use record
      SERCOM_I2CM  at 0 range 0 .. 415;
      SERCOM_I2CS  at 0 range 0 .. 351;
      SERCOM_SPI   at 0 range 0 .. 415;
      SERCOM_USART at 0 range 0 .. 415;
   end record;

end SAM.SERCOM;
