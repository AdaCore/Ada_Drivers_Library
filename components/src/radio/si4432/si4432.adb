------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

with Interfaces; use Interfaces;

package body Si4432 is

   function From_Boolean (Value : Boolean) return Bit;
   function To_Boolean (Value : Bit) return Boolean;

   function To_Synchronization_Word_Index
     (Index : UInt2)
      return Register_Name;

   function To_Transmit_Header_Index
     (Index : UInt2)
      return Register_Name;

   function To_Check_Header_Index
     (Index : UInt2)
      return Register_Name;

   function To_Header_Enable_Index
     (Index : UInt2)
      return Register_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This    : out Si4432_Driver;
      CSN_Pin : HAL.GPIO.Any_GPIO_Point;
      SDN_Pin : HAL.GPIO.Any_GPIO_Point;
      SPI     : HAL.SPI.Any_SPI_Port) is
   begin
      if CSN_Pin = null then
         This.Holder := Holder_Type'
           (Kind    => Hardware,
            SPI     => SPI,
            SDN_Pin => SDN_Pin);
      else
         This.Holder := Holder_Type'
           (Kind    => Software,
            SPI     => SPI,
            CSN_Pin => CSN_Pin,
            SDN_Pin => SDN_Pin);
      end if;
   end Initialize;

   ----------------------
   -- Device_Type_Code --
   ----------------------

   function Device_Type_Code (This : Si4432_Driver) return UInt8 is
   begin
      return UInt8 (Read_Register (This, Device_Type_Code_Name));
   end Device_Type_Code;

   ------------------
   -- Version_Code --
   ------------------

   function Version_Code (This : Si4432_Driver) return UInt8 is
   begin
      return UInt8 (Read_Register (This, Version_Code_Name));
   end Version_Code;

   ---------------
   -- Set_Power --
   ---------------

   procedure Set_Power
     (This : Si4432_Driver;
      Mode : On_Off) is
   begin
      if This.Holder.SDN_Pin /= null then
         if Mode = Off then
            This.Holder.SDN_Pin.Set;
         else
            This.Holder.SDN_Pin.Clear;
         end if;
      end if;
   end Set_Power;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
     (This  : Si4432_Driver;
      State : Chip_State)
   is
      Data : Register := Read_Register
        (This, Operating_Mode_And_Function_Control_1_Name);
      Reg  : Operating_Mode_And_Function_Control_1_Register with Import,
        Address => Data'Address;
   begin
      Reg.xton := 1;
      case State is
         when Idle =>
            Reg.rxon := 0;
            Reg.txon := 0;

         when RX =>
            Reg.rxon := 1;
            Reg.txon := 0;

         when TX =>
            Reg.rxon := 0;
            Reg.txon := 1;
      end case;

      Write_Register (This, Operating_Mode_And_Function_Control_1_Name, Data);
   end Set_State;

   --------------------
   -- Software_Reset --
   --------------------

   procedure Software_Reset (This : Si4432_Driver)
   is
      Data     : Register := 0;
      Register : Operating_Mode_And_Function_Control_1_Register with Import,
        Address => Data'Address;
   begin
      Register.swres := 1;
      Write_Register (This, Operating_Mode_And_Function_Control_1_Name, Data);
   end Software_Reset;

   ----------------------------
   -- Get_Interrupt_Statuses --
   ----------------------------

   function Get_Interrupt_Statuses
     (This : Si4432_Driver)
      return Interrupt_Statuses
   is
      Data : Interrupt_Statuses;
   begin
      Read_Register (This, Interrupt_Status_1_Name, Data);
      return Data;
   end Get_Interrupt_Statuses;

   ----------------------------
   -- Get_Interrupt_Status_1 --
   ----------------------------

   function Get_Interrupt_Status_1
     (This : Si4432_Driver)
      return UInt8
   is
      Data : constant Register :=
        Read_Register (This, Interrupt_Status_1_Name);
   begin
      return UInt8 (Data);
   end Get_Interrupt_Status_1;

   ----------------------------
   -- Get_Interrupt_Status_2 --
   ----------------------------

   function Get_Interrupt_Status_2
     (This : Si4432_Driver)
      return UInt8
   is
      Data : constant Register :=
        Read_Register (This, Interrupt_Status_2_Name);
   begin
      return UInt8 (Data);
   end Get_Interrupt_Status_2;

   ----------------------
   -- Clear_Interrupts --
   ----------------------

   procedure Clear_Interrupts (This : Si4432_Driver) is
      Dummy : Interrupt_Statuses;
   begin
      Dummy := Get_Interrupt_Statuses (This);
   end Clear_Interrupts;

   -------------------
   -- Set_Frequency --
   -------------------

   procedure Set_Frequency
     (This  : Si4432_Driver;
      Value : Frequency)
   is
      Data      : HAL.SPI.SPI_Data_8b (1 .. 3) := (others => 0);
      Reg       : Frequency_Band_Select_Register with Import,
        Address => Data (1)'Address;

      High_Band : constant Natural := (if Value >= 480.00 then 1 else 0);
      Calc      : constant Float :=
        (Float (Value) / Float (10 * (High_Band + 1))) - 24.0;
      Band      : UInt5;
      Carrier   : Unsigned_16;

   begin
      Band    := UInt5 (Float'Truncation (Calc));
      Carrier := Unsigned_16 ((Calc - Float (Band)) * 64000.0);

      Reg.fb    := Band;
      Reg.hbsel := Bit (High_Band);
      Reg.sbsel := 1;

      Data (2) := UInt8 (Shift_Right (Carrier, 8));
      Data (3) := UInt8 (Carrier and 16#FF#);

      Write_Register (This, Frequency_Band_Select_Name, Data);
   end Set_Frequency;

   ------------------------
   -- Set_Frequency_Band --
   ------------------------

   procedure Set_Frequency_Band
     (This      : Si4432_Driver;
      Band      : UInt5;
      High_Band : Boolean;
      Side_Band : Boolean)
   is
      Reg  : constant Frequency_Band_Select_Register :=
        (fb       => Band,
         hbsel    => From_Boolean (High_Band),
         sbsel    => From_Boolean (Side_Band),
         Reserved => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Frequency_Band_Select_Name, Data);
   end Set_Frequency_Band;

   ------------------------
   -- Get_Frequency_Band --
   ------------------------

   procedure Get_Frequency_Band
     (This      : Si4432_Driver;
      Band      : out UInt5;
      High_Band : out Boolean;
      Side_Band : out Boolean)
   is
      Data     : Register;
      Register : Frequency_Band_Select_Register with Import,
        Address => Data'Address;
   begin
      Data      := Read_Register (This, Frequency_Band_Select_Name);
      Band      := Register.fb;
      High_Band := To_Boolean (Register.hbsel);
      Side_Band := To_Boolean (Register.sbsel);
   end Get_Frequency_Band;

   -----------------------------------
   -- Set_Nominal_Carrier_Frequency --
   -----------------------------------

   procedure Set_Nominal_Carrier_Frequency
     (This  : Si4432_Driver;
      Value : UInt16)
   is
      Local : constant Unsigned_16 := Unsigned_16 (Value);
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
   begin
      Data (1) := UInt8 (Shift_Right (Local, 8));
      Data (2) := UInt8 (Local and 16#FF#);

      Write_Register (This, Nominal_Carrier_Frequency_1_Name, Data);
   end Set_Nominal_Carrier_Frequency;

   -----------------------------------
   -- Get_Nominal_Carrier_Frequency --
   -----------------------------------

   function Get_Nominal_Carrier_Frequency
     (This : Si4432_Driver)
      return UInt16
   is
      Data : HAL.SPI.SPI_Data_8b (1 .. 2);
   begin
      Read_Register (This, Nominal_Carrier_Frequency_1_Name, Data);

      return UInt16
        (Shift_Left (Unsigned_16 (Data (1)), 8) + Unsigned_16 (Data (2)));
   end Get_Nominal_Carrier_Frequency;

   --------------------------
   -- Set_Frequency_Offset --
   --------------------------

   procedure Set_Frequency_Offset
     (This  : Si4432_Driver;
      Value : Frequency_Offset)
   is
      Local : constant Unsigned_16 := Unsigned_16 (Value);
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
   begin
      Data (1) := UInt8 (Local and 16#FF#);
      Data (2) := UInt8 (Shift_Right (Local, 8));

      Write_Register (This, Frequency_Offset_1_Name, Data);
   end Set_Frequency_Offset;

   --------------------------
   -- Get_Frequency_Offset --
   --------------------------

   function Get_Frequency_Offset
     (This : Si4432_Driver)
      return Frequency_Offset
   is
      Data : HAL.SPI.SPI_Data_8b (1 .. 2);
   begin
      Read_Register (This, Frequency_Offset_1_Name, Data);

      return Frequency_Offset
        (Shift_Left (Unsigned_16 (Data (2)), 8) + Unsigned_16 (Data (1)));
   end Get_Frequency_Offset;

   ------------------------------------------
   -- Set_Frequency_Hopping_Channel_Select --
   ------------------------------------------

   procedure Set_Frequency_Hopping_Channel_Select
     (This  : Si4432_Driver;
      Value : UInt8) is
   begin
      Write_Register
        (This,
         Frequency_Hopping_Channel_Select_Name,
         Register (Value));
   end Set_Frequency_Hopping_Channel_Select;

   ------------------------------------------
   -- Get_Frequency_Hopping_Channel_Select --
   ------------------------------------------

   function Get_Frequency_Hopping_Channel_Select
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8
        (Read_Register (This, Frequency_Hopping_Channel_Select_Name));
   end Get_Frequency_Hopping_Channel_Select;

   -------------------------------------
   -- Set_Frequency_Hopping_Step_Size --
   -------------------------------------

   procedure Set_Frequency_Hopping_Step_Size
     (This  : Si4432_Driver;
      Value : UInt8) is
   begin
      Write_Register
        (This,
         Frequency_Hopping_Step_Size_Name,
         Register (Value));
   end Set_Frequency_Hopping_Step_Size;

   -------------------------------------
   -- Get_Frequency_Hopping_Step_Size --
   -------------------------------------

   function Get_Frequency_Hopping_Step_Size
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8
        (Read_Register (This, Frequency_Hopping_Step_Size_Name));
   end Get_Frequency_Hopping_Step_Size;

   ----------------------
   -- Set_TX_Data_Rate --
   ----------------------

   procedure Set_TX_Data_Rate
     (This  : Si4432_Driver;
      Value : UInt16)
   is
      Local : constant Unsigned_16 := Unsigned_16 (Value);
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
   begin
      Data (1) := UInt8 (Shift_Right (Local, 8));
      Data (2) := UInt8 (Local and 16#FF#);

      Write_Register (This, TX_Data_Rate_1_Name, Data);
   end Set_TX_Data_Rate;

   ----------------------
   -- Get_TX_Data_Rate --
   ----------------------

   function Get_TX_Data_Rate
     (This : Si4432_Driver)
      return UInt16
   is
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
   begin
      Read_Register (This, TX_Data_Rate_1_Name, Data);

      return UInt16
        (Shift_Left (Unsigned_16 (Data (1)), 8) + Unsigned_16 (Data (2)));
   end Get_TX_Data_Rate;

   --------------------------
   -- Set_Data_Rates_Below --
   --------------------------

   procedure Set_Data_Rates_Below
     (This  : Si4432_Driver;
      Value : Boolean)
   is
      Data : Register := Read_Register (This, Modulation_Mode_Control_1_Name);
      Reg  : Modulation_Mode_Control_1_Register with Import,
        Address => Data'Address;
   begin
      Reg.txdtrtscale := From_Boolean (Value);
      Write_Register (This, Modulation_Mode_Control_1_Name, Data);
   end Set_Data_Rates_Below;

   -----------------------------------
   -- Set_Modulation_Mode_Control_1 --
   -----------------------------------

   procedure Set_Modulation_Mode_Control_1
     (This                         : Si4432_Driver;
      Data_Whitening               : Boolean;
      Manchester_Coding            : Boolean;
      Manchester_Data_Inversion    : Boolean;
      Manchester_Preamble_Polarity : Boolean;
      Packet_Handler_Down          : Boolean;
      Data_Rates_Below             : Boolean)
   is
      Reg  : constant Modulation_Mode_Control_1_Register :=
        (enwhite     => From_Boolean (Data_Whitening),
         enmanch     => From_Boolean (Manchester_Coding),
         enmaninv    => From_Boolean (Manchester_Data_Inversion),
         manppol     => From_Boolean (Manchester_Preamble_Polarity),
         enphpwdn    => From_Boolean (Packet_Handler_Down),
         txdtrtscale => From_Boolean (Data_Rates_Below),
         Reserved    => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Modulation_Mode_Control_1_Name, Data);
   end Set_Modulation_Mode_Control_1;

   -----------------------------------
   -- Get_Modulation_Mode_Control_1 --
   -----------------------------------

   procedure Get_Modulation_Mode_Control_1
     (This                         : Si4432_Driver;
      Data_Whitening               : out Boolean;
      Manchester_Coding            : out Boolean;
      Manchester_Data_Inversion    : out Boolean;
      Manchester_Preamble_Polarity : out Boolean;
      Packet_Handler_Down          : out Boolean;
      Data_Rates_Below             : out Boolean)
   is
      Data : Register;
      Reg  : Modulation_Mode_Control_1_Register with Import,
        Address => Data'Address;

   begin
      Data := Read_Register (This, Modulation_Mode_Control_1_Name);

      Data_Whitening               := To_Boolean (Reg.enwhite);
      Manchester_Coding            := To_Boolean (Reg.enmanch);
      Manchester_Data_Inversion    := To_Boolean (Reg.enmaninv);
      Manchester_Preamble_Polarity := To_Boolean (Reg.manppol);
      Packet_Handler_Down          := To_Boolean (Reg.enphpwdn);
      Data_Rates_Below             := To_Boolean (Reg.txdtrtscale);
   end Get_Modulation_Mode_Control_1;

   -----------------------------------
   -- Set_Modulation_Mode_Control_2 --
   -----------------------------------

   procedure Set_Modulation_Mode_Control_2
     (This              : Si4432_Driver;
      Modulation        : Modulation_Mode;
      Invert_TX_RX      : Boolean;
      Source            : Modulation_Source;
      TX_Data_Clock     : Modulation_TX_Data_Clock)
   is
      Data : Register;
      Reg  : Modulation_Mode_Control_2_Register with Import,
        Address => Data'Address;
   begin
      Data := Read_Register (This, Modulation_Mode_Control_2_Name);

      Reg.modtyp := Modulation;
      Reg.eninv  := From_Boolean (Invert_TX_RX);
      Reg.dtmod  := Source;
      Reg.trclk  := TX_Data_Clock;

      Write_Register (This, Modulation_Mode_Control_2_Name, Data);
   end Set_Modulation_Mode_Control_2;

   -----------------------------------
   -- Get_Modulation_Mode_Control_2 --
   -----------------------------------

   procedure Get_Modulation_Mode_Control_2
     (This              : Si4432_Driver;
      Modulation        : out Modulation_Mode;
      Invert_TX_RX      : out Boolean;
      Source            : out Modulation_Source;
      TX_Data_Clock     : out Modulation_TX_Data_Clock)
   is
      Data : Register;
      Reg  : Modulation_Mode_Control_2_Register with Import,
        Address => Data'Address;
   begin
      Data := Read_Register (This, Modulation_Mode_Control_2_Name);

      Modulation    := Reg.modtyp;
      Invert_TX_RX  := To_Boolean (Reg.eninv);
      Source        := Reg.dtmod;
      TX_Data_Clock := Reg.trclk;
   end Get_Modulation_Mode_Control_2;

   --------------------------------
   -- Set_Frequency_Deviation_Hz --
   --------------------------------

   procedure Set_Frequency_Deviation_Hz
     (This  : Si4432_Driver;
      Value : Frequency_Deviation_Hz) is
   begin
      Set_Frequency_Deviation (This, UInt9 (Value / 625));
   end Set_Frequency_Deviation_Hz;

   -----------------------------
   -- Set_Frequency_Deviation --
   -----------------------------

   procedure Set_Frequency_Deviation
     (This  : Si4432_Driver;
      Value : UInt9)
   is
      Data : constant Unsigned_16 := Unsigned_16 (Value);
   begin
      Write_Register
        (This,
         Frequency_Deviation_Name,
         Register (Data and 16#FF#));

      if (Data and 2#1_0000_0000#) /= 0 then
         declare
            R    : Register := Read_Register
              (This, Modulation_Mode_Control_2_Name);
            Reg  : Modulation_Mode_Control_2_Register with Import,
              Address => R'Address;
         begin
            Reg.fd := 1;
            Write_Register (This, Modulation_Mode_Control_2_Name, R);
         end;
      end if;
   end Set_Frequency_Deviation;

   -----------------------------
   -- Get_Frequency_Deviation --
   -----------------------------

   function Get_Frequency_Deviation
     (This : Si4432_Driver)
      return UInt9
   is
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg   : Modulation_Mode_Control_2_Register with Import,
        Address => Data (1)'Address;

   begin
      Read_Register (This, Modulation_Mode_Control_2_Name, Data);

      return (if Reg.fd = 1 then 256 else 0) +
        UInt9 (Data (2));
   end Get_Frequency_Deviation;

   ------------------------
   -- Set_Header_Control --
   ------------------------

   procedure Set_Header_Control
     (This                    : Si4432_Driver;
      Header_Bytes_Check      : UInt4 := 16#C#;
      Broadcast_Address_Check : UInt4 := 0;
      Synchronization         : Synchronization_Word_Length := Word_3_2;
      Fix_TR_Packet_Length    : Boolean := False;
      Header                  : Header_Length := Header_3_2;
      Skipsyn                 : Boolean := False)
   is
      Data : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg1 : Header_Control_1_Register :=
        (hdch => Header_Bytes_Check,
         bcen => Broadcast_Address_Check)
          with Address => Data (1)'Address;
      Reg2 : Header_Control_2_Register with Import,
        Address => Data (2)'Address;

   begin
      Data (2) := UInt8 (Read_Register (This, Header_Control_2_Name));

      Reg2.synclen  := Synchronization;
      Reg2.fixpklen := From_Boolean (Fix_TR_Packet_Length);
      Reg2.hdlen    := Header;
      Reg2.skipsyn  := From_Boolean (Skipsyn);

      Write_Register (This, Header_Control_1_Name, Data);
   end Set_Header_Control;

   --------------------------
   -- Set_Header_Control_1 --
   --------------------------

   procedure Set_Header_Control_1
     (This                    : Si4432_Driver;
      Header_Bytes_Check      : UInt4;
      Broadcast_Address_Check : UInt4)
   is
      Reg : constant Header_Control_1_Register :=
        (hdch => Header_Bytes_Check,
         bcen => Broadcast_Address_Check);
      R   : Register with Import, Address => Reg'Address;

   begin
      Write_Register (This, Header_Control_1_Name, R);
   end Set_Header_Control_1;

   --------------------------
   -- Get_Header_Control_1 --
   --------------------------

   procedure Get_Header_Control_1
     (This                    : Si4432_Driver;
      Header_Bytes_Check      : out UInt4;
      Broadcast_Address_Check : out UInt4)
   is
      R   : Register;
      Reg : Header_Control_1_Register with Import,
        Address => R'Address;

   begin
      R := Read_Register (This, Header_Control_1_Name);

      Header_Bytes_Check      := Reg.hdch;
      Broadcast_Address_Check := Reg.bcen;
   end Get_Header_Control_1;

   --------------------------
   -- Set_Header_Control_2 --
   --------------------------

   procedure Set_Header_Control_2
     (This                 : Si4432_Driver;
      Synchronization      : Synchronization_Word_Length;
      Fix_TR_Packet_Length : Boolean;
      Header               : Header_Length;
      Skipsyn              : Boolean)
   is
      R   : Register := Read_Register (This, Header_Control_2_Name);
      Reg : Header_Control_2_Register with Import,
        Address => R'Address;

   begin
      Reg.synclen  := Synchronization;
      Reg.fixpklen := From_Boolean (Fix_TR_Packet_Length);
      Reg.hdlen    := Header;
      Reg.skipsyn  := From_Boolean (Skipsyn);

      Write_Register (This, Header_Control_2_Name, R);
   end Set_Header_Control_2;

   --------------------------
   -- Get_Header_Control_2 --
   --------------------------

   procedure Get_Header_Control_2
     (This                 : Si4432_Driver;
      Synchronization      : out Synchronization_Word_Length;
      Fix_TR_Packet_Length : out Boolean;
      Header               : out Header_Length;
      Skipsyn              : out Boolean)
   is
      R   : Register := Read_Register (This, Header_Control_2_Name);
      Reg : Header_Control_2_Register with Import,
        Address => R'Address;

   begin
      Synchronization      := Reg.synclen;
      Fix_TR_Packet_Length := To_Boolean (Reg.fixpklen);
      Header               := Reg.hdlen;
      Skipsyn              := To_Boolean (Reg.skipsyn);
   end Get_Header_Control_2;

   ------------------------------
   -- Set_Synchronization_Word --
   ------------------------------

   procedure Set_Synchronization_Word
     (This  : Si4432_Driver;
      Index : UInt2;
      Value : UInt8) is
   begin
      Write_Register
        (This, To_Synchronization_Word_Index (Index), Register (Value));
   end Set_Synchronization_Word;

   ------------------------------
   -- Get_Synchronization_Word --
   ------------------------------

   function Get_Synchronization_Word
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, To_Synchronization_Word_Index (Index)));
   end Get_Synchronization_Word;

   -----------------------------
   -- Set_Data_Access_Control --
   -----------------------------

   procedure Set_Data_Access_Control
     (This                              : Si4432_Driver;
      CRC_Selection                     : CRC_Polynomial;
      CRC_Enable                        : Boolean;
      Enable_Packet_TX_Handling         : Boolean;
      Skip_2nd_Phase_Preamble_Detection : Boolean;
      CRC_Data_Only_Enable              : Boolean;
      LSB_First_Enable                  : Boolean;
      Enable_Packet_RX_Handling         : Boolean)
   is
      Reg   : constant Data_Access_Control_Register :=
        (crc      => CRC_Selection,
         encrc    => From_Boolean (CRC_Enable),
         enpactx  => From_Boolean (Enable_Packet_TX_Handling),
         skip2ph  => From_Boolean (Skip_2nd_Phase_Preamble_Detection),
         crcdonly => From_Boolean (CRC_Data_Only_Enable),
         lsbfrst  => From_Boolean (LSB_First_Enable),
         enpacrx  => From_Boolean (Enable_Packet_RX_Handling));
      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Data_Access_Control_Name, Value);
   end Set_Data_Access_Control;

   -----------------------------
   -- Get_Data_Access_Control --
   -----------------------------

   procedure Get_Data_Access_Control
     (This                              : Si4432_Driver;
      CRC_Selection                     : out CRC_Polynomial;
      CRC_Enable                        : out Boolean;
      Enable_Packet_TX_Handling         : out Boolean;
      Skip_2nd_Phase_Preamble_Detection : out Boolean;
      CRC_Data_Only_Enable              : out Boolean;
      LSB_First_Enable                  : out Boolean;
      Enable_Packet_RX_Handling         : out Boolean)
   is
      Value : constant Register := Read_Register
        (This, Data_Access_Control_Name);

      Reg   : Data_Access_Control_Register with Import,
        Address => Value'Address;
   begin
      CRC_Selection                     := Reg.crc;
      CRC_Enable                        := To_Boolean (Reg.encrc);
      Enable_Packet_TX_Handling         := To_Boolean (Reg.enpactx);
      Skip_2nd_Phase_Preamble_Detection := To_Boolean (Reg.skip2ph);
      CRC_Data_Only_Enable              := To_Boolean (Reg.crcdonly);
      LSB_First_Enable                  := To_Boolean (Reg.lsbfrst);
      Enable_Packet_RX_Handling         := To_Boolean (Reg.enpacrx);
   end Get_Data_Access_Control;

   -----------------------------
   -- Set_GPIO0_Configuration --
   -----------------------------

   procedure Set_GPIO0_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : GPIO_0_Function;
      Pullup_Resistor_Enable : Boolean;
      Driving_Capability     : UInt2)
   is
      Reg   : constant GPIO0_Configuration_Register :=
        (gpio    => Pin_Function,
         pup     => From_Boolean (Pullup_Resistor_Enable),
         gpiodrv => Driving_Capability);
      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, GPIO0_Configuration_Name, Value);
   end Set_GPIO0_Configuration;

   -----------------------------
   -- Get_GPIO0_Configuration --
   -----------------------------

   procedure Get_GPIO0_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : out GPIO_0_Function;
      Pullup_Resistor_Enable : out Boolean;
      Driving_Capability     : out UInt2)
   is
      Value : constant Register := Read_Register
        (This, GPIO0_Configuration_Name);

      Reg   : GPIO0_Configuration_Register with Import,
        Address => Value'Address;
   begin
      Pin_Function           := Reg.gpio;
      Pullup_Resistor_Enable := To_Boolean (Reg.pup);
      Driving_Capability     := Reg.gpiodrv;
   end Get_GPIO0_Configuration;

   -----------------------------
   -- Set_GPIO1_Configuration --
   -----------------------------

   procedure Set_GPIO1_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : GPIO_1_Function;
      Pullup_Resistor_Enable : Boolean;
      Driving_Capability     : UInt2)
   is
      Reg   : constant GPIO1_Configuration_Register :=
        (gpio    => Pin_Function,
         pup     => From_Boolean (Pullup_Resistor_Enable),
         gpiodrv => Driving_Capability);
      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, GPIO1_Configuration_Name, Value);
   end Set_GPIO1_Configuration;

   -----------------------------
   -- Get_GPIO1_Configuration --
   -----------------------------

   procedure Get_GPIO1_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : out GPIO_1_Function;
      Pullup_Resistor_Enable : out Boolean;
      Driving_Capability     : out UInt2)
   is
      Value : constant Register := Read_Register
        (This, GPIO1_Configuration_Name);

      Reg   : GPIO1_Configuration_Register with Import,
        Address => Value'Address;
   begin
      Pin_Function           := Reg.gpio;
      Pullup_Resistor_Enable := To_Boolean (Reg.pup);
      Driving_Capability     := Reg.gpiodrv;
   end Get_GPIO1_Configuration;

   -----------------------------
   -- Set_GPIO2_Configuration --
   -----------------------------

   procedure Set_GPIO2_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : GPIO_2_Function;
      Pullup_Resistor_Enable : Boolean;
      Driving_Capability     : UInt2)
   is
      Reg   : constant GPIO2_Configuration_Register :=
        (gpio    => Pin_Function,
         pup     => From_Boolean (Pullup_Resistor_Enable),
         gpiodrv => Driving_Capability);
      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, GPIO2_Configuration_Name, Value);
   end Set_GPIO2_Configuration;

   -----------------------------
   -- Get_GPIO2_Configuration --
   -----------------------------

   procedure Get_GPIO2_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : out GPIO_2_Function;
      Pullup_Resistor_Enable : out Boolean;
      Driving_Capability     : out UInt2)
   is
      Value : constant Register := Read_Register
        (This, GPIO2_Configuration_Name);

      Reg   : GPIO2_Configuration_Register with Import,
        Address => Value'Address;
   begin
      Pin_Function           := Reg.gpio;
      Pullup_Resistor_Enable := To_Boolean (Reg.pup);
      Driving_Capability     := Reg.gpiodrv;
   end Get_GPIO2_Configuration;

   ---------------------------------------------
   -- Set_Crystal_Oscillator_Load_Capacitance --
   ---------------------------------------------

   procedure Set_Crystal_Oscillator_Load_Capacitance
     (This               : Si4432_Driver;
      Tuning_Capacitance : UInt7;
      Shft               : Bit)
   is
      Reg   : constant Crystal_Oscillator_Load_Capacitance_Register :=
        (xlc      => Tuning_Capacitance,
         xtalshft => Shft);
      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Crystal_Oscillator_Load_Capacitance_Name, Value);
   end Set_Crystal_Oscillator_Load_Capacitance;

   ---------------------------------------------
   -- Get_Crystal_Oscillator_Load_Capacitance --
   ---------------------------------------------

   procedure Get_Crystal_Oscillator_Load_Capacitance
     (This               : Si4432_Driver;
      Tuning_Capacitance : out UInt7;
      Shft               : out Bit)
   is
      Value : constant Register := Read_Register
        (This, Crystal_Oscillator_Load_Capacitance_Name);

      Reg   : Crystal_Oscillator_Load_Capacitance_Register with Import,
        Address => Value'Address;
   begin
      Tuning_Capacitance := Reg.xlc;
      Shft               := Reg.xtalshft;
   end Get_Crystal_Oscillator_Load_Capacitance;

   -----------------------
   -- Set_Packet_Length --
   -----------------------

   procedure Set_Packet_Length
     (This   : Si4432_Driver;
      Length : UInt8) is
   begin
      Write_Register (This, Packet_Length_Name, Register (Length));
   end Set_Packet_Length;

   -----------------------
   -- Get_Packet_Length --
   -----------------------

   function Get_Packet_Length
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, Packet_Length_Name));
   end Get_Packet_Length;

   --------------------------------
   -- Get_Received_Packet_Length --
   --------------------------------

   function Get_Received_Packet_Length
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, Received_Packet_Length_Name));
   end Get_Received_Packet_Length;

   --------------------------
   -- Set_Interrupt_Enable --
   --------------------------

   procedure Set_Interrupt_Enable
     (This                    : Si4432_Driver;
      CRC_Error               : Boolean := False;
      Valid_Packet_Received   : Boolean := False;
      Packet_Sent             : Boolean := False;
      External_Interrupt      : Boolean := False;
      RX_FIFO_Almost_Full     : Boolean := False;
      TX_FIFO_Almost_Empty    : Boolean := False;
      TX_FIFO_Almost_Full     : Boolean := False;
      FIFO_Underflow_Overflow : Boolean := False;
      POR                     : Boolean := True;
      Chip_Ready              : Boolean := True;
      Low_Battery             : Boolean := False;
      Wake_Up_Timer           : Boolean := False;
      RSSI                    : Boolean := False;
      Invalid_Preamble        : Boolean := False;
      Valid_Preamble          : Boolean := False;
      Sync_Word               : Boolean := False)
   is
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg1  : Interrupt_Enable_1_Register :=
        (encrcerror  => From_Boolean (CRC_Error),
         enpkvalid   => From_Boolean (Valid_Packet_Received),
         enpksent    => From_Boolean (Packet_Sent),
         enext       => From_Boolean (External_Interrupt),
         enrxffafull => From_Boolean (RX_FIFO_Almost_Full),
         entxffaem   => From_Boolean (TX_FIFO_Almost_Empty),
         entxffafull => From_Boolean (TX_FIFO_Almost_Full),
         enfferr     => From_Boolean (FIFO_Underflow_Overflow))
          with Address => Data (1)'Address;

      Reg2  : Interrupt_Enable_2_Register :=
        (enpor       => From_Boolean (POR),
         enchiprdy   => From_Boolean (Chip_Ready),
         enlbd       => From_Boolean (Low_Battery),
         enwut       => From_Boolean (Wake_Up_Timer),
         enrssi      => From_Boolean (RSSI),
         enpreainval => From_Boolean (Invalid_Preamble),
         enpreaval   => From_Boolean (Valid_Preamble),
         enswdet     => From_Boolean (Sync_Word))
          with Address => Data (2)'Address;

   begin
      Clear_Interrupts (This);
      Write_Register (This, Interrupt_Enable_1_Name, Data);
      Clear_Interrupts (This);
   end Set_Interrupt_Enable;

   ----------------------------
   -- Set_Interrupt_Enable_1 --
   ----------------------------

   procedure Set_Interrupt_Enable_1
     (This                    : Si4432_Driver;
      CRC_Error               : Boolean;
      Valid_Packet_Received   : Boolean;
      Packet_Sent             : Boolean;
      External_Interrupt      : Boolean;
      RX_FIFO_Almost_Full     : Boolean;
      TX_FIFO_Almost_Empty    : Boolean;
      TX_FIFO_Almost_Full     : Boolean;
      FIFO_Underflow_Overflow : Boolean)
   is
      Reg   : constant Interrupt_Enable_1_Register :=
        (encrcerror  => From_Boolean (CRC_Error),
         enpkvalid   => From_Boolean (Valid_Packet_Received),
         enpksent    => From_Boolean (Packet_Sent),
         enext       => From_Boolean (External_Interrupt),
         enrxffafull => From_Boolean (RX_FIFO_Almost_Full),
         entxffaem   => From_Boolean (TX_FIFO_Almost_Empty),
         entxffafull => From_Boolean (TX_FIFO_Almost_Full),
         enfferr     => From_Boolean (FIFO_Underflow_Overflow));
      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Interrupt_Enable_1_Name, Value);
   end Set_Interrupt_Enable_1;

   ----------------------------
   -- Get_Interrupt_Enable_1 --
   ----------------------------

   procedure Get_Interrupt_Enable_1
     (This                    : Si4432_Driver;
      CRC_Error               : out Boolean;
      Valid_Packet_Received   : out Boolean;
      Packet_Sent             : out Boolean;
      External_Interrupt      : out Boolean;
      RX_FIFO_Almost_Full     : out Boolean;
      TX_FIFO_Almost_Empty    : out Boolean;
      TX_FIFO_Almost_Full     : out Boolean;
      FIFO_Underflow_Overflow : out Boolean)
   is
      Value : constant Register := Read_Register
        (This, Interrupt_Enable_1_Name);

      Reg   : Interrupt_Enable_1_Register with Import,
        Address => Value'Address;
   begin
      CRC_Error               := To_Boolean (Reg.encrcerror);
      Valid_Packet_Received   := To_Boolean (Reg.enpkvalid);
      Packet_Sent             := To_Boolean (Reg.enpksent);
      External_Interrupt      := To_Boolean (Reg.enext);
      RX_FIFO_Almost_Full     := To_Boolean (Reg.enrxffafull);
      TX_FIFO_Almost_Empty    := To_Boolean (Reg.entxffaem);
      TX_FIFO_Almost_Full     := To_Boolean (Reg.entxffafull);
      FIFO_Underflow_Overflow := To_Boolean (Reg.enfferr);
   end Get_Interrupt_Enable_1;

   ----------------------------
   -- Set_Interrupt_Enable_2 --
   ----------------------------

   procedure Set_Interrupt_Enable_2
     (This             : Si4432_Driver;
      POR              : Boolean;
      Chip_Ready       : Boolean;
      Low_Battery      : Boolean;
      Wake_Up_Timer    : Boolean;
      RSSI             : Boolean;
      Invalid_Preamble : Boolean;
      Valid_Preamble   : Boolean;
      Sync_Word        : Boolean)
   is
      Reg   : constant Interrupt_Enable_2_Register :=
        (enpor       => From_Boolean (POR),
         enchiprdy   => From_Boolean (Chip_Ready),
         enlbd       => From_Boolean (Low_Battery),
         enwut       => From_Boolean (Wake_Up_Timer),
         enrssi      => From_Boolean (RSSI),
         enpreainval => From_Boolean (Invalid_Preamble),
         enpreaval   => From_Boolean (Valid_Preamble),
         enswdet     => From_Boolean (Sync_Word));

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Interrupt_Enable_2_Name, Value);
   end Set_Interrupt_Enable_2;

   ----------------------------
   -- Get_Interrupt_Enable_2 --
   ----------------------------

   procedure Get_Interrupt_Enable_2
     (This             : Si4432_Driver;
      POR              : out Boolean;
      Chip_Ready       : out Boolean;
      Low_Battery      : out Boolean;
      Wake_Up_Timer    : out Boolean;
      RSSI             : out Boolean;
      Invalid_Preamble : out Boolean;
      Valid_Preamble   : out Boolean;
      Sync_Word        : out Boolean)
   is
      Value : constant Register :=
        Read_Register (This, Interrupt_Enable_2_Name);

      Reg   : Interrupt_Enable_2_Register with Import,
        Address => Value'Address;

   begin
      POR              := To_Boolean (Reg.enpor);
      Chip_Ready       := To_Boolean (Reg.enchiprdy);
      Low_Battery      := To_Boolean (Reg.enlbd);
      Wake_Up_Timer    := To_Boolean (Reg.enwut);
      RSSI             := To_Boolean (Reg.enrssi);
      Invalid_Preamble := To_Boolean (Reg.enpreainval);
      Valid_Preamble   := To_Boolean (Reg.enpreaval);
      Sync_Word        := To_Boolean (Reg.enswdet);
   end Get_Interrupt_Enable_2;

   -----------------------------
   -- Set_IF_Filter_Bandwidth --
   -----------------------------

   procedure Set_IF_Filter_Bandwidth
     (This             : Si4432_Driver;
      Coefficient      : UInt4;
      Decimation_Rates : UInt3;
      Bypass_Decimate  : Boolean)
   is
      Reg   : constant IF_Filter_Bandwidth_Register :=
        (filset      => Coefficient,
         ndec_exp    => Decimation_Rates,
         dwn3_bypass => From_Boolean (Bypass_Decimate));

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, IF_Filter_Bandwidth_Name, Value);
   end Set_IF_Filter_Bandwidth;

   -----------------------------
   -- Get_IF_Filter_Bandwidth --
   -----------------------------

   procedure Get_IF_Filter_Bandwidth
     (This             : Si4432_Driver;
      Coefficient      : out UInt4;
      Decimation_Rates : out UInt3;
      Bypass_Decimate  : out Boolean)
   is
      Value : constant Register :=
        Read_Register (This, IF_Filter_Bandwidth_Name);

      Reg   : IF_Filter_Bandwidth_Register with Import,
        Address => Value'Address;
   begin
      Coefficient      := Reg.filset;
      Decimation_Rates := Reg.ndec_exp;
      Bypass_Decimate  := To_Boolean (Reg.dwn3_bypass);
   end Get_IF_Filter_Bandwidth;

   ------------------------------------------
   -- Set_Clock_Recovery_Oversampling_Rate --
   ------------------------------------------

   procedure Set_Clock_Recovery_Oversampling_Rate
     (This  : Si4432_Driver;
      Value : Clock_Recovery_Oversampling)
   is
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg   : Clock_Recovery_Offset_2_Register with Import,
        Address => Data (2)'Address;

   begin
      Data (2)  := UInt8 (Read_Register (This, Clock_Recovery_Offset_2_Name));
      Data (1)  := UInt8 (Unsigned_16 (Value) and 16#FF#);
      Reg.rxosr := UInt3 (Shift_Right (Unsigned_16 (Value), 8));

      Write_Register (This, Clock_Recovery_Oversampling_Rate_Name, Data);
   end Set_Clock_Recovery_Oversampling_Rate;

   ------------------------------------------
   -- Get_Clock_Recovery_Oversampling_Rate --
   ------------------------------------------

   function Get_Clock_Recovery_Oversampling_Rate
     (This  : Si4432_Driver)
      return Clock_Recovery_Oversampling
   is
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg   : Clock_Recovery_Offset_2_Register with Import,
        Address => Data (2)'Address;
   begin
      Read_Register (This, Clock_Recovery_Oversampling_Rate_Name, Data);

      return Clock_Recovery_Oversampling
        (Shift_Left (Unsigned_16 (Reg.rxosr), 8) + Unsigned_16 (Data (1)));
   end Get_Clock_Recovery_Oversampling_Rate;

   -------------------------------
   -- Set_Clock_Recovery_Offset --
   -------------------------------

   procedure Set_Clock_Recovery_Offset
     (This           : Si4432_Driver;
      NCO_Offset     : Clock_Recovery_Offset;
      Skip_2nd_Phase : Boolean)
   is
      Value : Unsigned_32 := Unsigned_32 (NCO_Offset);
      Data  : HAL.SPI.SPI_Data_8b (1 .. 3);
      Reg   : Clock_Recovery_Offset_2_Register with Import,
        Address => Data (1)'Address;

   begin
      Data (1) := UInt8 (Read_Register (This, Clock_Recovery_Offset_2_Name));

      Data (3)      := UInt8 (Value and 16#FF#);
      Value         := Shift_Right (Value, 8);
      Data (2)      := UInt8 (Value and 16#FF#);
      Reg.ncoff     := UInt4 (Shift_Right (Value, 8));
      Reg.skip2phth := From_Boolean (Skip_2nd_Phase);

      Write_Register (This, Clock_Recovery_Offset_2_Name, Data);
   end Set_Clock_Recovery_Offset;

   -------------------------------
   -- Get_Clock_Recovery_Offset --
   -------------------------------

   procedure Get_Clock_Recovery_Offset
     (This           : Si4432_Driver;
      NCO_Offset     : out Clock_Recovery_Offset;
      Skip_2nd_Phase : out Boolean)
   is
      Data  : HAL.SPI.SPI_Data_8b (1 .. 3);
      Reg   : Clock_Recovery_Offset_2_Register with Import,
        Address => Data (1)'Address;

      Result : Unsigned_32;
   begin
      Read_Register (This, Clock_Recovery_Offset_2_Name, Data);

      Result := Shift_Left (Unsigned_32 (Reg.ncoff), 8);
      Result := Result + Unsigned_32 (Data (2));
      Result := Shift_Left (Result, 8);
      Result := Result + Unsigned_32 (Data (3));

      NCO_Offset     := Clock_Recovery_Offset (Result);
      Skip_2nd_Phase := To_Boolean (Reg.skip2phth);
   end Get_Clock_Recovery_Offset;

   -----------------------------------------
   -- Set_Clock_Recovery_Timing_Loop_Gain --
   -----------------------------------------

   procedure Set_Clock_Recovery_Timing_Loop_Gain
     (This                : Si4432_Driver;
      Gain                : Clock_Recovery_Timing_Loop_Gain;
      Multiplying_By_2    : Boolean;
      Compensation_Enable : Boolean)
   is
      Local : constant Unsigned_16 := Unsigned_16 (Gain);
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg   : Clock_Recovery_Timing_Loop_Gain_1_Register :=
        (crgain    => UInt3 (Shift_Right (Local, 8)),
         cgainx2   => From_Boolean (Multiplying_By_2),
         rxncocomp => From_Boolean (Compensation_Enable),
         Reserved  => 0) with Address => Data (1)'Address;
   begin
      Data (2) := UInt8 (Local and 16#FF#);

      Write_Register (This, Clock_Recovery_Timing_Loop_Gain_1_Name, Data);
   end Set_Clock_Recovery_Timing_Loop_Gain;

   -----------------------------------------
   -- Get_Clock_Recovery_Timing_Loop_Gain --
   -----------------------------------------

   procedure Get_Clock_Recovery_Timing_Loop_Gain
     (This                : Si4432_Driver;
      Gain                : out Clock_Recovery_Timing_Loop_Gain;
      Multiplying_By_2    : out Boolean;
      Compensation_Enable : out Boolean)
   is
      Data : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg  : Clock_Recovery_Timing_Loop_Gain_1_Register with Import,
        Address => Data (1)'Address;

   begin
      Read_Register (This, Clock_Recovery_Timing_Loop_Gain_1_Name, Data);

      Gain                := Clock_Recovery_Timing_Loop_Gain
        (Shift_Left (Unsigned_16 (Reg.crgain), 8) + Unsigned_16 (Data (2)));
      Multiplying_By_2    := To_Boolean (Reg.cgainx2);
      Compensation_Enable := To_Boolean (Reg.rxncocomp);
   end Get_Clock_Recovery_Timing_Loop_Gain;

   -------------------------------------
   -- Set_AFC_Loop_Gearshift_Override --
   -------------------------------------

   procedure Set_AFC_Loop_Gearshift_Override
     (This           : Si4432_Driver;
      Reset_Preamble : Boolean;
      Taps           : Boolean;
      Bypass         : Boolean;
      AFC_High_Gear  : UInt3;
      AFC            : Boolean;
      AFC_Wideband   : Boolean)
   is
      Reg   : constant AFC_Loop_Gearshift_Override_Register :=
        (ph0size  => From_Boolean (Reset_Preamble),
         matap    => From_Boolean (Taps),
         p5bypass => From_Boolean (Bypass),
         afcgearh => AFC_High_Gear,
         enafc    => From_Boolean (AFC),
         afcbd    => From_Boolean (AFC_Wideband));

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, AFC_Loop_Gearshift_Override_Name, Value);
   end Set_AFC_Loop_Gearshift_Override;

   -------------------------------------
   -- Get_AFC_Loop_Gearshift_Override --
   -------------------------------------

   procedure Get_AFC_Loop_Gearshift_Override
     (This           : Si4432_Driver;
      Reset_Preamble : out Boolean;
      Taps           : out Boolean;
      Bypass         : out Boolean;
      AFC_High_Gear  : out UInt3;
      AFC            : out Boolean;
      AFC_Wideband   : out Boolean)
   is
      Value : constant Register := Read_Register
        (This, AFC_Loop_Gearshift_Override_Name);
      Reg   : AFC_Loop_Gearshift_Override_Register with Import,
        Address => Value'Address;
   begin
      Reset_Preamble := To_Boolean (Reg.ph0size);
      Taps           := To_Boolean (Reg.matap);
      Bypass         := To_Boolean (Reg.p5bypass);
      AFC_High_Gear  := Reg.afcgearh;
      AFC            := To_Boolean (Reg.enafc);
      AFC_Wideband   := To_Boolean (Reg.afcbd);
   end Get_AFC_Loop_Gearshift_Override;

   ---------------------
   -- Set_AFC_Limiter --
   ---------------------

   procedure Set_AFC_Limiter
     (This  : Si4432_Driver;
      Value : UInt8) is
   begin
      Write_Register (This, AFC_Limiter_Name, Register (Value));
   end Set_AFC_Limiter;

   ---------------------
   -- Get_AFC_Limiter --
   ---------------------

   function Get_AFC_Limiter
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, AFC_Limiter_Name));
   end Get_AFC_Limiter;

   -------------------------
   -- Set_Preamble_Length --
   -------------------------

   procedure Set_Preamble_Length
     (This  : Si4432_Driver;
      Value : UInt9)
   is
      Local : constant Unsigned_16 := Unsigned_16 (Value);
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg   : Header_Control_2_Register with Import,
        Address => Data (1)'Address;

   begin
      Data (1) := UInt8 (Read_Register (This, Header_Control_2_Name));
      Reg.prealen := Bit (Unsigned_16 (Value) and 2#1_0000_0000#);
      Data (2) := UInt8 (Local and 16#FF#);

      Write_Register (This, Header_Control_2_Name, Data);
   end Set_Preamble_Length;

   -------------------------
   -- Get_Preamble_Length --
   -------------------------

   function Get_Preamble_Length
     (This : Si4432_Driver)
      return UInt9
   is
      Data : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg  : Header_Control_2_Register with Import,
        Address => Data (1)'Address;

   begin
      Read_Register (This, Header_Control_2_Name, Data);

      return UInt9
        (Shift_Left (Unsigned_16 (Reg.prealen), 8) + Unsigned_16 (Data (2)));
   end Get_Preamble_Length;

   ------------------------------------
   -- Set_Preamble_Detection_Control --
   ------------------------------------

   procedure Set_Preamble_Detection_Control
     (This                : Si4432_Driver;
      RSSI_Offset         : UInt3;
      Detection_Threshold : UInt5)
   is
      Reg   : constant Preamble_Detection_Control_1_Register :=
        (rssi_offset => RSSI_Offset,
         preath      => Detection_Threshold);

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Preamble_Detection_Control_1_Name, Value);
   end Set_Preamble_Detection_Control;

   -------------------------------------
   --  Get_Preamble_Detection_Control --
   -------------------------------------

   procedure Get_Preamble_Detection_Control
     (This                : Si4432_Driver;
      RSSI_Offset         : out UInt3;
      Detection_Threshold : out UInt5)
   is
      Value : constant Register := Read_Register
        (This, Preamble_Detection_Control_1_Name);

      Reg   : constant Preamble_Detection_Control_1_Register with Import,
        Address => Value'Address;
   begin
      RSSI_Offset         := Reg.rssi_offset;
      Detection_Threshold := Reg.preath;
   end Get_Preamble_Detection_Control;

   ----------------------
   -- Set_AGC_Override --
   ----------------------

   procedure Set_AGC_Override
     (This                   : Si4432_Driver;
      Gain_Override          : AGC_Override_Gain;
      LNA_Gain_Select        : Boolean;
      Automatic_Gain_Control : Boolean;
      Sgin                   : Boolean)
   is
      Reg   : constant AGC_Override_Register :=
        (pga      => (if Gain_Override = 0 then 0
                      elsif Gain_Override = 3  then 2#0001#
                      elsif Gain_Override = 6  then 2#0010#
                      elsif Gain_Override = 12 then 2#0100#
                      else 2#1000#),
         lnagain  => From_Boolean (LNA_Gain_Select),
         agcen    => From_Boolean (Automatic_Gain_Control),
         sgin     => From_Boolean (Sgin),
         Reserved => 0);

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, AGC_Override_Name, Value);
   end Set_AGC_Override;

   ----------------------
   -- Get_AGC_Override --
   ----------------------

   procedure Get_AGC_Override
     (This                   : Si4432_Driver;
      Gain_Override          : out AGC_Override_Gain;
      LNA_Gain_Select        : out Boolean;
      Automatic_Gain_Control : out Boolean;
      Sgin                   : out Boolean)
   is
      Value : constant Register := Read_Register (This, AGC_Override_Name);
      Reg   : AGC_Override_Register with Import, Address => Value'Address;
   begin
      Gain_Override :=
        (if Reg.pga = 2#0000# then 0
         elsif Reg.pga = 2#0001# then 3
         elsif Reg.pga = 2#0010# then 6
         elsif Reg.pga = 2#0100# then 12
         else 24);

      LNA_Gain_Select        := To_Boolean (Reg.lnagain);
      Automatic_Gain_Control := To_Boolean (Reg.agcen);
      Sgin                   := To_Boolean (Reg.sgin);
   end Get_AGC_Override;

   ------------------
   -- Set_TX_Power --
   ------------------

   procedure Set_TX_Power
     (This         : Si4432_Driver;
      Output_Power : UInt3;
      LNA_Switch   : Boolean)
   is
      Reg   : constant TX_Power_Register :=
        (txpow    => Output_Power,
         lna_sw   => From_Boolean (LNA_Switch),
         Reserved => 0);

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, TX_Power_Name, Value);
   end Set_TX_Power;

   ------------------
   -- Get_TX_Power --
   ------------------

   procedure Get_TX_Power
     (This         : Si4432_Driver;
      Output_Power : out UInt3;
      LNA_Switch   : out Boolean)
   is
      Value : constant Register := Read_Register (This, TX_Power_Name);
      Reg   : TX_Power_Register with Import, Address => Value'Address;
   begin
      Output_Power := Reg.txpow;
      LNA_Switch   := To_Boolean (Reg.lna_sw);
   end Get_TX_Power;

   ---------------------------------------------
   -- Set_Operating_Mode_And_Function_Control --
   ---------------------------------------------

   procedure Set_Operating_Mode_And_Function_Control
     (This                   : Si4432_Driver;
      READY_Mode             : Boolean := True;
      TUNE_Mode              : Boolean := False;
      RX_On                  : Boolean := False;
      TX_On                  : Boolean := False;
      Oscillator_Select      : Crystal_Oscillator := RC_Oscillator;
      Wake_Up_Timer          : Boolean := False;
      Low_Battery_Detect     : Boolean := False;
      TX_FIFO_Reset          : Boolean := False;
      RX_FIFO_Reset          : Boolean := False;
      Low_Duty_Cycle_Mode    : Boolean := False;
      Automatic_Transmission : Boolean := False;
      RX_Multi_Packet        : Boolean := False;
      Antenna_Diversity      : UInt3 := 0)
   is
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg1  : Operating_Mode_And_Function_Control_1_Register :=
        (xton    => From_Boolean (READY_Mode),
         pllon   => From_Boolean (TUNE_Mode),
         rxon    => From_Boolean (RX_On),
         txon    => From_Boolean (TX_On),
         x32ksel => Oscillator_Select,
         enwt    => From_Boolean (Wake_Up_Timer),
         enlbd   => From_Boolean (Low_Battery_Detect),
         swres   => 0) with Address => Data (1)'Address;

      Reg2  : Operating_Mode_And_Function_Control_2_Register :=
        (ffclrtx => From_Boolean (TX_FIFO_Reset),
         ffclrrx => From_Boolean (RX_FIFO_Reset),
         enldm   => From_Boolean (Low_Duty_Cycle_Mode),
         autotx  => From_Boolean (Automatic_Transmission),
         rxmpk   => From_Boolean (RX_Multi_Packet),
         antdiv  => Antenna_Diversity)
          with Address => Data (2)'Address;
   begin
      Write_Register (This, Operating_Mode_And_Function_Control_1_Name, Data);
   end Set_Operating_Mode_And_Function_Control;

   -----------------------------------------------
   -- Set_Operating_Mode_And_Function_Control_1 --
   -----------------------------------------------

   procedure Set_Operating_Mode_And_Function_Control_1
     (This               : Si4432_Driver;
      READY_Mode         : Boolean;
      TUNE_Mode          : Boolean;
      RX_On              : Boolean;
      TX_On              : Boolean;
      Oscillator_Select  : Crystal_Oscillator;
      Wake_Up_Timer      : Boolean;
      Low_Battery_Detect : Boolean;
      Software_Reset     : Boolean)
   is
      Reg   : constant Operating_Mode_And_Function_Control_1_Register :=
        (xton    => From_Boolean (READY_Mode),
         pllon   => From_Boolean (TUNE_Mode),
         rxon    => From_Boolean (RX_On),
         txon    => From_Boolean (TX_On),
         x32ksel => Oscillator_Select,
         enwt    => From_Boolean (Wake_Up_Timer),
         enlbd   => From_Boolean (Low_Battery_Detect),
         swres   => From_Boolean (Software_Reset));

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Operating_Mode_And_Function_Control_1_Name, Value);
   end Set_Operating_Mode_And_Function_Control_1;

   -----------------------------------------------
   -- Get_Operating_Mode_And_Function_Control_1 --
   -----------------------------------------------

   procedure Get_Operating_Mode_And_Function_Control_1
     (This               : Si4432_Driver;
      READY_Mode         : out Boolean;
      TUNE_Mode          : out Boolean;
      RX_On              : out Boolean;
      TX_On              : out Boolean;
      Oscillator_Select  : out Crystal_Oscillator;
      Wake_Up_Timer      : out Boolean;
      Low_Battery_Detect : out Boolean)
   is
      Value : constant Register := Read_Register
        (This, Operating_Mode_And_Function_Control_1_Name);
      Reg   : Operating_Mode_And_Function_Control_1_Register with Import,
        Address => Value'Address;
   begin
      READY_Mode         := To_Boolean (Reg.xton);
      TUNE_Mode          := To_Boolean (Reg.pllon);
      RX_On              := To_Boolean (Reg.rxon);
      TX_On              := To_Boolean (Reg.txon);
      Oscillator_Select  := Reg.x32ksel;
      Wake_Up_Timer      := To_Boolean (Reg.enwt);
      Low_Battery_Detect := To_Boolean (Reg.enlbd);
   end Get_Operating_Mode_And_Function_Control_1;

   -----------------------------------------------
   -- Set_Operating_Mode_And_Function_Control_2 --
   -----------------------------------------------

   procedure Set_Operating_Mode_And_Function_Control_2
     (This                   : Si4432_Driver;
      TX_FIFO_Reset          : Boolean;
      RX_FIFO_Reset          : Boolean;
      Low_Duty_Cycle_Mode    : Boolean;
      Automatic_Transmission : Boolean;
      RX_Multi_Packet        : Boolean;
      Antenna_Diversity      : UInt3)
   is
      Reg   : constant Operating_Mode_And_Function_Control_2_Register :=
        (ffclrtx => From_Boolean (TX_FIFO_Reset),
         ffclrrx => From_Boolean (RX_FIFO_Reset),
         enldm   => From_Boolean (Low_Duty_Cycle_Mode),
         autotx  => From_Boolean (Automatic_Transmission),
         rxmpk   => From_Boolean (RX_Multi_Packet),
         antdiv  => Antenna_Diversity);

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Operating_Mode_And_Function_Control_2_Name, Value);
   end Set_Operating_Mode_And_Function_Control_2;

   -----------------------------------------------
   -- Get_Operating_Mode_And_Function_Control_2 --
   -----------------------------------------------

   procedure Get_Operating_Mode_And_Function_Control_2
     (This                   : Si4432_Driver;
      TX_FIFO_Reset          : out Boolean;
      RX_FIFO_Reset          : out Boolean;
      Low_Duty_Cycle_Mode    : out Boolean;
      Automatic_Transmission : out Boolean;
      RX_Multi_Packet        : out Boolean;
      Antenna_Diversity      : out UInt3)
   is
      Value : constant Register := Read_Register
        (This, Operating_Mode_And_Function_Control_2_Name);

      Reg   : Operating_Mode_And_Function_Control_2_Register with Import,
        Address => Value'Address;
   begin
      TX_FIFO_Reset          := To_Boolean (Reg.ffclrtx);
      RX_FIFO_Reset          := To_Boolean (Reg.ffclrrx);
      Low_Duty_Cycle_Mode    := To_Boolean (Reg.enldm);
      Automatic_Transmission := To_Boolean (Reg.autotx);
      RX_Multi_Packet        := To_Boolean (Reg.rxmpk);
      Antenna_Diversity      := Reg.antdiv;
   end Get_Operating_Mode_And_Function_Control_2;

   -----------------------
   -- Get_Device_Status --
   -----------------------

   procedure Get_Device_Status
     (This                 : Si4432_Driver;
      Power                : out Chip_State;
      Frequency_Error      : out Boolean;
      Header_Error         : out Boolean;
      RX_FIFO_Empty        : out Boolean;
      RX_TX_FIFO_Underflow : out Boolean;
      RX_TX_FIFO_Overflow  : out Boolean)
   is
      Value : constant Register := Read_Register (This, Device_Status_Name);
      Reg   : Device_Status_Register with Import,
        Address => Value'Address;
   begin
      Power                := Reg.cps;
      Frequency_Error      := To_Boolean (Reg.freqerr);
      Header_Error         := To_Boolean (Reg.headerr);
      RX_FIFO_Empty        := To_Boolean (Reg.rxffem);
      RX_TX_FIFO_Underflow := To_Boolean (Reg.ffunfl);
      RX_TX_FIFO_Overflow  := To_Boolean (Reg.ffovfl);
   end Get_Device_Status;

   --------------------------------------
   -- Set_Microcontroller_Output_Clock --
   --------------------------------------

   procedure Set_Microcontroller_Output_Clock
     (This                : Si4432_Driver;
      Clock               : Microcontroller_Clock;
      Low_Frequency_Clock : Boolean;
      Tail                : Clock_Tail)
   is
      Reg   : constant Microcontroller_Output_Clock_Register :=
        (mclk     => Clock,
         enlfc    => From_Boolean (Low_Frequency_Clock),
         clkt     => (if    Tail = 0   then 2#00#
                      elsif Tail = 128 then 2#01#
                      elsif Tail = 256 then 2#10#
                      else 2#11#),
         Reserved => 0);

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Microcontroller_Output_Clock_Name, Value);
   end Set_Microcontroller_Output_Clock;

   --------------------------------------
   -- Get_Microcontroller_Output_Clock --
   --------------------------------------

   procedure Get_Microcontroller_Output_Clock
     (This                : Si4432_Driver;
      Clock               : out Microcontroller_Clock;
      Low_Frequency_Clock : out Boolean;
      Tail                : out Clock_Tail)
   is
      Value : constant Register := Read_Register
        (This, Microcontroller_Output_Clock_Name);
      Reg   : Microcontroller_Output_Clock_Register with Import,
        Address => Value'Address;
   begin
      Clock := Reg.mclk;
      Low_Frequency_Clock := To_Boolean (Reg.enlfc);
      Tail := (if    Reg.clkt = 2#00# then 0
               elsif Reg.clkt = 2#01# then 128
               elsif Reg.clkt = 2#10# then 256
               else 512);
   end Get_Microcontroller_Output_Clock;

   -------------------------------
   -- Set_IO_Port_Configuration --
   -------------------------------

   procedure Set_IO_Port_Configuration
     (This                 : Si4432_Driver;
      Direct_GPIO0         : Boolean;
      Direct_GPIO1         : Boolean;
      Direct_GPIO2         : Boolean;
      Interrupt_Output_SDO : Boolean)
   is
      Reg   : constant IO_Port_Configuration_Register :=
        (dio0     => From_Boolean (Direct_GPIO0),
         dio1     => From_Boolean (Direct_GPIO1),
         dio2     => From_Boolean (Direct_GPIO2),
         itsdo    => From_Boolean (Interrupt_Output_SDO),
         extitst0 => 0,
         extitst1 => 0,
         extitst2 => 0,
         Reserved => 0);

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, IO_Port_Configuration_Name, Value);
   end Set_IO_Port_Configuration;

   -------------------------------
   -- Get_IO_Port_Configuration --
   -------------------------------

   procedure Get_IO_Port_Configuration
     (This                 : Si4432_Driver;
      Direct_GPIO0         : out Boolean;
      Direct_GPIO1         : out Boolean;
      Direct_GPIO2         : out Boolean;
      Interrupt_Output_SDO : out Boolean;
      External_0_Interrupt : out Boolean;
      External_1_Interrupt : out Boolean;
      External_2_Interrupt : out Boolean)
   is
      Value : constant Register := Read_Register
        (This, IO_Port_Configuration_Name);

      Reg   : IO_Port_Configuration_Register with Import,
        Address => Value'Address;
   begin
      Direct_GPIO0         := To_Boolean (Reg.dio0);
      Direct_GPIO1         := To_Boolean (Reg.dio1);
      Direct_GPIO2         := To_Boolean (Reg.dio2);
      Interrupt_Output_SDO := To_Boolean (Reg.itsdo);
      External_0_Interrupt := To_Boolean (Reg.extitst0);
      External_1_Interrupt := To_Boolean (Reg.extitst1);
      External_2_Interrupt := To_Boolean (Reg.extitst2);
   end Get_IO_Port_Configuration;

   ---------------------------
   -- Set_ADC_Configuration --
   ---------------------------

   procedure Set_ADC_Configuration
     (This                  : Si4432_Driver;
      Sensor_Amplifier_Gain : UInt2;
      Reference_Voltage     : UInt2;
      Input_Source          : ADC_Input_Source;
      Measurement_Start     : Boolean)
   is
      Reg   : constant ADC_Configuration_Register :=
        (adcgain  => Sensor_Amplifier_Gain,
         adcref   => Reference_Voltage,
         adcsel   => Input_Source,
         adcstart => From_Boolean (Measurement_Start));

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, ADC_Configuration_Name, Value);
   end Set_ADC_Configuration;

   ---------------------------
   -- Get_ADC_Configuration --
   ---------------------------

   procedure Get_ADC_Configuration
     (This                  : Si4432_Driver;
      Sensor_Amplifier_Gain : out UInt2;
      Reference_Voltage     : out UInt2;
      Input_Source          : out ADC_Input_Source;
      Measurement_Start     : out Boolean)
   is
      Value : constant Register := Read_Register
        (This, ADC_Configuration_Name);

      Reg   : ADC_Configuration_Register with Import,
        Address => Value'Address;
   begin
      Sensor_Amplifier_Gain := Reg.adcgain;
      Reference_Voltage     := Reg.adcref;
      Input_Source          := Reg.adcsel;
      Measurement_Start     := To_Boolean (Reg.adcstart);
   end Get_ADC_Configuration;

   -------------------------------------
   -- Set_ADC_Sensor_Amplifier_Offset --
   -------------------------------------

   procedure Set_ADC_Sensor_Amplifier_Offset
     (This             : Si4432_Driver;
      Amplifier_Offset : ADC_Sensor_Amplifier_Offset)
   is
      Local : constant ADC_Sensor_Amplifier_Offset := Amplifier_Offset;
      Data  : UInt4 with Import, Address => Local'Address;
      Reg   : constant ADC_Sensor_Amplifier_Offset_Register :=
        (adcoffs  => Data,
         Reserved => 0);
      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, ADC_Sensor_Amplifier_Offset_Name, Value);
   end Set_ADC_Sensor_Amplifier_Offset;

   -------------------------------------
   -- Get_ADC_Sensor_Amplifier_Offset --
   -------------------------------------

   function Get_ADC_Sensor_Amplifier_Offset
     (This : Si4432_Driver)
      return ADC_Sensor_Amplifier_Offset
   is
      Value : constant Register := Read_Register
        (This, ADC_Sensor_Amplifier_Offset_Name);
      Reg   : ADC_Sensor_Amplifier_Offset_Register with Import,
        Address => Value'Address;
      Data  : ADC_Sensor_Amplifier_Offset with Import,
        Address => Reg.adcoffs'Address;
   begin
      return Data;
   end Get_ADC_Sensor_Amplifier_Offset;

   -------------------
   -- Get_ADC_Value --
   -------------------

   function Get_ADC_Value
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, ADC_Value_Name));
   end Get_ADC_Value;

   ----------------------------------------
   -- Set_Temperature_Sensor_Calibration --
   ----------------------------------------

   procedure Set_Temperature_Sensor_Calibration
     (This         : Si4432_Driver;
      Trim_Value   : UInt4;
      Trim         : Boolean;
      Offset       : Boolean;
      Sensor_Range : Temperature_Sensor_Range)
   is
      Reg   : constant Temperature_Sensor_Calibration_Register :=
        (tstrim   => Trim_Value,
         entstrim => From_Boolean (Trim),
         entsoffs => From_Boolean (Offset),
         tsrange  => Sensor_Range);

      Value : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Temperature_Sensor_Calibration_Name, Value);
   end Set_Temperature_Sensor_Calibration;

   ----------------------------------------
   -- Get_Temperature_Sensor_Calibration --
   ----------------------------------------

   procedure Get_Temperature_Sensor_Calibration
     (This         : Si4432_Driver;
      Trim_Value   : out UInt4;
      Trim         : out Boolean;
      Offset       : out Boolean;
      Sensor_Range : out Temperature_Sensor_Range)
   is
      Value : constant Register := Read_Register
        (This, Temperature_Sensor_Calibration_Name);

      Reg   : Temperature_Sensor_Calibration_Register with Import,
        Address => Value'Address;
   begin
      Trim_Value   := Reg.tstrim;
      Trim         := To_Boolean (Reg.entstrim);
      Offset       := To_Boolean (Reg.entsoffs);
      Sensor_Range := Reg.tsrange;
   end Get_Temperature_Sensor_Calibration;

   ----------------------------------
   -- Set_Temperature_Value_Offset --
   ----------------------------------

   procedure Set_Temperature_Value_Offset
     (This  : Si4432_Driver;
      Value : UInt8) is
   begin
      Write_Register (This, Temperature_Value_Offset_Name, Register (Value));
   end Set_Temperature_Value_Offset;

   ----------------------------------
   -- Get_Temperature_Value_Offset --
   ----------------------------------

   function Get_Temperature_Value_Offset
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, Temperature_Value_Offset_Name));
   end Get_Temperature_Value_Offset;

   ------------------------------
   -- Set_Wake_Up_Timer_Period --
   ------------------------------

   procedure Set_Wake_Up_Timer_Period
     (This  : Si4432_Driver;
      Value : Wake_Up_Timer_Period)
   is
      Local : constant Unsigned_16 := Unsigned_16 (Value);
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
   begin
      Data (1) := UInt8 (Shift_Right (Local, 8));
      Data (2) := UInt8 (Local and 16#FF#);

      Write_Register (This, Wake_Up_Timer_Period_2_Name, Data);
   end Set_Wake_Up_Timer_Period;

   ------------------------------
   -- Get_Wake_Up_Timer_Period --
   ------------------------------

   function Get_Wake_Up_Timer_Period
     (This : Si4432_Driver)
      return Wake_Up_Timer_Period
   is
      Data : HAL.SPI.SPI_Data_8b (1 .. 2);
   begin
      Read_Register (This, Wake_Up_Timer_Period_2_Name, Data);

      return Wake_Up_Timer_Period
        (Shift_Left (Unsigned_16 (Data (1)), 8) + Unsigned_16 (Data (2)));
   end Get_Wake_Up_Timer_Period;

   -------------------------------------
   -- Get_Wake_Up_Timer_Current_Value --
   -------------------------------------

   function Get_Wake_Up_Timer_Current_Value
     (This : Si4432_Driver)
      return UInt16
   is
      Data : HAL.SPI.SPI_Data_8b (1 .. 2);
   begin
      Read_Register (This, Wake_Up_Timer_Value_1_Name, Data);

      return UInt16
        (Shift_Left (Unsigned_16 (Data (1)), 8) + Unsigned_16 (Data (2)));
   end Get_Wake_Up_Timer_Current_Value;

   --------------------------------
   -- Set_Wake_Up_Timer_Exponent --
   --------------------------------

   procedure Set_Wake_Up_Timer_Exponent
     (This  : Si4432_Driver;
      Value : Wake_Up_Timer_Exponent)
   is
      Reg  : constant Wake_Up_Timer_Period_1_Register :=
        (wtr      => UInt4 (Value),
         Reserved => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Wake_Up_Timer_Period_1_Name, Data);
   end Set_Wake_Up_Timer_Exponent;

   --------------------------------
   -- Get_Wake_Up_Timer_Exponent --
   --------------------------------

   function Get_Wake_Up_Timer_Exponent
     (This  : Si4432_Driver)
      return Wake_Up_Timer_Exponent
   is
      Value : constant Register := Read_Register
        (This, Wake_Up_Timer_Period_1_Name);

      Reg   : Wake_Up_Timer_Period_1_Register with Import,
        Address => Value'Address;
   begin
      return Wake_Up_Timer_Exponent (Reg.wtr);
   end Get_Wake_Up_Timer_Exponent;

   --------------------------------------
   -- Set_Low_Duty_Cycle_Mode_Duration --
   --------------------------------------

   procedure Set_Low_Duty_Cycle_Mode_Duration
     (This  : Si4432_Driver;
      Value : Low_Duty_Cycle_Mode_Duration) is
   begin
      Write_Register
        (This, Low_Duty_Cycle_Mode_Duration_Name, Register (Value));
   end Set_Low_Duty_Cycle_Mode_Duration;

   --------------------------------------
   -- Get_Low_Duty_Cycle_Mode_Duration --
   --------------------------------------

   function Get_Low_Duty_Cycle_Mode_Duration
     (This : Si4432_Driver)
      return Low_Duty_Cycle_Mode_Duration is
   begin
      return Low_Duty_Cycle_Mode_Duration
        (Read_Register (This, Low_Duty_Cycle_Mode_Duration_Name));
   end Get_Low_Duty_Cycle_Mode_Duration;

   ----------------------------------------
   -- Set_Low_Battery_Detector_Threshold --
   ----------------------------------------

   procedure Set_Low_Battery_Detector_Threshold
     (This  : Si4432_Driver;
      Value : UInt5)
   is
      Reg  : constant Low_Battery_Detector_Threshold_Register :=
        (lbdt     => Value,
         Reserved => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Low_Battery_Detector_Threshold_Name, Data);
   end Set_Low_Battery_Detector_Threshold;

   ----------------------------------------
   -- Get_Low_Battery_Detector_Threshold --
   ----------------------------------------

   function Get_Low_Battery_Detector_Threshold
     (This : Si4432_Driver)
      return UInt5
   is
      Value : constant Register := Read_Register
        (This, Low_Battery_Detector_Threshold_Name);

      Reg   : Low_Battery_Detector_Threshold_Register with Import,
        Address => Value'Address;
   begin
      return Reg.lbdt;
   end Get_Low_Battery_Detector_Threshold;

   -------------------------------
   -- Get_Battery_Voltage_Level --
   -------------------------------

   function Get_Battery_Voltage_Level
     (This : Si4432_Driver)
      return UInt5
   is
      Value : constant Register := Read_Register
        (This, Battery_Voltage_Level_Name);

      Reg   : Battery_Voltage_Level_Register with Import,
        Address => Value'Address;
   begin
      return Reg.vbat;
   end Get_Battery_Voltage_Level;

   ----------------------------
   -- Set_AFC_Timing_Control --
   ----------------------------

   procedure Set_AFC_Timing_Control
     (This        : Si4432_Driver;
      Anwait      : UInt3;
      Shwait      : UInt3;
      Swant_Timer : UInt2)
   is
      Reg  : constant AFC_Timing_Control_Register :=
        (anwait      => Anwait,
         shwait      => Shwait,
         swant_timer => Swant_Timer);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, AFC_Timing_Control_Name, Data);
   end Set_AFC_Timing_Control;

   ----------------------------
   -- Get_AFC_Timing_Control --
   ----------------------------

   procedure Get_AFC_Timing_Control
     (This        : Si4432_Driver;
      Anwait      : out UInt3;
      Shwait      : out UInt3;
      Swant_Timer : out UInt2)
   is
      Data : constant Register := Read_Register
        (This, AFC_Timing_Control_Name);
      Reg  : AFC_Timing_Control_Register with Import, Address => Data'Address;
   begin
      Anwait      := Reg.anwait;
      Shwait      := Reg.shwait;
      Swant_Timer := Reg.swant_timer;
   end Get_AFC_Timing_Control;

   -------------------------------------------
   -- Set_Clock_Recovery_Gearshift_Override --
   -------------------------------------------

   procedure Set_Clock_Recovery_Gearshift_Override
     (This : Si4432_Driver;
      Slow : UInt3;
      Fast : UInt3)
   is
      Reg  : constant Clock_Recovery_Gearshift_Override_Register :=
        (crslow   => Slow,
         crfast   => Fast,
         Reserved => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Clock_Recovery_Gearshift_Override_Name, Data);
   end Set_Clock_Recovery_Gearshift_Override;

   -------------------------------------------
   -- Get_Clock_Recovery_Gearshift_Override --
   -------------------------------------------

   procedure Get_Clock_Recovery_Gearshift_Override
     (This : Si4432_Driver;
      Slow : out UInt3;
      Fast : out UInt3)
   is
      Data : constant Register := Read_Register
        (This, Clock_Recovery_Gearshift_Override_Name);

      Reg  : Clock_Recovery_Gearshift_Override_Register with Import,
        Address => Data'Address;
   begin
      Slow := Reg.crslow;
      Fast := Reg.crfast;
   end Get_Clock_Recovery_Gearshift_Override;

   --------------------------------------------
   -- Get_Received_Signal_Strength_Indicator --
   --------------------------------------------

   function Get_Received_Signal_Strength_Indicator
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8
        (Read_Register (This, Received_Signal_Strength_Indicator_Name));
   end Get_Received_Signal_Strength_Indicator;

   ----------------------------------------------------
   -- Set_RSSI_Threshold_For_Clear_Channel_Indicator --
   ----------------------------------------------------

   procedure Set_RSSI_Threshold_For_Clear_Channel_Indicator
     (This  : Si4432_Driver;
      Value : UInt8) is
   begin
      Write_Register
        (This,
         RSSI_Threshold_For_Clear_Channel_Indicator_Name,
         Register (Value));
   end Set_RSSI_Threshold_For_Clear_Channel_Indicator;

   ----------------------------------------------------
   -- Get_RSSI_Threshold_For_Clear_Channel_Indicator --
   ----------------------------------------------------

   function Get_RSSI_Threshold_For_Clear_Channel_Indicator
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8
        (Read_Register
           (This, RSSI_Threshold_For_Clear_Channel_Indicator_Name));
   end Get_RSSI_Threshold_For_Clear_Channel_Indicator;

   -----------------------------
   -- Get_Antenna_Diversity_1 --
   -----------------------------

   function Get_Antenna_Diversity_1
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, Antenna_Diversity_1_Name));
   end Get_Antenna_Diversity_1;

   -----------------------------
   -- Get_Antenna_Diversity_2 --
   -----------------------------

   function Get_Antenna_Diversity_2
     (This : Si4432_Driver)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, Antenna_Diversity_2_Name));
   end Get_Antenna_Diversity_2;

   ------------------------
   -- Get_AFC_Correction --
   ------------------------

   function Get_AFC_Correction
     (This : Si4432_Driver)
      return AFC_Correction
   is
      Data : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg  : OOK_Counter_Value_1_Register with Import,
        Address => Data (2)'Address;

   begin
      Read_Register (This, AFC_Correction_Name, Data);

      return AFC_Correction
        (Shift_Left (Unsigned_16 (Data (1)), 2) + Unsigned_16 (Reg.afc_corr));
   end Get_AFC_Correction;

   ---------------------
   -- Set_OOK_Counter --
   ---------------------

   procedure Set_OOK_Counter
     (This          : Si4432_Driver;
      Value         : OOK_Counter;
      MA            : Boolean;
      Peak_Detector : Boolean;
      OOK_Freeze    : Boolean)
   is
      Local : constant Unsigned_16 := Unsigned_16 (Value);
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg   : OOK_Counter_Value_1_Register with Import,
        Address => Data (1)'Address;
   begin
      Data (1) := UInt8 (Read_Register (This, OOK_Counter_Value_1_Name));
      Reg.ookcnt    := UInt3 (Shift_Right (Local, 8));
      Reg.madeten   := From_Boolean (MA);
      Reg.peakdeten := From_Boolean (Peak_Detector);
      Reg.ookfrzen  := From_Boolean (OOK_Freeze);
      Data (2) := UInt8 (Local and 16#FF#);

      Write_Register (This, OOK_Counter_Value_1_Name, Data);
   end Set_OOK_Counter;

   ---------------------
   -- Get_OOK_Counter --
   ---------------------

   procedure Get_OOK_Counter
     (This          : Si4432_Driver;
      Value         : out OOK_Counter;
      MA            : out Boolean;
      Peak_Detector : out Boolean;
      OOK_Freeze    : out Boolean)
   is
      Data  : HAL.SPI.SPI_Data_8b (1 .. 2);
      Reg   : OOK_Counter_Value_1_Register with Import,
        Address => Data (1)'Address;
   begin
      Read_Register (This, OOK_Counter_Value_1_Name, Data);

      Value         := OOK_Counter
        (Shift_Left (Unsigned_16 (Reg.ookcnt), 8) + Unsigned_16 (Data (2)));
      MA            := To_Boolean (Reg.madeten);
      Peak_Detector := To_Boolean (Reg.peakdeten);
      OOK_Freeze    := To_Boolean (Reg.ookfrzen);
   end Get_OOK_Counter;

   ----------------------------
   -- Set_Slicer_Peak_Holder --
   ----------------------------

   procedure Set_Slicer_Peak_Holder
     (This   : Si4432_Driver;
      Decay  : UInt4;
      Attack : UInt3)
   is
      Reg  : constant Slicer_Peak_Holder_Register :=
        (decay    => Decay,
         attack   => Attack,
         Reserved => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Slicer_Peak_Holder_Name, Data);
   end Set_Slicer_Peak_Holder;

   ----------------------------
   -- Get_Slicer_Peak_Holder --
   ----------------------------

   procedure Get_Slicer_Peak_Holder
     (This   : Si4432_Driver;
      Decay  : out UInt4;
      Attack : out UInt3)
   is
      Data : constant Register := Read_Register
        (This, Slicer_Peak_Holder_Name);
      Reg  : Slicer_Peak_Holder_Register with Import, Address => Data'Address;
   begin
      Decay  := Reg.decay;
      Attack := Reg.attack;
   end Get_Slicer_Peak_Holder;

   ----------------------
   -- Get_EZMAC_Status --
   ----------------------

   procedure Get_EZMAC_Status
     (This                  : Si4432_Driver;
      Packet_Sent           : out Boolean;
      Packet_Transmitting   : out Boolean;
      CRC_Error             : out Boolean;
      Valid_Packet_Received : out Boolean;
      Packet_Receiving      : out Boolean;
      Packet_Searching      : out Boolean;
      Underflow_CRC         : out Boolean)
   is
      Data : constant Register := Read_Register (This, EZMAC_Status_Name);
      Reg  : EZMAC_Status_Register with Import, Address => Data'Address;
   begin
      Packet_Sent           := To_Boolean (Reg.pksent);
      Packet_Transmitting   := To_Boolean (Reg.pktx);
      CRC_Error             := To_Boolean (Reg.crcerror);
      Valid_Packet_Received := To_Boolean (Reg.pkvalid);
      Packet_Receiving      := To_Boolean (Reg.pkrx);
      Packet_Searching      := To_Boolean (Reg.pksrch);
      Underflow_CRC         := To_Boolean (Reg.rxcrc1);
   end Get_EZMAC_Status;

   -------------------------
   -- Set_Transmit_Header --
   -------------------------

   procedure Set_Transmit_Header
     (This  : Si4432_Driver;
      Index : UInt2;
      Value : UInt8) is
   begin
      Write_Register
        (This, To_Transmit_Header_Index (Index), Register (Value));
   end Set_Transmit_Header;

   -------------------------
   -- Get_Transmit_Header --
   -------------------------

   function Get_Transmit_Header
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, To_Transmit_Header_Index (Index)));
   end Get_Transmit_Header;

   ----------------------
   -- Set_Check_Header --
   ----------------------

   procedure Set_Check_Header
     (This  : Si4432_Driver;
      Index : UInt2;
      Value : UInt8) is
   begin
      Write_Register (This, To_Check_Header_Index (Index), Register (Value));
   end Set_Check_Header;

   ----------------------
   -- Get_Check_Header --
   ----------------------

   function Get_Check_Header
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, To_Check_Header_Index (Index)));
   end Get_Check_Header;

   -----------------------
   -- Set_Header_Enable --
   -----------------------

   procedure Set_Header_Enable
     (This  : Si4432_Driver;
      Index : UInt2;
      Value : UInt8) is
   begin
      Write_Register (This, To_Header_Enable_Index (Index), Register (Value));
   end Set_Header_Enable;

   -----------------------
   -- Get_Header_Enable --
   -----------------------

   function Get_Header_Enable
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8 is
   begin
      return UInt8 (Read_Register (This, To_Header_Enable_Index (Index)));
   end Get_Header_Enable;

   -------------------------
   -- Get_Received_Header --
   -------------------------

   function Get_Received_Header
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8
   is
      function To_Name
        (Index : UInt2)
         return Register_Name;

      function To_Name
        (Index : UInt2)
         return Register_Name is
      begin
         case Index is
         when 0 =>
            return Received_Header_0_Name;
         when 1 =>
            return Received_Header_1_Name;
         when 2 =>
            return Received_Header_2_Name;
         when 3 =>
            return Received_Header_3_Name;
         end case;
      end To_Name;

   begin
      return UInt8 (Read_Register (This, To_Name (Index)));
   end Get_Received_Header;

   ----------------------
   -- Set_ADC8_Control --
   ----------------------

   procedure Set_ADC8_Control
     (This  : Si4432_Driver;
      Value : ADC8_Control)
   is
      Reg  : constant ADC8_Control_Register :=
        (adc8     => UInt6 (Value),
         Reserved => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, ADC8_Control_Name, Data);
   end Set_ADC8_Control;

   ----------------------
   -- Get_ADC8_Control --
   ----------------------

   function Get_ADC8_Control
     (This : Si4432_Driver)
      return ADC8_Control
   is
      Data : constant Register := Read_Register (This, ADC8_Control_Name);
      Reg  : ADC8_Control_Register with Import, Address => Data'Address;
   begin
      return ADC8_Control (Reg.adc8);
   end Get_ADC8_Control;

   --------------------------------------------
   -- Set_Channel_Filter_Coefficient_Address --
   --------------------------------------------

   procedure Set_Channel_Filter_Coefficient_Address
     (This  : Si4432_Driver;
      Value : UInt4)
   is
      Reg  : constant Channel_Filter_Coefficient_Address_Register :=
        (Reserved                   => 0,
         invalid_preamble_threshold => Value);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Channel_Filter_Coefficient_Address_Name, Data);
   end Set_Channel_Filter_Coefficient_Address;

   --------------------------------------------
   -- Get_Channel_Filter_Coefficient_Address --
   --------------------------------------------

   function Get_Channel_Filter_Coefficient_Address
     (This : Si4432_Driver)
      return UInt4
   is
      Data : constant Register := Read_Register
        (This, Channel_Filter_Coefficient_Address_Name);
      Reg  : Channel_Filter_Coefficient_Address_Register with Import,
        Address => Data'Address;
   begin
      return Reg.invalid_preamble_threshold;
   end Get_Channel_Filter_Coefficient_Address;

   ----------------------------
   -- Set_Crystal_Oscillator --
   ----------------------------

   procedure Set_Crystal_Oscillator
     (This                           : Si4432_Driver;
      Output_Buffer                  : Boolean;
      Output_Buffer_Override         : Boolean;
      Two_Times_Higher_Amplification : Boolean;
      Two_Times_Higher_Bias_Current  : Boolean;
      Clock_Hysteresis               : Boolean)
   is
      Reg  : constant Crystal_Oscillator_Register :=
        (enbuf    => From_Boolean (Output_Buffer),
         bufovr   => From_Boolean (Output_Buffer_Override),
         enamp2x  => From_Boolean (Two_Times_Higher_Amplification),
         enbias2x => From_Boolean (Two_Times_Higher_Bias_Current),
         clkhyst  => From_Boolean (Clock_Hysteresis),
         pwst     => LP);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, Crystal_Oscillator_Name, Data);
   end Set_Crystal_Oscillator;

   ----------------------------
   -- Get_Crystal_Oscillator --
   ----------------------------

   procedure Get_Crystal_Oscillator
     (This                           : Si4432_Driver;
      Output_Buffer                  : out Boolean;
      Output_Buffer_Override         : out Boolean;
      Two_Times_Higher_Amplification : out Boolean;
      Two_Times_Higher_Bias_Current  : out Boolean;
      Clock_Hysteresis               : out Boolean;
      Internal_Power_State           : out Internal_Chip_State)
   is
      Data : constant Register := Read_Register
        (This, Crystal_Oscillator_Name);
      Reg  : Crystal_Oscillator_Register with Import, Address => Data'Address;
   begin
      Output_Buffer                  := To_Boolean (Reg.enbuf);
      Output_Buffer_Override         := To_Boolean (Reg.bufovr);
      Two_Times_Higher_Amplification := To_Boolean (Reg.enamp2x);
      Two_Times_Higher_Bias_Current  := To_Boolean (Reg.enbias2x);
      Clock_Hysteresis               := To_Boolean (Reg.clkhyst);
      Internal_Power_State           := Reg.pwst;
   end Get_Crystal_Oscillator;

   -----------------------------
   -- Set_TX_FIFO_Almost_Full --
   -----------------------------

   procedure Set_TX_FIFO_Almost_Full
     (This  : Si4432_Driver;
      Value : FIFO_Threshold)
   is
      Reg  : constant TX_FIFO_Control_1_Register :=
        (txafthr  => UInt6 (Value),
         Reserved => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, TX_FIFO_Control_1_Name, Data);
   end Set_TX_FIFO_Almost_Full;

   -----------------------------
   -- Get_TX_FIFO_Almost_Full --
   -----------------------------

   function Get_TX_FIFO_Almost_Full
     (This : Si4432_Driver)
      return FIFO_Threshold
   is
      Data : constant Register := Read_Register (This, TX_FIFO_Control_1_Name);
      Reg  : TX_FIFO_Control_1_Register with Import, Address => Data'Address;
   begin
      return FIFO_Threshold (Reg.txafthr);
   end Get_TX_FIFO_Almost_Full;

   ------------------------------
   -- Set_TX_FIFO_Almost_Empty --
   ------------------------------

   procedure Set_TX_FIFO_Almost_Empty
     (This  : Si4432_Driver;
      Value : FIFO_Threshold)
   is
      Reg  : constant TX_FIFO_Control_2_Register :=
        (txfaethr => UInt6 (Value),
         Reserved => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, TX_FIFO_Control_2_Name, Data);
   end Set_TX_FIFO_Almost_Empty;

   ------------------------------
   -- Get_TX_FIFO_Almost_Empty --
   ------------------------------

   function Get_TX_FIFO_Almost_Empty
     (This : Si4432_Driver)
      return FIFO_Threshold
   is
      Data : constant Register := Read_Register (This, TX_FIFO_Control_2_Name);
      Reg  : TX_FIFO_Control_2_Register with Import, Address => Data'Address;
   begin
      return FIFO_Threshold (Reg.txfaethr);
   end Get_TX_FIFO_Almost_Empty;

   -----------------------------
   -- Set_RX_FIFO_Almost_Full --
   -----------------------------

   procedure Set_RX_FIFO_Almost_Full
     (This  : Si4432_Driver;
      Value : FIFO_Threshold)
   is
      Reg  : constant RX_FIFO_Control_Register :=
        (rxafthr  => UInt6 (Value),
         Reserved => 0);

      Data : Register with Import, Address => Reg'Address;
   begin
      Write_Register (This, RX_FIFO_Control_Name, Data);
   end Set_RX_FIFO_Almost_Full;

   -----------------------------
   -- Get_RX_FIFO_Almost_Full --
   -----------------------------

   function Get_RX_FIFO_Almost_Full
     (This : Si4432_Driver)
      return FIFO_Threshold
   is
      Data : constant Register := Read_Register (This, RX_FIFO_Control_Name);
      Reg  : RX_FIFO_Control_Register with Import, Address => Data'Address;
   begin
      return FIFO_Threshold (Reg.rxafthr);
   end Get_RX_FIFO_Almost_Full;

   ----------
   -- Send --
   ----------

   procedure Send
     (This : Si4432_Driver;
      Data : SPI_Data_8b) is
   begin
      Set_Packet_Length (This, Data'Length);
      Write_Register (This, FIFO_Access_Name, Data);
      Set_State (This, TX);
   end Send;

   ------------------
   -- Get_Received --
   ------------------

   procedure Get_Received
     (This : Si4432_Driver;
      Data : out SPI_Data_8b)
   is
      Received : constant Natural := Natural
        (Get_Received_Packet_Length (This));
      Length   : constant Natural := Natural'Min (Received, Data'Length);
   begin
      Read_Register
        (This, FIFO_Access_Name, Data (Data'First .. Data'First + Length - 1));

      if Received <= Data'Length then
         Clear_RX_FIFO (This);
      end if;
   end Get_Received;

   -------------------
   -- Clear_RX_FIFO --
   -------------------

   procedure Clear_RX_FIFO (This : Si4432_Driver)
   is
      Value : Register := Read_Register
        (This, Operating_Mode_And_Function_Control_2_Name);

      Reg   : Operating_Mode_And_Function_Control_2_Register with Import,
        Address => Value'Address;
   begin
      Reg.ffclrrx := 1;
      Write_Register (This, Operating_Mode_And_Function_Control_2_Name, Value);
      Reg.ffclrrx := 0;
      Write_Register (This, Operating_Mode_And_Function_Control_2_Name, Value);
   end Clear_RX_FIFO;

   -------------------
   -- Clear_TX_FIFO --
   -------------------

   procedure Clear_TX_FIFO (This : Si4432_Driver)
   is
      Value : Register := Read_Register
        (This, Operating_Mode_And_Function_Control_2_Name);

      Reg   : Operating_Mode_And_Function_Control_2_Register with Import,
        Address => Value'Address;
   begin
      Reg.ffclrtx := 1;
      Write_Register (This, Operating_Mode_And_Function_Control_2_Name, Value);
      Reg.ffclrtx := 0;
      Write_Register (This, Operating_Mode_And_Function_Control_2_Name, Value);
   end Clear_TX_FIFO;

   --------------
   -- CSN_High --
   --------------

   procedure CSN_High (This : Si4432_Driver) is
   begin
      if This.Holder.Kind = Software then
         This.Holder.CSN_Pin.Set;
      end if;
   end CSN_High;

   -------------
   -- CSN_Low --
   -------------

   procedure CSN_Low (This : Si4432_Driver) is
   begin
      if This.Holder.Kind = Software then
         This.Holder.CSN_Pin.Clear;
      end if;
   end CSN_Low;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register
     (This : Si4432_Driver;
      Name : Register_Name)
      return Register
   is
      Command : constant Command_Register :=
        (Address   => Registers_Addressses (Name),
         Operation => Read);
      Cmd    : HAL.SPI.SPI_Data_8b (1 .. 1) with Import,
        Address => Command'Address;

      Data   : HAL.SPI.SPI_Data_8b (1 .. 1);
      Result : Register
        with Import, Address => Data (Data'First)'Address;
   begin
      Write_And_Read (This, Cmd, Data);
      return Result;
   end Read_Register;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register
     (This : Si4432_Driver;
      Name : Register_Name;
      Data : out HAL.SPI.SPI_Data_8b)
   is
      Command : constant Command_Register :=
        (Address   => Registers_Addressses (Name),
         Operation => Read);
      Cmd    : HAL.SPI.SPI_Data_8b (1 .. 1) with Import,
        Address => Command'Address;

   begin
      Write_And_Read (This, Cmd, Data);
   end Read_Register;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register
     (This : Si4432_Driver;
      Name : Register_Name;
      Data : Register)
   is
      Buff    : HAL.SPI.SPI_Data_8b (1 .. 2);
      Command : Command_Register :=
        (Address   => Registers_Addressses (Name),
         Operation => Write) with Address => Buff (1)'Address;
   begin
      Buff (2) := UInt8 (Data);
      Write (This, Buff);
   end Write_Register;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register
     (This : Si4432_Driver;
      Name : Register_Name;
      Data : HAL.SPI.SPI_Data_8b)
   is
      Buff    : HAL.SPI.SPI_Data_8b (1 .. Data'Length + 1);
      Command : Command_Register :=
        (Address   => Registers_Addressses (Name),
         Operation => Write) with Address => Buff (1)'Address;
   begin
      Buff (2 .. Buff'Last) := Data;
      Write (This, Buff);
   end Write_Register;

   -----------
   -- Write --
   -----------

   procedure Write
     (This  : Si4432_Driver;
      Data  : HAL.SPI.SPI_Data_8b)
   is
      Status : HAL.SPI.SPI_Status;
   begin
      CSN_Low (This);
      This.Holder.SPI.Transmit (Data, Status);
      CSN_High (This);

      if Status /= HAL.SPI.Ok then
         raise Program_Error;
      end if;
   end Write;

   --------------------
   -- Write_And_Read --
   --------------------

   procedure Write_And_Read
     (This    : Si4432_Driver;
      Command : HAL.SPI.SPI_Data_8b;
      Data    : out HAL.SPI.SPI_Data_8b)
   is
      Status : HAL.SPI.SPI_Status;
   begin
      CSN_Low (This);
      This.Holder.SPI.Transmit (Command, Status);
      if Status /= HAL.SPI.Ok then
         CSN_High (This);
         raise Program_Error;
      end if;

      This.Holder.SPI.Receive (Data, Status);
      CSN_High (This);

      if Status /= HAL.SPI.Ok then
         raise Program_Error;
      end if;
   end Write_And_Read;

   ------------------
   -- From_Boolean --
   ------------------

   function From_Boolean (Value : Boolean) return Bit is
   begin
      return (if Value then 1 else 0);
   end From_Boolean;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Value : Bit) return Boolean is
   begin
      return (if Value = 1 then True else False);
   end To_Boolean;

   -----------------------------------
   -- To_Synchronization_Word_Index --
   -----------------------------------

   function To_Synchronization_Word_Index
     (Index : UInt2)
      return Register_Name is
   begin
      case Index is
         when 0 =>
            return Synchronization_Word_0_Name;
         when 1 =>
            return Synchronization_Word_1_Name;
         when 2 =>
            return Synchronization_Word_2_Name;
         when 3 =>
            return Synchronization_Word_3_Name;
      end case;
   end To_Synchronization_Word_Index;

   ------------------------------
   -- To_Transmit_Header_Index --
   ------------------------------

   function To_Transmit_Header_Index
     (Index : UInt2)
      return Register_Name is
   begin
      case Index is
         when 0 =>
            return Transmit_Header_0_Name;
         when 1 =>
            return Transmit_Header_1_Name;
         when 2 =>
            return Transmit_Header_2_Name;
         when 3 =>
            return Transmit_Header_3_Name;
      end case;
   end To_Transmit_Header_Index;

   ---------------------------
   -- To_Check_Header_Index --
   ---------------------------

   function To_Check_Header_Index
     (Index : UInt2)
      return Register_Name is
   begin
      case Index is
         when 0 =>
            return Check_Header_0_Name;
         when 1 =>
            return Check_Header_1_Name;
         when 2 =>
            return Check_Header_2_Name;
         when 3 =>
            return Check_Header_3_Name;
      end case;
   end To_Check_Header_Index;

   ----------------------------
   -- To_Header_Enable_Index --
   ----------------------------

   function To_Header_Enable_Index
     (Index : UInt2)
      return Register_Name is
   begin
      case Index is
         when 0 =>
            return Header_Enable_0_Name;
         when 1 =>
            return Header_Enable_1_Name;
         when 2 =>
            return Header_Enable_2_Name;
         when 3 =>
            return Header_Enable_3_Name;
      end case;
   end To_Header_Enable_Index;

end Si4432;
