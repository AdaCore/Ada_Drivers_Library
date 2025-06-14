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

--  Simple test/example for STM32F429disco with two Si4432B1 attached

with Ada.Exceptions;
with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with HAL.SPI;
with STM32.Board;    use STM32.Board;
with STM32.Device;
with STM32.GPIO;     use STM32.GPIO;
with STM32.SPI;      use STM32.SPI;
with STM32.EXTI;

with Si4432;         use Si4432;
with Watchdogs;      use Watchdogs;

--  Service
with Ada.Real_Time;  use Ada.Real_Time;
with HAL.Bitmap;     use HAL.Bitmap;
with BMP_Fonts;
with LCD_Std_Out;

--  TX:
--   SDN PB7
--   CSN PE3
--   IRQ PC3
--     SPI4 Pins:
--   PE2  SPI4_SCK
--   PE5  SPI4_MISO
--   PE6  SPI4_MOSI

--  RX:
--   SDN PC11
--   CSN PC12
--   IRQ PB4
--     SPI4 Pins:
--   PE2  SPI4_SCK
--   PE5  SPI4_MISO
--   PE6  SPI4_MOSI

procedure Si4432_Example is

   use type HAL.UInt8;

   SPI4_Pins : constant GPIO_Points :=
     (STM32.Device.PE2, STM32.Device.PE5, STM32.Device.PE6);
   SPI       : SPI_Port   renames STM32.Device.SPI_4;

   procedure Init_Pins; --  Initialize pins
   procedure Init_SPI;  --  Initialize SPI

   procedure Configure_RX; --  Configure RX side
   procedure Configure_TX; --  Configure TX side

   procedure Configure (Driver : Si4432_Driver);
   --  Apply common RX/TX settings

   procedure On_RX_IRQ; -- RX IRQ handler
   procedure On_TX_IRQ; -- TX IRQ handler

   --  Wait for the specified RX/TX statuses
   procedure Wait_For_RX (Status1, Status2 : HAL.UInt8);
   procedure Wait_For_TX (Status1, Status2 : HAL.UInt8);

   procedure Ckeck_RX_Data (Value : String); --  Check that we got proper data
   procedure On_Error (Msg : String);  -- Prints error message

   ------------------
   -- Configure_RX --
   ------------------

   -- RX --
   RX_SDN     : GPIO_Point renames STM32.Device.PC11;
   RX_CSN     : GPIO_Point renames STM32.Device.PC12;
   RX         : Si4432_Driver;

   Has_RX_IRQ : Boolean := False with Volatile, Atomic;
   RX_Status  : Interrupt_Statuses := (others => 0) with Volatile, Atomic;

   procedure Configure_RX is
   begin
      --  Initialize RX --
      Initialize
        (This    => RX,
         CSN_Pin => RX_CSN'Access,
         SDN_Pin => RX_SDN'Access,
         SPI     => SPI'Access);
      delay 0.1;

      --  Reset
      Set_Power (RX, On);
      delay 0.2;
      Clear_Interrupts (RX);
      Has_RX_IRQ := False;
      Software_Reset (RX);
      Wait_For_RX (0, Status_Chip_Ready);

      Set_Interrupt_Enable
        (RX,
         CRC_Error             => True,
         Valid_Packet_Received => True,
         RX_FIFO_Almost_Full   => False,
         Valid_Preamble        => False,
         Sync_Word             => False,
         RSSI                  => False,
         POR                   => False,
         Chip_Ready            => False);

      LCD_Std_Out.Put_Line ("RX code:" & Device_Type_Code (RX)'Img);
      LCD_Std_Out.Put_Line ("RX version:" & Version_Code (RX)'Img);

      Configure (RX);

      Set_Modulation_Mode_Control_1
        (RX,
         Data_Whitening               => False,
         Manchester_Coding            => False,
         Manchester_Data_Inversion    => False,
         Manchester_Preamble_Polarity => False,
         Packet_Handler_Down          => False,
         Data_Rates_Below             => False);

      --  Set the modem parameters according to the exel calculator
      --  (parameters: 9.6 kbps, deviation: 45 kHz, channel filter
      --   BW: 102.2 kHz)
      Set_IF_Filter_Bandwidth
        (RX,
         Coefficient      => 16#E#,
         Decimation_Rates => 1,
         Bypass_Decimate  => False);

      Set_Clock_Recovery_Oversampling_Rate (RX, 16#D0#);

      Set_Clock_Recovery_Offset
        (RX,
         NCO_Offset     => 16#9D49#,
         Skip_2nd_Phase => False);

      Set_Clock_Recovery_Timing_Loop_Gain
        (RX,
         Gain                => 16#24#,
         Multiplying_By_2    => False,
         Compensation_Enable => False);

      Set_AFC_Loop_Gearshift_Override
        (RX,
         Reset_Preamble => False,
         Taps           => False,
         Bypass         => False,
         AFC_High_Gear  => 0,
         AFC            => True,
         AFC_Wideband   => False);

      --  Si443x revision B1, there are dedicated registers for setting the
      --  AFC Limit. It makes the radio easier to use; there is no need to
      --  change the Frequency Deviation register
      Set_AFC_Limiter (RX, 16#20#);

      Set_Data_Access_Control
        (RX,
         CRC_Selection                     => CRC_16,
         CRC_Enable                        => True,
         Enable_Packet_TX_Handling         => False,
         Skip_2nd_Phase_Preamble_Detection => False,
         CRC_Data_Only_Enable              => False,
         LSB_First_Enable                  => False,
         Enable_Packet_RX_Handling         => True);

      Set_Preamble_Detection_Control
        (RX,
         RSSI_Offset         => 0,
         Detection_Threshold => 5);

      Set_AGC_Override
        (RX,
         Gain_Override          => 0,
         LNA_Gain_Select        => False,
         Automatic_Gain_Control => True,
         Sgin                   => True);
   end Configure_RX;

   ------------------
   -- Configure_TX --
   ------------------

   -- TX --
   TX_SDN     : GPIO_Point renames STM32.Device.PB7;
   TX_CSN     : GPIO_Point renames STM32.Device.PE3;
   TX         : Si4432_Driver;

   Has_TX_IRQ : Boolean := False with Volatile, Atomic;
   TX_Status  : Interrupt_Statuses := (others => 0) with Volatile, Atomic;

   procedure Configure_TX is
   begin
      --  Initialize TX --
      Initialize
        (This    => TX,
         CSN_Pin => TX_CSN'Access,
         SDN_Pin => TX_SDN'Access,
         SPI     => SPI'Access);
      delay 0.1;

      --  Reset
      Set_Power (TX, On);
      delay 0.2;
      Clear_Interrupts (TX);
      Has_TX_IRQ := False;
      Software_Reset (TX);
      Wait_For_TX (0, Status_Chip_Ready);

      Set_Interrupt_Enable
        (TX,
         Packet_Sent         => True,
         TX_FIFO_Almost_Full => True,
         POR                 => False,
         Chip_Ready          => False);

      LCD_Std_Out.Put_Line ("TX code:" & Device_Type_Code (TX)'Img);
      LCD_Std_Out.Put_Line ("TX version:" & Version_Code (TX)'Img);

      Configure (TX);

      Set_Frequency_Deviation_Hz (TX, 45_000);  -- +/- 45 kHz

      Set_Data_Access_Control
        (TX,
         CRC_Selection                     => CRC_16,
         CRC_Enable                        => True,
         Enable_Packet_TX_Handling         => True,
         Skip_2nd_Phase_Preamble_Detection => False,
         CRC_Data_Only_Enable              => False,
         LSB_First_Enable                  => False,
         Enable_Packet_RX_Handling         => False);

      --  9.6kbps
      Set_TX_Data_Rate (TX, 16#4EA5#);
      Set_Modulation_Mode_Control_1
        (TX,
         Data_Whitening               => False,
         Manchester_Coding            => False,
         Manchester_Data_Inversion    => True,
         Manchester_Preamble_Polarity => True,
         Packet_Handler_Down          => False,
         Data_Rates_Below             => True);

      Set_Preamble_Length (TX, 9);

      Set_TX_Power
        (TX,
         Output_Power => 0, --  min output power
         LNA_Switch   => True);
   end Configure_TX;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Driver : Si4432_Driver) is
   begin
      -- Frequency --
      Set_Frequency (Driver, 915.0); --  915 MHz
      Set_Nominal_Carrier_Frequency (Driver, 16#BB80#);

      Set_Header_Control
        (Driver,
         Header_Bytes_Check      => 0,     -- No heareds
         Broadcast_Address_Check => 0,     -- No broadcasts
         Synchronization         => Word_3_2,
         Fix_TR_Packet_Length    => False, -- variable packet length
         Header                  => No_Header,
         Skipsyn                 => False);

      --  Synch Word
      Set_Synchronization_Word (Driver, 3, 16#DD#);
      Set_Synchronization_Word (Driver, 2, 16#4D#);

      --  Modulation
      Set_Modulation_Mode_Control_2
        (Driver,
         Modulation    => GFSK,
         Invert_TX_RX  => False,
         Source        => FIFO,
         TX_Data_Clock => CLK_GPIO);

      --  GPIO
      Set_GPIO1_Configuration
        (Driver,
         Pin_Function           => TX_State,
         Pullup_Resistor_Enable => False,
         Driving_Capability     => 0);

      Set_GPIO2_Configuration
        (Driver,
         Pin_Function           => RX_State,
         Pullup_Resistor_Enable => False,
         Driving_Capability     => 0);

      Set_Crystal_Oscillator_Load_Capacitance
        (Driver,
         Tuning_Capacitance => 16#57#,
         Shft               => 1);
   end Configure;

   ---------------
   -- Init_Pins --
   ---------------

   procedure Init_Pins is
      --  Initialize pins
   begin

      -- TX --
      STM32.Device.Enable_Clock (GPIO_Points'(TX_SDN, TX_CSN, TX_IRQ));

      Configure_IO
        (Points => (TX_SDN, TX_CSN),
         Config =>
           (Mode        => Mode_Out,
            Resistors   => Floating,
            Output_Type => Push_Pull,
            Speed       => Speed_Low));

      TX_SDN.Set;
      TX_CSN.Set;

      --  RX IRQ pin
      TX_IRQ.Configure_IO
        (Config =>
           (Mode      => Mode_In,
            Resistors => Floating));

      TX_Watchdog.Set_Callback (On_TX_IRQ'Unrestricted_Access);
      TX_IRQ.Configure_Trigger (STM32.EXTI.Interrupt_Falling_Edge);

      -- RX --
      STM32.Device.Enable_Clock (GPIO_Points'(RX_SDN, RX_CSN, RX_IRQ));

      Configure_IO
        (Points => (RX_SDN, RX_CSN),
         Config =>
           (Mode        => Mode_Out,
            Resistors   => Floating,
            Output_Type => Push_Pull,
            Speed       => Speed_Low));

      RX_SDN.Set;
      RX_CSN.Set;

      --  RX IRQ pin
      RX_IRQ.Configure_IO
        (Config =>
           (Mode      => Mode_In,
            Resistors => Floating));

      RX_Watchdog.Set_Callback (On_RX_IRQ'Unrestricted_Access);
      RX_IRQ.Configure_Trigger (STM32.EXTI.Interrupt_Falling_Edge);
   end Init_Pins;

   --------------
   -- Init_SPI --
   --------------

   procedure Init_SPI
   --  Initialize SPI
   is
      use STM32.Device;

      Conf      : constant GPIO_Port_Configuration :=
        (Mode           => Mode_AF,
         Resistors      => Floating,
         AF_Output_Type => Push_Pull,
         AF_Speed       => Speed_Very_High,
         AF             => GPIO_AF_SPI4_5);

      SPI_Conf  : constant SPI_Configuration :=
        (Direction           => D2Lines_FullDuplex,
         Mode                => Master,
         Data_Size           => HAL.SPI.Data_Size_8b,
         Clock_Polarity      => Low,
         Clock_Phase         => P1Edge,
         Slave_Management    => Software_Managed,
         Baud_Rate_Prescaler => BRP_16,
         First_Bit           => MSB,
         CRC_Poly            => 10);
   begin
      Enable_Clock (SPI4_Pins);
      Enable_Clock (SPI);

      Configure_IO (SPI4_Pins, Conf);

      Reset (SPI);
      if not SPI.Enabled then
         SPI.Configure (SPI_Conf);
         SPI.Enable;
      end if;
   end Init_SPI;

   Period       : constant Time_Span := Milliseconds (200);
   Next_Release : Time := Clock;
   BG           : constant Bitmap_Color := (Alpha => 255, others => 64);

   ---------------
   -- On_RX_IRQ --
   ---------------

   procedure On_RX_IRQ is
   begin
      RX_Status  := Get_Interrupt_Statuses (RX);
      Has_RX_IRQ := True;
   end On_RX_IRQ;

   ---------------
   -- On_TX_IRQ --
   ---------------

   procedure On_TX_IRQ is
   begin
      TX_Status  := Get_Interrupt_Statuses (TX);
      Has_TX_IRQ := True;
   end On_TX_IRQ;

   --------------
   -- On_Error --
   --------------

   procedure On_Error (Msg : String) is
   begin
      Set_Power (RX, Off);
      Set_Power (TX, Off);

      LCD_Std_Out.Put_Line (Msg);
      Display.Update_Layer (1, Copy_Back => True);

      loop
         STM32.Board.Toggle (Red_LED);
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end On_Error;

   -----------------
   -- Wait_For_RX --
   -----------------

   procedure Wait_For_RX (Status1, Status2 : HAL.UInt8) is
      --  Waiting for status on the RX side
   begin
      loop
         if Has_RX_IRQ then
            Has_RX_IRQ := False;
            if (Status1 and RX_Status (1)) > 0
              or else (Status2 and RX_Status (2)) > 0
            then
               exit;
            end if;
         end if;
         delay 0.1;
      end loop;
   end Wait_For_RX;

   -----------------
   -- Wait_For_TX --
   -----------------

   procedure Wait_For_TX (Status1, Status2 : HAL.UInt8) is
      --  Waiting for status on the TX side
   begin
      loop
         if Has_TX_IRQ then
            Has_TX_IRQ := False;
            if (Status1 and TX_Status (1)) > 0
              or else (Status2 and TX_Status (2)) > 0
            then
               exit;
            end if;
         end if;
         delay 0.1;
      end loop;
   end Wait_For_TX;

   -------------------
   -- Ckeck_RX_Data --
   -------------------

   procedure Ckeck_RX_Data (Value : String)
   --  Checks data on the RX side
   is
      Length : Natural;
   begin
      LCD_Std_Out.Put_Line ("Ckeck_RX_Data");
      Length := Natural (Get_Received_Packet_Length (RX));
      LCD_Std_Out.Put_Line (Length'Img);

      if Length > 0 then
         declare
            Data : HAL.SPI.SPI_Data_8b (1 .. Length);
            Str  : String (Data'Range) with Import, Address => Data'Address;
         begin
            Get_Received (RX, Data);
            LCD_Std_Out.Put_Line ("Got: '" & Str & "'");
            Display.Update_Layer (1, Copy_Back => True);

            if Str /= Value then
               On_Error ("RX DATA FAILS");
            end if;
         end;

      else
         On_Error ("RX DATA EMPTY");
      end if;
   end Ckeck_RX_Data;

begin
   delay 2.0;

   --  Initialize GPIO/SPI
   Init_Pins;
   Init_SPI;
   delay 1.0;

   --  Init testing infrastructure
   STM32.Board.Initialize_LEDs;
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_8888);
   LCD_Std_Out.Set_Font (BMP_Fonts.Font12x12);
   LCD_Std_Out.Current_Background_Color := BG;
   Display.Hidden_Buffer (1).Set_Source (BG);
   Display.Hidden_Buffer (1).Fill;
   LCD_Std_Out.Clear_Screen;

   ----------------
   --  Configure --
   ----------------

   Configure_RX;
   Configure_TX;

   -------------------
   --  Send message --
   -------------------

   LCD_Std_Out.Put_Line ("Send message");

   --  Set RX in active state
   Set_State (RX, Si4432.RX);

   --  Wait until RX is active
   declare
      Power                : Chip_State;
      Frequency_Error      : Boolean;
      Header_Error         : Boolean;
      RX_FIFO_Empty        : Boolean;
      RX_TX_FIFO_Underflow : Boolean;
      RX_TX_FIFO_Overflow  : Boolean;
   begin
      loop
         Get_Device_Status
           (RX, Power, Frequency_Error, Header_Error, RX_FIFO_Empty,
            RX_TX_FIFO_Underflow, RX_TX_FIFO_Overflow);
         exit when Power = Si4432.RX;
         delay 0.5;
      end loop;
   end;

   Has_TX_IRQ := False;
   Has_RX_IRQ := False;

   pragma Assert ((TX_Status (1) and Status_Packet_Sent) = 0);

   --  Send message from the TX
   declare
      S    : constant String := "Hello";
      Data : HAL.SPI.SPI_Data_8b (S'Range) with Import, Address => S'Address;
   begin
      Send (TX, Data);
   end;

   --  Wait until the message has been sent
   Wait_For_TX (Status_Packet_Sent, 0);
   --  Inactivate TX
   Set_State (TX, Idle);

   --  Wait until the message has been received on the RX side
   Wait_For_RX
     ((Status_CRC_Error or Status_Valid_Packet_Received),
      0);
   --  Inactivate RX
   Set_State (RX, Idle);

   --  Check RX status
   if (RX_Status (1) and Status_Valid_Packet_Received) > 0 then
      --  Have valid packet, check if it is equal to sent message
      Ckeck_RX_Data ("Hello");
   else
      --  CRC error in packet
      On_Error ("CRC error");
   end if;

   --  Power Off
   Set_Power (RX, Off);
   Set_Power (TX, Off);

   --  All is OK
   LCD_Std_Out.Put_Line ("All tests passed");
   Display.Update_Layer (1, Copy_Back => True);

   loop
      STM32.Board.Toggle (Green_LED);
      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;

exception
   when E : others =>
      On_Error (Ada.Exceptions.Exception_Information (E));
end Si4432_Example;
