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

--  Simple test/example for STM32F429disco with two nrf24l01p attached

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

with NRF24L01P;      use NRF24L01P;
with Watchdogs;      use Watchdogs;

--  Service
with Ada.Real_Time;  use Ada.Real_Time;
with HAL.Bitmap;     use HAL.Bitmap;
with BMP_Fonts;
with LCD_Std_Out;

--  TX:
--   CE  PB7
--   CSN PE3
--     SPI4 Pins:
--   PE2  SPI4_SCK
--   PE5  SPI4_MISO
--   PE6  SPI4_MOSI


--  RX:
--   CE  PC11
--   CSN PC12
--   IRQ PB4
--     SPI4 Pins:
--   PE2  SPI4_SCK
--   PE5  SPI4_MISO
--   PE6  SPI4_MOSI

procedure nRF24L01P_Example is

   -- TX --
   TX_CE     : GPIO_Point renames STM32.Device.PB7;
   TX_CSN    : GPIO_Point renames STM32.Device.PE3;
   SPI4_Pins : constant GPIO_Points :=
     (STM32.Device.PE2, STM32.Device.PE5, STM32.Device.PE6);
   TX_SPI    : SPI_Port   renames STM32.Device.SPI_4;
   TX        : NRF24L01P_Driver;

   TX_Config : constant Configuration :=
     (Mode       => PTX,
      Power      => Power_Minimum,
      Rate       => Rate_1Mbps,
      CRC        => Enabled_2byte,
      Chanel     => 10,
      RX_IRQ     => True,
      TX_IRQ     => True,
      MAX_TR_IRQ => True);

   -- RX --
   RX_CE     : GPIO_Point renames STM32.Device.PC11;
   RX_CSN    : GPIO_Point renames STM32.Device.PC12;
   RX_SPI    : SPI_Port   renames STM32.Device.SPI_4;
   RX        : NRF24L01P_Driver;

   RX_Config : constant Configuration :=
     (Mode       => PRX,
      Power      => Power_Minimum,
      Rate       => Rate_1Mbps,
      CRC        => Enabled_2byte,
      Chanel     => 10,
      RX_IRQ     => True, --  Generate IRQ only when RX data arrived
      TX_IRQ     => False,
      MAX_TR_IRQ => False);

   Address : constant Pipe_Address := (1, 2, 3, 4, 5);
   --  Will be used for both TX/RX

   procedure Init_Pins;
   procedure Init_SPI;
   procedure On_RX_IRQ;

   ---------------
   -- Init_Pins --
   ---------------

   procedure Init_Pins is
      --  Initialize pins
   begin

      -- TX --
      STM32.Device.Enable_Clock (GPIO_Points'(TX_CE, TX_CSN));

      Configure_IO
        (Points => (TX_CE, TX_CSN),
         Config =>
           (Mode        => Mode_Out,
            Resistors   => Floating,
            Output_Type => Push_Pull,
            Speed       => Speed_Low));

      TX_CE.Clear;
      TX_CSN.Set;

      -- RX --
      STM32.Device.Enable_Clock (GPIO_Points'(RX_CE, RX_CSN, RX_IRQ));

      Configure_IO
        (Points => (RX_CE, RX_CSN),
         Config =>
           (Mode        => Mode_Out,
            Resistors   => Floating,
            Output_Type => Push_Pull,
            Speed       => Speed_Low));

      RX_CE.Clear;
      RX_CSN.Set;

      -- RX IRQ pin
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
      Enable_Clock (TX_SPI);

      Configure_IO (SPI4_Pins, Conf);

      Reset (TX_SPI);
      if not TX_SPI.Enabled then
         TX_SPI.Configure (SPI_Conf);
         TX_SPI.Enable;
      end if;
   end Init_SPI;

   Period       : constant Time_Span := Milliseconds (200);
   Next_Release : Time := Clock;
   BG           : constant Bitmap_Color := (Alpha => 255, others => 64);

   --------------
   -- On_Error --
   --------------

   procedure On_Error (Msg : String) is
   begin
      Power_Down (TX);
      Power_Down (RX);

      LCD_Std_Out.Put_Line (Msg);
      Display.Update_Layer (1, Copy_Back => True);

      loop
         STM32.Board.Toggle (Red_LED);
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end On_Error;

   ---------------
   -- On_RX_IRQ --
   ---------------

   Has_RX_IRQ : Boolean := False
     with Volatile, Atomic;
   --  Did we have RX IRQ

   procedure On_RX_IRQ is
      --  Simple IRQ callback
   begin
      if Is_Received (RX) then
         Has_RX_IRQ := True;
      end if;
   end On_RX_IRQ;

   -----------------
   -- Wait_For_RX --
   -----------------

   procedure Wait_For_RX is
      -- Waiting for data on the RX side
   begin
      loop
         if Has_RX_IRQ then
            Has_RX_IRQ := False;
            exit;
         end if;
         delay 0.1;
      end loop;
   end Wait_For_RX;

   -------------------
   -- Ckeck_RX_Data --
   -------------------

   procedure Ckeck_RX_Data
     (Driver : in out NRF24L01P_Driver;
      Value  : String)
     --  Checks data on the RX side
   is
      Pipe      : RX_Pipe;
      Have_More : Boolean;
      Data      : TX_RX_Data := Receive (Driver, Pipe, Have_More);
      Str       : String (Integer (Data'First) .. Integer (Data'Last))
        with Import, Address => Data'Address;
   begin
      LCD_Std_Out.Put_Line ("Get: '" & Str & "'");
      Display.Update_Layer (1, Copy_Back => True);

      if Pipe /= 0 then
         On_Error ("RX Pipe FAILS" & Pipe'Img);
      end if;

      if Have_More then
         On_Error ("RX Have_More FAILS");
      end if;

      if Str /= Value then
         On_Error ("RX DATA FAILS");
      end if;
   end Ckeck_RX_Data;

begin
   delay 2.0;

   --  Initialize GPIO/SPI for NRF
   Init_Pins;
   Init_SPI;
   delay 0.1;

   -- Init testing infrastructure
   STM32.Board.Initialize_LEDs;
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_8888);
   LCD_Std_Out.Set_Font (BMP_Fonts.Font12x12);
   LCD_Std_Out.Current_Background_Color := BG;
   Display.Hidden_Buffer (1).Set_Source (BG);
   Display.Hidden_Buffer (1).Fill;
   LCD_Std_Out.Clear_Screen;

   --  Static payload test --

   --  Initialize TX --
   Initialize
     (This    => TX,
      CE_Pin  => TX_CE'Access,
      CSN_Pin => TX_CSN'Access,
      SPI     => TX_SPI'Access,
      Config  => TX_Config);
   delay 0.1;

   Set_Max_Pipe_Address_Size (TX, 5);
   Set_TX_Address (TX, Address);

   Configure_And_Enable_RX_Pipe
     (This    => TX,
      Pipe    => 0,
      Addr    => Address,
      ACK     => False,
      Payload =>
        (Payload => Static_Payload,
         Size    => 5));

   Disable_RX_Pipe (TX, 1);
   Disable_RX_Pipe (TX, 2);
   Disable_RX_Pipe (TX, 3);
   Disable_RX_Pipe (TX, 4);
   Disable_RX_Pipe (TX, 5);

   Set_Features
     (This            => TX,
      Dynamic_Payload => False,
      ACK_Payload     => False,
      NO_ACK_Allowed  => False);

   --  Initialize RX --
   Initialize
     (This    => RX,
      CE_Pin  => RX_CE'Access,
      CSN_Pin => RX_CSN'Access,
      SPI     => RX_SPI'Access,
      Config  => RX_Config);
   delay 0.1;

   Set_Max_Pipe_Address_Size (RX, 5);

   Configure_And_Enable_RX_Pipe
     (This    => RX,
      Pipe    => 0,
      Addr    => Address,
      ACK     => False,
      Payload =>
        (Payload => Static_Payload,
         Size    => 5));

   Disable_RX_Pipe (RX, 1);
   Disable_RX_Pipe (RX, 2);
   Disable_RX_Pipe (RX, 3);
   Disable_RX_Pipe (RX, 4);
   Disable_RX_Pipe (RX, 5);

   Set_Features
     (This            => RX,
      Dynamic_Payload => False,
      ACK_Payload     => False,
      NO_ACK_Allowed  => False);
   delay 0.1;

   -- Turn on RX --
   Power_Up (RX);
   delay 0.0015;
   Start_Wating_RX (RX);

   -- Transmit over TX --
   Power_Up (TX);
   delay 0.0015;

   declare
      Data : constant String := "Hello";
      Buf  : TX_RX_Data
        (Payload_Length (Data'First) .. Payload_Length (Data'Last))
        with Import, Address => Data'Address;
   begin
      Transmit (TX, Buf);
      LCD_Std_Out.Put_Line ("Send: '" & Data & "'");
      Display.Update_Layer (1, Copy_Back => True);
   end;

   --  Work with RX side
   Wait_For_RX;
   Ckeck_RX_Data (RX, "Hello");
   Stand_By (RX);

   if not Is_Transmitted (TX) then
      On_Error ("TX_Data_Sent FAILS");
   end if;
   Stand_By (TX);
   Clear_Status (TX);

   Power_Down (TX);
   Power_Down (RX);
   delay 0.1;

   --  Dynamic payload test --

   --  TX
   Set_Features
     (This            => TX,
      Dynamic_Payload => True,
      ACK_Payload     => False,
      NO_ACK_Allowed  => False);

   Configure_And_Enable_RX_Pipe
     (This    => TX,
      Pipe    => 0,
      Addr    => Address,
      ACK     => True,
      Payload => (Payload => Dynamic_Payload));

   --  RX
   Set_Features
     (This            => RX,
      Dynamic_Payload => True,
      ACK_Payload     => False,
      NO_ACK_Allowed  => False);

   Configure_And_Enable_RX_Pipe
     (This    => RX,
      Pipe    => 0,
      Addr    => Address,
      ACK     => True,
      Payload => (Payload => Dynamic_Payload));

   --  Prepare RX
   Power_Up (RX);
   delay 0.0015;
   Start_Wating_RX (RX);

   --  Send over TX
   Power_Up (TX);
   delay 0.0015;

   declare
      Data : constant String := "Hello world";
      Buf  : TX_RX_Data
        (Payload_Length (Data'First) .. Payload_Length (Data'Last))
        with Import, Address => Data'Address;
   begin
      Transmit (TX, Buf);
      LCD_Std_Out.Put_Line ("Send: '" & Data & "'");
      Display.Update_Layer (1, Copy_Back => True);
   end;

   --  Check on RX side
   Wait_For_RX;
   Ckeck_RX_Data (RX, "Hello world");
   Stand_By (RX);

   if not Is_Transmitted (TX) then
      On_Error ("TX_Data_Sent FAILS");
   end if;
   Stand_By (TX);
   Clear_Status (TX);

   Power_Down (TX);
   Power_Down (RX);
   delay 0.1;

   -- ACK payload back test --

   --  TX
   Set_Features
     (This            => TX,
      Dynamic_Payload => True,
      ACK_Payload     => True,
      NO_ACK_Allowed  => False);

   Set_Auto_Retransmit (This => TX, Count => 5, A_Delay => 5);

   --  RX
   Set_Features
     (This            => RX,
      Dynamic_Payload => True,
      ACK_Payload     => True,
      NO_ACK_Allowed  => False);

   --  Prepare RX
   Power_Up (RX);
   delay 0.0015;
   Start_Wating_RX (RX);

   declare
      Data : constant String := "ACK back";
      Buf  : TX_RX_Data
        (Payload_Length (Data'First) .. Payload_Length (Data'Last))
        with Import, Address => Data'Address;
   begin
      Write_ACK_Payload (This => RX, Pipe => 0, Data => Buf);
   end;

   -- Send over TX
   Power_Up (TX);
   delay 0.0015;

   declare
      Data : constant String := "With ACK";
      Buf  : TX_RX_Data
        (Payload_Length (Data'First) .. Payload_Length (Data'Last))
        with Import, Address => Data'Address;
   begin
      Transmit (TX, Buf);
      LCD_Std_Out.Put_Line ("Send: '" & Data & "'");
      Display.Update_Layer (1, Copy_Back => True);
   end;

   --  Check on RX side
   Wait_For_RX;
   Ckeck_RX_Data (RX, "With ACK");
   Stand_By (RX);

   loop
      exit when Is_Transmitted (TX);
      delay 0.1;
   end loop;
   Ckeck_RX_Data (TX, "ACK back");
   Stand_By (TX);

   Power_Down (TX);
   Power_Down (RX);

   -- All is OK
   LCD_Std_Out.Put_Line ("All tests passed");
   Display.Update_Layer (1, Copy_Back => True);

   loop
      STM32.Board.Toggle (Green_LED);
      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end nRF24L01P_Example;
