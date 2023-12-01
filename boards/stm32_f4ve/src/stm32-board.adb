------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2023, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with HAL.SPI;
with STM32.SPI;

package body STM32.Board is

   ------------------
   -- All_LEDs_Off --
   ------------------

   procedure All_LEDs_Off is
   begin
      Set (All_LEDs);
   end All_LEDs_Off;

   -----------------
   -- All_LEDs_On --
   -----------------

   procedure All_LEDs_On is
   begin
      Clear (All_LEDs);
   end All_LEDs_On;

   --------------------------------
   -- Configure_User_Button_GPIO --
   --------------------------------

   procedure Configure_User_Button_GPIO is
   begin
      Enable_Clock (All_Buttons);
      Configure_IO (All_Buttons, (Mode_In, Resistors => Floating));
   end Configure_User_Button_GPIO;

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
   begin
      Enable_Clock (All_LEDs);

      Configure_IO
        (All_LEDs,
         (Mode_Out,
          Resistors   => Floating,
          Output_Type => Push_Pull,
          Speed       => Speed_100MHz));

      All_LEDs_Off;
   end Initialize_LEDs;

   -----------------------------
   -- Initialize_Flash_Memory --
   -----------------------------

   procedure Initialize_Flash_Memory is
      SPI      : STM32.SPI.SPI_Port renames STM32.Device.SPI_1;

      SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PB3;
      SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PB4;
      SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PB5;
      SPI_CS   : STM32.GPIO.GPIO_Point renames STM32.Device.PA15;

      SPI_Pins : constant STM32.GPIO.GPIO_Points :=
        (SPI_SCK, SPI_MISO, SPI_MOSI);
   begin
      STM32.Device.Enable_Clock (SPI_Pins & SPI_CS);

      STM32.GPIO.Configure_IO
        (SPI_CS,
         (Mode        => STM32.GPIO.Mode_Out,
          Resistors   => STM32.GPIO.Floating,
          Output_Type => STM32.GPIO.Push_Pull,
          Speed       => STM32.GPIO.Speed_100MHz));

      STM32.GPIO.Configure_IO
        (SPI_Pins,
         (Mode           => STM32.GPIO.Mode_AF,
          Resistors      => STM32.GPIO.Pull_Up,
          AF_Output_Type => STM32.GPIO.Push_Pull,
          AF_Speed       => STM32.GPIO.Speed_100MHz,
          AF             => STM32.Device.GPIO_AF_SPI1_5));

      STM32.Device.Enable_Clock (SPI);

      STM32.SPI.Configure
        (SPI,
         (Direction           => STM32.SPI.D2Lines_FullDuplex,
          Mode                => STM32.SPI.Master,
          Data_Size           => HAL.SPI.Data_Size_8b,
          Clock_Polarity      => STM32.SPI.High,   --   Mode 3
          Clock_Phase         => STM32.SPI.P2Edge,
          Slave_Management    => STM32.SPI.Software_Managed,
          Baud_Rate_Prescaler => STM32.SPI.BRP_2,
          First_Bit           => STM32.SPI.MSB,
          CRC_Poly            => 0));
      --  SPI1 sits on APB2, which is 84MHz, so SPI rate in 84/2=42MHz
   end Initialize_Flash_Memory;

   ---------------------
   -- Initialize_FSMC --
   ---------------------

   procedure Initialize_FSMC (Pins : GPIO_Points) is
   begin
      Enable_FSMC_Clock;
      Enable_Clock (Pins);

      STM32.GPIO.Configure_IO
        (Pins,
         (STM32.GPIO.Mode_AF,
          Resistors      => STM32.GPIO.Floating,
          AF_Output_Type => STM32.GPIO.Push_Pull,
          AF_Speed       => STM32.GPIO.Speed_100MHz,
          AF             => STM32.Device.GPIO_AF_FMC_12));
   end Initialize_FSMC;

   ---------------------
   -- Initialize_UART --
   ---------------------

   procedure Initialize_UART
     (Speed  : STM32.USARTs.Baud_Rates := 115_200;
      Stop   : STM32.USARTs.Stop_Bits := STM32.USARTs.Stopbits_1;
      Parity : STM32.USARTs.Parities := STM32.USARTs.No_Parity;
      Flow   : STM32.USARTs.Flow_Control := STM32.USARTs.No_Flow_Control)
   is
      TX_Pin : GPIO_Point renames STM32.Device.PA9;
      RX_Pin : GPIO_Point renames STM32.Device.PA10;
   begin
      Enable_Clock (UART);
      Enable_Clock (RX_Pin & TX_Pin);

      Configure_IO
        (TX_Pin & RX_Pin,
         (Mode           => Mode_AF,
          AF             => GPIO_AF_USART1_7,
          Resistors      => Pull_Up,
          AF_Speed       => Speed_50MHz,
          AF_Output_Type => Push_Pull));

      UART.Disable;

      UART.Set_Baud_Rate    (Speed);
      UART.Set_Mode         (STM32.USARTs.Tx_Rx_Mode);
      UART.Set_Stop_Bits    (Stop);
      UART.Set_Word_Length  (STM32.USARTs.Word_Length_8);
      UART.Set_Parity       (Parity);
      UART.Set_Flow_Control (Flow);

      UART.Enable;
   end Initialize_UART;

end STM32.Board;
