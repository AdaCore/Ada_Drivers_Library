------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2024, AdaCore                         --
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

with STM32.GPIO;
with STM32.SPI;

with Ravenscar_Time;

package body Display_ILI9341 is

   use type STM32.GPIO.GPIO_Points;

   procedure Initialize_SPI;

   --------------------
   -- Initialize_SPI --
   --------------------

   procedure Initialize_SPI is

      SPI      : STM32.SPI.SPI_Port renames STM32.Device.SPI_1;

      SPI_CS   : STM32.GPIO.GPIO_Point renames STM32.Device.PA4;
      SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PA5;
      SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PA6;
      SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PA7;

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

      SPI_CS.Set;

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
          Clock_Polarity      => STM32.SPI.Low,   --   Mode 0
          Clock_Phase         => STM32.SPI.P1Edge,
          Slave_Management    => STM32.SPI.Software_Managed,
          Baud_Rate_Prescaler => STM32.SPI.BRP_16,
          First_Bit           => STM32.SPI.MSB,
          CRC_Poly            => 0));
      --  SPI1 sits on APB2, which is 84MHz, so SPI rate in 84/16=5.25MHz
   end Initialize_SPI;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Display) is
      TFT_Reset : STM32.GPIO.GPIO_Point := STM32.Device.PA1;
      --  Reset pin
      TFT_WRX : STM32.GPIO.GPIO_Point := STM32.Device.PA2;
      --  D/C pin
      TFT_BLK : STM32.GPIO.GPIO_Point := STM32.Device.PA3;
      --  STM32.Board.TFT_BLK
      TFT_Pins : constant STM32.GPIO.GPIO_Points :=
        (TFT_Reset, TFT_WRX, TFT_BLK);
   begin
      STM32.Device.Enable_Clock (TFT_Pins);

      STM32.GPIO.Configure_IO
        (TFT_Pins,
         ((STM32.GPIO.Mode_Out,
          Resistors   => STM32.GPIO.Floating,
          Output_Type => STM32.GPIO.Push_Pull,
          Speed       => STM32.GPIO.Speed_100MHz)));

      TFT_Reset.Clear;  --  Reset
      Ravenscar_Time.Delays.Delay_Microseconds (11);
      TFT_Reset.Set;
      Ravenscar_Time.Delays.Delay_Milliseconds (121);

      TFT_BLK.Set;  --  Turn LCD backlight on
      TFT_WRX.Clear;

      Initialize_SPI;

      This.Device.Initialize (Ravenscar_Time.Delays);
      This.Set_Orientation (HAL.Framebuffer.Default);
   end Initialize;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (This        : in out Display;
      Orientation : HAL.Framebuffer.Display_Orientation) is
   begin
      This.Device.Set_Orientation
        (case Orientation is
            when HAL.Framebuffer.Landscape =>
              ILI9341_Device.Landscape_1,
            when others => ILI9341_Device.Portrait_2);
   end Set_Orientation;

end Display_ILI9341;
