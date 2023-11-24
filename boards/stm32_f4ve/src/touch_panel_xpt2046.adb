------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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
with STM32.Board;
with STM32.GPIO;
with STM32.SPI;

package body Touch_Panel_XPT2046 is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This        : in out Touch_Panel;
      Orientation : HAL.Framebuffer.Display_Orientation :=
        HAL.Framebuffer.Default)
   is
      SPI_Pins : constant STM32.GPIO.GPIO_Points :=
        (STM32.Board.SPI2_SCK,
         STM32.Board.SPI2_MISO,
         STM32.Board.SPI2_MOSI);
   begin
      STM32.Device.Enable_Clock (STM32.Board.TFT_RS);
      STM32.Device.Enable_Clock (STM32.Board.TFT_CS);
      STM32.Device.Enable_Clock (SPI_Pins);
      STM32.Device.Enable_Clock (STM32.Device.SPI_2);

      STM32.Board.TFT_RS.Configure_IO  --  Pen IRQ pin
        ((Mode        => STM32.GPIO.Mode_In,
          Resistors   => STM32.GPIO.Floating));

      STM32.Board.TFT_CS.Configure_IO
        ((Mode        => STM32.GPIO.Mode_Out,
          Resistors   => STM32.GPIO.Floating,
          Output_Type => STM32.GPIO.Push_Pull,
          Speed       => STM32.GPIO.Speed_100MHz));

      STM32.GPIO.Configure_IO
        (SPI_Pins,
         (Mode           => STM32.GPIO.Mode_AF,
          Resistors      => STM32.GPIO.Pull_Up,
          AF_Output_Type => STM32.GPIO.Push_Pull,
          AF_Speed       => STM32.GPIO.Speed_100MHz,
          AF             => STM32.Device.GPIO_AF_SPI2_5));

      STM32.SPI.Configure
        (STM32.Device.SPI_2,
         (Direction           => STM32.SPI.D2Lines_FullDuplex,
          Mode                => STM32.SPI.Master,
          Data_Size           => HAL.SPI.Data_Size_8b,
          Clock_Polarity      => STM32.SPI.Low,  -- Mode 0
          Clock_Phase         => STM32.SPI.P1Edge,
          Slave_Management    => STM32.SPI.Software_Managed,
          Baud_Rate_Prescaler => STM32.SPI.BRP_32,
          First_Bit           => STM32.SPI.MSB,
          CRC_Poly            => 0));
      --  SPI2 sits on APB1, which is 42MHz, so SPI rate in 42/32=1.3MHz

      This.Set_Orientation (Orientation);

      This.Calibrate
        (Min_X => 300,
         Max_X => 3768,
         Min_Y => 140,
         Max_Y => 3813);
   end Initialize;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (This        : in out Touch_Panel;
      Orientation : HAL.Framebuffer.Display_Orientation) is
   begin
      case Orientation is
         when HAL.Framebuffer.Default | HAL.Framebuffer.Portrait =>
            This.Set_Bounds (240, 320, HAL.Touch_Panel.Invert_Y);
         when HAL.Framebuffer.Landscape =>
            This.Set_Bounds (240, 320, HAL.Touch_Panel.Swap_XY);
      end case;
   end Set_Orientation;

end Touch_Panel_XPT2046;
