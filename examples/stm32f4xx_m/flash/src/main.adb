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

--  In this example we erase and flash part of the W25Q16 flash memory
--  when K1 button is pressed on reset. If the button is not pressed, the
--  program reads the flash memory and checks its content. If everything is
--  OK, the program blinks the green LED slowly (1 Hz), but otherwise it blinks
--  the LED faster (5 Hz).

with Ada.Real_Time;

with HAL.SPI;
with HAL.Flash;

with STM32.Board;
with STM32.GPIO;
with STM32.SPI;
with STM32.Device;

with W25Q16;

procedure Main is
   use type Ada.Real_Time.Time;

   procedure Write_Flash
     (Flash : in out W25Q16.Flash_Memory;
      Size  : Positive;
      Ok    : out Boolean);
   --  Read part of flash and check the content with expected values

   procedure Check_Flash
     (Flash : in out W25Q16.Flash_Memory;
      Size  : Positive;
      Ok    : out Boolean);
   --  Erase and write part of flash with some pattern

   -----------------
   -- Check_Flash --
   -----------------

   procedure Check_Flash
     (Flash : in out W25Q16.Flash_Memory;
      Size  : Positive;
      Ok    : out Boolean)
   is
      From : Natural := 0;
   begin
      Ok := True;

      while Ok and From < Size loop
         declare
            Ints  : array (1 .. 64) of Natural;
            Bytes : HAL.UInt8_Array (1 .. 256)
               with Import, Address => Ints'Address;
         begin
            Flash.Read (From, Bytes, Ok);

            for J in Ints'Range loop
               Ok := Ok and (Ints (J) = From + (J - 1) * 4);
            end loop;

            From := From + Bytes'Length;
         end;
      end loop;
   end Check_Flash;

   -----------------
   -- Write_Flash --
   -----------------

   procedure Write_Flash
     (Flash : in out W25Q16.Flash_Memory;
      Size  : Positive;
      Ok    : out Boolean)
   is
      Region : constant HAL.Flash.Memory_Region :=
        Flash.Erasable_Region ((From => 0, To => Size - 1));

      From : Natural := 0;
   begin
      Flash.Erase (Region, Ok);

      while Ok and From < Size loop
         declare
            Ints  : array (1 .. 64) of Natural;
            Bytes : HAL.UInt8_Array (1 .. 256)
               with Import, Address => Ints'Address;
         begin
            for J in Ints'Range loop
               Ints (J) := From + (J - 1) * 4;
            end loop;

            while Flash.Is_Busy loop
               delay until Ada.Real_Time.Clock
                  + Ada.Real_Time.Microseconds (1);
            end loop;

            Flash.Write (From, Bytes, Ok);
            From := From + Bytes'Length;
         end;
      end loop;
   end Write_Flash;

   Button : STM32.GPIO.GPIO_Point renames STM32.Board.User_Button_Point;

   Next : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   Span : Ada.Real_Time.Time_Span;

   SPI : STM32.SPI.SPI_Port renames STM32.Device.SPI_1;
   SPI_SCK  : STM32.GPIO.GPIO_Point renames STM32.Device.PB3;
   SPI_MISO : STM32.GPIO.GPIO_Point renames STM32.Device.PB4;
   SPI_MOSI : STM32.GPIO.GPIO_Point renames STM32.Device.PB5;
   SPI_CS   : STM32.GPIO.GPIO_Point renames STM32.Device.PA15;

   SPI_Pins : constant STM32.GPIO.GPIO_Points :=
     (SPI_SCK, SPI_MISO, SPI_MOSI);

   Flash : W25Q16.Flash_Memory
     (SPI'Unchecked_Access, SPI_CS'Unchecked_Access);

   Ok    : Boolean := False;
begin
   STM32.Board.Initialize_LEDs;
   STM32.Board.Configure_User_Button_GPIO;
   STM32.Device.Enable_Clock (SPI_Pins);

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

   Flash.Check_JEDEC_ID (Ok);

   if Ok then
      STM32.Board.Turn_On (STM32.Board.Green_LED);
   end if;

   if Button.Set then
      Write_Flash (Flash, 4096, Ok);
   else
      Check_Flash (Flash, 4096, Ok);
   end if;

   Span := Ada.Real_Time.Milliseconds (if Ok then 500 else 100);
   --  Blink slow or fast depending on Ok status

   loop
      STM32.Board.Toggle (STM32.Board.Green_LED);
      Next := Next + Span;
      delay until Next;
   end loop;
end Main;
