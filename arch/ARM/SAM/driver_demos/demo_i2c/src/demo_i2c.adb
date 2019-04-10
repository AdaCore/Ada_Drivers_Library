with SAM.Device;  use SAM.Device;

with HAL; use HAL;
with HAL.GPIO;
with HAL.SPI; use HAL.SPI;

with SAM.GPIO; use SAM.GPIO;
with SAM.SPI;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

procedure Demo_I2C
is
   Port : SAM.SPI.SPI_Port renames SPI_0_Cs1;

   MISO : SAM.GPIO.GPIO_Point renames PD20;
   MOSI : SAM.GPIO.GPIO_Point renames PD21;
   SPCK : SAM.GPIO.GPIO_Point renames PD22;
   NPCS1 : SAM.GPIO.GPIO_Point renames PD25;

   Data_Stream : HAL.SPI.SPI_Data_8b (1 .. 8);
   Status : HAL.SPI.SPI_Status;

   Counter : UInt8 := 0;

   Cfg     : SAM.SPI.Configuration := (Baud   => 250_000,
                                       others => <>);

   procedure Wait (Period : Time_Span)
   is
   begin
      delay until (Period + Clock);
   end Wait;
begin
   Put_Line ("Setting up...");
   Enable_Clock (GPIO_D);
   MISO.Configure_IO (Config => (Mode => Mode_AF,
                                 Resistors => <>,
                                 AF_Output_Type => Open_Drain,
                                 AF             => Periph_B));
   MOSI.Configure_IO (Config => (Mode => Mode_AF,
                                 Resistors => <>,
                                 AF_Output_Type => Open_Drain,
                                 AF             => Periph_B));
   SPCK.Configure_IO (Config => (Mode => Mode_AF,
                                 Resistors => <>,
                                 AF_Output_Type => Open_Drain,
                                 AF             => Periph_B));
   NPCS1.Configure_IO (Config => (Mode => Mode_AF,
                                 Resistors => <>,
                                 AF_Output_Type => Open_Drain,
                                 AF             => Periph_B));

   SAM.SPI.Configure (This => Port,
                      Cfg  => Cfg);
   Enable_Clock (This => Port);

   loop
      Put ("Sending: ");
      for I in Data_Stream'Range loop
         Data_Stream (I) := Counter;
         Put (Counter'Img);
         Counter := Counter + 1;
      end loop;

      SAM.SPI.Transmit (This    => Port,
                        Data    => Data_Stream,
                        Status  => Status);
      New_Line;

      if Status /= HAL.SPI.Ok then
         Put_Line ("There was an error transmitting.");
         return;
      end if;

      Wait (Period => Milliseconds (1000));
   end loop;
end Demo_I2C;
