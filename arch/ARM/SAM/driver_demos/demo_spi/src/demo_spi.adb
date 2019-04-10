with SAM.Device;

with HAL; use HAL;
with HAL.GPIO;
with HAL.SPI; use HAL.SPI;

with SAM.GPIO;
with SAM.SPI;

with Ada.Real_Time; use Ada.Real_Time;

procedure Demo_Spi
is

   LED0 : SAM.GPIO.GPIO_Point renames SAM.Device.PA23;
   LED1 : SAM.GPIO.GPIO_Point renames SAM.Device.PC9;

   Port : SAM.SPI.SPI_Port renames SAM.Device.SPI_0_Cs1;

   MISO : SAM.GPIO.GPIO_Point renames SAM.Device.PD20;
   MOSI : SAM.GPIO.GPIO_Point renames SAM.Device.PD21;
   SPCK : SAM.GPIO.GPIO_Point renames SAM.Device.PD22;
   NPCS1 : SAM.GPIO.GPIO_Point renames SAM.Device.PD25;

   Data_Stream : HAL.SPI.SPI_Data_8b (1 .. 8);
   Status : HAL.SPI.SPI_Status;

   Counter : UInt8 := 0;

   Cfg     : SAM.SPI.Configuration := (Baud           => 25_000_000,
                                       Tx_On_Rx_Empty => False,
                                       others         => <>);

   procedure Wait (Period : Time_Span)
   is
   begin
      delay until (Period + Clock);
   end Wait;

   procedure Initialize_LEDs is
      use SAM.GPIO;
   begin
      Enable_Clock (SAM.Device.GPIO_A);
      Enable_Clock (SAM.Device.GPIO_B);

      LED0.Configure_IO
        (Config => GPIO_Port_Configuration'(Mode        => Mode_Out,
                                            Resistors   => <>,
                                            Output_Type => Open_Drain));
      LED1.Configure_IO
        (Config => GPIO_Port_Configuration'(Mode        => Mode_Out,
                                            Resistors   => <>,
                                            Output_Type => Open_Drain));
   end Initialize_LEDs;

   procedure Initialize_SPI is
      use SAM.GPIO;
   begin
      MISO.Configure_IO (Config => (Mode => Mode_AF,
                                    Resistors      => <>,
                                    AF_Output_Type => Push_Pull,
                                    AF             => Periph_B));
      MOSI.Configure_IO (Config => (Mode => Mode_AF,
                                    Resistors      => <>,
                                    AF_Output_Type => Push_Pull,
                                    AF             => Periph_B));
      SPCK.Configure_IO (Config => (Mode => Mode_AF,
                                    Resistors      => <>,
                                    AF_Output_Type => Push_Pull,
                                    AF             => Periph_B));
      NPCS1.Configure_IO (Config => (Mode => Mode_AF,
                                     Resistors      => <>,
                                     AF_Output_Type => Push_Pull,
                                     AF             => Periph_B));

      SAM.SPI.Configure (This => Port,
                         Cfg  => Cfg);
   end Initialize_SPI;
begin

   Initialize_LEDs;
   Initialize_SPI;
   loop
      LED0.Set;
      LED1.Set;
      Wait (Period => Milliseconds (1000));
      LED0.Clear;
      LED1.Clear;

      for I in Data_Stream'Range loop
         Data_Stream (I) := Counter;
         Counter := Counter + 1;
      end loop;

      SAM.SPI.Transmit (This    => Port,
                        Data    => Data_Stream,
                        Status  => Status);

      if Status /= HAL.SPI.Ok then
         LED0.Set;
      else
         LED1.Set;
      end if;

      Wait (Period => Milliseconds (1000));
   end loop;
end Demo_SPI;
