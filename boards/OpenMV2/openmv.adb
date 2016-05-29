with HAL.SPI;

package body OpenMV is

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (All_LEDs);

      Conf.Mode        := Mode_Out;
      Conf.Output_Type := Push_Pull;
      Conf.Speed       := Speed_100MHz;
      Conf.Resistors   := Floating;

      Configure_IO (All_LEDs, Conf);
   end Initialize_LEDs;

   -----------------
   -- Set_RGB_LED --
   -----------------

   procedure Set_RGB_LED (C : LED_Color) is
   begin
      --  Clear to turn on LED
      --  Set to turn off

      case C is
         when White | Red | Yellow | Magenta =>
            Clear (Red_LED);
         when others =>
            Set (Red_LED);
      end case;
      case C is
         when White | Green | Yellow | Cyan =>
            Clear (Green_LED);
         when others =>
            Set (Green_LED);
      end case;
      case C is
         when White | Cyan | Blue | Magenta =>
            Clear (Blue_LED);
         when others =>
            Set (Blue_LED);
      end case;
   end Set_RGB_LED;

   ----------------
   -- Turn_On_IR --
   ----------------

   procedure Turn_On_IR is
   begin
      Set (IR_LED);
   end Turn_On_IR;

   -----------------
   -- Turn_Off_IR --
   -----------------

   procedure Turn_Off_IR is
   begin
      Clear (IR_LED);
   end Turn_Off_IR;

   ---------------------------
   -- Initialize_Shield_SPI --
   ---------------------------

   procedure Initialize_Shield_SPI is
      SPI_Conf  : STM32.SPI.SPI_Configuration;
      GPIO_Conf : STM32.GPIO.GPIO_Port_Configuration;
   begin
      STM32.Device.Enable_Clock (Shield_SPI_Points);

      GPIO_Conf.Speed       := STM32.GPIO.Speed_100MHz;
      GPIO_Conf.Mode        := STM32.GPIO.Mode_AF;
      GPIO_Conf.Output_Type := STM32.GPIO.Push_Pull;
      GPIO_Conf.Resistors   := STM32.GPIO.Pull_Down; --  SPI low polarity

      STM32.GPIO.Configure_IO (Shield_SPI_Points, GPIO_Conf);

      STM32.GPIO.Configure_Alternate_Function
        (Shield_SPI_Points,
         STM32.GPIO.GPIO_AF_SPI2);

      STM32.Device.Enable_Clock (Shield_SPI);

      STM32.SPI.Disable (Shield_SPI);

      SPI_Conf.Direction           := STM32.SPI.D2Lines_FullDuplex;
      SPI_Conf.Mode                := STM32.SPI.Master;
      SPI_Conf.Data_Size           := HAL.SPI.Data_Size_8b;
      SPI_Conf.Clock_Polarity      := STM32.SPI.Low;
      SPI_Conf.Clock_Phase         := STM32.SPI.P1Edge;
      SPI_Conf.Slave_Management    := STM32.SPI.Software_Managed;
      SPI_Conf.Baud_Rate_Prescaler := STM32.SPI.BRP_2;
      SPI_Conf.First_Bit           := STM32.SPI.MSB;
      SPI_Conf.CRC_Poly            := 7;

      STM32.SPI.Configure (Shield_SPI, SPI_Conf);

      STM32.SPI.Enable (Shield_SPI);
   end Initialize_Shield_SPI;

end OpenMV;
