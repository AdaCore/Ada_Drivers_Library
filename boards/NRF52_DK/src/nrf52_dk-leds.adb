package body NRF52_DK.LEDs is
   
   LEDs : array(1 .. 4) of GPIO_Point := (LED1, LED2, LED3, LED4);

   procedure Initialize_LEDs is
      Conf : GPIO_Configuration;
   begin
      Conf.Mode := Mode_Out;
      Conf.Resistors := No_Pull;
      for LED of LEDs loop
         LED.Configure_IO(Conf);
         LED.Set;
      end loop;
   end Initialize_LEDs;
     
   procedure Turn_On( This : in out User_LED) is
   begin
      This.Clear;
   end Turn_On;
   
   procedure Turn_Off( This : in out User_LED) is
   begin
      This.Set;
   end Turn_Off;

end NRF52_DK.LEDs;
