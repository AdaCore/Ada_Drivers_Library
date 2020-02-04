with nRF.Device; use NRF.Device;
with nRF.GPIO; use NRF.GPIO;
  
package NRF52_DK.LEDs is

   subtype User_LED is GPIO_Point;
   LED1 : User_LED renames P17;
   LED2 : User_LED renames P18;
   LED3 : User_LED renames P19;
   LED4 : User_LED renames P20;
   
   procedure Initialize_LEDs;
   
   procedure Turn_On( This : in out User_LED);
   procedure Turn_Off( This : in out User_LED);

end NRF52_DK.LEDS;
