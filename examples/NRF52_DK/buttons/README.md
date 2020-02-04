Buttons Example
===============

In this example we will see how to use the four buttons of the nRF52 Development Kit.

Code
====

To know if a button is pressed or not, we will use the procedure `State` of the
`NRF52_DK.Buttons` package.

```ada
   type Button_State is (Pressed, Released);

   type Button_Id is (Button_1, Button_2, Button_3, Button_4);

   function State (Button : Button_Id) return Button_State;
```

Arguments:

 - Button : The Id of the button that we want to check. There are four Ids:
            `Button_1`, `Button_2`, `Button_3`, and `Button_4` which correspond
	    to the silkscreen on the board.

Return value:

The procedure `State` return the `Button_State` that can be either `Pressed` or
`Released`.

Here is the code:
```ada
with NRF52_DK.Buttons; use NRF52_DK.Buttons;
with NRF52_DK.LEDs; use NRF52_DK.LEDs;
with NRF52_DK.Time;

procedure Main is
begin
   Initialize_LEDs;
   loop
      Turn_Off (LED1);
      Turn_Off (LED2);
      Turn_Off (LED3);
      Turn_Off (LED4);
      if NRF52_DK.Buttons.State (Button_1) = Pressed then
         Turn_On (LED1);
      end if;

      if NRF52_DK.Buttons.State (Button_2) = Pressed then
         Turn_On (LED2);
      end if;

      if NRF52_DK.Buttons.State (Button_3) = Pressed then
         Turn_On (LED3);
      end if;

      if NRF52_DK.Buttons.State (Button_4) = Pressed then
         Turn_On (LED4);
      end if;

      NRF52_DK.Time.Delay_Ms (200);
   end loop;
end Main;
```
