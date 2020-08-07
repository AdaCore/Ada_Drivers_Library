Buttons Example
===============

In this example we will see how to use the two buttons of the micro:bit.

Code
====

To know if a button is pressed or not, we will use the function `State` of the
`MicroBit.Buttons` package.

```ada
   type Button_State is (Pressed, Released);

   type Button_Id is (Button_A, Button_B);

   function State (Button : Button_Id) return Button_State;
```

Arguments:

 - Button : The Id of the button that we want to check. There are two Ids:
            `Button_A` or `Button_B`.

Return value:

The function `State` return the `Button_State` that can be either `Pressed` or
`Released`.

Here is the code:
```ada
with MicroBit.Display;
with MicroBit.Buttons; use MicroBit.Buttons;
with MicroBit.Time;

procedure Main is
begin

   loop
      MicroBit.Display.Clear;

      if MicroBit.Buttons.State (Button_A) = Pressed then
         --  If button A is pressed

         --  Display the letter A
         MicroBit.Display.Display ('A');

      elsif MicroBit.Buttons.State (Button_B) = Pressed then
         --  If button B is pressed

         --  Display the letter B
         MicroBit.Display.Display ('B');
      end if;

      MicroBit.Time.Delay_Ms (200);
   end loop;
end Main;
```
