Digital Output Example
======================

In this example we will see how to control a pin as digital output by flashing
an on-board LED. This means that the voltage on the pin connected to the LED
will be either 0 volts or 3.3 volts.

Code
====

To control the IO pin we are going to use the procedure `Set` of the package
`NRF52_DK.IOs`.

```ada
   procedure Set (Pin : Pin_Id; Value : Boolean)
     with Pre => Supports (Pin, Digital);
```

Arguments:

 - Pin   : The id of the pin that we want to control as digital output
 - Value : A boolean that says if we want the pin to be high (True) or low
   (False)

Precondition:

The procedure `Set` has a precondition that the pin must support digital IO.

We also use the procedure `Delay_Ms` of the package `NRF52_DK.Time` to stop the
program for a short amount of time.

Here is the code:
```ada
with NRF52_DK.IOs;
with NRF52_DK.Time;

procedure Main is
begin

   --  Loop forever
   loop
      --  Turn on the LED connected to pin 17
      NRF52_DK.IOs.Set (17, True);

      --  Wait 500 milliseconds
      NRF52_DK.Time.Delay_Ms (500);

      --  Turn off the LED connected to pin 17
      NRF52_DK.IOs.Set (17, False);

      --  Wait 500 milliseconds
      NRF52_DK.Time.Delay_Ms (500);
   end loop;
end Main;

```
