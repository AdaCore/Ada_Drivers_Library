Analog Output Example
=====================

In this example we will see how to write an analog value to a pin. The
micro:bit doesn't have a real digital to analog converter, so the analog signal
is actually a Pulse Width Modulation (PWM).

This is good enough to control the speed of a motor or the brightness of an LED.

There is a limit of three analog (PWM) signals on the micro:bit, if you try to
write an analog value to more than three pins an exception will be raised.

Wiring Diagram
==============

We use the same circuit as the [Digital Out](../digital_out) example.

Extra hardware:

 - A breadboard
 - An LED
 - A 47k ohm resistor

Wiring:

<img src="../doc/fritzing_sketches/screenshots/digital_out.png" width="300">

Code
====

To write an analog value to the IO pin we are going to use the procedure `Write`
of the package `MicroBit.IOs`.

```ada
   procedure Write (Pin : Pin_Id; Value : Analog_Value)
     with Pre => Supports (Pin, Analog);
```

Arguments:

 - Pin   : The id of the pin that we want to read as digital input
 - Value : The analog value for the pin, between 0 and 1023

Precondition:

The procedure `Write` has a precondition that the pin must support analog IO.

In the code, we are going to write an loop with a value that goes from 0 to 128
and set write this value to pin 0. We could go from 0 to 1023 but since the LED
doesn't get brighter after 128, there is no need to go beyond that value.

We also use the procedure `Delay_Ms` of the package `MicroBit.Time` to stop the
program for a short amount of time.

Here is the code:
```ada
with MicroBit.IOs;
with MicroBit.Time;

procedure Main is
begin

   --  Loop forever
   loop

      --  Loop for value between 0 and 128
      for Value in MicroBit.IOs.Analog_Value range 0 .. 128 loop

         --  Write the analog value of pin 0
         MicroBit.IOs.Write (0, Value);

         --  Wait 20 milliseconds
         MicroBit.Time.Delay_Ms (20);
      end loop;
   end loop;
end Main;
```
