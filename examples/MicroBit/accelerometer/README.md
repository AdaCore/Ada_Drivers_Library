Accelerometer Example
=====================

In this example we will see how to use the accelerometer of the micro:bit. The
accelerometer can, for instance, be used to know which way the micro:bit is
oriented.

Code
====

To get the acceleration value for all axes, we will just call the function
`MicroBit.Accelerometer.Data`. This function returns a record with `X`, `Y`
and `Z` field giving the value for each axis.


```ada
declare

   Data : MMA8653.All_Axes_Data;
   --  A variable to store the accelerometer data
begin

   --  Read Accelerometer data
   Data := Accelerometer.Data;
end;
```

We can then use the value in the record to get some information about the
orientation of the micro:bit. For example, if the Y value is below `-200`
the micro:bit is vertical.

Note that the type used to store the values of the accelerometer is declared in
the package `MMA8653` (the driver), so we have to `with` and `use` this package
to be have acces to the operations for this type.

```ada
if Data.Y < -200 then
   --  The micro:bit is vertical
end if;
```
