This is an Ada-language driver for the STMicroelectronics [VL53L1X](https://www.st.com/en/imaging-and-photonics-solutions/vl53l1x.html) ranging sensor.

It's based on STMicroelectronics's STSW-IMG009 C-language driver.

## Usage ##

The essentials are:

* configure your I2C and GPIO pins, specifically enabling interrupts on the GPIO1 pin (default, interrupt on rising edge).
* call `Boot_Device`. This waits for a period to allow the VL53L1X to boot, then checks that the device is ready.
* call `Sensor_Init`.
* call `Start_Ranging`.
* loop, waiting for interrupts; on interrupt,
  * call `Get_Measurement`
  * call `Clear_Interrupt` (note, this is aside from the MCU's GPIO interrupt processing: it clears the interrupt indication on GPIO1 and restarts the device's ranging process).

If you don't want to use the device's interrupt-generating feature, you can do a "busy" loop using `Wait_For_Measurement` (whose `Loop_Interval_Ms` parameter controls the delay between each check).

Other controls provided are:

* `Set_Device_Address` allows to change the I2C address from the default (`16#52#`). This is useful if you have more than one VL53L1X connected: disable all the devices' I2C communications by clearing their XSHUT pins, then enable them one-by-one setting distinct addresses.
  **N.B.** the device address remains as set until power is cycled!
* `Set_Distance_Mode`: the choices are `Long` (default) and `Short`. `Short` mode has better ambient light immunity but the maximum distance measurable is limited to 1.3 m. `Long` distance mode ranges up to 4 m but is less performant under ambient light.
* `Set_Inter_Measurement_Time`: the inter-measurement period is the time between two ranging operations. Should be no shorter than the timing budget.
* `Set_Measurement_Budget`: the timing budget is the time required by the sensor to make one distance measurement. The values permitted are 15 (only in `Short` distance mode), 20, 33, 50, 100, 200 or 500 ms.

## Exceptions ##

Except in `Boot_Device`, exceptions are raised for I2C errors, largely on the grounds that it's not obvious how to recover from them.
