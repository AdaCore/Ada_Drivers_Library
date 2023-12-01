# XPT2046

The XPT2046 is a 4-wire resistive touch screen controller that incorporates
a 12-bit 125 kHz sampling SAR type A/D converter. Many cheap LCD displays
contain this controller. Typically, it is connected via SPI.

To align the touch point with the display coordinates, it might be
necessary to calibrate the touch screen. The XPT2046 sensor provides
raw values within the range of 0 to 4095. If you identify the raw
values associated with the screen edges, you can perform touch
panel calibration by invoking the Calibrate procedure.

```ada
subtype Sensor_Value is HAL.UInt16 range 0 .. 2 ** 12 - 1;

procedure Calibrate
  (This  : in out XPT2046_Device'Class;
   Min_X : Sensor_Value;
   Max_X : Sensor_Value;
   Min_Y : Sensor_Value;
   Max_Y : Sensor_Value);
```

* [Datasheet](https://www.datasheet-pdf.info/attach/1/3898350023.pdf)
* [Example for STM32 F4VE board](../../../../examples/stm32_f4ve/lcd)
