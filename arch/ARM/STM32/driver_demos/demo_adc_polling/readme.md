This demonstration illustrates reading the values from an ADC unit
using polling.

There are four separate main programs in this demonstration. These
programs are set up to run on the STM32F429 Discovery boards because
they use the LCD to display results. This display usage is entirely for
convenience -- some other means for showing the results could be used,
such as a serial port.

In all programs an error in reading from the ADC raises an exception,
invoking the last chance handler.

1) The first program ("demo_adc_gpio_polling.adb") demonstrates reading an
externally-supplied analog voltage value on a GPIO pin with an ADC unit,
using polling. The pin is continously polled in an infinite loop,
displaying the current sample on each iteration. Connect the pin to an
appropriate external input voltage to drive the displayed value. Absent
an input, the sensed (and displayed) value will be meaningless. The
green LED will toggle on each iteration, as an additional indication of
execution.

NB: The input voltage must not exceed the maximum allowed for the board's
circuitry!  A value between zero and three volts (inclusive) is safe.

The displayed value is the raw sample quantity in the range 0 .. 4095,
representing an input voltage of 0.0 to 3.0 volts. Thus, for an
incoming voltage of 1.5 volts, the sample would be approximately half of
4095, i.e., 2048.

2) The second program ("demo_adc_temperature_polling.adb") demonstrates
reading the internal temperature sensor value from an ADC unit. This
temperature can be used to calibrate the ADC results because those
results vary with the internal device temperature. Note that this is not
an external temperature (ambient air) reading. Both raw (sensed) and
scaled values are displayed.

3) The third program ("demo_adc_vbat_polling.adb") demonstrates reading
the VBat (battery voltage) value from an ADC unit. See the reference
manual for VBat details. Both raw (sensed) and scaled values are
displayed.

4) The fourth program ("demo_adc_vref_polling.adb") demonstrates reading
the VRef (reference voltage) value from an ADC unit. See the reference
manual for VRef details. Both raw (sensed) and scaled values are
displayed.
