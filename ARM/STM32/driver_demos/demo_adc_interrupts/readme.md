This program demonstrates reading the VBat (battery voltage) value from
an ADC unit, using interrupts.

This program is expected to run on the STM32F429 Discovery boards because
it uses the LCD to display results.  This display usage is entirely for
convenience -- some other means for showing the results could be used, such
as a serial port, or some other board with a different display.

Both raw (sensed) and scaled values are continuously displayed.