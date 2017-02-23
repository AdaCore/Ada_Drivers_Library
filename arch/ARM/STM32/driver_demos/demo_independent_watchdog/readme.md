This program demonstrates the on-board "independent watchdog" provided by
the STM32F4 family.

The program first blinks all the LEDs on and off three times to signal
initial execution. It then blinks the green LED indefinitely, also
resetting the watchdog so that the board is not reset. If you see all the
LEDs blink after initial power-up you know that the board has been reset
by the watchdog. That makes it easy to experiment with the watchdog's
parameters.
