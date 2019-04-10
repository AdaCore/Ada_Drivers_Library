This program demonstrates using the GPIO driver and interrupt
configuration to blink the four LEDs on the STM32F4 Discovery board.

The LEDs blink in a circular pattern, initially rotating clockwise. When
the blue user button is pressed the rotation direction changes. Button
presses generate interrupts that signal state changes that can then be
queried by application code.
