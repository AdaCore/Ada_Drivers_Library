These projects represent specific implementations, "boards" such as the
`*_Discovery` kit boards.

They include the peripheral declarations for their specific MCU device, such as
the GPIO ports and timers on the F405 or F429, as well as declarations specific
to that board, such as the LEDs.

Use one of these project if that is the specific target you are using.

Alternatively, if you have some platform that is based on a given MCU device
but is not a specific board (e.g., not one of the `*_Discovery` boards), use
the corresponding MCU device project instead. For example, the Crazyflie 2.0
uses an STM32F405, so project stm32f40x.gpr would be appropriate. With that
approach the hardware that the `*_Discovery` boards include will not be
available, such as the user buttons.
