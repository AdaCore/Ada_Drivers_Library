A simple example that blinks all the LEDs simultaneously, indefinitely. 
It does not use the convenience routine defined in the STM32.Boards
packages to initialize the LEDs, but instead directly invokes the GPIO
driver to configure them, for the sake of illustration.

Note that this code is independent of the specific MCU device and board
in use because it uses names that are common across all of them. In
particular, the name "All_LEDs" refers to different GPIO pins on different
boards, and indeed defines a different number of LEDs on different boards.

The gpr file determines which board is actually used.
