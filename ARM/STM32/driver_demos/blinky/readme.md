A simple example that blinks all the LEDs simultaneously, indefinitely. 
It does not use the convenience routine defined in the STM32.Boards
packages to initialize the LEDs, but instead directly invokes the GPIO
package to configure them, for the sake of illustration.

Note that this code is independent of the specific MCU device and board
in use because it uses names that are common across all of them. In
particular, the name "All_LEDs" refers to different GPIO pins on different
boards, and indeed defines a different number of LEDs on different boards.
Not all boards have user-controllable LEDs, of course, but those that do, 
have the name "All_LEDs" defined to indicate all of them.

The gpr file determines which board is actually used.
