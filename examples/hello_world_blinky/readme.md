A simple example that blinks all the LEDs simultaneously, indefinitely. 
It does not use the convenience routine defined in the STM32.Boards
packages to initialize the LEDs, but instead directly invokes the GPIO
package to configure them, for the sake of illustration.

NOTE: some of the demos require the "ravenscar-full" runtime libraries 
so you will have to set the RTS scenario varible accordingly. 

These demos are independent of the specific MCU device and board
in use because it refers to the LED(s) with a name that is common across
all boards that have user-controlled LEDs available. Specifically, the
name "All_LEDs" refers to different LEDs (GPIO pins) on different boards,
and indeed defines a different number of LEDs on different boards. Some
boards have four, some two, some only one (the F7 Disco board, for example).
Not all boards have user-controllable LEDs, of course, but those that do,
have the name "All_LEDs" defined to indicate all of them.

The gpr file determines which board is actually used. Invoke the builder
and/or GPS with the one that corresponds to the board you have available,
or modify one to match your specific hardware.
