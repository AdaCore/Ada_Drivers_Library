This program demonstrates use of the L3GD20 gyroscope, provided as a
separate component (external to the MCU) by some of the STM Discovery
boards.

The pitch, roll, and yaw values are continuously displayed on the LCD,
as are the adjusted raw values. Move the board to see them change. The
values will be positive or negative, depending on the direction of
movement. Note that the values are not constant, even when the board is
not moving, due to noise.

This program demonstrates use of interrupts rather than polling.

Note that you will need to set the RTS scenario variable to "ravenscar-full"
because the demo code uses the floating point image attribute.