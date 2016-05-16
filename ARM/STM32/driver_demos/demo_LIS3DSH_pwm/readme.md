This program demonstrates use of the LIS3DSH accelerometer and a timer to
drive the brightness of the LEDs.

Note that this demonstration program is specific to the STM32F4 Discovery
boards because it references the specific accelerometer used on (later
versions of) those boards and because it references the four user LEDs
on those boards. (The LIS3DSH accelerometer is used on board versions
designated by the number MB997C printed on the top of the board.)

The idea is that the person holding the board will "pitch" it up and down
and "roll" it left and right around the Z axis running through the center
of the chip. As the board is moved, the brightness of the four LEDs
surrounding the accelerometer will vary with the accelerations experienced
by the board. In particular, as the angles increase the LEDs corresponding
to those sides of the board will become brighter. The LEDs will thus become
brightest as the board is held with any one side down, pointing toward the
ground.
