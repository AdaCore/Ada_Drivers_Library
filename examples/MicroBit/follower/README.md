Line Follower Example
=====================

This example builds upon the "servos" and "neopixel" examples to implement
a buggy with line following ability, based on the Kitronik :MOVE line
follower add-on (which provides two optical sensors on the underside of the buggy).

The control loop is designed to follow a black strip (e.g. 20 mm black tape)
on a light background. If both sensors get dark (e.g. due to a strip of tape
across the main line), the buggy stops.

The motors are initially stopped. Press button A to start/stop running along
the strip.

