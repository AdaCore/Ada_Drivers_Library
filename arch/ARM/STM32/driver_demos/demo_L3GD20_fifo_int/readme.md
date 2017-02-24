This program demonstrates use of the L3GD20 gyroscope, provided as a
separate component (external to the MCU) by some of the STM Discovery boards.

This program demonstrates use of the gyro's on-board FIFO, driven by an
interrupt signalling that the FIFO is partially full. In particular, the
gyro generates an interrupt when the FIFO has at least the number of
entries specified by a "watermark" configured by the main program. The
FIFO contains at least that many sets of raw values for the three X, Y,
and Z axes. We read all those actually in the FIFO, even if greater than
the number specified by the watermark. We then average them into one set
of three axis readings, and use that for further processing. In
particular, on each iteration we subtract the stable bias offset from
each axis value and scale by the selected sensitivity. The adjusted and
scaled values are displayed on each iteration. The stable bias offsets
are also displayed, initially (not iteratively since they are not
recomputed).

Note that you will need to set the RTS scenario variable to "ravenscar-full"
because the demo code uses the floating point image attribute.