This program demonstrates basic DAC use, using explicit calls to control
the output value and conversion starting/stopping.

The user increments the percentage of the DAC output (VRef+) voltage using
the blue User button on the board (e.g., a STM32F429 Discovery board).
The current percentage is displayed on the LCD. When incremented, the
new percentage is converted into an absolute value based on the selected
resolution. For example, a digital value of 2048 represents 50% in 12-bit
resolution.

Attach a voltmeter to PA4 (or PA5 if channel 2 is used) to see the voltage
change as a result of pushing the blue button.