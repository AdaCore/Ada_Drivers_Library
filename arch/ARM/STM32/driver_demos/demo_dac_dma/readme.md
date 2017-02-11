This program demonstrates digital-to-analog conversion (DAC) output using
DMA driven by a timer for the DAC input.

The user increments the percentage of the DAC output (VRef+) voltage 
using the blue User button on the board (e.g., a STM32F429 Discovery 
board). The current percentage is displayed on the LCD. The percentage 
is converted into an absolute value based on the selected resolution. 
For example, a digital value of 2048 represents 50% in 12-bit 
resolution. The DMA controller periodically transfers this digital value 
to the DAC, driven by a timer. The DAC continuously converts this 
digital value to an analog voltage. 

Attach a voltmeter to PA4 (or PA5 if channel 2 is used) to see the voltage
changes.