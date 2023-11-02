# SD card read example

Turns on the LED and waits for the SD card. Then, it reads the first 512
bytes from the SD card and checks for the 0x55 0xAA pattern at the end of
the block. If everything is okay, the program blinks the green LED slowly
at a rate of 1 Hz. Otherwise, it blinks the LED faster at 5 Hz.
