# Flash memory erase/write/read example

In this example we erase and flash part of the W25Q16 flash memory
when K1 button is pressed on reset. If the button is not pressed, the
program reads the flash memory and checks its content. If everything is
OK, the program blinks the green LED slowly (1 Hz), but otherwise it blinks
the LED faster (5 Hz).
