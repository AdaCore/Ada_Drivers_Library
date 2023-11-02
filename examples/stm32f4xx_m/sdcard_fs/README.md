# SD card filesystem example

Turns on the LED and waits for the SD card. Then, it mounts the first FAT
partition as '/card/', opens the `/file.txt` file and reads 512 bytes from
it. If everything is okay, the program blinks the green LED slowly
at a rate of 1 Hz. Otherwise, it blinks the LED faster at 5 Hz.
