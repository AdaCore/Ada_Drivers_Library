This program demonstrates using one timer to generate several periodic
interrupts, each with a different period.  Each interrupt is tied to a
separate LED such that the LED blinks at the corresponding rate.

It uses the STM F4 Discovery board for the sake of the four LEDs but another
board could be used, with different LED configurations. Using a board
with fewer LEDs is possible. 