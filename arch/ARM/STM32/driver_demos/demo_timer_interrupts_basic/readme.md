This program demonstrates basic timer interrupts. On each interrupt the 
green LED will blink on the Discovery board. Interrupts occur at one 
second intervals (an arbitrary choice) so the LED will be on for a 
second, off for a second, and so on, indefinitely. 

The demo uses the green LED, which should be fairly common across 
multiple STM boards. 

Note that the control over the LED the toggling) is done within the 
interrupt handler, not in the main procedure. 

