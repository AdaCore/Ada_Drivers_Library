This program demonstrates the Window Watchdog (WWDG) included with the 
STM32F4x MCUs. It also demonstrates calculating the "refresh window" 
width in terms of watchdog counts and time. 

The program requires two LEDs. It references these as "red" and "green" 
but those are not required colors: a different board with different 
colored LEDs would work just as well. 

The program activates the watchdog and then loops indefinitely, toggling 
the board's green LED. Within the loop there is a delay statement 
representing some application work being done. After that delay the 
program refreshes the watchdog counter to avoid the reset. The delay 
value itself, therefore, can be used to demonstrate the fact that the 
watchdog must only be refreshed within the configured window of time: 
doing so too soon or too late (or not at all) indicates some fault in 
the software and thus triggers the reset. When the program first runs it 
checks for a previous system reset (something an application might want 
to do), and in that case it turns on the red LED. That is the only 
indication that the window was missed because once the board resets it 
will begin toggling the green LED again. The red LED will remain turned 
on, however, so you can see the evidence. 

The project file is set to use the STM32F429I Discovery board but the 
program is dependent only on having the two LEDs available. It will run 
correctly without source code changes on the STM32F4 Discovery board. 

The code will configure itself automatically for the clocks on the 
board. However, the values within the inline comments that explain the 
watchdog clock calculations are in terms of the F429I Discovery board. 

Note that, for proper execution, you may need to reset the board 
manually after downloading. If the *unaltered* program immediately turns 
on the red LED, as if a reset has occurred, then simply use the black 
reset button to reset the board, or cycle power. The red LED should then 
not come on unless a true watchdog reset has occurred. 
