A simple example that blinks all the LEDs sequentially, indefinitely.

The purpose of the example is to illustrate the architecture of a 
Ravenscar-based application using a periodic task and an interrupt 
handler. This architecture is idiomatic of Ravenscar-based applications. 

Specifically, there is a periodic task that blinks the LEDs on the board 
in a sequence. The blue User button generates an interrupt that is used 
to reverse the order of that sequence. 

This example is independent of the specific MCU device and board in use 
but it does require that the board provide four LEDs so that a sequence 
can be discerned by the viewer. 

The gpr file determines which board is actually used. Invoke the builder
and/or GPS with the one that corresponds to the board you have available,
or modify one to match your specific hardware.
