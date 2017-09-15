We understand that there might be some confusion between the two repositories
`Ada_Drivers_Library` and `bb-runtimes`. Who does what? What should go where?
This page will try to provide an answer.

Don't hesitate to ask more questions on this subject or suggest improvements to
this page.

# Ada_Drivers_Library vs bb-runtimes

The run-times (`bb-runtimes` repository) provide an implementation of Ada
language features as defined by the standard. These features may include
interrupt handling, tasking and time services that are usually provided by what
is called an RTOS in the C/C++ world.

The run-times will need to control a few peripherals to provide those features
(interrupt controller, one timer, some CPU registers), but it won't touch the
other peripherals (I2C, SPI, GPIOs, Ethernet, etc.).

`Ada_Drivers_Library` on the other side is a library for firmware development
in Ada. It provides drivers for remaining peripherals on the system as well
drivers for external components and also middleware services. Note that
`Ada_Drivers_Library` may rely on Ada run-time features but it does not mean
that the run-time has to be from the `bb-runtimes` repository, there are other
implementation possible depending on the architecture.

The limit between the two projects is the Ada language standard.

Even if for most projects `Ada_Drivers_Library` and `bb-runtimes` will be used
together, in the end the two remain independent. You can use the run-times with
your own drivers or bindings to drivers implemented in C. You can also use
`Ada_Drivers_Library` without the `bb-runtimes`, for instance on a Raspberry Pi
running Linux or if you use a third-party RTOS.
