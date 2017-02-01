# Design

This document describes some of the design of the Ada_Drivers_Library. It might
not be 100% accurate or up-to-date, but it gives a good overview and hopefully
can help you understand the project.

The main design goal of the Ada_Drivers_Library is code reuse. Each part of the
project should be made - as much as possible/reasonable - so that others can
reuse it in a different context. For example, a driver for an external
temperature sensor can be used with an STM32F4 micro-controller, but also with
an nRF51 or even a Raspberry Pi running Linux.

## Overall architecture

There's 2 main logical parts in this project:

Driver, it is where the low-level drivers for micro-controller features are
implemented. Examples: Timers, GPIO, I2C controllers, DMA, etc.

Middleware, it's a machine agnostic part that provides - or will provide - a
variety of service like bitmap drawing, communication stacks, file systems or
component drivers (more on that later).

|         Driver        |       Middleware      |
|-----------------------|-----------------------|
| Machine specific code | Machine agnostic code |

Middleware often needs to rely on Driver to provide its services. The HAL
(Hardware Abstraction Layer) defines interfaces to allow communication between
the two parts. The HAL is the heart of the project, seen and used by everything
so it's important that it remains as light as possible and depends only on
itself or on Ada features available on every configuration. For those reasons,
the HAL provides only definitions of features - using Ada interfaces - and no
implementation.

Note that it is not a problem for the Driver code to use features of the
Middleware when necessary, however it's is crucial that Middleware never
directly depend on the Driver otherwise it's not agnostic anymore...

Both part of the project can be quite large and they will continue to grow.
That is why we put in place subdivisions in the form of libraries.

The drivers are divided by architectures, vendors and then chips. For the
moment we have:

 * ARM
   * STM32
     * STM32F40x
     * STM32F7x9
     * ...
   * Nordic
     * nRF51

The most of the Middleware part is located in the middleware/ directory at the
root of the project. It is subdivided in Protocols, Bitmap_Drawings, Audio,
Tools, etc.

There is also the Components library, it's a little bit special because it
implements drivers for external chips (IO expander, gyro, thermal printer,
etc). That doesn't sound very hardware agnostic, however the code can be used
on any micro-controller. From a logical standpoint, it belongs in the machine
agnostic part of the project. Because of this specificity and for historical
reason, The Components library has its own subdir in the root of the project.
It might move to the middleware/ directory at some point.
