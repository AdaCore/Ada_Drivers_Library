This project defines multiple forms of higher-level "serial port"
abstractions, i.e., higher than direct use of USART/UART devices, along
with demonstrations of their use.

Each demo illustrates a distinct serial port abstraction.

One such serial port uses polling to control the underlying USART 
interaction for sending and receiving. This is said to be a "blocking" 
serial port because calls to send and receive using these ports wait 
until the I/O completes before returning to the callers. 

Another serial port uses interrupts to control the USART interactions
and is said to be "non-blocking" because callers will return from the
send and receive calls without waiting for sending or receiving to
complete. (A means of waiting for eventual completion is provided.)

For those two abstractions' demonstrations, you must use an app on the 
host that communicates with host serial ports to show the characters 
sent to and received from the target. For example, we use one named 
HyperSerialPort on Windows. 

Another serial port abstraction supports use of streams to send and 
receive binary values of arbitrary types, not just characters and arrays 
of characters. NOTE: THIS VERSION REQUIRES THE RAVENSCAR-FULL-* RUNTIME 
LIBRARIES. Set the scenario variable accordingly. 

In all cases the program running on the target board communicates with a
program running on the host computer. Characters are sent and received
over a serial line presumably connected to a host computer (see below
for connection hardware and software).

The first two kind of serial ports just send and receive characters
directly (via message buffers), rather than using streams. As such, a
program such as HyperSerialPort will suffice to send and receive
characters on the host.

For the streams-oriented serial port demo, we provide the sources for a
"host" app to run on the host computer. This "host" app interacts with
the user to send and receive strings to the target board using streams
rather than raw characters. Although streams allow arbitrary binary
values to be sent over a common, shared stream, and thus provide great
flexibility, for this demonstration we only work with Strings.

In all cases you need a way to connect the GPIO pins on the target board
to a serial port on your host computer.

You can use a USB cable specifically designed to appear as a COM port.
For example:

* Mouser Part No: 895-TTL-232R-5V
* Manufacturer Part No: TTL-232R-5V
* Manufacturer: FTDI
* Description: USB Cables / IEEE 1394 Cables USB Embedded Serial Conv 5V 0.1" Header

Note that that is a 5-volt cable, and that some pins cannot handle 5
volts. The pins we use in these demonstrations are 5-volt compatible so
that is not a problem, but it could be a problem with other boards or pins.
If so, 3-volt versions of these cables are also available.

The end of the cable is a female header, described in the datasheet
(`DS_TTL-232R_CABLES-217672.pdf`).  See pages 10 and 11 in particular.

Using male-to-female connector wires, connect the following on the target
board GPIO pins:

* header pin 1, the black wire's header slot, to a ground pin on the board
* header pin 4, the orange wire's header slot, to **PB7**
* header pin 5, the yellow wire's header slot, to **PB6**

Header pin 4 on the cable is TXD, the transmit data output.
Header pin 5 on the cable is RXD, the receive data input.
See table 4.1 on page 11 of the datasheet for all the pins.

On the host, just plug in the USB-to-serial cable.

The USART is set to: 115_200, N81, no flow control so change the app's
connection accordingly.
