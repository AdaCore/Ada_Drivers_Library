You need a way to connect the GPIO pins on the target board to a serial port on
your host computer.

You can use a USB cable specifically designed to appear as a COM port:

* Mouser Part No: 	895-TTL-232R-5V
* Manufacturer Part No: 	TTL-232R-5V
* Manufacturer: 	FTDI
* Description: 	USB Cables / IEEE 1394 Cables USB Embedded Serial Conv 5V 0.1" Header

The end of the cable is a female header, described in the datasheet
(`DS_TTL-232R_CABLES-217672.pdf`).  See pages 10 and 11 in particular.

Using male-to-female connector wires, connect the following on the STM32F4:

* header pin 1, the black wire's header slot, to a ground pin on the board
* header pin 4, the orange wire's header slot, to **PA2**
* header pin 5, the yellow wire's header slot, to **PA3**

Header pin 4 on the cable is TXD, the transmit data output.
Header pin 5 on the cable is RXD, the receive data input.
See table 4.1 on page 11 of the datasheet for all the pins.

On the host, just plug in the USB-to-serial cable.

We use an app named HyperSerialPort on the host.  Once the cable is connected it
will appear like a COM port so use the "Refresh" button in the Settings dialog
to find the port represented by the cable.

The USART is set to: 115_200, N81, no flow control so change the app's connection
accordingly.
