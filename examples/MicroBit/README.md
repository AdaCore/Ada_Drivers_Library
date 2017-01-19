The Micro:Bit is a very small ARM Cortex-M0 board designed by the BBC for
computer education. It's fitted with a [Nordic
nRF51](https://www.nordicsemi.com/eng/Products/Bluetooth-low-energy/nRF51822)
Bluetooth enabled microcontroller and an embedded programmer. You can get it
at:

 - [Pimoroni (UK/EU)](https://shop.pimoroni.com/collections/micro-bit/products/microbit)
 - [Kitronik (UK/EU)](https://www.kitronik.co.uk/5613-bbc-microbit-board-only.html)
 - [ThePiHut (UK/EU)](https://thepihut.com/collections/microbit/products/micro-bit)
 - [AdaFruit (US)](https://www.adafruit.com/products/3362)

# How to setup the Ada development environment for the Micro:Bit

## J-Link embedded programmer

In order to use gdb to program and debug the Micro:Bit, you have to replace the
firmware of the embedded programmer. Go to this page:
https://www.segger.com/bbc-micro-bit.html and follow the instructions toinstall
the J-Link firmware on the Micro:Bit.

## J-Link GDBServer

Now you need to download and install the "J-Link Software and Documentation
Pack" for your development machine (Linux, Windows, MacOS) from this page:
https://www.segger.com/downloads/jlink

You can now start the J-Link GDB server, for instance on Linux:

`$ <JLINK_INSTALLATION_DIR>/JLinkGDBServer -device nrf51822`

Note that JLinkGDBServer may require administrator/root access to talk to the
board. To avoid running JLinkGDBServer as root on Linux, copy the UDev rule
file "99-jlink.rules" from the J-Link installation to "/etc/udev" and reload
the rules:


```
$ sudo cp  <JLINK_INSTALLATION_DIR>/99-jlink.rules /etc/udev/rules.d
$ sudo udevadm control --reload-rules
```

## Install the Ada ZFP run-time

Go to theMicro:Bit example directory and download or clone the run-time from
Shawn Nock's GitHub repository: https://github.com/nocko/zfp-nrf51

```
$ cd da_Drivers_Library/examples/MicroBit/
$ git clone https://github.com/nocko/zfp-nrf51
```

## Open the example project and build it

Start GNAT Programming studio (GPS) and open the Micro:Bit example project:
"Ada_Drivers_Library/examples/MicroBit/microbit_example.gpr".

Press F4 and then press Enter to build the project.

## program and debugthe board

In GPS, start a debug session with the top menu "Debug -> Initialize -> main".
GPS will start gdb and connect it to the J-Link debugger.

In the gdb console, use the "load" command to program the board:

```
(gdb) load
Loading section .text, size 0xbd04 lma 0x0
Loading section .ARM.exidx, size 0x8 lma 0xbd04
[...]
```
Reset the board with this command

`(gdb) monitor reset`

And finally use the "continue" command to run the program:

`(gdb) continue`

You can interrupt the execution with the "CTRL+backslash" shortcut and then
insert breakpoints, step through the application, inspect memory, etc.
