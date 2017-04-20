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

## pyOCD programmer

The Micro:Bit comes with an embedded programming/debugging probe implementing
the
[CMSIS-DAP](https://docs.mbed.com/docs/mbed-os-handbook/en/latest/advanced/DAP/)
protocol defined by ARM. In order to use it, you have to install a Python
library called pyOCD. Here's the procedure:

```
$ sudo apt-get install python-pip
$ pip install --pre -U pyocd
```

pyOCD will need premissions to talk with the Micro:Bit. Instead of running the
pyOCD as priviledged user (root), it's better to add a UDEV rules saying that
the device is accessible for non-priviledged users:

`$ sudo sh -c 'echo SUBSYSTEM==\"usb\", ATTR{idVendor}==\"0d28\", ATTR{idProduct}==\"0204\", MODE:=\"666\" > /etc/udev/rules.d/mbed.rules'`

Now that there's a new UDEV rule and if you already pluged your Micro:Bit
before, you have to unplug it and plug it back again.

To run pyOCD, use the following command:

```
$ pyocd-gdbserver -S -p 1234
INFO:root:DAP SWD MODE initialised
INFO:root:ROM table #0 @ 0xf0000000 cidr=b105100d pidr=2007c4001
INFO:root:[0]<e00ff000: cidr=b105100d, pidr=4000bb471, class=1>
INFO:root:ROM table #1 @ 0xe00ff000 cidr=b105100d pidr=4000bb471
INFO:root:[0]<e000e000:SCS-M0+ cidr=b105e00d, pidr=4000bb008, class=14>
INFO:root:[1]<e0001000:DWT-M0+ cidr=b105e00d, pidr=4000bb00a, class=14>
INFO:root:[2]<e0002000:BPU cidr=b105e00d, pidr=4000bb00b, class=14>
INFO:root:[1]<f0002000: cidr=b105900d, pidr=4000bb9a3, class=9, devtype=13, devid=0>
INFO:root:CPU core is Cortex-M0
INFO:root:4 hardware breakpoints, 0 literal comparators
INFO:root:2 hardware watchpoints
INFO:root:Telnet: server started on port 4444
INFO:root:GDB server started at port:1234
[...]
```

`-S` is to enable semihosting support, `-p 1234` is the port that Gdb will use
to talk with pyOCD.

At this point, pyOCD is waiting for a connection from Gdb.

## Install the Ada ZFP run-time

Go to the Micro:Bit example directory and download or clone the run-time from
Shawn Nock's GitHub repository: https://github.com/nocko/zfp-nrf51

```
$ cd Ada_Drivers_Library/examples/MicroBit/
$ git clone https://github.com/nocko/zfp-nrf51
```

## Open the example project and build it

Start GNAT Programming studio (GPS) and open the Micro:Bit example project:
"Ada_Drivers_Library/examples/MicroBit/microbit_example.gpr".

Press F4 and then press Enter to build the project.

## program and debug the board

In GPS, start a debug session with the top menu "Debug -> Initialize -> main".
GPS will start Gdb and connect it to pyOCD.

In the gdb console, use the "load" command to program the board:

```
(gdb) load
Loading section .text, size 0xbd04 lma 0x0
Loading section .ARM.exidx, size 0x8 lma 0xbd04
[...]
```
Reset the board with this command:

`(gdb) monitor reset`

And finally use the "continue" command to run the program:

`(gdb) continue`

You can interrupt the execution with the "CTRL+backslash" shortcut and then
insert breakpoints, step through the application, inspect memory, etc.
