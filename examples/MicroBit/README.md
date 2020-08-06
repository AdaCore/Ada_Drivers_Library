The Micro:Bit is a very small ARM Cortex-M0 board designed by the BBC for
computer education. It's fitted with a [Nordic
nRF51](https://www.nordicsemi.com/eng/Products/Bluetooth-low-energy/nRF51822)
Bluetooth enabled microcontroller and an embedded programmer. You can get it
at:

 - [Pimoroni (UK/EU)](https://shop.pimoroni.com/collections/micro-bit/products/microbit)
 - [Kitronik (UK/EU)](https://www.kitronik.co.uk/5613-bbc-microbit-board-only.html)
 - [ThePiHut (UK/EU)](https://thepihut.com/collections/microbit/products/micro-bit)
 - [AdaFruit (US)](https://www.adafruit.com/products/3530)

# How to setup the Ada development environment for the Micro:Bit

GNAT Community now comes with micro:bit and pyOCD support built-in. So you only
need to download the ARM ELF and the native package from
[here](https://www.adacore.com/download)

## pyOCD programmer

The Micro:Bit comes with an embedded programming/debugging probe implementing
the
[CMSIS-DAP](https://docs.mbed.com/docs/mbed-os-handbook/en/latest/advanced/DAP/)
protocol defined by ARM.

To use it on Linux, you might need privileges to access the USB ports without
which the flash program will say "No connected boards".

On Ubuntu, you can do it by creating (as administrator) the file
/etc/udev/rules.d/mbed.rules and add the line:
```
SUBSYSTEM=="usb", ATTR{idVendor}=="0d28", ATTR{idProduct}=="0204", MODE="0666"
```
then restarting the service by doing

```shell
$ sudo udevadm trigger
```

## Open one of example projects and build it

Start GNAT Programming studio (GPS) and open the micro:bit example project:

 - [Text Scrolling](text_scrolling/)
 - [Buttons](buttons/)
 - [Digital Output](digital_out/)
 - [Digital Input](digital_in/)
 - [Analog Output](analog_out/)
 - [Analog Input](analog_in/)
 - [Music](music/)
 - [BLE beacon](BLE_beacon/)

Press F4 and then press Enter to build the project.

## Program the board

Plug your micro:bit board with a USB cable, and wait for the system to
recognize it. This can take a few seconds

In the GPS toolbar, click on the "flash to board" button to program the
micro:bit.

After a few seconds, you should see a text scrolling on the LED matrix.

That's it, you are ready to hack the micro:bit with Ada!
