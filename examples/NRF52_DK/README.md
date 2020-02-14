The nRF52 Development Kit is a board designed by Nordic Semiconductor to evaluate
the nRF52832 chip. This chip has an Arm Cortex-M4F core and an integrated multi-
protocol 2.4GHz radio. You can get it for most large distributors including:

 - [Digikey (US)](https://www.digikey.com/en/products/detail/nordic-semiconductor-asa/nrf52-dk/5773879)
 - [Digikey (CA)](https://www.digikey.ca/en/products/detail/nordic-semiconductor-asa/nrf52-dk/5773879)
 - [Farnell (UK/EU)](https://uk.farnell.com/nordic-semiconductor/nrf52-dk/dev-kit-bluetooth-low-energy-soc/dp/2842319)

# How to setup the Ada development environment for the Micro:Bit

GNAT Community now comes with nRF52 support built-in. So you only
need to download the ARM ELF and the native package from
[here](https://www.adacore.com/download)

## pyOCD programmer

The nRF52 DK comes with a Segger J-Link embedded
programming/debugging. You can use the J-Link tools to program the board.

A better option is a build of the
[DAPLink](https://github.com/ARMmbed/DAPLink) firmware implementing
[CMSIS-DAP](https://docs.mbed.com/docs/mbed-os-handbook/en/latest/advanced/DAP/)
protocol defined by ARM that can be written over the JLink firmware.

The binary and the instructions for programing the firmware over USB
can be found on the [DAPLink release
site](https://armmbed.github.io/DAPLink/) by searching for `Nordic-nRF52-DK`

## Open one of example projects and build it

Start GNAT Programming studio (GPS) and open the micro:bit example project:

 - [Buttons](buttons/)
 - [Digital Output](digital_out/)
 - [BLE beacon](BLE_beacon/)

Press F4 and then press Enter to build the project.

## Program the board

In the GPS toolbar, click on the "flash to board" button to program the
nRF52 DK.

After a few seconds, you should be able to interact with the example
application as specified in the project's README.md file.
