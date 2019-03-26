SiFive's E31Arty is an FPGA evaluation kit featuring the standard RISC-V E31
core and minimal peripherals.

You can get it after registering for free at at
[dev.sifive.com](https://dev.sifive.com/risc-v-core-ip/evaluate/fpga/).

## How to setup the Ada development environment for the E31Arty

GNAT Community now comes with E31Arty support built-in. So you only need to
download the RISC-V32 ELF and the native package from
[here](adacore.com/download).

## Building the example

To build the example, make sure sure you have all the tools in your PATH and
run:

`$ gprbuild -f -p -P e31arty_example.gpr -XPLATFORM_BUILD=Debug`

Follow the instructions in the freedom-e-sdk to flash the example on the board
using OpenOCD.
