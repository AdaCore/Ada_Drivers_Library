SiFive's HiFive1 is an Arduino compatible dev kit featuring the RISC-V Freedom
E310 micro-controller.

You can get it at [sifive.com](https://www.sifive.com/products/hifive1/).

## How to setup the Ada development environment for the HiFive1

GNAT Community now comes with HiFive1 support built-in. So you only need to
download the RISC-V32 ELF and the native package from
[here](adacore.com/download).

## Building the example

To build the example, make sure sure you have all the tools in your PATH and
run:

`$ gprbuild -f -p -P hifive1_example.gpr -XPLATFORM_BUILD=Debug`

Follow the instructions in the freedom-e-sdk to flash the example on the board
using OpenOCD.
