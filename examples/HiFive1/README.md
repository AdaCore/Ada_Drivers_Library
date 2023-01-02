SiFive's HiFive1 is an Arduino compatible dev kit featuring the RISC-V Freedom
E310 micro-controller.

You can get it at [sifive.com](https://www.sifive.com/products/hifive1/).

## How to setup the Ada development environment for the HiFive1

You can use GNAT FSF riscv64-elf and GPRbuild releases from the Alire project,
download [here](https://github.com/alire-project/GNAT-FSF-builds/releases).

A public release of GNAT Studio is also available
[here](https://github.com/AdaCore/gnatstudio/releases).

## Building the example

To build the example, make sure sure you have all the tools in your PATH and
run:

`$ gprbuild -f -p -P hifive1_example.gpr -XPLATFORM_BUILD=Debug`

Follow the instructions in the freedom-e-sdk to flash the example on the board
using OpenOCD.
