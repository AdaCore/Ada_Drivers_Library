SiFive's HiFive1 is an Arduino compatible dev kit featuring the RISC-V Freedom
E310 micro-controller.

You can get it at [sifive.com](https://www.sifive.com/products/hifive1/).

As of today, the HiFive1 and FE310 support in Ada_Drivers_Library is
experimental. We document here a procedure to build the toolchain on Linux.

## Building the tool chain

SiFive - manufacturer of the MCU - provides an SDK repository with scripts to build a cross RISC-V GCC. There is a fork of this repository modified to enable Ada support in the compiler.

Just clone it

`$ git clone --recursive https://github.com/Fabien-Chouteau/freedom-e-sdk`

install a native GNAT from your Linux distrib (for instance on Ubuntu)

`$ sudo apt-get install gnat`

and start the build

```
$ cd freedom-e-sdk
$ make tools
```

This step will also download a release of GNAT GPL for arm-elf to give you access to gprbuild and Gnat Programming Studio.
If you have a problem with this procedure don’t hesitate to open an issue on GitHub.

## The run-time

Ada programs always need a run-time library, but there are different run-time profiles depending on the constraints of the platform. In GNAT we have the so called Zero FootPrint run-time (ZFP) that provides the bare minimum and therefore is quite easy to port to a new platform (no exception propagation, no tasking, no containers, no file system access, etc.).

You can find the ZFP run-time for the HiFive1 in this repository: https://github.com/Fabien-Chouteau/zfp-hifive1

Clone it in `examples/HiFive1` directory (where this README is).

## Building the example

To build the example, make sure sure you have all the tools in your PATH and run:

`$ gprbuild -f -p -P hifive1_example.gpr -XPLATFORM_BUILD=Debug`

Follow the instructions in the freedom-e-sdk to flash the example on the board using OpenOCD.
