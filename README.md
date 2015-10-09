1. Introduction
---------------

This repository contains Ada source code and complete sample GNAT projects for
selected bare-board platforms supported by GNAT.  Initially the repository
contains software for ARM platforms from a specific vendor, but we intend this
to be a location for both AdaCore and the community in general to contribute
support for additional processors, platforms, and vendors.


2. License
----------

All files are provided under a non-restrictive Berkeley Software Distribution
(BSD) license.  As such, and within the conditions required by the license, the
files are available both for proprietary ("commercial") and non-proprietary use.

For details, see the "License" section in the release notes accompanying the HAL
drivers provided by ST Microelectronics.


3. Requirements
---------------

The software is written in Ada 2012 and uses, for example, preconditions,
postconditions, and the high-level iterator form of for-loops.

In addition, a GNAT implementation-defined pragma is used extensively. This
pragma makes it possible to avoid explicit temporary copies when assigning
components of types representing hardware registers requiring full word or full
half-word accesses. The pragma is named Volatile_Full_Access. Those persons
wishing to submit additions to the library should see the GNAT Reference Manual
for details.

Therefore, building with the sources requires a compiler supporting both Ada
2012 and the GNAT-defined pragma Volatile_Full_Access.  The "GNAT GPL 2015"
compiler for ARM (ELF) is one such compiler.  A recent GNAT Pro compiler for
that target will also suffice.


4. Content
----------

Initial provision is for the hardware in the STM32F4 family of 32-bit MCUs, as
defined in the "RM0090 Reference Manual" (Doc ID 018909 Rev 6, Feb 2014) by
STMicroelectronics and made available on the "STM32F4 Discovery" and the
"STM32F429 Discovery" kit boards.

Specifically, there are low-level device drivers, higher-level component drivers,
small demonstration programs for the drivers, and larger example applications.
"Component" drivers are those that are implemented using the lower-level device
drivers (e.g., SPI or GPIO), such as the gyroscope and accelerometer on the
Discovery boards.

The small driver demonstration programs and the larger applications programs are
provided as full projects, including GNAT project files, and are ready to build
either within GPS or on the command-line.

Not all devices defined by the Reference Manual are supported, and not all those
supported are complete. We encourage contributions of corrections, enhancements,
and new drivers.
