[![Build Status](https://travis-ci.org/AdaCore/Ada_Drivers_Library.svg?branch=master)](https://travis-ci.org/AdaCore/Ada_Drivers_Library)
[![Build status](https://ci.appveyor.com/api/projects/status/dvay075xkwxgppwm?svg=true)](https://ci.appveyor.com/project/AdaCore/ada-drivers-library)

# 1. Introduction

This repository contains Ada source code and complete sample GNAT projects for
selected bare-board platforms supported by GNAT.  Initially the repository
contains software for ARM platforms from a specific vendor, but we intend this
to be a location for both AdaCore and the community in general to contribute
support for additional processors, platforms, and vendors.

# 2. Getting started

To start using the Ada_Drivers_Library, please go to the [examples directory](examples/)
where you will find instructions to run your first project.

# 3. License

All files are provided under a 3-clause Berkeley Software Distribution (BSD)
license. As such, and within the conditions required by the license, the files
are available both for proprietary ("commercial") and non-proprietary use.

For details, see the `LICENSE` file in the root directory.

# 4. Requirements

The software is written in Ada 2012 and uses, for example, preconditions,
postconditions, and the high-level iterator form of for-loops.

In addition, a GNAT implementation-defined pragma is used extensively. This
pragma makes it possible to avoid explicit temporary copies when assigning
components of types representing hardware registers requiring full word or full
half-word accesses. The pragma is named `Volatile_Full_Access`. Those persons
wishing to submit additions to the library should see the GNAT Reference Manual
for details.

Therefore, building with the sources requires a compiler supporting both Ada
2012 and the GNAT-defined pragma `Volatile_Full_Access`. The "GNAT GPL 2017"
compiler for ARM ELF is one such compiler [(Download it
here)](http://libre.adacore.com/download/configurations). A recent GNAT Pro
compiler for that target will also suffice.

# 5. Content

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

# 6. Roadmap

Here is a list of projects that we are either dreaming about or already working
on. If you are interested by one of those, please contact us on the projects's
GitHub page.

* ARM
 * STM32F4/7 USB drivers
* Components
 * BlueNRG-MS (Bluetooth Low Energy Network Processor)
* Services
 * Bluetooth Low Energy stack
 * USB stack

# 7. Project using the Ada_Drivers_Library

 * [Certyflie: Ada/SPARK flight controller for the Crazyflie 2.0](https://github.com/AdaCore/Certyflie)
 * [ACNC: A Gcode interpreter and CNC controller](https://github.com/Fabien-Chouteau/ACNC)
 * [AMCQ: Multiple Choice Questions candy dispenser](https://github.com/Fabien-Chouteau/AMCQ)
 * [Giza: Giza is trying to be a simple widget tool kit for embedded platforms](https://github.com/Fabien-Chouteau/Giza)
 * [solenoid-engine-controller: Software controller for solenoid engines](https://github.com/Fabien-Chouteau/solenoid-engine-controller)
 * [un_pola: DIY instant camera with OpenMV](https://github.com/Fabien-Chouteau/un_pola)
 * [bare metal demos: Various Ada Demos on STM32*-Disco boards using this library](https://github.com/lambourg/Ada_Bare_Metal_Demos)
 * [SPARK Railway Demo: Simulated railway network in SPARK/Ada](https://github.com/AdaCore/SPARK_Railway_Simulation_Demo)

(Add yours to the list!)
