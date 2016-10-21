# Warning!

This repository uses git submodule, please clone with the --recursive option:

```shell
git clone --recursive https://github.com/AdaCore/Ada_Drivers_Library.git
```

1. Introduction
---------------

This repository contains Ada source code and complete sample GNAT projects for
selected bare-board platforms supported by GNAT.  Initially the repository
contains software for ARM platforms from a specific vendor, but we intend this
to be a location for both AdaCore and the community in general to contribute
support for additional processors, platforms, and vendors.


2. License
----------

The Drivers are provided under a non-restrictive Berkeley Software Distribution
(BSD) license.  As such, and within the conditions required by the license, the
files are available both for proprietary ("commercial") and non-proprietary use.

The Services layer (as opposed to the main AdaCore repository) is provided as
a LGPL license. This is still available both for proprietary and free software
use, with a copyleft license on the layer itself. Please refer to the exact
terms of this license (see lgpl-3.0).


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
2012 and the GNAT-defined pragma Volatile_Full_Access. The "GNAT GPL 2016"
compiler for ARM ELF is one such compiler [(Download it
here)](http://libre.adacore.com/download/configurations). A recent GNAT Pro
compiler for that target will also suffice.


4. Content
----------

Initial provision is for the hardware in the STM32-discovery family of 32-bit
Boards, namely the STM32F407-disco, F429, F469, F746 and F769.

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

5. Roadmap
----------

Here is a list of projects that we are either dreaming about or already working
on. If you are interested by one of those, please contact us on the projects's
GitHub page.

* ARM
 * STM32F4/7 USB drivers
* Components
* Services
 * Bluetooth Low Energy stack
 * USB stack

6. Project using the Ada_Drivers_Library
----------------------------------------

 * [Certyflie: Ada/SPARK flight controller for the Crazyflie 2.0](https://github.com/AdaCore/Certyflie)
 * [ACNC: A Gcode interpreter and CNC controller](https://github.com/Fabien-Chouteau/ACNC)
 * [AMCQ: Multiple Choice Questions candy dispenser](https://github.com/Fabien-Chouteau/AMCQ)
 * [Giza: Giza is trying to be a simple widget tool kit for embedded platforms](https://github.com/Fabien-Chouteau/Giza)
 * [solenoid-engine-controller: Software controller for solenoid engines](https://github.com/Fabien-Chouteau/solenoid-engine-controller)
 * [un_pola: DIY instant camera with OpenMV](https://github.com/Fabien-Chouteau/un_pola)
 
(Add yours to the list!)
