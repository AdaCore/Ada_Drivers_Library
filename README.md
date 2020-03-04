[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/ada-lang/Ada_Drivers_Library)
[![Linux Build Status](https://travis-ci.org/AdaCore/Ada_Drivers_Library.svg?branch=master)](https://travis-ci.org/AdaCore/Ada_Drivers_Library)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/AdaCore/Ada_Drivers_Library?branch=master&svg=true)](https://ci.appveyor.com/project/github-integration-adacore/ada-drivers-library)

# 1. Introduction

This repository contains drivers and sample projects to program
micro-controllers with the Ada and SPARK languages. The library also provides
some middleware services and drivers for external devices such as sensors. We
intend this to be a location for both AdaCore and the community in general to
contribute support for additional processors, platforms, and vendors.

# 2. Supported hardware

Ada_Drivers_Library provides support for various devices in the ARM Cortex-M
and RISC-V architectures. Some devices are only partially supported. Go to the
[boards directory](boards/) for a list of supported hardware.

# 3. Getting started

To start using the Ada_Drivers_Library, please go to the [examples
directory](examples/) where you will find instructions to run your first
project.

# 4. License

All files are provided under a 3-clause Berkeley Software Distribution (BSD)
license. As such, and within the conditions required by the license, the files
are available both for proprietary ("commercial") and non-proprietary use.

For details, see the `LICENSE` file in the root directory.

# 5. Requirements

The software is written in Ada 2012 and uses, for example, preconditions,
postconditions, and the high-level iterator form of for-loops.

In addition, a GNAT implementation-defined pragma is used extensively. This
pragma makes it possible to avoid explicit temporary copies when assigning
components of types representing hardware registers requiring full word or full
half-word accesses. The pragma is named `Volatile_Full_Access`. Those persons
wishing to submit additions to the library should see the GNAT Reference Manual
for details.

Therefore, building with the sources requires a compiler supporting both Ada
2012 and the GNAT-defined pragma `Volatile_Full_Access`. For instance a recent
GNAT Pro compiler or GNAT Community 2018 for ARM ELF or RISC-V ELF [(Download
here)](http://adacore.com/download).

# 6. Roadmap

Here is a list of projects that we are either dreaming about or already working
on. If you are interested by one of those, please contact us on the project's
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
 * [NXT Lego Robotics: Ada drivers and demos for Lego NXT sensors and motors](https://github.com/AdaCore/Robotics_with_Ada)
 * [Bosch 9DOF IMU demo: Bosch 9DOF IMU driving a 3-D model of a lunar lander](
https://github.com/AdaCore/Lunar_Lander_Rotation_Demo)
 * [Bare metal sudoku solver](https://github.com/stangassinger/sudoku)
 * [Demo of an Ada physics engine](https://github.com/Kidev/DemoAdaPhysics2D)
 * [High Integrity Sumobot: A fully functional sumobot written in Ada/SPARK](https://github.com/bosepchuk/High_Integrity_Sumobot)
 * [RC Car: A remotely-controlled car built without the NXT Brick, written completely in Ada and SPARK](https://github.com/AdaCore/RC_Car_Demo)

(Add yours to the list!)

