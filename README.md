[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/ada-lang/Ada_Drivers_Library)
![CI status](https://github.com/AdaCore/Ada_Drivers_Library/actions/workflows/main.yml/badge.svg)
  
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

## Board list

| Arch   | Board                 |
| ------ | --------------------- |
| ARM    | [STM32F407_Discovery] |
| ARM    | [STM32F429_Discovery] |
| ARM    | [STM32F469_Discovery] |
| ARM    | [STM32F4XX_M]         |
| ARM    | [STM32_F4VE]          |
| ARM    | [STM32F746_Discovery] |
| ARM    | [STM32F769_Discovery] |
| ARM    | [STM32_H405]          |
| ARM    | [NUCLEO_F446ZE]       |
| ARM    | [Crazyflie]           |
| ARM    | [Feather_STM32F405]   |
| ARM    | [OpenMV2]             |
| ARM    | [MicroBit]            |
| ARM    | [NRF52_DK]            |
| RISC-V | [HiFive1]             |
| RISC-V | [HiFive1_rev_B]       |
| RISC-V | [Unleashed]           |
|        | [Native]              |

<!-- Examples: -->
[HiFive1]:             examples/HiFive1/README.md
[HiFive1_rev_B]:       examples/HiFive1_rev_B/README.md
[MicroBit]:            examples/MicroBit/README.md
[NRF52_DK]:            examples/NRF52_DK/README.md
[OpenMV2]:             examples/OpenMV2/README.md
[STM32F429_Discovery]: examples/STM32F429_Discovery/README.md
[STM32F469_Discovery]: examples/STM32F469_Discovery/README.md
[STM32F407_Discovery]: examples/STM32F4_DISCO/README.md
[STM32_F4VE]:          examples/stm32_f4ve/README.md
[STM32F4XX_M]:         examples/stm32f4xx_m/README.md
[STM32F746_Discovery]: examples/STM32F746_Discovery/README.md
[STM32F769_Discovery]: examples/STM32F769_Discovery/README.md

<!-- Undocumented: -->
[STM32_H405]:        examples/stm32_h405/
[NUCLEO_F446ZE]:     examples/nucleo_f446ze/
[Feather_STM32F405]: examples/feather_stm32f405
[Crazyflie]:         boards/crazyflie/
[Unleashed]:         examples/Unleashed
[Native]:            boards/native

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
GNAT Pro compiler or GNAT FSF 12 for ARM ELF or RISC-V ELF [(Download
here)](https://github.com/alire-project/GNAT-FSF-builds/releases).

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

