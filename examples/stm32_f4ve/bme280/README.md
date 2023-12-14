# BME280 demo

This folder contains a demonstration program showcasing the functionality
of a temperature, humidity, and pressure sensor using the STM32 F4VE board.
The program reads sensor data and prints it over ST-Util semihosting
interface.

## Requirements

* STM32 F4VE development board
* Any BME280 module
* ST-Link V2 debug probe
* Development environment compatible with STM32F4 microcontrollers

## Setup

* Attach BME280 by I2C to PB9 (SDA), PB8 (SCL)
* Attach the debug probe to the designated port on the STM32F4VE board.
* Connect the STM32 F4VE board to your development environment.

## Usage

Compile and upload the program to the STM32 F4VE board. Upon successful upload,
the demonstration program will run, printing sensor data over semishosting
channel. To see the output

* launch a debug session in GNAT Studio, or
* run the debugger in the command line:

  ```sh
  st-util --semihosting
  arm-eabi-gdb -ex 'target remote localhost:4242' -ex cont
  ```
