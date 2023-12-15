# BME280 LCD demo

This folder contains a demonstration program showcasing the functionality
of a temperature, humidity, and pressure sensor using the STM32 F4VE board
and an LCD display included in the kit. The program features a straightforward
graphical user interface (GUI) for configuring sensor parameters.

![Demo screenshot](bme280_lcd_x2.png)

## Overview

The demonstration program is designed to work with the STM32 F4VE development
board and a compatible LCD display. It provides a GUI interface to configure
sensor parameters such as temperature, humidity, and pressure.
The display includes buttons for enabling/disabling the display of
temperature (Te), humidity (Hu), and pressure (Pr) data. Additionally,
there are buttons (`x1` .. `16`) next to each data type for controlling
the oversampling parameters. Additionally, buttons labeled `No`, `x2` .. `16`
adjust the IRR filter.

## Requirements

* STM32 F4VE development board
* Any BME280 module
* Compatible LCD display/touch panel included in the kit
* Development environment compatible with STM32F4 microcontrollers

## Setup

* Attach BME280 by I2C to PB9 (SDA), PB8 (SCL)
* Attach the LCD display to the designated port on the STM32F4VE board.
* Connect the STM32 F4VE board to your development environment.

## Usage

Compile and upload the program to the STM32 F4VE board. Upon successful upload,
the demonstration program will run, displaying sensor data on the LCD screen.
Activate the buttons on the GUI interface using the touch panel.
Simply touch the corresponding button on the LCD screen to toggle its state.
