# ILI9341

TFT LCD Single Chip Driver 240RGBx320 Resolution and 262K color.
It has parallel and serial interfaces for MCU. It's also able to
receive data over RGB interface.

The STM32F429 Discovery board utilizes an RGB interface and stores
graphic data in the main memory. The STM32 F4VE board uses a parallel
interface and employs ILI9341 memory, which significantly slows down
drawing but conserves main memory. When using the parallel interface,
ILI9341 registers are mapped to the MCU's address space through the
memory controller (FSMC), simplifying and speeding up the drawing.

* [Datasheet](http://www.datasheet-pdf.info/attach/1/3707537954.pdf)
* [Example for RGB interface](../../../../examples/STM32F429_Discovery/draw_stm32f429disco.gpr) for STM32F429 Discovery
* [Example for parallel interface](../../../../examples/stm32_f4ve/lcd) for STM32 F4VE board
