# LCD demo

This example demonstrates working with the LCD using the
HAL.Bitmap.Bitmap_Buffer interface.
Colorful rectangles are displayed on the screen, and their
colors change based on the X and Y coordinates.

For this demo, you need an LCD, (which is usually sold as
part of the STM32 F4VE board kit) connected as SPI:

* PA1 LCD RESET
* PA2 LCD D/C
* PA3 LCD BKL
* PA4 SPI1 NSS
* PA5 SPI1 SCK
* PA6 SPI1 MISO
* PA7 SPI1 MOSI

