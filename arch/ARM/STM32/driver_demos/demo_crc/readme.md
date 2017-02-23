This program demonstrates use of the CRC processor on STM32F4x MCUs. (The
STM32F3 MCUs, among others, have additional CRC capabilities.)

The checksum for a given block of 32-bit words is calculated two ways and
then compared.

The checksum is first computed on the data block by calling a routine that
transfers the block content to the CRC processor in a loop, which is to say
that the CPU does the transfer. In the second approach, DMA is used to transfer
the data block to the CRC processor. Obviously for a large data block, this
second approach is far quicker.

This program is expected to run on the STM32F429 Discovery boards because
it uses the LCD to display results.  This display usage is entirely for
convenience -- some other means for showing the results could be used, such
as a serial port, or some other board with a different display.
