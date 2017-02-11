This demonstration illustrates using the DMA controller to send a block
of data from memory to memory.

There are two versions of the demonstration, therefore two main
programs. In both cases the data are copied (via DMA) only once. The
program simply verifies that the destination value is identical to the
source value.

One program ("demo_dma_polling.adb") uses polling to determine when the
DMA transfer is complete. The other program ("demo_dma_interrupts.adb")
uses interrupts.

In both programs the green LED will blink rapidly (permanently) if there
is an error indicated by the DMA controller. Otherwise, If the copied
data values are identical to the source values then the green LED will
be on steadily, otherwise the red LED will be on.
