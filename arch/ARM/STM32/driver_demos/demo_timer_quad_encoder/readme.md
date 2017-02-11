This program demonstrates the encoder interface provided by the ST Micro
timers. We use a timer (TIM3) to emulate quadrature encoder hardware as if
attached to a motor, providing the two off-phase signals a hardware encoder
would produce. These two signals are fed into another timer (TIM1) working
in the encoder interface mode. It is this encoder interface mode that we
are demonstrating. 

The emulated encoder hardware outputs are connected to the encoder interface
using two jumper wires connecting the corresponding pins indicated by the
diagram below. YOU MUST CONNECT THESE JUMPER WIRES ON YOUR BOARD.

    Emulated_Encoder                               Interface
     -------------                               -------------
    |             |CH1:PC6              CH1:PA8 |             |
    |             |---------------------------->|             |
    |    TIM3     |                             |    TIM1     |
    |             |CH2:PC7             CH2:PE11 |             |
    | Quadrature  |---------------------------->| Quadrature  |
    | encoder     |                             | encoder     |
    | emulator    |                             | interface   |
    |             |                             |             |
    |             |                             |             |
     -------------                               -------------

Timer channels 1 and 2 are used on both sides, and are thus connected to
the indicated GPIO pins.

When run, the green and red LEDs will alternate blinking at one-second
intervals as the emulated encoder is "driven" forward and backward at
that rate. The red LED indicates backward, the green forward.

This demonstration should work on any STM32 board (that has two LEDs), and
is known to work on F4 Discovery and F429 Discovery boards.

Based on a demonstration provided by ST Micro:
C:\STM32Cube_FW_F4_V1.6.0\Projects\STM324x9I_EVAL\Examples\TIM\TIM_Encoder
