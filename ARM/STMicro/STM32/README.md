These projects represent general MCU devices, such as a platform based on an
STM32F405.

Use one of these project if that is the kind of hardware your target is based
upon. For example, the Crazyflie 2.0 uses an STM32F405, so project
stm32f40x.gpr would be appropriate.

Note that if you are using one of the `*_Discovery` boards, there are projects
available in the boards directory are the root of this repository. The
corresponding project would be appropriate since it includes the additional
hardware specific to the board. With that approach the additional hardware
specific to that board will available, such as the user buttons and LEDs.
