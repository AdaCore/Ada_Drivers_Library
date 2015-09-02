These packages represent general MCU devices, such as a platform based on an
STM32F405.

Use one of these packages if that is the kind of hardware your target is based
upon. For example, the Crazyflie 2.0 uses an STM32F405, so package STM32F40xxx
would be appropriate.

Note that if you are using one of the `*_Discovery` boards, using the
corresponding package would be appropriate since those packages include the
additional hardware specific to those boards. With that approach the additional
hardware specific to that board will available, such as the user buttons and
LEDs.