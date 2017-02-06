This demonstration causes an on-board LED to increase and decrease in
brightness, iteratively, for as long as the application runs. In effect
the LED light waxes and wanes.
See http://visualgdb.com/tutorials/arm/stm32/fpu/ for the inspiration.

There are two demonstrations, hence main programs, showing how to 
achieve the effect. Both use pulse-width-modulation (PWM) to make the 
LED change brightness. Both are making use of the STM timer's built-in 
PWM functionality. One program uses the STM32.Timer package directly to
achieve the effect. The other program uses the STM32.PWM package that is
a simplifying wrapper around the timer PWM functionality. 

The direct use is demonstrated by "demo_timer_pwm.adb" whereas the program
"demo_pwm_adt.adb" illustrates the abstract data type PWM package use.
