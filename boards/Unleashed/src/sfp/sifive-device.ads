with SiFive.SPI; use SiFive.SPI;
with SiFive.GPIO; use SiFive.GPIO;
with SiFive.UART; use SiFive.UART;
with SiFive.PWM; use SiFive.PWM;
with System; use System;

package SiFive.Device is

   -- GPIO0 --

   GPIO0 : aliased GPIO_Controller (268828672);

   P00 : aliased GPIO_Point (GPIO0'Access, 0);
   P01 : aliased GPIO_Point (GPIO0'Access, 1);
   P02 : aliased GPIO_Point (GPIO0'Access, 2);
   P03 : aliased GPIO_Point (GPIO0'Access, 3);
   P04 : aliased GPIO_Point (GPIO0'Access, 4);
   P05 : aliased GPIO_Point (GPIO0'Access, 5);
   P06 : aliased GPIO_Point (GPIO0'Access, 6);
   P07 : aliased GPIO_Point (GPIO0'Access, 7);
   P08 : aliased GPIO_Point (GPIO0'Access, 8);
   P09 : aliased GPIO_Point (GPIO0'Access, 9);
   P010 : aliased GPIO_Point (GPIO0'Access, 10);
   P011 : aliased GPIO_Point (GPIO0'Access, 11);
   P012 : aliased GPIO_Point (GPIO0'Access, 12);
   P013 : aliased GPIO_Point (GPIO0'Access, 13);
   P014 : aliased GPIO_Point (GPIO0'Access, 14);
   P015 : aliased GPIO_Point (GPIO0'Access, 15);

   -- QSPI0 --

   QSPI0 : aliased SPI_Controller (268697600);

   -- QSPI1 --

   QSPI1 : aliased SPI_Controller (268701696);

   -- QSPI2 --

   QSPI2 : aliased SPI_Controller (268763136);

   -- PWM0 --

   PWM0_Internal : aliased SiFive.PWM.Internal_PWM
      with Import, Address => System'To_Address (268566528);
   PWM0 : aliased SiFive.PWM.PWM_Device (PWM0_Internal'Access);

   -- PWM1 --

   PWM1_Internal : aliased SiFive.PWM.Internal_PWM
      with Import, Address => System'To_Address (268570624);
   PWM1 : aliased SiFive.PWM.PWM_Device (PWM1_Internal'Access);

   -- UART0 --

   UART0 : aliased SiFive.UART.UART_Device (268500992);

   -- UART1 --

   UART1 : aliased SiFive.UART.UART_Device (268505088);

end SiFive.Device;
