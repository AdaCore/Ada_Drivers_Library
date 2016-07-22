with STM32_SVD.I2C;

package STM32.RCC is

   Unknown_Device : exception;
   --  Raised by the routines below for a device passed as an actual parameter
   --  when that device is not present on the given hardware instance.

   procedure Enable_Clock (This : STM32_SVD.I2C.I2C_Peripheral);
   procedure Reset (This : STM32_SVD.I2C.I2C_Peripheral);

   procedure SYSCFG_Clock_Enable with Inline;

end STM32.RCC;
