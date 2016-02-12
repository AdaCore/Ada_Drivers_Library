with System;

package STM32.SDRAM is

   procedure Initialize;

   function Base_Address return System.Address;

   function Reserve (Amount : Word) return System.Address;

end STM32.SDRAM;
