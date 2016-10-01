with System;

package STM32.SDRAM is

   procedure Initialize;

   function Base_Address return System.Address;

   function Reserve
     (Amount : UInt32;
      Align  : UInt32 := Standard'Maximum_Alignment) return System.Address;

end STM32.SDRAM;
