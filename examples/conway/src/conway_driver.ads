
with System;

package Conway_Driver is

   task Driver is
      pragma Storage_Size (16 * 1024);
      pragma Priority (System.Default_Priority);
   end Driver;

end Conway_Driver;
