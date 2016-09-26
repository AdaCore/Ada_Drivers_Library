
--  This file provides the declaration for the task controlling the devices on
--  the train controller demo.

with System;

package Driver is

   task Controller is
      pragma Storage_Size (4 * 1024);
      pragma Priority (System.Default_Priority);
   end Controller;

end Driver;
