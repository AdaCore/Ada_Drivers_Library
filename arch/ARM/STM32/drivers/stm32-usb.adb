with STM32_SVD.USB_OTG_FS; use STM32.USB_OTG_FS;

package body STM32.USB is

   procedure Driver_Init 
   
   procedure Driver_Reset is
   begin
      -- Wait for master to be idle
      while not OTG_FS_GLOBAL_Periph.FS_GRSTCTL.AHBIDL loop
         null;
         -- XXX: Must do smthg if looping is stuck
      end loop;
      
      OTG_FS_GLOBAL_Periph.FS_GRSTCTL.CSRST := True;
      
      -- Wait for device reset
      while not OTG_FS_GLOBAL_Periph.FS_GRSTCTL.CSRST loop
         null;
         -- XXX: Must do smthg if looping is stuck
      end loop;
   end Driver_Reset;

end STM32.USB;
