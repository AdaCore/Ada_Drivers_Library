
with STM32.Board; use STM32.Board;

package body Framebuffer_Helper is

   procedure Update_All_Layers
   is
   begin
      --  With the DSIHost, there's no need to "copy back" data as there's
      --  only one frame buffer per layer, the driver pushing it upon request
      --  to the display.
      Display.Update_Layers;
   end Update_All_Layers;

end Framebuffer_Helper;
