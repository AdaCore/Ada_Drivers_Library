
with STM32.Board;           use STM32.Board;

package body Framebuffer_Helper is

   procedure Update_All_Layers is
   begin
      Display.Update_Layers (Copy_Layer1 => True,
                             Copy_Layer2 => True);
   end Update_All_Layers;

end Framebuffer_Helper;
