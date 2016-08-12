with HAL.Framebuffer; use HAL.Framebuffer;

with Framebuffer_LTDC;

package Framebuffer_RK043FN48H is

   LCD_Natural_Width  : constant := 480;
   LCD_Natural_Height : constant := 272;

   type Frame_Buffer is limited new Framebuffer_LTDC.Frame_Buffer with private;

   procedure Initialize
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation := Default;
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt);

private

   type Frame_Buffer is limited new Framebuffer_LTDC.Frame_Buffer with
      null record;

end Framebuffer_RK043FN48H;
