
with Ada.Real_Time;    use Ada.Real_Time;

with STM32.Board;           use STM32.Board;
with HAL.Framebuffer;       use HAL.Framebuffer;
with HAL.Bitmap;            use HAL.Bitmap;

with Screen_Interface;
with Railroad;

package body Driver is

   ----------------
   -- Controller --
   ----------------

   task body Controller is
      Period : constant Time_Span := Milliseconds (60);
      --  arbitrary, but directly affects how fast the trains move
      --  and how quickly the screen responds to touch

      Next_Start : Time := Clock + Seconds (1);

      Current  : Screen_Interface.Touch_State;
      Previous : Screen_Interface.Touch_State;
   begin
      delay until Next_Start;

      Display.Initialize (Portrait, Interrupt);
      Display.Initialize_Layer (1, RGB_565);
      Display.Initialize_Layer (2, ARGB_1555);
      Touch_Panel.Initialize (Portrait);
      Railroad.Initialize;

      Current  := Screen_Interface.Current_Touch_State;
      Previous := Current;

      loop
         Current := Screen_Interface.Current_Touch_State;

         if Current.Touch_Detected /= Previous.Touch_Detected then
            if Current.Touch_Detected then
               Railroad.Respond_To_Touch (Current.X, Current.Y);
            end if;
            Previous := Current;
         end if;

         Railroad.Step_Simulation;
         Display.Get_Hidden_Buffer (2).Fill (0);
         Railroad.Draw_Layout (Display.Get_Hidden_Buffer (2));
         Display.Update_Layer (2);

         Next_Start := Next_Start + Period;
         delay until Next_Start;
      end loop;
   end Controller;

end Driver;
