
--  The file declares the main procedure for the demonstration.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Text_IO;

with STM32.Board;           use STM32.Board;
with STM32.Button;          use STM32;
with STM32.SDRAM;           use STM32.SDRAM;

with HAL.Bitmap;            use HAL.Bitmap;
with HAL.Framebuffer;

with Bitmapped_Drawing;     use Bitmapped_Drawing;
with Framebuffer_Helper;    use Framebuffer_Helper;

with TP;

with Game;
with Grid;
with Solver;
with Status;

procedure Demo_2048 is
   Period           : constant Time_Span := Milliseconds (10);
   Do_Slide         : Boolean := False;
   Do_Toggle_Solver : Boolean := False;
   Slide_Vect       : TP.Touch_Vector;

   procedure On_Autoplay_Clicked (X, Y : Natural);
   procedure On_Slide (Vect : TP.Touch_Vector);

   -------------------------
   -- On_Autoplay_Clicked --
   -------------------------

   procedure On_Autoplay_Clicked (X, Y : Natural)
   is
      pragma Unreferenced (X, Y);
   begin
      Solver.Solver_Enabled := not Solver.Solver_Enabled;
      Do_Toggle_Solver := True;
   end On_Autoplay_Clicked;

   --------------
   -- On_Slide --
   --------------

   procedure On_Slide (Vect : TP.Touch_Vector) is
   begin
      if not Game.Is_Sliding and then not Solver.Solver_Enabled then
         Slide_Vect := Vect;
         Do_Slide := True;
      end if;
   end On_Slide;

   Status_Layer_Area : constant Rect := Game.Get_Status_Area;

begin
   Ada.Text_IO.Put_Line ("Ready");
   Initialize_LEDs;
   STM32.SDRAM.Initialize;
   Display.Initialize (Mode => HAL.Framebuffer.Polling);
   Display.Initialize_Layer (1, ARGB_1555);
   Display.Initialize_Layer (2, ARGB_1555,
                             Status_Layer_Area.Position.X,
                             Status_Layer_Area.Position.Y,
                             Status_Layer_Area.Width,
                             Status_Layer_Area.Height);
   Touch_Panel.Initialize;

   Display.Set_Background (240, 240, 240);

   STM32.Button.Initialize;

   Game.Init;
   Game.Start;

   Game.Draw (Display.Get_Hidden_Buffer (1));
   Status.Init_Area (Display.Get_Hidden_Buffer (1));

   Status.Set_Score (0);

   if Status.Has_Buttons then
      Status.Set_Autoplay (Solver.Solver_Enabled);
      TP.Add_Button_Area
        (Status.Get_Autoplay_Btn_Area,
         On_Autoplay_Clicked'Unrestricted_Access);
   end if;

   Update_All_Layers;

   TP.Set_Slide_Callback (On_Slide'Unrestricted_Access);

   Solver.Init_Solver;

   STM32.Board.Turn_Off (STM32.Board.Green);

   loop
      if Game.Is_Sliding then
         while Game.Is_Sliding loop
            if not Game.Slide (Display.Get_Hidden_Buffer (1)) then
               Game.Add_Value;
               Game.Draw (Display.Get_Hidden_Buffer (1));

               Status.Set_Score (Game.Grid.Score);
               Update_All_Layers;
            else
               Display.Update_Layer (1, False);
            end if;
         end loop;
      end if;

      if Button.Has_Been_Pressed then
         On_Autoplay_Clicked (0, 0);
      end if;

      if Do_Toggle_Solver then
         Status.Set_Autoplay (Solver.Solver_Enabled);
         Display.Update_Layer (2, True);

         if Solver.Solver_Enabled then
            Turn_On (STM32.Board.Green);
         else
            Turn_Off (STM32.Board.Green);
         end if;

         Do_Toggle_Solver := False;
      end if;

      if Solver.Solver_Enabled then
         case Solver.Next_Move is
            when Solver.Up =>
               Game.Move (Direction => Grid.Up);
            when Solver.Down =>
               Game.Move (Direction => Grid.Down);
            when Solver.Left =>
               Game.Move (Direction => Grid.Left);
            when Solver.Right =>
               Game.Move (Direction => Grid.Right);
            when Solver.None =>
               --  Solver.None may arise in two different situations: either
               --  because the solver has been interrupted, or because no more
               --  move is possible (game over). So if the solver is still
               --  enabled (e.g. has not been interrupted), we restart a new
               --  game.
               if Solver.Solver_Enabled then
                  Game.Start;
               end if;
         end case;

      elsif Do_Slide then
         Game.Treat_Touch (Slide_Vect);
         Do_Slide := False;
      end if;

      delay until Clock + Period;
   end loop;
end Demo_2048;
