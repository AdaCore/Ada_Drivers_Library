---------------------------------------
--  Sudoku solving GUI Application
---------------------------------------

with Last_Chance_Handler; pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with STM32.Board;         use STM32.Board;
with STM32.DMA2D_Bitmap;  use STM32.DMA2D_Bitmap;
with HAL.Bitmap;          use HAL.Bitmap;
with Ada.Real_Time;       use Ada.Real_Time;
with HAL.Touch_Panel;     use HAL.Touch_Panel;

with Bitmapped_Drawing;
with BMP_Fonts;

with Ada.Text_IO;

with Sudoku;



procedure Sudoku_GUI
is
   X_Coord : Integer := -1;
   Y_Coord : Integer := -1;


   Width  : Natural;  -- 240
   Height : Natural;  -- 320

   cell_width  : Natural;
   hight_start : Natural;

   num_selection_width : Natural;

   type coord_ar_t is array (1 .. 2) of Natural range 1 .. 9;

   type range_one_to_eight_t is range 1 .. 8;
   cell_width_ar  : array (range_one_to_eight_t) of Natural;

   type range_one_to_three_t is range 1 .. 3;
   num_selection_width_ar  : array (range_one_to_three_t) of Natural;

   type range_one_to_81_t is range 1 .. 81;
   sudoku_ar  : array (range_one_to_81_t) of Natural range 0 .. 9;

   type selected_number_at_pos_t is array (1 .. 2) of Natural range 0 .. 81;  -- 1: Position 2: Number
   selected_number_at_pos : selected_number_at_pos_t;

   type status_marker_t is (MODE_START, MODE_FIELD_SELECT_PRESSED,
                            MODE_SHOW_NUM_SELECT, MODE_NUM_SELECT_PRESSED,
                            MODE_CALCULATE, MODE_CALCULATION_DONE, MODE_UNRESOLVABLE);
   status_marker : status_marker_t := MODE_START;

   Period       : constant Time_Span := Milliseconds (100);
   Next_Release : Time := Clock;





   function Bitmap_Buffer return not null Any_Bitmap_Buffer;
   function Bitmap_Buffer return not null Any_Bitmap_Buffer is
   begin
      if Display.Hidden_Buffer (1).all not in DMA2D_Bitmap_Buffer then
         raise Program_Error with "We expect a DM2D buffer here";
      end if;

      return Display.Hidden_Buffer (1);
   end Bitmap_Buffer;




   function get_sudoku_field_position (X_Coord : Integer; Y_Coord : Integer; cell_width : Integer) return coord_ar_t;
   function get_sudoku_field_position (X_Coord : Integer; Y_Coord : Integer; cell_width : Integer) return coord_ar_t is
      x_pos : Integer;
      y_pos : Integer;
   begin
      x_pos := X_Coord / (cell_width + 1) + 1;
      y_pos := Y_Coord / (cell_width + 1) + 1;

      return (x_pos, y_pos);
   end get_sudoku_field_position;


   type sudoku_number_t is range 1 .. 9;
   function get_selected_number (X_Coord : Integer; Y_Coord : Integer; cell_width : Integer) return sudoku_number_t;
   function get_selected_number (X_Coord : Integer; Y_Coord : Integer; cell_width : Integer) return sudoku_number_t is
      x_pos : Integer;
      y_pos : Integer;
   begin
      x_pos := X_Coord / cell_width + 1;
      y_pos := Y_Coord / cell_width + 1;
      return sudoku_number_t (3 * (y_pos - 1) + x_pos);
   end get_selected_number;


   function calc_sudoku_pos (x_pos : Natural; y_pos : Natural) return Positive;
   function calc_sudoku_pos (x_pos : Natural; y_pos : Natural) return Positive is
      ret : Natural;
   begin
      ret := 9 * (y_pos - 1) + x_pos;
      return ret;
   end calc_sudoku_pos;



   function transfer_pos_to_coord (position_number : Natural) return coord_ar_t;
   function transfer_pos_to_coord (position_number : Natural) return coord_ar_t is
      x : Natural;
      y : Natural;
   begin
      y := (position_number - 1) / 9 + 1;
      --  x := position_number - (y * 9) + 1;
      x := (position_number - 1) mod 9 + 1;
      return (x, y);
   end transfer_pos_to_coord;



   procedure transfer_to_complete_array;
   procedure transfer_to_complete_array is
   begin
      selected_number_at_pos (2) := Integer (get_selected_number (X_Coord, Y_Coord -
                                               hight_start, num_selection_width));
      sudoku_ar (range_one_to_81_t (selected_number_at_pos (1))) := selected_number_at_pos (2);
   end transfer_to_complete_array;


   procedure delete_from_complete_array;
   procedure delete_from_complete_array is
   begin
      selected_number_at_pos (2) := 0;
      sudoku_ar (range_one_to_81_t (selected_number_at_pos (1))) := 0;
   end delete_from_complete_array;



   procedure print_numbers_in_field;
   procedure print_numbers_in_field is
      coord_ar : coord_ar_t;
   begin
      for i in sudoku_ar'Range loop
         if sudoku_ar (i) /= 0 then
            coord_ar := transfer_pos_to_coord (Integer (i));
            Bitmapped_Drawing.Draw_String
              (Bitmap_Buffer.all,
               Start      => (X => coord_ar (1) * cell_width - cell_width / 2 - cell_width / 3,
                              Y => coord_ar (2) * cell_width - cell_width / 2 - cell_width / 4 + hight_start),
               Msg        => sudoku_ar (i)'Image,
               Font       => BMP_Fonts.Font8x8,
               Foreground => HAL.Bitmap.Black,
               Background => HAL.Bitmap.Dark_Green);
         end if;
      end loop;
   end print_numbers_in_field;


   procedure draw_sudoku_header;
   procedure draw_sudoku_header is
   begin
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Blue);
      Bitmap_Buffer.Fill_Rect (((0, 0), Width, hight_start));

      Bitmapped_Drawing.Draw_String
        (Bitmap_Buffer.all,
         Start      => (X => 70,
                        Y => 25),
         Msg        => "Sudoku",
         Font       => BMP_Fonts.Font16x24,
         Foreground => (128, 255, 255, 255),
         Background => HAL.Bitmap.Blue);
   end draw_sudoku_header;


   procedure draw_number_selection_field;
   procedure draw_number_selection_field is
   begin
      Bitmap_Buffer.Set_Source (HAL.Bitmap.White);
      Bitmap_Buffer.Fill_Rect (((0, hight_start), Width, Height - hight_start));
      for i in range_one_to_three_t loop
         --  vertical lines
         Bitmap_Buffer.Set_Source (HAL.Bitmap.Black);
         Bitmap_Buffer.Draw_Line ((num_selection_width_ar (i), hight_start),
                                  (num_selection_width_ar (i), Height), 4, False);
         --  horizontal lines
         Bitmap_Buffer.Set_Source (HAL.Bitmap.Black);
         Bitmap_Buffer.Draw_Line ((0, num_selection_width_ar (i) + hight_start),
                                  (Width, num_selection_width_ar (i) + hight_start), 4, False);


         declare
            tmp : Integer;
         begin
            tmp := Integer (i) - 1;
            Bitmapped_Drawing.Draw_String
              (Bitmap_Buffer.all,
               Start      => (X => tmp * num_selection_width_ar (1) + num_selection_width_ar (1) / 3,
                              Y => hight_start + 0 * num_selection_width_ar (1) +
                                num_selection_width_ar (1) / 3),
               Msg        => Integer (i)'Image,
               Font       => BMP_Fonts.Font16x24,
               Foreground => HAL.Bitmap.Blue,
               Background => HAL.Bitmap.White);

            Bitmapped_Drawing.Draw_String
              (Bitmap_Buffer.all,
               Start      => (X => tmp * num_selection_width_ar (1) + num_selection_width_ar (1) / 3,
                              Y => hight_start + 1 * num_selection_width_ar (1) +
                                num_selection_width_ar (1) / 3),
               Msg        => Integer (i + 3)'Image,
               Font       => BMP_Fonts.Font16x24,
               Foreground => HAL.Bitmap.Blue,
               Background => HAL.Bitmap.White);

            Bitmapped_Drawing.Draw_String
              (Bitmap_Buffer.all,
               Start      => (X => tmp * num_selection_width_ar (1) + num_selection_width_ar (1) / 3,
                              Y => hight_start + 2 * num_selection_width_ar (1) +
                                num_selection_width_ar (1) / 3),
               Msg        => Integer (i + 6)'Image,
               Font       => BMP_Fonts.Font16x24,
               Foreground => HAL.Bitmap.Blue,
               Background => HAL.Bitmap.White);

         end;
      end loop;
   end draw_number_selection_field;


   procedure draw_pressed_field_pos;
   procedure draw_pressed_field_pos is
      coord_ar : coord_ar_t;
   begin
      coord_ar := get_sudoku_field_position (X_Coord, Y_Coord - hight_start, cell_width);
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Red);
      Bitmap_Buffer.Fill_Circle (
                                 Center => (coord_ar (1) * cell_width - cell_width / 2,
                                            coord_ar (2) * cell_width - cell_width / 2 + hight_start),
                                 Radius => 10);
      selected_number_at_pos (1) := calc_sudoku_pos (coord_ar (1), coord_ar (2));
   end draw_pressed_field_pos;


   procedure draw_selection_button;
   procedure draw_selection_button is
   begin
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Blue);
      Bitmap_Buffer.Fill_Rect (((0, 0), Width / 2, hight_start));
      Bitmap_Buffer.Set_Source (HAL.Bitmap.White);
      Bitmap_Buffer.Fill_Rect (((Width / 2, 0), Width / 2, hight_start));

      Bitmapped_Drawing.Draw_String
        (Bitmap_Buffer.all,
         Start      => (X => 10,
                        Y => 30),
         Msg        => "Select",
         Font       => BMP_Fonts.Font16x24,
         Foreground => (128, 255, 255, 255),
         Background => HAL.Bitmap.Blue);
   end draw_selection_button;


   procedure draw_delete_button;
   procedure draw_delete_button is
   begin
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Dark_Green);
      Bitmap_Buffer.Fill_Rect (((0, 0), Width / 2, hight_start));
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Red);
      Bitmap_Buffer.Fill_Rect (((Width / 2, 0), Width / 2, hight_start));

      Bitmapped_Drawing.Draw_String
        (Bitmap_Buffer.all,
         Start      => (X => 130,
                        Y => 30),
         Msg        => "Delete",
         Font       => BMP_Fonts.Font16x24,
         Foreground => (255, 255, 255, 255),
         Background => HAL.Bitmap.Red);
   end draw_delete_button;


   procedure draw_calculate_button;
   procedure draw_calculate_button is
   begin
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Blue);
      Bitmap_Buffer.Fill_Rect (((0, 0), Width, hight_start));

      Bitmapped_Drawing.Draw_String
        (Bitmap_Buffer.all,
         Start      => (X => 25,
                        Y => 30),
         Msg        => "Calculate !",
         Font       => BMP_Fonts.Font16x24,
         Foreground => (128, 255, 255, 255),
         Background => HAL.Bitmap.Blue);
   end draw_calculate_button;

   procedure draw_done_button;
   procedure draw_done_button is
   begin
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Blue);
      Bitmap_Buffer.Fill_Rect (((0, 0), Width, hight_start));

      Bitmapped_Drawing.Draw_String
        (Bitmap_Buffer.all,
         Start      => (X => 25,
                        Y => 30),
         Msg        => "DONE",
         Font       => BMP_Fonts.Font16x24,
         Foreground => (128, 255, 255, 255),
         Background => HAL.Bitmap.Blue);
   end draw_done_button;


   procedure draw_sudoku_field;
   procedure draw_sudoku_field is
   begin
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Dark_Green);
      Bitmap_Buffer.Fill;

      Bitmap_Buffer.Set_Source (HAL.Bitmap.White_Smoke);
      for i in range_one_to_eight_t loop
         --  vertical lines
         Bitmap_Buffer.Draw_Line ((cell_width_ar (i), hight_start),  (cell_width_ar (i), Height), 1, False);
         --  horizontal lines
         Bitmap_Buffer.Draw_Line ((0, cell_width_ar (i) + hight_start),
                                  (Width, cell_width_ar (i) + hight_start), 1, False);
      end loop;
   end draw_sudoku_field;



   function touched_left_top return Boolean;
   function touched_left_top return Boolean
   is
   begin
      if (X_Coord > 0 and X_Coord < Width / 2) and (Y_Coord > 0 and Y_Coord  < hight_start) then
         return True;
      else
         return False;
      end if;
   end touched_left_top;


   function touched_right_top return Boolean;
   function touched_right_top return Boolean
   is
   begin
      if (X_Coord > Width / 2 and X_Coord < Width) and (Y_Coord  > 0 and Y_Coord < hight_start) then
         return True;
      else
         return False;
      end if;
   end touched_right_top;



   function touched_bottom return Boolean;
   function touched_bottom return Boolean
   is
   begin
      if (X_Coord > 0) and (Y_Coord  > hight_start) then
         return True;
      else
         return False;
      end if;
   end touched_bottom;

   procedure resetCoord;
   procedure resetCoord
   is
   begin
      X_Coord := -1;
      Y_Coord := -1;
   end resetCoord;





   procedure debug;
   pragma Unreferenced (debug);
   procedure debug is
   begin
      Ada.Text_IO.Put_Line (Y_Coord'Image & ":M" & Height'Image);
      Ada.Text_IO.Put_Line (X_Coord'Image & ":A" & Width'Image);
   end debug;


   procedure printline (x : Natural; y : Natural; text : String);
   procedure printline (x : Natural; y : Natural; text : String) is
   begin
      Bitmapped_Drawing.Draw_String
        (Bitmap_Buffer.all,
         Start      => (X => x,
                        Y => y),
         Msg        => text,
         Font       => BMP_Fonts.Font16x24,
         Foreground => (128, 255, 255, 255),
         Background => HAL.Bitmap.Red);
   end printline;


begin
   Display.Initialize;
   Display.Initialize_Layer (1, HAL.Bitmap.ARGB_8888);

   Touch_Panel.Initialize;
   --  Ada.Text_IO.Put_Line (num_selection_width_ar'Length'Image);
   Width  := Bitmap_Buffer.Width;
   Height := Bitmap_Buffer.Height;

   cell_width  := Width / 9;
   hight_start := Height - Width;

   selected_number_at_pos := (0, 0);
   sudoku_ar := (others => 0);

   for i in range_one_to_eight_t loop
      cell_width_ar (i)  := Natural (i) * cell_width + 1;
   end loop;


   num_selection_width := Width / 3;
   for i in range_one_to_three_t loop
      num_selection_width_ar (i)  := Natural (i) * num_selection_width + 1;
   end loop;


   loop
      declare
         State : constant TP_State := Touch_Panel.Get_All_Touch_Points;
      begin
         if State'Length > 0 then
            X_Coord := State (State'First).X;
            Y_Coord := State (State'First).Y;
         end if;
      end;


      case status_marker is
         when MODE_START =>
            draw_sudoku_field;
            draw_sudoku_header;
            if  touched_bottom then
               status_marker := MODE_FIELD_SELECT_PRESSED;
            end if;

         when MODE_FIELD_SELECT_PRESSED =>
            if  touched_bottom then
               draw_sudoku_field;
               draw_selection_button;
               draw_pressed_field_pos;
            end if;

            if touched_left_top  then
               status_marker := MODE_SHOW_NUM_SELECT;
               resetCoord;
            end if;

         when MODE_SHOW_NUM_SELECT =>
            draw_sudoku_field;
            draw_number_selection_field;
            draw_delete_button;
            if  touched_bottom then
               status_marker := MODE_NUM_SELECT_PRESSED;
               transfer_to_complete_array;
               resetCoord;
            end if;

            if  touched_right_top then
               status_marker := MODE_NUM_SELECT_PRESSED;
               delete_from_complete_array;
               resetCoord;
            end if;

         when MODE_NUM_SELECT_PRESSED =>
            draw_sudoku_field;
            draw_calculate_button;
            print_numbers_in_field;
            if  touched_bottom then
               status_marker := MODE_FIELD_SELECT_PRESSED;
            end if;

            if touched_left_top or touched_right_top then
               status_marker := MODE_CALCULATE;
            end if;

         when MODE_CALCULATE =>
            if not Sudoku.solve (Sudoku.sudoku_ar_t (sudoku_ar)) then
               status_marker := MODE_UNRESOLVABLE;
            else
               status_marker := MODE_CALCULATION_DONE;
            end if;

         when MODE_CALCULATION_DONE =>
            draw_sudoku_field;
            draw_done_button;
            print_numbers_in_field;


         when MODE_UNRESOLVABLE =>
            printline (10, 100, "Unresolvable !");


      end case;


      Display.Update_Layers;

      Next_Release := Next_Release + Period;
      delay until Next_Release;

   end loop;
end Sudoku_GUI;
