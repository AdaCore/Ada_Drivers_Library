------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with STM32.Board;      use STM32.Board;
with HAL.Bitmap;       use HAL.Bitmap;

with Tracks_Display;   use Tracks_Display;
with Bitmapped_Drawing;
with Trains;           use Trains;
with BMP_Fonts;        use BMP_Fonts;

package body Railroad is

   Block_Size : constant := 40; --  Pixels

   --  Touch Areas
   subtype Spawn_X is Natural range
     Natural (Block_Size * 1.5) .. Natural (Block_Size * 4.5);
   subtype Spawn_Y is Natural range
     Natural (Block_Size * 0.9) .. Natural (Block_Size * 1.9);

   subtype Sw1_X is Natural range
     Natural (Block_Size * 5.0) .. Natural (Block_Size * 6.0 - 1.0);
   subtype Sw1_Y is Natural range
     Natural (Block_Size * 5.0) .. Natural (Block_Size * 6.0);

   subtype Sw2_X is Natural range
     Natural (Block_Size * 1.5) .. Natural (Block_Size * 2.5);
   subtype Sw2_Y is Natural range
     Natural (Block_Size * 4.0) .. Natural (Block_Size * 5.0);

   subtype Sw3_X is Natural range
     Natural (Block_Size * 3.5) .. Natural (Block_Size * 4.5 - 1.0);
   subtype Sw3_Y is Natural range
     Natural (Block_Size * 2.0) .. Natural (Block_Size * 3.3);

   Out_Loop_Track_Nbr : constant Positive := (6 + 4 + 6 + 4);
   In_Loop_Track_Nbr : constant Positive := (3 + 1 + 3 + 1);

   Straight_Tracks : array (1 .. (Out_Loop_Track_Nbr + In_Loop_Track_Nbr)) of
       aliased Displayed_Track (20);
   Curve_Tracks      : array (1 .. 8) of aliased Displayed_Track (16);
   Switch_Tracks     : array (1 .. 3) of aliased Track_Access;
   Spawn_Tracks      : array (1 .. 2) of aliased Displayed_Track (20);
   Connection_Tracks : array (1 .. 3) of aliased Displayed_Track (50);

   Max_Bogies_Per_Train : constant := 13;

   My_Trains : array (Trains.Train_Id) of
     Displayed_Train (Max_Bogies_Per_Train);

   type Location_Point is record
      Coord : Point;
      Used  : Boolean := False;
   end record;

   Trains_Locations : array (Trains.Location) of Location_Point;

   procedure Create_Outer_Loop;
   procedure Create_Inner_Loop;
   procedure Create_Connection_Tracks;
   function Can_Spawn_Train return Boolean;
   procedure Convert_Railway_Map;

   -----------------------
   -- Create_Outer_Loop --
   -----------------------

   procedure Create_Outer_Loop
   is
      WLast             : constant Natural := 6 * Block_Size;
      HLast             : constant Natural := 8 * Block_Size;
      Top_Line_First    : constant Positive := 1;
      Top_Line_Last     : constant Positive := 6;
      Left_Line_First   : constant Positive := Top_Line_Last + 1;
      Left_Line_Last    : constant Positive := 10;
      Bottom_Line_First : constant Positive := Left_Line_Last + 1;
      Bottom_Line_Last  : constant Positive := 16;
      Right_Line_First  : constant Positive := Bottom_Line_Last + 1;
      Right_Line_Last   : constant Positive := 20;
   begin
      --  Top Line
      for Cnt in 1 .. 6 loop
         declare
            X     : constant := Block_Size / 2;
            Index : constant Positive := Cnt;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                                  (X, Block_Size * Cnt),
                                  (X, Block_Size * (Cnt + 1)));
            Set_Sign_Position (Straight_Tracks (Index), Top);

            if Cnt > 1 then
               Connect_Track (Straight_Tracks (Index - 1),
                              Straight_Tracks (Index)'Access,
                              null);
            end if;
         end;
      end loop;

      --  Left Line
      for Cnt in 1 .. 4 loop
         declare
            Y     : constant Natural := HLast - (Block_Size / 2);
            Index : constant Positive := Cnt + Top_Line_Last;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                                  (Block_Size * Cnt, Y),
                                  (Block_Size * (Cnt + 1), Y));
            Set_Sign_Position (Straight_Tracks (Index), Left);
            if Cnt > 1 then
               Connect_Track (Straight_Tracks (Index - 1),
                              Straight_Tracks (Index)'Access,
                              null);
            end if;
         end;
      end loop;

      --  Bottom Line
      for Cnt in 1 .. 6 loop
         declare
            X     : constant Natural := WLast - (Block_Size / 2);
            Index : constant Positive := Cnt + Left_Line_Last;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                                  (X, HLast - Block_Size * Cnt),
                                  (X, HLast - Block_Size * (Cnt + 1)));
            Set_Sign_Position (Straight_Tracks (Index), Bottom);

            if Cnt > 1 then
               Connect_Track (Straight_Tracks (Index - 1),
                              Straight_Tracks (Index)'Access,
                              null);
            end if;
         end;
      end loop;

      --  Right Line
      for Cnt in 1 .. 4 loop
         declare
            Y     : constant := Block_Size / 2;
            Index : constant Positive := Cnt + Bottom_Line_Last;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                                  (WLast - Block_Size * Cnt, Y),
                                  (WLast - Block_Size * (Cnt + 1), Y));
            Set_Sign_Position (Straight_Tracks (Index), Right);

            if Cnt > 1 then
               Connect_Track (Straight_Tracks (Index - 1),
                              Straight_Tracks (Index)'Access,
                              null);
            end if;
         end;
      end loop;

      --  Top Right Curve
      Build_Curve_Track (Curve_Tracks (1),
                         (Block_Size, Block_Size / 2),
                         (Block_Size / 2, Block_Size / 2),
                         (Block_Size / 2, Block_Size / 2),
                         (Block_Size / 2, Block_Size));
      Set_Sign_Position (Curve_Tracks (1), Right);
      Connect_Track (Straight_Tracks (Right_Line_Last),
                     Curve_Tracks (1)'Access, null);
      Connect_Track (Curve_Tracks (1),
                     Straight_Tracks (Top_Line_First)'Access, null);

      --  Top Left Curve
      Build_Curve_Track (Curve_Tracks (2),
                         (Block_Size / 2, HLast -  Block_Size),
                         (Block_Size / 2, HLast -  Block_Size / 2),
                         (Block_Size / 2, HLast -  Block_Size / 2),
                         (Block_Size, HLast -  Block_Size / 2));
      Set_Sign_Position (Curve_Tracks (2), Top);
      Connect_Track (Straight_Tracks (Top_Line_Last),
                     Curve_Tracks (2)'Access, null);
      Connect_Track (Curve_Tracks (2),
                     Straight_Tracks (Left_Line_First)'Access, null);

      --  Bottom Left Curve
      Build_Curve_Track (Curve_Tracks (3),
                         (WLast - Block_Size, HLast -  Block_Size / 2),
                         (WLast - Block_Size / 2, HLast - Block_Size / 2),
                         (WLast - Block_Size / 2, HLast - Block_Size / 2),
                         (WLast - Block_Size / 2, HLast -  Block_Size));
      Set_Sign_Position (Curve_Tracks (3), Left);
      Connect_Track (Straight_Tracks (Left_Line_Last),
                     Curve_Tracks (3)'Access, null);
      Connect_Track (Curve_Tracks (3),
                     Straight_Tracks (Bottom_Line_First)'Access, null);

      --  Bottom Right Curve
      Build_Curve_Track (Curve_Tracks (4),
                         (WLast - Block_Size / 2, Block_Size),
                         (WLast - Block_Size / 2, Block_Size / 2),
                         (WLast - Block_Size / 2, Block_Size / 2),
                         (WLast - Block_Size, Block_Size / 2));
      Set_Sign_Position (Curve_Tracks (4), Bottom);
      Connect_Track (Straight_Tracks (Bottom_Line_Last),
                     Curve_Tracks (4)'Access, null);
      Connect_Track (Curve_Tracks (4),
                     Straight_Tracks (Right_Line_First)'Access, null);

      --  Spawn Track
      Build_Straight_Track (Spawn_Tracks (1),
                            (Block_Size,  2 * Block_Size),
                            (Block_Size,  3 * Block_Size));
      Build_Straight_Track (Spawn_Tracks (2),
                            (Block_Size,  3 * Block_Size),
                            (Block_Size / 2,  4 * Block_Size));
      Connect_Track (Spawn_Tracks (1), Spawn_Tracks (2)'Access, null);
      Connect_Track (Spawn_Tracks (2), Straight_Tracks (4)'Access, null);
   end Create_Outer_Loop;

   -----------------------
   -- Create_Inner_Loop --
   -----------------------

   procedure Create_Inner_Loop is
      HLast             : constant Natural := 8 * Block_Size;
      Top_Line_First    : constant Positive := Out_Loop_Track_Nbr + 1;
      Top_Line_Last     : constant Positive := Top_Line_First + 2;
      Left_Line_First   : constant Positive := Top_Line_Last + 1;
      Left_Line_Last    : constant Positive := Left_Line_First;
      Bottom_Line_First : constant Positive := Left_Line_Last + 1;
      Bottom_Line_Last  : constant Positive := Bottom_Line_First + 2;
      Right_Line_First  : constant Positive := Bottom_Line_Last + 1;
      Right_Line_Last   : constant Positive := Right_Line_First;
   begin

      --  Top Line
      for Cnt in 1 .. 3 loop
         declare
            X      : constant Natural := Block_Size * 2;
            Y_Base : constant := Block_Size * 2;
            Index  : constant Positive := Top_Line_First + Cnt - 1;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                            (X, Y_Base + Block_Size * Cnt),
                            (X, Y_Base + Block_Size * (Cnt + 1)));
            Set_Sign_Position (Straight_Tracks (Index), Top);

            if Cnt > 1 then
               Connect_Track (Straight_Tracks (Index - 1),
                              Straight_Tracks (Index)'Access,
                              null);
            end if;
         end;
      end loop;

      --  There is a switch here so we move the sign to the other side of the
      --  track so that it won't overlap with switche's sign.
      Set_Sign_Position (Straight_Tracks (Top_Line_First + 1), Bottom);

      --  Left Line
      declare
         Index : constant Positive := Left_Line_First;
      begin
         Build_Straight_Track (Straight_Tracks (Index),
                         (Natural (Block_Size * 2.5),
                          Natural (Block_Size * 6.5)),
                         (Natural (Block_Size * 3.5),
                          Natural (Block_Size * 6.5)));
         Set_Sign_Position (Straight_Tracks (Index), Left);
      end;

      --  Bottom Line
      for Cnt in 1 .. 3 loop
         declare
            X      : constant := Natural (Block_Size * 4);
            Y_Base : constant := Block_Size;
            Index  : constant Positive := Cnt + Left_Line_Last;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                            (X, HLast - Y_Base - Block_Size * Cnt),
                            (X, HLast
                               - Y_Base - Block_Size * (Cnt + 1)));
            Set_Sign_Position (Straight_Tracks (Index), Bottom);

            if Cnt > 1 then
               Connect_Track (Straight_Tracks (Index - 1),
                              Straight_Tracks (Index)'Access,
                              null);
            end if;
         end;
      end loop;

      --  Right Line
      declare
         Index : constant Positive := Right_Line_First;
      begin
         Build_Straight_Track (Straight_Tracks (Index),
                         (Natural (Block_Size * 3.5),
                          Natural (Block_Size * 2.5)),
                         (Natural (Block_Size * 2.5),
                          Natural (Block_Size * 2.5)));

         Set_Sign_Position (Straight_Tracks (Index), Right);
      end;

      --  Top Right Curve
      Build_Curve_Track (Curve_Tracks (5),
                   (Natural (Block_Size * 2.5), Natural (Block_Size * 2.5)),
                   (Natural (Block_Size * 2.0), Natural (Block_Size * 2.5)),
                   (Natural (Block_Size * 2.0), Natural (Block_Size * 2.5)),
                   (Natural (Block_Size * 2.0), Natural (Block_Size * 3.0)));
      Set_Sign_Position (Curve_Tracks (5), Right);
      Connect_Track (Straight_Tracks (Right_Line_Last),
                  Curve_Tracks (5)'Access, null);
      Connect_Track (Curve_Tracks (5),
                     Straight_Tracks (Top_Line_First)'Access, null);

      --  Top Left Curve
      Build_Curve_Track (Curve_Tracks (6),
                   (Natural (Block_Size * 2.0), Natural (Block_Size * 6.0)),
                   (Natural (Block_Size * 2.0), Natural (Block_Size * 6.5)),
                   (Natural (Block_Size * 2.0), Natural (Block_Size * 6.5)),
                   (Natural (Block_Size * 2.5), Natural (Block_Size * 6.5)));
      Set_Sign_Position (Curve_Tracks (6), Top);
      Connect_Track (Straight_Tracks (Top_Line_Last),
                     Curve_Tracks (6)'Access, null);
      Connect_Track (Curve_Tracks (6),
                     Straight_Tracks (Left_Line_First)'Access, null);

      --  Bottom Left Curve
      Build_Curve_Track (Curve_Tracks (7),
                   (Natural (Block_Size * 3.5), Natural (Block_Size * 6.5)),
                   (Natural (Block_Size * 4.0), Natural (Block_Size * 6.5)),
                   (Natural (Block_Size * 4.0), Natural (Block_Size * 6.5)),
                   (Natural (Block_Size * 4.0), Natural (Block_Size * 6.0)));
      Set_Sign_Position (Curve_Tracks (7), Left);
      Connect_Track (Straight_Tracks (Left_Line_Last),
                     Curve_Tracks (7)'Access, null);
      Connect_Track (Curve_Tracks (7),
                     Straight_Tracks (Bottom_Line_First)'Access, null);

      --  Bottom Right Curve
      Build_Curve_Track (Curve_Tracks (8),
                   (Natural (Block_Size * 4.0), Natural (Block_Size * 3.0)),
                   (Natural (Block_Size * 4.0), Natural (Block_Size * 2.5)),
                   (Natural (Block_Size * 4.0), Natural (Block_Size * 2.5)),
                   (Natural (Block_Size * 3.5), Natural (Block_Size * 2.5)));
      --  There is a switch here so we move the sign to the other side of the
      --  track so that it won't overlap with switche's sign.
      Set_Sign_Position (Curve_Tracks (8), Top);
      Connect_Track (Straight_Tracks (Bottom_Line_Last),
                     Curve_Tracks (8)'Access, null);
      Connect_Track (Curve_Tracks (8),
                     Straight_Tracks (Right_Line_First)'Access, null);
   end Create_Inner_Loop;

   ------------------------------
   -- Create_Connection_Tracks --
   ------------------------------

   procedure Create_Connection_Tracks is
   begin
      Build_Curve_Track (Connection_Tracks (1),
                   End_Coord (Straight_Tracks (11)),
                   End_Coord (Straight_Tracks (12)),
                   Start_Coord (Straight_Tracks (Out_Loop_Track_Nbr + 6)),
                   Start_Coord (Straight_Tracks (Out_Loop_Track_Nbr + 7)));
      Connect_Track (Connection_Tracks (1),
                     Straight_Tracks (Out_Loop_Track_Nbr + 7)'Access,
                     null);
      Connect_Track (Straight_Tracks (11),
                     Straight_Tracks (12)'Access,
                     Connection_Tracks (1)'Access);

      Switch_Tracks (1) := Straight_Tracks (11)'Access;

      Build_Curve_Track (Connection_Tracks (2),
                   End_Coord (Straight_Tracks (Out_Loop_Track_Nbr + 1)),
                   End_Coord (Straight_Tracks (Out_Loop_Track_Nbr + 2)),
                   Start_Coord (Straight_Tracks (5)),
                   Start_Coord (Straight_Tracks (6)));
      Connect_Track (Connection_Tracks (2), Straight_Tracks (6)'Access, null);
      Connect_Track (Straight_Tracks (Out_Loop_Track_Nbr + 1),
                     Straight_Tracks (Out_Loop_Track_Nbr + 2)'Access,
                     Connection_Tracks (2)'Access);
      Set_Sign_Position (Connection_Tracks (2), Top);

      Switch_Tracks (2) := Straight_Tracks (Out_Loop_Track_Nbr + 1)'Access;

      Build_Curve_Track (Connection_Tracks (3),
                         End_Coord (Straight_Tracks (Out_Loop_Track_Nbr + 7)),
                         End_Coord (Straight_Tracks (Out_Loop_Track_Nbr + 7))
                                    - (0, Block_Size),
                         Start_Coord (Curve_Tracks (4)) + (0, Block_Size),
                         Start_Coord (Curve_Tracks (4)));
      Connect_Track (Connection_Tracks (3), Curve_Tracks (4)'Access, null);
      Set_Sign_Position (Connection_Tracks (3), Bottom);
      Connect_Track (Straight_Tracks (Out_Loop_Track_Nbr + 7),
                     Curve_Tracks (8)'Access,
                     Connection_Tracks (3)'Access);

      Switch_Tracks (3) := Straight_Tracks (Out_Loop_Track_Nbr + 7)'Access;
   end Create_Connection_Tracks;

   ---------------------
   -- Can_Spawn_Train --
   ---------------------

   function Can_Spawn_Train return Boolean is
   begin
      return Cur_Num_Trains < Train_Id'Last
        and then Spawn_Tracks (1).Entry_Sign.Color /= Tracks_Display.Red;
   end Can_Spawn_Train;

   -----------------
   -- Spawn_Train --
   -----------------

   procedure Spawn_Train is
   begin
      if Can_Spawn_Train then
         Init_Train (My_Trains (Cur_Num_Trains), Spawn_Tracks (1)'Access);
         My_Trains (Cur_Num_Trains).Speed := 2;
         Trains.Trains (Cur_Num_Trains) :=
           (Spawn_Tracks (1).Id, 1, Spawn_Tracks (1).Id);

         Trains.Cur_Num_Trains := Trains.Cur_Num_Trains + 1;
         Trains.Track_Signals (Spawn_Tracks (1).Id) := Trains.Red;
      end if;
   end Spawn_Train;

   ---------------------
   -- Step_Simulation --
   ---------------------

   procedure Step_Simulation is
   begin
      for Index in Trains.Train_Id'First .. Trains.Cur_Num_Trains - 1 loop
         Move_Train (My_Trains (Index));
      end loop;

      for Track of Straight_Tracks loop
         Update_Sign (Track);
      end loop;

      for Track of Spawn_Tracks loop
         Update_Sign (Track);
      end loop;

      for Track of Curve_Tracks loop
         Update_Sign (Track);
      end loop;

      for Track of Connection_Tracks loop
         Update_Sign (Track);
      end loop;
   end Step_Simulation;

   -----------------
   -- Draw_Layout --
   -----------------

   procedure Draw_Layout
     (Buffer : HAL.Bitmap.Bitmap_Buffer'Class;
      Init   : Boolean := False)
   is
   begin
      if Init then
         --  Tracks
         for Track of Straight_Tracks loop
            Draw_Track (Buffer, Track);
         end loop;

         for Track of Spawn_Tracks loop
            Draw_Track (Buffer, Track);
         end loop;

         for Track of Curve_Tracks loop
            Draw_Track (Buffer, Track);
         end loop;

         for Track of Connection_Tracks loop
            Draw_Track (Buffer, Track);
         end loop;

         --  Draw touch areas
         Buffer.Draw_Rect
           (Color  => HAL.Bitmap.White,
            X      => Sw1_X'First,
            Y      => Sw1_Y'First,
            Width  => Sw1_X'Last - Sw1_X'First + 1,
            Height => Sw1_Y'Last - Sw1_Y'First + 1);
         Buffer.Draw_Rect
           (Color  => HAL.Bitmap.White,
            X      => Sw2_X'First,
            Y      => Sw2_Y'First,
            Width  => Sw2_X'Last - Sw2_X'First + 1,
            Height => Sw2_Y'Last - Sw2_Y'First + 1);
         Buffer.Draw_Rect
           (Color  => HAL.Bitmap.White,
            X      => Sw3_X'First,
            Y      => Sw3_Y'First,
            Width  => Sw3_X'Last - Sw3_X'First + 1,
            Height => Sw3_Y'Last - Sw3_Y'First + 1);

      else
         --  Tracks
         for Track of Straight_Tracks loop
            Draw_Sign (Buffer, Track);
         end loop;

         for Track of Spawn_Tracks loop
            Draw_Sign (Buffer, Track);
         end loop;

         for Track of Curve_Tracks loop
            Draw_Sign (Buffer, Track);
         end loop;

         for Track of Connection_Tracks loop
            Draw_Sign (Buffer, Track);
         end loop;

         --  Switches
         for Track of Switch_Tracks loop
            if Track /= null then
               Draw_Switch (Buffer, Track.all);
            end if;
         end loop;

         --  Trains
         for Index in Trains.Train_Id'First .. Trains.Cur_Num_Trains - 1 loop
            Draw_Train (Buffer, My_Trains (Index));
         end loop;

         if Can_Spawn_Train then
            Buffer.Draw_Rect
              (Color  => HAL.Bitmap.White,
               X      => Spawn_X'First,
               Y      => Spawn_Y'First,
               Width  => Spawn_X'Last - Spawn_X'First + 1,
               Height => Spawn_Y'Last - Spawn_Y'First + 1);
            Bitmapped_Drawing.Draw_String
              (Buffer,
               Start      => (Spawn_X'First + 5, Spawn_Y'First + 5),
               Msg        => "Touch here to",
               Font       => Font8x8,
               Foreground => HAL.Bitmap.White,
               Background => HAL.Bitmap.Transparent);
            Bitmapped_Drawing.Draw_String
              (Buffer,
               Start      => (Spawn_X'First + 5, Spawn_Y'First + 22),
               Msg        => "spawn a train",
               Font       => Font8x8,
               Foreground => HAL.Bitmap.White,
               Background => HAL.Bitmap.Transparent);
         end if;
      end if;
   end Draw_Layout;

   -------------------------
   -- Convert_Railway_Map --
   -------------------------

   procedure Convert_Railway_Map is
      Cur_Loc   : Trains.Location := Trains.Location'First;
      Cur_Track : Trains.Track_Id := Trains.Track_Id'First;

      --  Return Location coresponding to a Point.
      --  Create a new Locatio if needed.
      function Get_Loc (P : Point) return Trains.Location;
      procedure Add_Track (Track : in out Displayed_Track);
      procedure Add_Previous_Track (Loc : Trains.Location;
                                    Id  : Trains.Track_Id);


      -------------
      -- Get_Loc --
      -------------

      function Get_Loc (P : Point) return Trains.Location is
      begin
         for Index in Trains.Location loop
            exit when not Trains_Locations (Index).Used;
            if Trains_Locations (Index).Coord = P then
               return Index;
            end if;
         end loop;

         Trains_Locations (Cur_Loc).Used := True;
         Trains_Locations (Cur_Loc).Coord := P;
         Cur_Loc := Cur_Loc + 1;
         return Cur_Loc - 1;
      end Get_Loc;

      ---------------
      -- Add_Track --
      ---------------

      procedure Add_Track (Track : in out Displayed_Track) is
      begin
         Track.Id := Cur_Track;
         --  Define the tracks From and To locations
         Trains.Tracks (Cur_Track) := (Get_Loc (Start_Coord (Track)),
                                       Get_Loc (End_Coord (Track)),
                                       Track.Points'Last);
         Cur_Track := Cur_Track + 1;
      end Add_Track;

      ------------------------
      -- Add_Previous_Track --
      ------------------------

      procedure Add_Previous_Track (Loc : Trains.Location;
                                    Id  : Trains.Track_Id)
      is
      begin
         for Index in Trains.Prev_Id loop
            if Trains.Previous_Tracks (Loc) (Index) = Trains.No_Track_Id then
               Trains.Previous_Tracks (Loc) (Index) := Id;
               return;
            end if;
         end loop;
      end Add_Previous_Track;

   begin
      --  Create track map for Trains package
      for Track of Straight_Tracks loop
         Add_Track (Track);
         Add_Previous_Track (Get_Loc (End_Coord (Track)), Track.Id);
      end loop;
      for Track of Curve_Tracks loop
         Add_Track (Track);
         Add_Previous_Track (Get_Loc (End_Coord (Track)), Track.Id);
      end loop;
      for Track of Connection_Tracks loop
         Add_Track (Track);
         Add_Previous_Track (Get_Loc (End_Coord (Track)), Track.Id);
      end loop;
      for Track of Spawn_Tracks loop
         Add_Track (Track);
         Add_Previous_Track (Get_Loc (End_Coord (Track)), Track.Id);
      end loop;
   end Convert_Railway_Map;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create_Outer_Loop;
      Create_Inner_Loop;
      Create_Connection_Tracks;
      Convert_Railway_Map;
      Draw_Layout (Display.Get_Hidden_Buffer (1), True);
      Display.Update_Layer (1);
   end Initialize;

   ----------------------
   -- Respond_To_Touch --
   ----------------------

   procedure Respond_To_Touch (X, Y : Natural) is
   begin
      if X in Spawn_X and then Y in Spawn_Y then
         Spawn_Train;
      end if;

      if X in Sw1_X and then Y in Sw1_Y then
         Change_Switch (Switch_Tracks (1).all);
      end if;

      if X in Sw2_X and then Y in Sw2_Y then
         Change_Switch (Switch_Tracks (2).all);
      end if;

      if X in Sw3_X and then Y in Sw3_Y then
         Change_Switch (Switch_Tracks (3).all);
      end if;
   end Respond_To_Touch;

end Railroad;
