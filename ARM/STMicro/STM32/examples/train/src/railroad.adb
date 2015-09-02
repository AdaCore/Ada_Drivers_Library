------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Tracks_Display;   use Tracks_Display;
with Screen_Interface; use Screen_Interface;
with Drawing;      --          use Drawing;
with Trains;           use Trains;
with BMP_Fonts;        use BMP_Fonts;

package body Railroad is

   Block_Size : constant := 40; --  Pixels

   --  Touch Areas
   subtype Spawn_X is Screen_Interface.Width range
     Width (Block_Size * 1.5) .. Width (Block_Size * 4.5);
   subtype Spawn_Y is Height range
     Height (Block_Size * 0.9) .. Height (Block_Size * 1.9);

   subtype Sw1_X is Width range
     Width (Block_Size * 5.0) .. Width (Block_Size * 6.0 - 1.0);
   subtype Sw1_Y is Height range
     Height (Block_Size * 5.0) .. Height (Block_Size * 6.0);

   subtype Sw2_X is Width range
     Width (Block_Size * 1.5) .. Width (Block_Size * 2.5);
   subtype Sw2_Y is Height range
     Height (Block_Size * 4.0) .. Height (Block_Size * 5.0);

   subtype Sw3_X is Width range
     Width (Block_Size * 3.5) .. Width (Block_Size * 4.5 - 1.0);
   subtype Sw3_Y is Height range
     Height (Block_Size * 2.0) .. Height (Block_Size * 3.3);

   Out_Loop_Track_Nbr : constant Positive := (6 + 4 + 6 + 4);
   In_Loop_Track_Nbr : constant Positive := (3 + 1 + 3 + 1);

   Straight_Tracks : array (1 .. (Out_Loop_Track_Nbr + In_Loop_Track_Nbr)) of
       aliased Displayed_Track (20);
   Curve_Tracks      : array (1 .. 8) of aliased Displayed_Track (16);
   Switch_Tracks     : array (1 .. 3) of aliased Track_Access;
   Spawn_Tracks      : array (1 .. 2) of aliased Displayed_Track (20);
   Connection_Tracks : array (1 .. 3) of aliased Displayed_Track (50);

   Max_Bogies_Per_Train : constant := 13;

   My_Trains : array (Trains.Train_Id) of Displayed_Train (Max_Bogies_Per_Train);

   type Location_Point is record
      Coord : Point;
      Used  : Boolean := False;
   end record;

   Trains_Locations : array (Trains.Location) of Location_Point;

   -----------------------
   -- Create_Outer_Loop --
   -----------------------

   procedure Create_Outer_Loop is
      WLast : constant := Width'Last + 1;
      HLast : constant := Height'Last + 1;
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
                                  (X, Height (Block_Size * Cnt)),
                                  (X, Height (Block_Size * (Cnt + 1))));
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
            Y     : constant := HLast - (Block_Size / 2);
            Index : constant Positive := Cnt + Top_Line_Last;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                                  (Width (Block_Size * Cnt), Y),
                                  (Width (Block_Size * (Cnt + 1)), Y));
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
            X     : constant := WLast - (Block_Size / 2);
            Index : constant Positive := Cnt + Left_Line_Last;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                                  (X, HLast - Height (Block_Size * Cnt)),
                                  (X, HLast - Height (Block_Size * (Cnt + 1))));
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
                                  (WLast - Width (Block_Size * Cnt), Y),
                                  (WLast - Width (Block_Size * (Cnt + 1)), Y));
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
                         (WLast - Block_Size/ 2, HLast - Block_Size / 2),
                         (WLast - Block_Size/ 2, HLast - Block_Size / 2),
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
                            (Width (Block_Size),  Height (2 * Block_Size)),
                            (Width (Block_Size),  Height (3 * Block_Size)));
      Build_Straight_Track (Spawn_Tracks (2),
                            (Width (Block_Size),  Height (3 * Block_Size)),
                            (Width (Block_Size / 2),  Height (4 * Block_Size)));
      Connect_Track (Spawn_Tracks (1), Spawn_Tracks (2)'Access, null);
      Connect_Track (Spawn_Tracks (2), Straight_Tracks (4)'Access, null);
   end Create_Outer_Loop;

   -----------------------
   -- Create_Inner_Loop --
   -----------------------

   procedure Create_Inner_Loop is
      HLast : constant := Height'Last + 1;
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
            X      : constant Width := Width (Block_Size * 2);
            Y_Base : constant := Block_Size * 2;
            Index  : constant Positive := Top_Line_First + Cnt - 1;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                            (X, Height (Y_Base + Block_Size * Cnt)),
                            (X, Height (Y_Base + Block_Size * (Cnt + 1))));
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
                         (Width (Block_Size * 2.5),
                          Height (Block_Size * 6.5)),
                         (Width (Block_Size * 3.5),
                          Height (Block_Size * 6.5)));
         Set_Sign_Position (Straight_Tracks (Index), Left);
      end;

      --  Bottom Line
      for Cnt in 1 .. 3 loop
         declare
            X     : constant := Width (Block_Size * 4);
            Y_Base : constant := Block_Size;
            Index : constant Positive := Cnt + Left_Line_Last;
         begin
            Build_Straight_Track (Straight_Tracks (Index),
                            (X, HLast - Height (Y_Base + Block_Size * Cnt)),
                            (X, HLast
                               - Height (Y_Base + Block_Size * (Cnt + 1))));
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
                         (Width (Block_Size * 3.5),
                          Height (Block_Size * 2.5)),
                         (Width (Block_Size * 2.5),
                          Height (Block_Size * 2.5)));

         Set_Sign_Position (Straight_Tracks (Index), Right);
      end;

      --  Top Right Curve
      Build_Curve_Track (Curve_Tracks (5),
                   (Width (Block_Size * 2.5), Height (Block_Size * 2.5)),
                   (Width (Block_Size * 2.0), Height (Block_Size * 2.5)),
                   (Width (Block_Size * 2.0), Height (Block_Size * 2.5)),
                   (Width (Block_Size * 2.0), Height (Block_Size * 3.0)));
      Set_Sign_Position (Curve_Tracks (5), Right);
      Connect_Track (Straight_Tracks (Right_Line_Last),
                  Curve_Tracks (5)'Access, null);
      Connect_Track (Curve_Tracks (5),
                     Straight_Tracks (Top_Line_First)'Access, null);

      --  Top Left Curve
      Build_Curve_Track (Curve_Tracks (6),
                   (Width (Block_Size * 2.0), Height (Block_Size * 6.0)),
                   (Width (Block_Size * 2.0), Height (Block_Size * 6.5)),
                   (Width (Block_Size * 2.0), Height (Block_Size * 6.5)),
                   (Width (Block_Size * 2.5), Height (Block_Size * 6.5)));
      Set_Sign_Position (Curve_Tracks (6), Top);
      Connect_Track (Straight_Tracks (Top_Line_Last),
                     Curve_Tracks (6)'Access, null);
      Connect_Track (Curve_Tracks (6),
                     Straight_Tracks (Left_Line_First)'Access, null);

      --  Bottom Left Curve
      Build_Curve_Track (Curve_Tracks (7),
                   (Width (Block_Size * 3.5), Height (Block_Size * 6.5)),
                   (Width (Block_Size * 4.0), Height (Block_Size * 6.5)),
                   (Width (Block_Size * 4.0), Height (Block_Size * 6.5)),
                   (Width (Block_Size * 4.0), Height (Block_Size * 6.0)));
      Set_Sign_Position (Curve_Tracks (7), Left);
      Connect_Track (Straight_Tracks (Left_Line_Last),
                     Curve_Tracks (7)'Access, null);
      Connect_Track (Curve_Tracks (7),
                     Straight_Tracks (Bottom_Line_First)'Access, null);

      --  Bottom Right Curve
      Build_Curve_Track (Curve_Tracks (8),
                   (Width (Block_Size * 4.0), Height (Block_Size * 3.0)),
                   (Width (Block_Size * 4.0), Height (Block_Size * 2.5)),
                   (Width (Block_Size * 4.0), Height (Block_Size * 2.5)),
                   (Width (Block_Size * 3.5), Height (Block_Size * 2.5)));
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
                   end_Coord (Straight_Tracks (Out_Loop_Track_Nbr + 2)),
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

   procedure Draw_Layout (Init : Boolean := False) is
   begin
      if Init then
         --  Tracks
         for Track of Straight_Tracks loop
            Draw_Track (Track);
         end loop;

         for Track of Spawn_Tracks loop
            Draw_Track (Track);
         end loop;

         for Track of Curve_Tracks loop
            Draw_Track (Track);
         end loop;

         for Track of Connection_Tracks loop
            Draw_Track (Track);
         end loop;
      end if;

      --  Switches
      for Track of Switch_Tracks loop
         if Track /= null then
            Draw_Switch (Track.all);
         end if;
      end loop;

      --  Trains
      for Index in Trains.Train_Id'First .. Trains.Cur_Num_Trains - 1 loop
         Draw_Train (My_Trains (Index));
      end loop;

      --  Draw touch areas
      Drawing.Draw_Rectangle ((Sw1_X'First, Sw1_Y'First),
            (Sw1_X'Last, Sw1_Y'Last),
            White);
      Drawing.Draw_Rectangle ((Sw2_X'First, Sw2_Y'First),
            (Sw2_X'Last, Sw2_Y'Last),
            White);
      Drawing.Draw_Rectangle ((Sw3_X'First, Sw3_Y'First),
            (Sw3_X'Last, Sw3_Y'Last),
            White);

      if Can_Spawn_Train then
         Drawing.Draw_Rectangle
           ((Spawn_X'First, Spawn_Y'First),
            (Spawn_X'Last, Spawn_Y'Last),
            White);
         Drawing.Draw_String
           (Start      => (Spawn_X'First + 5, Spawn_Y'First + 5),
            Msg        => "Touch here to",
            Font       => Font8x8,
            Foreground => Screen_Interface.White,
            Background => Screen_Interface.Black);
         Drawing.Draw_String
           (Start      => (Spawn_X'First + 5, Spawn_Y'First + 22),
            Msg        => "spawn a train",
            Font       => Font8x8,
            Foreground => Screen_Interface.White,
            Background => Screen_Interface.Black);
      else
         Drawing.Fill_Rectangle
           ((Spawn_X'First, Spawn_Y'First),
            (Spawn_X'Last, Spawn_Y'Last),
            Screen_Interface.Black);
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

      procedure Add_Previous_Track
        (Loc : Trains.Location;
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
      Draw_Layout (True);
   end Initialize;

   ----------------------
   -- Respond_To_Touch --
   ----------------------

   procedure Respond_To_Touch (P : Point) is
   begin
      if P.X in Spawn_X and then P.Y in Spawn_Y then
         Spawn_Train;
      end if;

      if P.X in Sw1_X and then P.Y in Sw1_Y then
         Change_Switch (Switch_Tracks (1).all);
      end if;

      if P.X in Sw2_X and then P.Y in Sw2_Y then
         Change_Switch (Switch_Tracks (2).all);
      end if;

      if P.X in Sw3_X and then P.Y in Sw3_Y then
         Change_Switch (Switch_Tracks (3).all);
      end if;
   end Respond_To_Touch;

end Railroad;
