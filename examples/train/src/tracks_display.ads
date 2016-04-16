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

with Screen_Interface; use Screen_Interface;
with Trains;

package Tracks_Display is

   type Track_Points is array (Positive range <>) of Point;

   type Displayed_Track;
   type Track_Access is access all Displayed_Track;

   type Switch_States is (S1, S2);

   type Switch is array (Switch_States) of Track_Access;

   type Entry_Sign_Position is (Top, Left, Bottom, Right);

   type Entry_Sign_Color is (Green, Orange, Red);

   type Displayed_Entry_Sign is record
      Coord    : Point := (0, 0);
      Color    : Entry_Sign_Color := Green;
      Disabled : Boolean := False;
   end record;

   type Displayed_Track (Nb_Points : Positive) is record
      Id           : Trains.Track_Opt_Id := Trains.No_Track_Id;
      Entry_Sign   : Displayed_Entry_Sign;
      Points       : Track_Points (1 .. Nb_Points);
      Is_Straight  : Boolean       := False;
      Switchable   : Boolean       := False;
      Switch_State : Switch_States := S1;
      Exits        : Switch;
   end record;

   type Bogie is record
      Track     : Track_Access;
      Track_Pos : Positive;
   end record;

   type Bogie_Array is array (Positive range <>) of Bogie;

   type Displayed_Train (Bogie_Capacity : Positive) is record
      Id     : Trains.Train_Id;
      Bogies : Bogie_Array (1 .. Bogie_Capacity);
      Speed  : Natural := 0;
   end record;


   procedure Build_Straight_Track
     (Track       : in out Displayed_Track;
      Start, Stop : Point);

   procedure Build_Curve_Track
     (Track          : in out Displayed_Track;
      P1, P2, P3, P4 : Point);

   procedure Connect_Track
     (Track  : in out Displayed_Track;
      E1, E2 : Track_Access);

   function Start_Coord (Track : Displayed_Track) return Point;

   function End_Coord (Track : Displayed_Track) return Point;

   procedure Set_Sign_Position
     (Track : in out Displayed_Track;
      Pos   : Entry_Sign_Position);

   procedure Update_Sign (Track : in out Displayed_Track);

   procedure Change_Switch (Track : in out Displayed_Track);

   procedure Draw_Track (Track : Displayed_Track);

   procedure Draw_Switch (Track : Displayed_Track);

   procedure Init_Train (Train : in out Displayed_Train; Track : Track_Access);

   procedure Move_Train (Train : in out Displayed_Train);

   procedure Draw_Train (Train : Displayed_Train);

   function Location (This : Bogie) return Point;

end Tracks_Display;
