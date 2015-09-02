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
      Disabled : Boolean := false;
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
