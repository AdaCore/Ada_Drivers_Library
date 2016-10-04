------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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

with nRF51.GPIO;    use nRF51.GPIO;
with nRF51.Device;  use nRF51.Device;
with MicroBit.Time; use MicroBit.Time;

package body MicroBit.Display is

   Is_Initialized : Boolean := False;

   Bitmap : array (Coord, Coord) of Boolean := (others => (others => False));
   Current_X, Current_Y : Coord := 0;

   subtype Row_Range is Natural range 1 .. 3;
   subtype Column_Range is Natural range 1 .. 9;

   type LED_Point is record
      Row_Id    : Row_Range;
      Column_Id : Column_Range;
   end record;

   Row_Points : array (Row_Range) of GPIO_Point :=
     (P13, P14, P15);

   Column_Points : array (Column_Range) of GPIO_Point :=
     (P04, P05, P06, P07, P08, P09, P10, P11, P12);

   Map : constant array (Coord, Coord) of LED_Point :=
     (((1, 1), (3, 4), (2, 2), (1, 8), (3, 3)),
      ((2, 4), (3, 5), (1, 9), (1, 7), (2, 7)),
      ((1, 2), (3, 6), (2, 3), (1, 6), (3, 1)),
      ((2, 5), (3, 7), (3, 9), (1, 5), (2, 6)),
      ((1, 3), (3, 8), (2, 1), (1, 4), (3, 2))
     );

   procedure Tick_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Conf : GPIO_Configuration;
   begin
      Conf.Mode      := Mode_Out;
      Conf.Resistors := Pull_Up;

      for Point of Row_Points loop
         Point.Configure_IO (Conf);
         Point.Clear;
      end loop;

      for Point of Column_Points loop
         Point.Configure_IO (Conf);
         Point.Set;
      end loop;

      Is_Initialized := True;

      if not Tick_Subscribe (Tick_Handler'Access) then
         raise Program_Error;
      end if;
   end Initialize;

   ------------------
   -- Tick_Handler --
   ------------------

   procedure Tick_Handler is
   begin

      --  Turn Off

      --  Row source current
      Row_Points (Map (Current_X, Current_Y).Row_Id).Clear;
      --  Column sink current
      Column_Points (Map (Current_X, Current_Y).Column_Id).Set;

      if Current_X = Coord'Last then
         Current_X := Coord'First;

         if Current_Y = Coord'Last then
            Current_Y := Coord'First;
         else
            Current_Y := Current_Y + 1;
         end if;
      else
         Current_X := Current_X + 1;
      end if;


      --  Turn on?
      if Bitmap (Current_X, Current_Y) then
         --  Row source current
         Row_Points (Map (Current_X, Current_Y).Row_Id).Set;
         --  Column sink current
         Column_Points (Map (Current_X, Current_Y).Column_Id).Clear;
      end if;
   end Tick_Handler;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is
   begin
      return Is_Initialized;
   end Initialized;

   ---------
   -- Set --
   ---------

   procedure Set (X, Y : Coord) is
   begin
      Bitmap (X, Y) := True;
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (X, Y : Coord) is
   begin
      Bitmap (X, Y) := False;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Bitmap := (others => (others => False));
   end Clear;

   -------------
   -- Display --
   -------------

   procedure Display (C : Character) is
   begin
      Clear;
      case C is
         when 'A' =>
            Bitmap :=  ((False, True,  True, True,  True),
                        (True,  False, True, False, False),
                        (True,  False, True, False, False),
                        (True,  False, True, False, False),
                        (False, True,  True, True,  True));
         when 'B' =>
            Bitmap :=  ((True,  True,  True,  True,  True),
                        (True,  False, True,  False, True),
                        (True,  False, True,  False, True),
                        (True,  False, True,  False, True),
                        (False, True,  False, True,  False));
         when 'C' =>
            Bitmap := ((False, True,  True,  True,  False),
                       (True,  False, False, False, True),
                       (True,  False, False, False, True),
                       (True,  False, False, False, True),
                       (True, False, False, False, True));
         when 'D' =>
            Bitmap := ((True,  True,  True,  True,  True),
                       (True,  False, False, False, True),
                       (True,  False, False, False, True),
                       (True,  False, False, False, True),
                       (False, True,  True,  True,  False));
         when 'E' =>
            Bitmap :=  ((True,  True,  True,  True,  True),
                        (True,  False, True,  False, True),
                        (True,  False, True,  False, True),
                        (True,  False, False, False, True),
                        (True,  False, False, False, True));
         when 'F' =>
            Bitmap :=  ((True,  True, True,  True,  True),
                        (True, False, True,  False, False),
                        (True, False, True,  False, False),
                        (True, False, False, False, False),
                        (True, False, False, False, False));
         when 'G' =>
            Bitmap := ((False, True,  True,  True,  False),
                       (True,  False, False, False, True),
                       (True,  False, True,  False, True),
                       (True,  False, True,  False, True),
                       (False, False, True,  True, False));
         when 'H' =>
            Bitmap := ((True,  True,  True, True,  True),
                       (False, False, True, False, False),
                       (False, False, True, False, False),
                       (False, False, True, False, False),
                       (True,  True,  True, True,  True));
         when 'I' =>
            Bitmap := ((False, False, False, False, False),
                       (False, False, False, False, False),
                       (True,  True,  True,  True,  True),
                       (False, False, False, False, False),
                       (False, False, False, False, False));
         when 'J' =>
            Bitmap := ((False, False, False, True,  False),
                       (False, False, False, False, True),
                       (False, False, False, False, True),
                       (False, False, False, False, True),
                       (True,  True,  True,  True,  False));
         when 'K' =>
            Bitmap :=  ((True,  True,  True,  True,  True),
                        (False, False, True,  False, False),
                        (False, True,  False, True,  False),
                        (True,  False, False, False, True),
                        (False, False, False, False, False));
         when 'L' =>
            Bitmap := ((True,  True,  True,  True,  True),
                       (False, False, False, False, True),
                       (False, False, False, False, True),
                       (False, False, False, False, True),
                       (False, False, False, False, True));
         when 'M' =>
            Bitmap := ((True,  True,  True,  True,  True),
                       (False, True,  False, False, False),
                       (False, False, True,  False, False),
                       (False, True,  False, False, False),
                       (True,  True,  True,  True,  True));
         when 'N' =>
            Bitmap := ((True,  True,  True,  True,  True),
                       (False, True,  False, False, False),
                       (False, False, True,  False, False),
                       (False, False, False, True,  False),
                       (True,  True,  True,  True,  True));
         when 'O' =>
            Bitmap := ((False, True,  True,  True,  False),
                       (True,  False, False, False, True),
                       (True,  False, False, False, True),
                       (True,  False, False, False, True),
                       (False, True,  True,  True,  False));
         when 'P' =>
            Bitmap :=  ((True,  True,  True,  True,  True),
                        (True,  False, True,  False, False),
                        (True,  False, True,  False, False),
                        (True,  False, True,  False, False),
                        (False, True,  False, False, False));
         when 'Q' =>
            Bitmap := ((False, True,  True,  True,  False),
                       (True,  False, False, False, True),
                       (True,  False, False, False, True),
                       (True,  False, False, True,  True),
                       (False, True,  True,  True,  True));
         when 'R' =>
            Bitmap :=  ((True,  True,  True,  True,  True),
                        (True,  False, True,  False, False),
                        (True,  False, True,  False, False),
                        (True,  False, True,  True, False),
                        (False, True,  False, False, True));
         when 'S' =>
            Bitmap :=  ((False, True,  False, False, True),
                        (True,  False, True,  False, True),
                        (True,  False, True,  False, True),
                        (True,  False, True,  False, True),
                        (True,  False, False, True,  False));
         when 'T' =>
            Bitmap := ((True, False, False, False, False),
                       (True, False, False, False, False),
                       (True, True,  True,  True,  True),
                       (True, False, False, False, False),
                       (True, False, False, False, False));
         when 'U' =>
            Bitmap := ((True,  True,  True,  True,  False),
                       (False, False, False, False, True),
                       (False, False, False, False, True),
                       (False, False, False, False, True),
                       (True,  True,  True,  True,  False));
         when 'V' =>
            Bitmap := ((True,  True,  False, False, False),
                       (False, False, True,  True,  False),
                       (False, False, False, False, True),
                       (False, False, True,  True,  False),
                       (True,  True,  False, False, False));
         when 'W' =>
            Bitmap := ((True,  True,  True,  False, False),
                       (False, False, False, True,  True),
                       (True,  True,  True,  False, False),
                       (False, False, False, True,  True),
                       (True,  True,  True,  False, False));
         when 'X' =>
            Bitmap := ((True,  False, False, False, True),
                       (False, True,  False, True,  False),
                       (False, False, True,  False, False),
                       (False, True,  False, True,  False),
                       (True,  False, False, False, True));
         when 'Y' =>
            Bitmap := ((True,  False, False, False, False),
                       (False, True,  False, False, False),
                       (False, False, True,  True,  True),
                       (False, True,  False, False, False),
                       (True,  False, False, False, False));
         when 'Z' =>
            Bitmap := ((True, False, False, False, True),
                       (True, False, False, True,  True),
                       (True, False, True,  False, True),
                       (True, True,  False, False, True),
                       (True, False, False, False, True));

         when others => null;
      end case;
   end Display;

end MicroBit.Display;
