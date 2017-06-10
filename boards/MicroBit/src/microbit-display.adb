------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016-2017, AdaCore                      --
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

with HAL;                  use HAL;
with nRF51.GPIO;           use nRF51.GPIO;
with nRF51.Device;         use nRF51.Device;
with MicroBit.Time;        use MicroBit.Time;
with System.Machine_Code;  use System.Machine_Code;

package body MicroBit.Display is

   type Animation_Mode is (None, Scroll_Text);

   Animation_Step_Duration_Ms : Natural := 200;
   --  How many milliseconds between two animation steps

   Animation_Elapsed : Natural := 0;
   --  How many milliseconds since the last animation step

   Animation_State : Animation_Mode := None;
   --  Current animation

   subtype Width is Natural range
     Coord'First .. Coord'First + Coord'Range_Length * 2;

   Bitmap : array (Width, Coord) of Boolean := (others => (others => False));
   --  The bitmap width is 2 time the display size so we can instert hidden
   --  characters to the right of the screen and scroll them in with the
   --  Shift_Left procedure.

   Current_X, Current_Y : Coord := 0;
   --  Current coordinate in LED matrix scan

   ----------------------
   -- Pixel to IO Pins --
   ----------------------

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
   --

   --------------------
   -- Text scrolling --
   --------------------

   Scroll_Text_Buffer : String (1 .. Scroll_Text_Max_Length) :=
     (others => ASCII.NUL);
   --  Buffer to stored the scroll text

   Scroll_Text_Length : Natural := 0;
   --  Length of the text stored in the Scroll_Text_Buffer

   Scroll_Text_Index  : Natural := 0;
   --  Index of the character to display next

   Scroll_Position    : Natural := 0;
   --  Scroll position in the screen

   ----------
   -- Font --
   ----------

   type Glyph is array (0 .. 4) of UInt5;
   Font : constant array (0 .. 93) of Glyph :=
     ((2#00100#, -- !
       2#00100#,
       2#00100#,
       2#00000#,
       2#00100#),
      (2#01010#, -- "
       2#01010#,
       2#00000#,
       2#00000#,
       2#00000#),
      (2#01010#, -- #
       2#11111#,
       2#01010#,
       2#11111#,
       2#01010#),
      (2#11110#, -- $
       2#00101#,
       2#01110#,
       2#00100#,
       2#01111#),
      (2#10001#, -- %
       2#01000#,
       2#00100#,
       2#00010#,
       2#10001#),
      (2#00100#, -- &
       2#01010#,
       2#00100#,
       2#01010#,
       2#10100#),
      (2#01000#, -- '
       2#00100#,
       2#00000#,
       2#00000#,
       2#00000#),
      (2#01000#, -- (
       2#00100#,
       2#00100#,
       2#00100#,
       2#01000#),
      (2#00010#, -- )
       2#00100#,
       2#00100#,
       2#00100#,
       2#00010#),
      (2#00000#, -- *
       2#00100#,
       2#01010#,
       2#00100#,
       2#00000#),
      (2#00000#, -- +
       2#00100#,
       2#01110#,
       2#00100#,
       2#00000#),
      (2#00000#, -- ,
       2#00000#,
       2#00000#,
       2#00100#,
       2#00010#),
      (2#00000#, -- -
       2#00000#,
       2#01110#,
       2#00000#,
       2#00000#),
      (2#00000#, --  .
       2#00000#,
       2#00000#,
       2#00000#,
       2#00010#),
      (2#10000#, -- /
       2#01000#,
       2#00100#,
       2#00010#,
       2#00001#),
      (2#01110#, -- 0
       2#10001#,
       2#10001#,
       2#10001#,
       2#01110#),
      (2#00100#, -- 1
       2#00110#,
       2#00100#,
       2#00100#,
       2#00100#),
      (2#01110#, -- 2
       2#10001#,
       2#01000#,
       2#00100#,
       2#11111#),
      (2#01111#, -- 3
       2#10000#,
       2#01111#,
       2#10000#,
       2#01111#),
      (2#01000#, -- 4
       2#01010#,
       2#01001#,
       2#11111#,
       2#01000#),
      (2#11111#, -- 5
       2#00001#,
       2#01111#,
       2#10000#,
       2#01111#),
      (2#01110#, -- 6
       2#00001#,
       2#00111#,
       2#01001#,
       2#01110#),
      (2#01110#, -- 7
       2#01000#,
       2#00100#,
       2#00100#,
       2#00100#),
      (2#01110#, -- 8
       2#10001#,
       2#01110#,
       2#10001#,
       2#01110#),
      (2#01110#, -- 9
       2#10001#,
       2#11110#,
       2#10000#,
       2#01110#),
      (2#00000#, -- :
       2#00100#,
       2#00000#,
       2#00100#,
       2#00000#),
      (2#00000#, -- ;
       2#00100#,
       2#00000#,
       2#00100#,
       2#00010#),
      (2#00000#, -- <
       2#00100#,
       2#00010#,
       2#00100#,
       2#00000#),
      (2#00000#, -- =
       2#01110#,
       2#00000#,
       2#01110#,
       2#00000#),
      (2#00000#, -- >
       2#00100#,
       2#01000#,
       2#00100#,
       2#00000#),
      (2#00100#, -- ?
       2#01000#,
       2#00100#,
       2#00000#,
       2#00100#),
      (2#01110#, -- @
       2#10001#,
       2#10101#,
       2#10001#,
       2#00110#),
      (2#01110#, -- A
       2#10001#,
       2#11111#,
       2#10001#,
       2#10001#),
      (2#01111#, -- B
       2#10001#,
       2#01111#,
       2#10001#,
       2#01111#),
      (2#11110#, -- C
       2#00001#,
       2#00001#,
       2#00001#,
       2#11110#),
      (2#01111#, -- D
       2#10001#,
       2#10001#,
       2#10001#,
       2#01111#),
      (2#11111#, -- E
       2#00001#,
       2#00111#,
       2#00001#,
       2#11111#),
      (2#11111#, -- F
       2#00001#,
       2#00111#,
       2#00001#,
       2#00001#),
      (2#11110#, -- G
       2#00001#,
       2#11101#,
       2#10001#,
       2#01110#),
      (2#10001#, -- H
       2#10001#,
       2#11111#,
       2#10001#,
       2#10001#),
      (2#00100#, -- I
       2#00100#,
       2#00100#,
       2#00100#,
       2#00100#),
      (2#10000#, -- J
       2#10000#,
       2#10000#,
       2#10001#,
       2#01110#),
      (2#01001#, -- K
       2#00101#,
       2#00011#,
       2#00101#,
       2#01001#),
      (2#00001#, -- L
       2#00001#,
       2#00001#,
       2#00001#,
       2#11111#),
      (2#10001#, -- M
       2#11011#,
       2#10101#,
       2#10001#,
       2#10001#),
      (2#10001#, -- N
       2#10011#,
       2#10101#,
       2#11001#,
       2#10001#),
      (2#01110#, -- O
       2#10001#,
       2#10001#,
       2#10001#,
       2#01110#),
      (2#01111#, -- P
       2#10001#,
       2#01111#,
       2#00001#,
       2#00001#),
      (2#01110#, -- Q
       2#10001#,
       2#10001#,
       2#11001#,
       2#11110#),
      (2#01111#, -- R
       2#10001#,
       2#01111#,
       2#01001#,
       2#10001#),
      (2#11110#, -- S
       2#00001#,
       2#01110#,
       2#10000#,
       2#01111#),
      (2#11111#, -- T
       2#00100#,
       2#00100#,
       2#00100#,
       2#00100#),
      (2#10001#, -- U
       2#10001#,
       2#10001#,
       2#10001#,
       2#01110#),
      (2#10001#, -- V
       2#10001#,
       2#01010#,
       2#01010#,
       2#00100#),
      (2#10101#, -- W
       2#10101#,
       2#10101#,
       2#01010#,
       2#01010#),
      (2#10001#, -- X
       2#01010#,
       2#00100#,
       2#01010#,
       2#10001#),
      (2#10001#, -- Y
       2#01010#,
       2#00100#,
       2#00100#,
       2#00100#),
      (2#11111#, -- Z
       2#01000#,
       2#00100#,
       2#00010#,
       2#11111#),
      (2#01110#, -- [
       2#00010#,
       2#00010#,
       2#00010#,
       2#01110#),
      (2#00001#, -- \
       2#00010#,
       2#00100#,
       2#01000#,
       2#10000#),
      (2#01110#, -- ]
       2#01000#,
       2#01000#,
       2#01000#,
       2#01110#),
      (2#00100#, -- ^
       2#01010#,
       2#10001#,
       2#00000#,
       2#00000#),
      (2#00000#, -- _
       2#00000#,
       2#00000#,
       2#00000#,
       2#11111#),
      (2#00010#, -- `
       2#00100#,
       2#00000#,
       2#00000#,
       2#00000#),
      (2#01111#, -- a
       2#10000#,
       2#11110#,
       2#10001#,
       2#11110#),
      (2#00001#, -- b
       2#01111#,
       2#10001#,
       2#10001#,
       2#01111#),
      (2#01110#, -- c
       2#10001#,
       2#00001#,
       2#10001#,
       2#01110#),
      (2#10000#, -- d
       2#11110#,
       2#10001#,
       2#10001#,
       2#11110#),
      (2#01110#, -- e
       2#10001#,
       2#11111#,
       2#00001#,
       2#11110#),
      (2#11110#, -- f
       2#00001#,
       2#00111#,
       2#00001#,
       2#00001#),
      (2#01110#, -- g
       2#10001#,
       2#11110#,
       2#10000#,
       2#01111#),
      (2#00001#, -- h
       2#01111#,
       2#10001#,
       2#10001#,
       2#10001#),
      (2#00100#, -- i
       2#00000#,
       2#00100#,
       2#00100#,
       2#00100#),
      (2#10000#, -- j
       2#10000#,
       2#10000#,
       2#10000#,
       2#01111#),
      (2#10001#, -- k
       2#01001#,
       2#00111#,
       2#01001#,
       2#10001#),
      (2#00001#, -- l
       2#00001#,
       2#00001#,
       2#00001#,
       2#11110#),
      (2#01010#, -- m
       2#10101#,
       2#10101#,
       2#10101#,
       2#10101#),
      (2#01111#, -- n
       2#10001#,
       2#10001#,
       2#10001#,
       2#10001#),
      (2#01110#, -- o
       2#10001#,
       2#10001#,
       2#10001#,
       2#01110#),
      (2#01111#, -- p
       2#10001#,
       2#10001#,
       2#01111#,
       2#00001#),
      (2#11110#, -- q
       2#10001#,
       2#10001#,
       2#11110#,
       2#10000#),
      (2#01101#, -- r
       2#10011#,
       2#00001#,
       2#00001#,
       2#00001#),
      (2#11110#, -- s
       2#00001#,
       2#01110#,
       2#10000#,
       2#01111#),
      (2#00001#, -- t
       2#00111#,
       2#00001#,
       2#10001#,
       2#01110#),
      (2#10001#, -- u
       2#10001#,
       2#10001#,
       2#11001#,
       2#10110#),
      (2#10001#, -- v
       2#10001#,
       2#01010#,
       2#01010#,
       2#00100#),
      (2#10101#, -- w
       2#10101#,
       2#10101#,
       2#10101#,
       2#01010#),
      (2#10001#, -- x
       2#10001#,
       2#01110#,
       2#10001#,
       2#10001#),
      (2#10001#, -- y
       2#10001#,
       2#11110#,
       2#10000#,
       2#01111#),
      (2#11111#, -- z
       2#01000#,
       2#00100#,
       2#00010#,
       2#11111#),
      (2#00100#, -- {
       2#00100#,
       2#00010#,
       2#00100#,
       2#00100#),
      (2#00100#, -- |
       2#00100#,
       2#00100#,
       2#00100#,
       2#00100#),
      (2#00100#, -- }
       2#00100#,
       2#01000#,
       2#00100#,
       2#00100#),
      (2#00000#, -- ~
       2#00000#,
       2#01010#,
       2#10101#,
       2#00000#)
     );

   procedure Print_C (X_Org : Width;
                      C     : Character);
   procedure Initialize;
   procedure Tick_Handler;
   procedure Update_Animation;

   -------------
   -- Print_C --
   -------------

   procedure Print_C (X_Org : Width;
                      C     : Character)
   is
      C_Index : constant Integer := Character'Pos (C) - Character'Pos ('!');
   begin
      if C_Index not in Font'Range then
         return;
      end if;

      for X in Coord loop
         for Y in Coord loop
            if X_Org + X in Width then
               if (Font (C_Index) (Y) and 2**X) /= 0 then
                  Bitmap (X_Org + X, Y) := True;
               end if;
            end if;
         end loop;
      end loop;
   end Print_C;

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

      --  Animation

      if Animation_Elapsed = Animation_Step_Duration_Ms then
         Animation_Elapsed := 0;
         Update_Animation;
      else
         Animation_Elapsed := Animation_Elapsed + 1;
      end if;
   end Tick_Handler;

   ----------------------
   -- Update_Animation --
   ----------------------

   procedure Update_Animation is
   begin
      case Animation_State is
         when None =>
            null;
         when Scroll_Text =>
            Shift_Left;

            Scroll_Position := Scroll_Position + 1;
            if Scroll_Position >= Coord'Range_Length + 1 then
               --  We finished scrolling the current character

               Scroll_Position := 0;

               if Scroll_Text_Index > Scroll_Text_Length + 1 then
                  Animation_State := None;
               elsif Scroll_Text_Index = Scroll_Text_Length + 1 then
                  null; -- Leave the screen empty until the character is flushed
               else
                  --  Print new char
                  Print_C (5, Scroll_Text_Buffer (Scroll_Text_Index));
               end if;
               Scroll_Text_Index := Scroll_Text_Index + 1;
            end if;
      end case;
   end Update_Animation;

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
      Print_C (0, C);
   end Display;

   -------------
   -- Display --
   -------------

   procedure Display (Str : String) is
   begin
      Display_Async (Str);
      while Animation_State /= None loop
         Asm (Template => "wfi", -- Wait for interrupt
              Volatile => True);
      end loop;
   end Display;

   -------------------
   -- Display_Async --
   -------------------

   procedure Display_Async (Str : String) is
   begin
      Scroll_Text_Buffer (Scroll_Text_Buffer'First .. Scroll_Text_Buffer'First + Str'Length - 1) :=  Str;
      Animation_State := Scroll_Text;
      Scroll_Text_Length := Str'Length;
      Scroll_Text_Index := Scroll_Text_Buffer'First;
      Scroll_Position := Coord'Last + 1;
   end Display_Async;

   ----------------
   -- Shift_Left --
   ----------------

   procedure Shift_Left is
   begin
      --  Shift pixel columns to the left, erasing the left most one
      for X in Bitmap'First (1) .. Bitmap'Last (1) - 1 loop
         for Y in Bitmap'Range (2) loop
            Bitmap (X, Y) := Bitmap (X + 1, Y);
         end loop;
      end loop;

      --  Insert black pixels to the right most column
      for Y in Bitmap'Range (2) loop
         Bitmap (Bitmap'Last (1), Y) := False;
      end loop;
   end Shift_Left;

   ---------------------------------
   -- Set_Animation_Step_Duration --
   ---------------------------------

   procedure Set_Animation_Step_Duration (Ms : Natural) is
   begin
      Animation_Step_Duration_Ms := Ms;
   end Set_Animation_Step_Duration;

   ---------------------------
   -- Animation_In_Progress --
   ---------------------------

   function Animation_In_Progress return Boolean
   is (Animation_State /= None);

begin
   Initialize;
end MicroBit.Display;
