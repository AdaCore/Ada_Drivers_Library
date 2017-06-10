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

package MicroBit.Display is

   subtype Coord is Natural range 0 .. 4;

   procedure Set (X, Y : Coord);
   --  Set one pixel

   procedure Clear (X, Y : Coord);
   --  Clear one pixel

   procedure Clear;
   --  Clear all the pixels

   procedure Display (C : Character);
   --  Display a character on the screen

   Scroll_Text_Max_Length : constant := 128;
   --  Maximum length of a string displayed

   procedure Display (Str : String)
     with Pre => Str'Length <= Scroll_Text_Max_Length;
   --  Display a string on the screen and wait until the end of scrolling

   procedure Display_Async (Str : String)
     with Pre => Str'Length <= Scroll_Text_Max_Length;
   --  Start scrolling a string on the screen and return imediatly while the
   --  scroll animation continues in the background.

   procedure Shift_Left;
   --  Shift all the pixels to the left

   procedure Set_Animation_Step_Duration (Ms : Natural);
   --  Set the number of miliseconds between two animation steps

   function Animation_In_Progress return Boolean;
   --  Is there an animation in progress?

end MicroBit.Display;
