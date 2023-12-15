------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

pragma Ada_2022;

with GUI_Buttons;
with HAL.Bitmap;
with HAL.Touch_Panel;

package GUI is

   type Button_Kind is
     (
      Te,
      Te_X1,
      Te_X2,
      Te_X4,
      Te_X8,
      Te_16,
      Hu,
      Hu_X1,
      Hu_X2,
      Hu_X4,
      Hu_X8,
      Hu_16,
      Pr,
      Pr_X1,
      Pr_X2,
      Pr_X4,
      Pr_X8,
      Pr_16,
      Fi,
      Fi_No,
      Fi_X2,
      Fi_X4,
      Fi_X8,
      Fi_16);

   function "+" (X : Button_Kind) return Natural is (Button_Kind'Pos (X))
     with Static;

   Buttons : constant GUI_Buttons.Button_Info_Array :=
     [
      (Label  => "Te",
       Center => (23 * 1, 20),
       Color  => HAL.Bitmap.Dark_Red),
      (Label  => "x1",
       Center => (23 * 2, 20),
       Color  => HAL.Bitmap.Dark_Red),
      (Label  => "x2",
       Center => (23 * 3, 20),
       Color  => HAL.Bitmap.Dark_Red),
      (Label  => "x4",
       Center => (23 * 4, 20),
       Color  => HAL.Bitmap.Dark_Red),
      (Label  => "x8",
       Center => (23 * 5, 20),
       Color  => HAL.Bitmap.Dark_Red),
      (Label  => "16",
       Center => (23 * 6, 20),
       Color  => HAL.Bitmap.Dark_Red),

      (Label  => "Hu",
       Center => (23 * 1 + 160, 20),
       Color  => HAL.Bitmap.Dark_Green),
      (Label  => "x1",
       Center => (23 * 2 + 160, 20),
       Color  => HAL.Bitmap.Dark_Green),
      (Label  => "x2",
       Center => (23 * 3 + 160, 20),
       Color  => HAL.Bitmap.Dark_Green),
      (Label  => "x4",
       Center => (23 * 4 + 160, 20),
       Color  => HAL.Bitmap.Dark_Green),
      (Label  => "x8",
       Center => (23 * 5 + 160, 20),
       Color  => HAL.Bitmap.Dark_Green),
      (Label  => "16",
       Center => (23 * 6 + 160, 20),
       Color  => HAL.Bitmap.Dark_Green),

      (Label  => "Pr",
       Center => (23 * 1, 220),
       Color  => HAL.Bitmap.Dark_Blue),
      (Label  => "x1",
       Center => (23 * 2, 220),
       Color  => HAL.Bitmap.Dark_Blue),
      (Label  => "x2",
       Center => (23 * 3, 220),
       Color  => HAL.Bitmap.Dark_Blue),
      (Label  => "x4",
       Center => (23 * 4, 220),
       Color  => HAL.Bitmap.Dark_Blue),
      (Label  => "x8",
       Center => (23 * 5, 220),
       Color  => HAL.Bitmap.Dark_Blue),
      (Label  => "16",
       Center => (23 * 6, 220),
       Color  => HAL.Bitmap.Dark_Blue),

      (Label  => "Fi",
       Center => (23 * 1 + 160, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "No",
       Center => (23 * 2 + 160, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "x2",
       Center => (23 * 3 + 160, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "x4",
       Center => (23 * 4 + 160, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "x8",
       Center => (23 * 5 + 160, 220),
       Color  => HAL.Bitmap.Dim_Grey),
      (Label  => "16",
       Center => (23 * 6 + 160, 220),
       Color  => HAL.Bitmap.Dim_Grey)];

   State : GUI_Buttons.Boolean_Array (Buttons'Range) :=
     [+Hu    | +Te    | +Pr |
      +Hu_X1 | +Te_X1 | +Pr_X1 |
      +Fi_No => True,
      others => False];

   procedure Check_Touch
     (TP     : in out HAL.Touch_Panel.Touch_Panel_Device'Class;
      Update : out Boolean);
   --  Check buttons touchedm update State, set Update = True if State changed

   procedure Draw
     (LCD   : in out HAL.Bitmap.Bitmap_Buffer'Class;
      Clear : Boolean := False);

   procedure Dump_Screen (LCD : in out HAL.Bitmap.Bitmap_Buffer'Class);

end GUI;
