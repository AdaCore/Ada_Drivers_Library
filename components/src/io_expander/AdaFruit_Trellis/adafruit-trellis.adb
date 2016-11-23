------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package body AdaFruit.Trellis is

   type LED_Address is record
      Row : HT16K33.LED_Row_Addr;
      Column : HT16K33.LED_Column_Addr;
   end record;

   LED_Mapping : constant array (Trellis_Coord, Trellis_Coord) of LED_Address :=
     (
      ((7, 2), (6, 7), (6, 5), (6, 4)),
      ((5, 0), (5, 1), (4, 3), (4, 4)),
      ((2, 6), (3, 3), (2, 1), (2, 0)),
      ((1, 6), (1, 5), (1, 4), (0, 2))
     );

   type Key_Address is record
      Row : HT16K33.Key_Row_Addr;
      Column : HT16K33.Key_Column_Addr;
   end record;

   Key_Mapping : constant array (Trellis_Coord, Trellis_Coord) of Key_Address :=
     (((0, 7), (0, 4), (0, 2), (2, 2)),
      ((0, 5), (0, 6), (0, 0), (0, 1)),
      ((0, 3), (1, 0), (3, 0), (2, 1)),
      ((1, 3), (1, 2), (1, 1), (3, 1)));

   -------------
   -- Set_LED --
   -------------

   procedure Set_LED (This   : in out Trellis_Device;
                      X, Y   : Trellis_Coord;
                      Enable : Boolean := True)
   is
   begin
      This.Set_LED (LED_Mapping (X, Y).Row,
                    LED_Mapping (X, Y).Column,
                    Enable);
   end Set_LED;

   ---------
   -- Key --
   ---------

   function Key (This : in out Trellis_Device;
                 X, Y : Trellis_Coord) return Boolean
   is
   begin
      return This.Get_Key (Key_Mapping (X, Y).Row, Key_Mapping (X, Y).Column);
   end Key;
end AdaFruit.Trellis;
