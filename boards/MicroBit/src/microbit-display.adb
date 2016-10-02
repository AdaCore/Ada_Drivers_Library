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

with nRF51.GPIO;   use nRF51.GPIO;
with nRF51.Device; use nRF51.Device;

package body MicroBit.Display is

   Is_Initialized : Boolean := False;


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
     (((1, 1), (2, 4), (1, 2), (2, 5), (1, 3)),
      ((3, 4), (3, 5), (3, 6), (3, 7), (3, 8)),
      ((2, 2), (1, 9), (2, 3), (3, 9), (2, 1)),
      ((1, 8), (1, 7), (1, 6), (1, 5), (1, 4)),
      ((3, 3), (2, 7), (3, 1), (2, 6), (3, 2))
     );

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
   end Initialize;

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
      --  Row source current
      Row_Points (Map (X, Y).Row_Id).Set;
      --  Column sink current
      Column_Points (Map (X, Y).Column_Id).Clear;
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (X, Y : Coord) is
   begin
      --  Row source current
      Row_Points (Map (X, Y).Row_Id).Clear;
      --  Column sink current
      Column_Points (Map (X, Y).Column_Id).Set;
   end Clear;

end MicroBit.Display;
