------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with HAL; use HAL;

package body Hex_Dump is

   procedure Hex_Dump (Data     : HAL.UInt8_Array;
                       Put_Line : Put_Line_Procedure;
                       Base_Addr : HAL.UInt64 := 0)
   is

      function UInt8_To_Char (Val : UInt8) return Character;
      procedure Start_New_Line;

      --  Hexdump format:
      --         0000_0000_0000_0000: 57 69 6B 69 70 65 64 69 61 2C 20 74 68 65 20 66  Wikipedia, the f
      --  Addr : ^^^^^^^^^^^^^^^^^^^^
      --  Hex  :                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      --  ASCII:                                                                      ^^^^^^^^^^^^^^^^^

      Addr_Len  : constant := 16 + 3 + 1;
      Hex_Len   : constant := 3 * 16;
      ASCII_Len : constant := 1 + 16;
      Str   : String (1 .. Addr_Len + Hex_Len + ASCII_Len) := (others => ' ');
      UInt4_To_Char : constant array (UInt4) of Character
        := (0 =>  '0',
            1 =>  '1',
            2 =>  '2',
            3 =>  '3',
            4 =>  '4',
            5 =>  '5',
            6 =>  '6',
            7 =>  '7',
            8 =>  '8',
            9 =>  '9',
            10 => 'A',
            11 => 'B',
            12 => 'C',
            13 => 'D',
            14 => 'E',
            15 => 'F');

      -------------------
      -- UInt8_To_Char --
      -------------------

      function UInt8_To_Char (Val : UInt8) return Character is
      begin
         case Val is
            when 0 .. 31 | 127 .. 255 =>
               return '.';
            when others =>
               return Character'Val (Val);
         end case;
      end UInt8_To_Char;

      Index : Natural;
      Cnt   : Natural;
      Addr  : Natural := 0;

      --------------------
      -- Start_New_Line --
      --------------------

      procedure Start_New_Line is
         Addr_Val : UInt64 := UInt64 (Addr) + Base_Addr;
      begin

         --  Address
         for X in reverse 1 .. 19 loop
            if X in 5 | 10 | 15 then
               Str (X) := '_';
            else
               Str (X) := UInt4_To_Char (UInt4 (Addr_Val and 16#0F#));
               Addr_Val := Shift_Right (Addr_Val, 4);
            end if;
         end loop;

         Str (20) := ':';
         Str (21 .. Str'Last) := (others => ' ');

         Cnt := 0;
         Index := Str'First + Addr_Len;
      end Start_New_Line;

   begin

      Start_New_Line;

      for Elt of Data loop

         --  Hex
         Str (Index + 1) := UInt4_To_Char (UInt4 (Shift_Right (Elt, 4)));
         Str (Index + 2) := UInt4_To_Char (UInt4 (Elt and 16#0F#));

         --  ASCII
         Str (Str'Last - (15 - Cnt)) := UInt8_To_Char (Elt);

         Index := Index + 3;
         Cnt   := Cnt + 1;
         Addr  := Addr + 1;
         if Cnt = 16 then
            Put_Line (Str);
            Start_New_Line;
         end if;
      end loop;

      if Cnt /= 0 then
         Put_Line (Str);
      end if;
   end Hex_Dump;

end Hex_Dump;
