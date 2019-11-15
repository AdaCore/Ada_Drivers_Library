------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Feather_STM32F405.I2C;
with AdaFruit.CharlieWing;
with HAL; use HAL;

procedure Main is

   Matrix : AdaFruit.CharlieWing.Device (Feather_STM32F405.I2C.Controller, 0);

   pragma Style_Checks (Off);

   A : constant := 1;
   Text : constant array (AdaFruit.CharlieWing.Y_Coord, 0 .. 74) of HAL.Bit :=
     ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, A, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, A, 0, 0, 0, 0, 0, others => 0),
      (A, 0, 0, 0, A, 0, 0, 0, 0, 0, 0, A, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, A, 0, 0, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, 0, 0, 0, 0, 0, 0, A, A, 0, 0, 0, 0, 0, A, 0, 0, 0, 0, 0, others => 0),
      (A, A, 0, A, A, 0, 0, A, A, 0, 0, A, 0, 0, A, 0, 0, A, A, 0, 0, 0, A, 0, 0, 0, A, 0, 0, 0, 0, A, A, A, 0, A, 0, 0, 0, 0, 0, A, 0, 0, A, 0, 0, A, A, A, 0, 0, A, A, 0, others => 0),
      (A, 0, A, 0, A, 0, 0, 0, 0, A, 0, A, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, 0, 0, 0, A, 0, A, A, 0, 0, A, 0, 0, A, A, A, 0, 0, 0, A, 0, 0, A, 0, A, 0, 0, A, 0, 0, 0, 0, A, others => 0),
      (A, 0, 0, 0, A, 0, 0, A, A, A, 0, A, A, 0, 0, 0, A, A, A, A, 0, 0, A, 0, A, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, A, A, A, 0, A, 0, 0, A, 0, 0, A, A, A, others => 0),
      (A, 0, 0, 0, A, 0, A, 0, 0, A, 0, A, 0, A, 0, 0, A, 0, 0, 0, 0, 0, A, A, 0, A, A, 0, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, 0, A, 0, 0, A, 0, A, 0, 0, A, others => 0),
      (A, 0, 0, 0, A, 0, 0, A, A, A, 0, A, 0, 0, A, 0, 0, A, A, 0, 0, 0, A, 0, 0, 0, A, 0, A, A, A, 0, A, A, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, 0, 0, A, A, A, 0, 0, A, A, A, others => 0));

   pragma Style_Checks (On);

begin
   Feather_STM32F405.I2C.Initialize (400_000);

   Matrix.Initialize;
   Matrix.Fill (10);
   loop

      for Column in Text'Range (2) loop

         for X in AdaFruit.CharlieWing.X_Coord loop
            for Y in AdaFruit.CharlieWing.Y_Coord loop

               if Text (Y, (X + Column) mod Text'Length (2)) = 1 then
                  Matrix.Enable (X, Y);
               else
                  Matrix.Disable (X, Y);
               end if;
            end loop;
         end loop;
         delay 0.1;
      end loop;
   end loop;
end Main;
