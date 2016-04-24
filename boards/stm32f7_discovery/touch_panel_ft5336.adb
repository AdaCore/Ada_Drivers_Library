------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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

--  Based on ft5336.h from MCD Application Team

with Ada.Real_Time;        use Ada.Real_Time;
with Ada.Unchecked_Conversion;


with STM32.Board;          use STM32.Board;
with STM32.Device;         use STM32.Device;
with FT5336;               use FT5336;

package body Touch_Panel_FT5336 is

   ----------------
   -- Initialize --
   ----------------

   overriding function Initialize (This : in out Touch_Panel) return Boolean is
   begin
      Initialize_I2C_GPIO (TP_I2C);

      --  Wait at least 200ms after power up before accessing the TP registers
      delay until Clock + Milliseconds (200);

      Configure_I2C (TP_I2C);

      This.TP_Set_Use_Interrupts (False);
      This.Set_Bounds (LCD_Natural_Width,
                       LCD_Natural_Height);

      return This.Check_Id;
   end Initialize;

   -----------------
   -- Swap_Points --
   -----------------

   overriding function Swap_Points (This : Touch_Panel) return Boolean
   is
      pragma Unreferenced (This);
   begin
      return Display.Is_Swapped;
   end Swap_Points;

end Touch_Panel_FT5336;
