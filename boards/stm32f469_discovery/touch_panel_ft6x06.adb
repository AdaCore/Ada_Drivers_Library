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

--  Based on ft6x06.h from MCD Application Team

with Ada.Real_Time;        use Ada.Real_Time;
with Ada.Unchecked_Conversion;

with STM32.Board;          use STM32.Board;
with STM32.Device;         use STM32.Device;
with STM32.I2C;            use STM32.I2C;
with STM32.GPIO;           use STM32.GPIO;
with HAL.Touch_Panel;      use HAL.Touch_Panel;

with FT6x06;               use FT6x06;

package body Touch_Panel_FT6x06 is

   procedure TP_Init_Pins;
   --  Initializes the Touch Panel GPIO pins

   procedure TP_I2C_Config;
   --  Initializes the I2C bus

   ---------------
   -- Init_Pins --
   ---------------

   procedure TP_Init_Pins
   is
      Pins : constant GPIO_Points := TP_Pins;
   begin
      Enable_Clock (Pins);

      Reset (TP_I2C);

      Configure_Alternate_Function (Pins, GPIO_AF_I2C);
      Configure_IO (Pins,
                    (Speed       => Speed_25MHz,
                     Mode        => Mode_AF,
                     Output_Type => Open_Drain,
                     Resistors   => Floating));
      Lock (Pins);

      Enable_Clock (TP_INT);

      Configure_IO (TP_INT,
                    (Speed       => Speed_50MHz,
                     Mode        => Mode_In,
                     Output_Type => Open_Drain,
                     Resistors   => Pull_Up));
      Lock (TP_INT);
   end TP_Init_Pins;

   -------------------
   -- TP_I2C_Config --
   -------------------

   procedure TP_I2C_Config
   is
      I2C_Conf : I2C_Configuration;
   begin
      Enable_Clock (TP_I2C);

      --  Wait at least 200ms after power up before accessing the TP registers
      delay until Clock + Milliseconds (200);

      if not TP_I2C.Port_Enabled then
         Reset (TP_I2C);

         I2C_Conf.Own_Address := 16#00#;
         I2C_Conf.Addressing_Mode := Addressing_Mode_7bit;
         I2C_Conf.General_Call_Enabled := False;
         I2C_Conf.Clock_Stretching_Enabled := True;

         I2C_Conf.Clock_Speed := 100_000;

         TP_I2C.Configure (I2C_Conf);
      end if;
   end TP_I2C_Config;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (This : in out Touch_Panel;
      Orientation : HAL.Framebuffer.Display_Orientation :=
        HAL.Framebuffer.Default) return Boolean
   is
   begin
      TP_Init_Pins;
      TP_I2C_Config;

      This.TP_Set_Use_Interrupts (False);
      This.Set_Orientation (Orientation);

      return This.Check_Id;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Touch_Panel;
      Orientation : HAL.Framebuffer.Display_Orientation :=
                           HAL.Framebuffer.Default) is
   begin
      if not This.Initialize (Orientation) then
         raise Constraint_Error with "Cannot initialize the touch panel";
      end if;
   end Initialize;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (This        : in out Touch_Panel;
      Orientation : HAL.Framebuffer.Display_Orientation)
   is
   begin
      case Orientation is
         when HAL.Framebuffer.Default | HAL.Framebuffer.Landscape =>
            This.Set_Bounds (LCD_Natural_Width,
                             LCD_Natural_Height,
                             Invert_Y);
         when HAL.Framebuffer.Portrait =>
            This.Set_Bounds (LCD_Natural_Width,
                             LCD_Natural_Height,
                             Swap_XY);
      end case;
   end Set_Orientation;

end Touch_Panel_FT6x06;
