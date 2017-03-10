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
with STM32.Setup;
--  with STM32.GPIO;           use STM32.GPIO;
with HAL.Touch_Panel;      use HAL.Touch_Panel;

with FT6x06;               use FT6x06;

package body Touch_Panel_FT6x06 is

   procedure TP_Init_Pins;
   --  Initializes the Touch Panel GPIO pins

   ---------------
   -- Init_Pins --
   ---------------

   procedure TP_Init_Pins
   is
   begin
      null;
--        Enable_Clock (TP_INT);
--
--        Configure_IO (TP_INT,
--                      (Speed       => Speed_50MHz,
--                       Mode        => Mode_In,
--                       Output_Type => Open_Drain,
--                       Resistors   => Pull_Up));
--        Configure_Trigger (TP_INT, STM32.EXTI.Interrupt_Falling_Edge);
--        Lock (TP_INT);
   end TP_Init_Pins;

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
      if not TP_I2C.Port_Enabled then
         STM32.Setup.Setup_I2C_Master (Port        => TP_I2C,
                                       SDA         => TP_I2C_SDA,
                                       SCL         => TP_I2C_SCL,
                                       SDA_AF      => TP_I2C_SDA_AF,
                                       SCL_AF      => TP_I2C_SCL_AF,
                                       Clock_Speed => 100_000);
      end if;

      --  Wait at least 200ms after power up before accessing the TP registers
      delay until Clock + Milliseconds (200);

      if This.Check_Id then
         This.TP_Set_Use_Interrupts (False);
         This.Set_Orientation (Orientation);

         return True;
      else
         return False;
      end if;
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
