------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f429i_discovery_ts.c                                     --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file provides a set of functions needed to manage Touch  --
--            screen available with STMPE811 IO Expander device mounted on  --
--            STM32F429I-Discovery Kit.                                     --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32.Board;          use STM32;
with STM32.I2C;            use STM32.I2C;
with STM32.GPIO;           use STM32.GPIO;
with STM32.Setup;

with STM32.Device;         use STM32.Device;

with HAL.Touch_Panel;      use HAL.Touch_Panel;

with STMPE811;             use STMPE811;

package body Touch_Panel_STMPE811 is

   SCL        : GPIO_Point renames PA8;
   SDA        : GPIO_Point renames PC9;
   SCL_SDA_AF : constant GPIO_Alternate_Function := GPIO_AF_I2C3_4;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (This : in out Touch_Panel;
      Orientation : HAL.Framebuffer.Display_Orientation :=
        HAL.Framebuffer.Default) return Boolean
   is
      Status : Boolean;
   begin
      if not TP_I2C.Port_Enabled then
         STM32.Setup.Setup_I2C_Master (Port        => TP_I2C,
                                       SDA         => SDA,
                                       SCL         => SCL,
                                       SDA_AF      => SCL_SDA_AF,
                                       SCL_AF      => SCL_SDA_AF,
                                       Clock_Speed => 100_000);
      end if;

      Status := STMPE811_Device (This).Initialize;
      This.Set_Orientation (Orientation);

      return Status;
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
         when HAL.Framebuffer.Default | HAL.Framebuffer.Portrait =>
            This.Set_Bounds (STM32.Board.LCD_Natural_Width,
                             STM32.Board.LCD_Natural_Height,
                             0);
         when HAL.Framebuffer.Landscape =>
            This.Set_Bounds (STM32.Board.LCD_Natural_Width,
                             STM32.Board.LCD_Natural_Height,
                             Swap_XY or Invert_Y);
      end case;
   end Set_Orientation;

end Touch_Panel_STMPE811;
