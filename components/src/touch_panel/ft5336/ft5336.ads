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

--  Generic driver for the FT5336 touch panel

with HAL;             use HAL;
with HAL.I2C;         use HAL.I2C;
with HAL.Touch_Panel; use HAL.Touch_Panel;

package FT5336 is

   type FT5336_Device (Port     : not null Any_I2C_Port;
                       I2C_Addr : I2C_Address) is
     limited new Touch_Panel_Device with private;

   function Check_Id (This : in out FT5336_Device) return Boolean;
   --  Checks the ID of the touch panel controller, returns false if not found
   --  or invalid.

   procedure TP_Set_Use_Interrupts (This : in out FT5336_Device;
                                    Enabled : Boolean);
   --  Whether the data is retrieved upon interrupt or by polling by the
   --  software.

   overriding
   procedure Set_Bounds (This   : in out FT5336_Device;
                         Width  : Natural;
                         Height : Natural;
                         Swap   : HAL.Touch_Panel.Swap_State);
   --  Set screen bounds. Touch_State must should stay within screen bounds

   overriding
   function Active_Touch_Points (This : in out FT5336_Device)
                                 return HAL.Touch_Panel.Touch_Identifier;
   --  Retrieve the number of active touch points

   overriding
   function Get_Touch_Point (This     : in out FT5336_Device;
                             Touch_Id : HAL.Touch_Panel.Touch_Identifier)
                             return HAL.Touch_Panel.TP_Touch_State;
   --  Retrieves the position and pressure information of the specified
   --  touch

   overriding
   function Get_All_Touch_Points
     (This     : in out FT5336_Device)
      return HAL.Touch_Panel.TP_State;
   --  Retrieves the position and pressure information of every active touch
   --  points

private

   type FT5336_Device (Port     : not null Any_I2C_Port;
                       I2C_Addr : I2C_Address) is
     limited new HAL.Touch_Panel.Touch_Panel_Device with record
      LCD_Natural_Width  : Natural := 0;
      LCD_Natural_Height : Natural := 0;
      Swap               : Swap_State := 0;
   end record;

   function I2C_Read (This   : in out FT5336_Device;
                      Reg    : UInt8;
                      Status : out Boolean)
                      return UInt8;

   procedure I2C_Write (This   : in out FT5336_Device;
                        Reg    : UInt8;
                        Data   : UInt8;
                        Status : out Boolean);

end FT5336;
