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

with Interfaces.Bit_Types; use Interfaces.Bit_Types;
with HAL.I2C; use HAL.I2C;
with HAL.Touch_Panel; use HAL.Touch_Panel;

package STMPE811 is

   type STMPE811_Device (Port     : I2C_Port_Ref;
                         I2C_Addr : HAL.I2C.I2C_Address) is
     new Touch_Panel_Device with private;

   function Initialize (This : in out STMPE811_Device) return Boolean;
   --  Initializes the LCD touch panel

   procedure Initialize (This : in out STMPE811_Device);
   --  Initializes the LCD touch panel

   overriding
   procedure Set_Bounds (This   : in out STMPE811_Device;
                         Width  : Natural;
                         Height : Natural);

   overriding
   function Active_Touch_Points (This : in out STMPE811_Device)
                                 return Touch_Identifier;
   --  Retrieve the number of active touch points

   overriding
   function Get_Touch_Point (This     : in out STMPE811_Device;
                             Touch_Id : Touch_Identifier;
                             SwapXY   : Boolean := False)
                             return HAL.Touch_Panel.TP_Touch_State;
   --  Retrieves the position and pressure information of the specified
   --  touch

   overriding
   function Get_All_Touch_Points
     (This     : in out STMPE811_Device;
      SwapXY   : Boolean := False)
      return HAL.Touch_Panel.TP_State;
   --  Retrieves the position and pressure information of every active touch
   --  points
private
   type STMPE811_Device (Port     : I2C_Port_Ref;
                         I2C_Addr : HAL.I2C.I2C_Address) is
     new Touch_Panel_Device with record
      LCD_Natural_Width  : Natural := 240; -- Arbitrary
      LCD_Natural_Height : Natural := 320; -- Arbitrary
   end record;

   subtype TSC_Data is I2C_Data;

   function Read_Data (This      : in out STMPE811_Device;
                       Data_Addr : Byte;
                       Length    : Natural) return TSC_Data;
   function Read_Register (This     : STMPE811_Device;
                           Reg_Addr : Byte) return Byte;
   procedure Write_Register (This     : in out STMPE811_Device;
                             Reg_Addr : Byte;
                             Data     : Byte);
   procedure IOE_Reset (This : in out STMPE811_Device);
   procedure IOE_Function_Command (This : in out STMPE811_Device;
                                   Func : Byte;
                                   Enabled : Boolean);
   procedure IOE_AF_Config (This      : in out STMPE811_Device;
                            Pin       : Byte;
                            Enabled   : Boolean);
   function Get_IOE_ID (This : in out STMPE811_Device) return Short;

end STMPE811;
