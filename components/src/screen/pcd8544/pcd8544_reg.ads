------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Unchecked_Conversion;
with HAL;

package PCD8544_Reg is

   --  Basic mode
   PCD8544_CMD_FUNCTION : constant := 2#0010_0000#;
   PCD8544_CMD_DISPLAY  : constant := 2#0000_1000#;
   PCD8544_CMD_SET_X    : constant := 2#1000_0000#;
   PCD8544_CMD_SET_Y    : constant := 2#0100_0000#;

   --  Extended mode
   PCD8544_CMD_SET_TC   : constant := 2#0000_0100#;
   PCD8544_CMD_SET_BIAS : constant := 2#0001_0000#;
   PCD8544_CMD_SET_VOP  : constant := 2#1000_0000#;

   type PCD8544_Address_Mode is (Horizontal, Vertical);

   type PCD8544_Function_Register is record
      Power_Down    : Boolean              := True;
      Address_Mode  : PCD8544_Address_Mode := Horizontal;
      Extended_Mode : Boolean              := False;
      Reserved      : Boolean              := False;
   end record;

   for PCD8544_Function_Register use record
      Reserved      at 0 range 3 .. 7;
      Power_Down    at 0 range 2 .. 2;
      Address_Mode  at 0 range 1 .. 1;
      Extended_Mode at 0 range 0 .. 0;
   end record;
   for PCD8544_Function_Register'Size use 8;

   type PCD8544_Display_Register is record
      Enable    : Boolean := False;
      Invert    : Boolean := False;
      Reserved1 : Boolean := False;
      Reserved2 : Boolean := False;
   end record;

   for PCD8544_Display_Register use record
      Reserved2 at 0 range 3 .. 7;
      Reserved1 at 0 range 1 .. 1;
      Enable    at 0 range 2 .. 2;
      Invert    at 0 range 0 .. 0;
   end record;
   for PCD8544_Display_Register'Size use 8;

   function Convert is new Ada.Unchecked_Conversion
     (PCD8544_Function_Register, HAL.UInt8);
   function Convert is new Ada.Unchecked_Conversion
     (PCD8544_Display_Register, HAL.UInt8);

end PCD8544_Reg;
