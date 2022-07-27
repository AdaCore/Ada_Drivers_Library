------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015-2018, 2022, AdaCore                --
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
with HAL.GPIO;                   use HAL.GPIO;

package body MCP23x08 is

   function To_UInt8 is
      new Ada.Unchecked_Conversion (Source => ALl_IO_Array,
                                    Target => UInt8);
   function To_All_IO_Array is
      new Ada.Unchecked_Conversion (Source => UInt8,
                                    Target => ALl_IO_Array);
   procedure Loc_IO_Write
     (This      : in out MCP23x08_IO_Expander'Class;
      WriteAddr : Register_Address;
      Value     : UInt8)
     with Inline_Always;

   procedure Loc_IO_Read
     (This     : MCP23x08_IO_Expander'Class;
      ReadAddr : Register_Address;
      Value    : out UInt8)
     with Inline_Always;

   procedure Set_Bit
     (This    : in out MCP23x08_IO_Expander;
      RegAddr : Register_Address;
      Pin     : MCP23x08_Pin);

   procedure Clear_Bit
     (This    : in out MCP23x08_IO_Expander;
      RegAddr : Register_Address;
      Pin     : MCP23x08_Pin);

   function Read_Bit
     (This    : MCP23x08_IO_Expander;
      RegAddr : Register_Address;
      Pin     : MCP23x08_Pin)
      return Boolean;

   ------------------
   -- Loc_IO_Write --
   ------------------

   procedure Loc_IO_Write
     (This      : in out MCP23x08_IO_Expander'Class;
      WriteAddr : Register_Address;
      Value     : UInt8)
   is

   begin
      IO_Write (This, WriteAddr, Value);
   end Loc_IO_Write;

   -----------------
   -- Loc_IO_Read --
   -----------------

   procedure Loc_IO_Read
     (This     : MCP23x08_IO_Expander'Class;
      ReadAddr : Register_Address;
      Value    : out UInt8)
      is
   begin
      IO_Read (This, ReadAddr, Value);
   end Loc_IO_Read;

   -------------
   -- Set_Bit --
   -------------

   procedure Set_Bit
     (This    : in out MCP23x08_IO_Expander;
      RegAddr : Register_Address;
      Pin     : MCP23x08_Pin)
   is
      Prev, Next : UInt8;
   begin
      Loc_IO_Read (This, RegAddr, Prev);
      Next := Prev or Pin'Enum_Rep;
      if Next /= Prev then
         Loc_IO_Write (This, RegAddr, Next);
      end if;
   end Set_Bit;

   ---------------
   -- Clear_Bit --
   ---------------

   procedure Clear_Bit
     (This    : in out MCP23x08_IO_Expander;
      RegAddr : Register_Address;
      Pin     : MCP23x08_Pin)
   is
      Prev, Next : UInt8;
   begin
      Loc_IO_Read (This, RegAddr, Prev);
      Next := Prev and (not  Pin'Enum_Rep);
      if Next /= Prev then
         Loc_IO_Write (This, RegAddr, Next);
      end if;
   end Clear_Bit;

   --------------
   -- Read_Bit --
   --------------

   function Read_Bit
     (This    : MCP23x08_IO_Expander;
      RegAddr : Register_Address;
      Pin     : MCP23x08_Pin)
      return Boolean
   is
      Reg : UInt8;
   begin
      Loc_IO_Read (This, RegAddr, Reg);
      return (Reg and Pin'Enum_Rep) /= 0;
   end Read_Bit;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (This    : in out MCP23x08_IO_Expander;
                        Pin     : MCP23x08_Pin;
                        Output  : Boolean;
                        Pull_Up : Boolean)
   is
   begin
      This.Configure_Mode (Pin, Output);
      This.Configure_Pull (Pin, Pull_Up);
   end Configure;

   procedure Configure_Mode (This    : in out MCP23x08_IO_Expander;
                             Pin     : MCP23x08_Pin;
                             Output  : Boolean)
   is
   begin
      if Output then
         Clear_Bit (This, IO_DIRECTION_REG, Pin);
      else
         Set_Bit (This, IO_DIRECTION_REG, Pin);
      end if;
   end Configure_Mode;

   ---------------
   -- Is_Output --
   ---------------

   function Is_Output (This : in out MCP23x08_IO_Expander;
                       Pin  : MCP23x08_Pin)
                       return Boolean
   is
   begin
      return not Read_Bit (This, IO_DIRECTION_REG, Pin);
   end Is_Output;


   --------------------
   -- Configure_Pull --
   --------------------

   procedure Configure_Pull (This    : in out MCP23x08_IO_Expander;
                             Pin     : MCP23x08_Pin;
                             Pull_Up : Boolean)
   is
   begin
      if Pull_Up then
         Set_Bit (This, PULL_UP_REG, Pin);
      else
         Clear_Bit (This, PULL_UP_REG, Pin);
      end if;
   end Configure_Pull;

   -------------
   -- Pull_Up --
   -------------

   function Pull_Up (This : MCP23x08_IO_Expander;
                     Pin  : MCP23x08_Pin) return Boolean
   is
   begin
      return Read_Bit (This, PULL_UP_REG, Pin);
   end Pull_Up;

   ---------
   -- Set --
   ---------

   function Set (This  : MCP23x08_IO_Expander;
                 Pin   : MCP23x08_Pin) return Boolean
   is
      Val : UInt8;
   begin
      Loc_IO_Read (This, LOGIC_LEVLEL_REG, Val);
      return (Pin'Enum_Rep and Val) /= 0;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This  : in out MCP23x08_IO_Expander;
                  Pin   : MCP23x08_Pin)
   is
   begin
      Set_Bit (This, LOGIC_LEVLEL_REG, Pin);
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (This  : in out MCP23x08_IO_Expander;
                    Pin   : MCP23x08_Pin)
   is
   begin
      Clear_Bit (This, LOGIC_LEVLEL_REG, Pin);
   end Clear;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (This  : in out MCP23x08_IO_Expander;
                     Pin   : MCP23x08_Pin)
   is
   begin
      if This.Set (Pin) then
         This.Clear (Pin);
      else
         This.Set (Pin);
      end if;
   end Toggle;

   ------------
   -- All_IO --
   ------------

   function All_IO (This : in out MCP23x08_IO_Expander) return ALl_IO_Array is
      Val : UInt8;
   begin
      Loc_IO_Read (This, LOGIC_LEVLEL_REG, Val);
      return To_All_IO_Array (Val);
   end All_IO;

   ----------------
   -- Set_All_IO --
   ----------------

   procedure Set_All_IO (This : in out MCP23x08_IO_Expander; IOs : ALl_IO_Array) is
   begin
      Loc_IO_Write (This, LOGIC_LEVLEL_REG, To_UInt8 (IOs));
   end Set_All_IO;

   -------------------
   -- As_GPIO_Point --
   -------------------

   function As_GPIO_Point (This : in out MCP23x08_IO_Expander;
                            Pin  : MCP23x08_Pin)
                            return not null HAL.GPIO.Any_GPIO_Point
   is
   begin
      This.Points (Pin) := (Device => This'Unchecked_Access,
                            Pin    => Pin);
      return This.Points (Pin)'Unchecked_Access;
   end As_GPIO_Point;

   ----------
   -- Mode --
   ----------

   overriding
   function Mode (This : MCP23_GPIO_Point) return HAL.GPIO.GPIO_Mode is
   begin
      if This.Device.Is_Output (This.Pin) then
         return HAL.GPIO.Output;
      else
         return HAL.GPIO.Input;
      end if;
   end Mode;


   --------------
   -- Set_Mode --
   --------------

   overriding
   procedure Set_Mode (This : in out MCP23_GPIO_Point;
                       Mode : HAL.GPIO.GPIO_Config_Mode)
   is
   begin
      This.Device.Configure_Mode (Pin    => This.Pin,
                                  Output => (Mode = HAL.GPIO.Output));
   end Set_Mode;

   -------------------
   -- Pull_Resistor --
   -------------------

   overriding
   function Pull_Resistor (This : MCP23_GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor
   is
   begin
      return (if This.Device.Pull_Up (This.Pin) then
                 HAL.GPIO.Pull_Up
              else
                 HAL.GPIO.Floating);
   end Pull_Resistor;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding
   procedure Set_Pull_Resistor (This : in out MCP23_GPIO_Point;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor)
   is
   begin
      This.Device.Configure_Pull (This.Pin, Pull = HAL.GPIO.Pull_Up);
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : MCP23_GPIO_Point) return Boolean is
   begin
      return This.Device.Set (This.Pin);
   end Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out MCP23_GPIO_Point) is
   begin
      This.Device.Set (This.Pin);
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out MCP23_GPIO_Point) is
   begin
      This.Device.Clear (This.Pin);
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out MCP23_GPIO_Point) is
   begin
      This.Device.Toggle (This.Pin);
   end Toggle;

end MCP23x08;
