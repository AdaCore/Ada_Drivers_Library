------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2022, AdaCore                           --
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

package body MCP23x17 is

   function "+" is new Ada.Unchecked_Conversion (Source => UInt8,
                                                 Target => Port_IO_Array);

   function "+" is new Ada.Unchecked_Conversion (Source => Port_IO_Array,
                                                 Target => UInt8);


   procedure Loc_IO_Write
     (This  : in out MCP23x17_IO_Expander'Class;
      Addr  : Register_Address;
      Value : UInt8)
     with Inline_Always;

   procedure Loc_IO_Read
     (This  : MCP23x17_IO_Expander'Class;
      Addr  : Register_Address;
      Value : out UInt8)
     with Inline_Always;

   procedure Set_Bit
     (This : in out MCP23x17_IO_Expander;
      Addr : Register_Address;
      Pin  : MCP23x17_Pin_Number;
      Val  : Boolean := True);

   function Read_Bit
     (This : MCP23x17_IO_Expander;
      Addr : Register_Address;
      Pin  : MCP23x17_Pin_Number)
      return Boolean;

   ------------------
   -- Loc_IO_Write --
   ------------------

   procedure Loc_IO_Write
     (This   : in out MCP23x17_IO_Expander'Class;
      Addr   : Register_Address;
      Value  : UInt8)
   is

   begin
      IO_Write (This, Addr, Value);
   end Loc_IO_Write;

   -----------------
   -- Loc_IO_Read --
   -----------------

   procedure Loc_IO_Read
     (This  : MCP23x17_IO_Expander'Class;
      Addr  : Register_Address;
      Value : out UInt8)
      is
   begin
      IO_Read (This, Addr, Value);
   end Loc_IO_Read;

   -------------
   -- Set_Bit --
   -------------

   procedure Set_Bit
     (This : in out MCP23x17_IO_Expander;
      Addr : Register_Address;
      Pin  : MCP23x17_Pin_Number;
      Val  : Boolean := True)
   is
      Prev : UInt8;
      Next : Port_IO_Array;
   begin
      Loc_IO_Read (This, Addr, Prev);
      Next := +Prev;
      Next (Pin) := Val;
      if +Next /= Prev then
         Loc_IO_Write (This, Addr, +Next);
      end if;
   end Set_Bit;

   --------------
   -- Read_Bit --
   --------------

   function Read_Bit
     (This : MCP23x17_IO_Expander;
      Addr : Register_Address;
      Pin  : MCP23x17_Pin_Number)
      return Boolean
   is
      Reg : UInt8;
      IOs : Port_IO_Array;
   begin
      Loc_IO_Read (This, Addr, Reg);
      IOs := +Reg;
      return IOs (Pin);
   end Read_Bit;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (This         : in out MCP23x17_IO_Expander;
                        Pin          : MCP23x17_Pin;
                        Output       : Boolean;
                        Pull_Up      : Boolean;
                        Invert_Input : Boolean := False)
   is
   begin
      This.Configure_Mode (Pin, Output);
      This.Configure_Pull_Up (Pin, Pull_Up);
      This.Configure_Polarity (Pin, Invert_Input);
   end Configure;

   procedure Configure_Mode (This    : in out MCP23x17_IO_Expander;
                             Pin     : MCP23x17_Pin;
                             Output  : Boolean)
   is
      --  1 = Pin is configured as an input.
      --  0 = Pin is configured as an output.
      --  Data Sheet 3.5.1 I/O Direction Register
   begin
      case Pin.Port is
      when A => Set_Bit (This, IO_Dir_A, Pin.Pin_Nr, not Output);
      when B => Set_Bit (This, IO_Dir_B, Pin.Pin_Nr, not Output);
      end case;
   end Configure_Mode;

   --  Configure all pins of a port as input or output
   procedure Configure_Mode (This    : in out MCP23x17_IO_Expander;
                             Port    : MCP23x17_Port;
                             Output  : Boolean)
   is begin
      case Port is
      when A => Loc_IO_Write (This, IO_Dir_A, (if Output then 16#00# else 16#FF#));
      when B => Loc_IO_Write (This, IO_Dir_B, (if Output then 16#00# else 16#FF#));
      end case;
   end Configure_Mode;

   procedure Configure_Polarity (This         : in out MCP23x17_IO_Expander;
                                 Pin          : MCP23x17_Pin;
                                 Invert_Input : Boolean)
   is
   begin
      case Pin.Port is
      when A => Set_Bit (This, I_Pol_A, Pin.Pin_Nr, Invert_Input);
      when B => Set_Bit (This, I_Pol_B, Pin.Pin_Nr, Invert_Input);
      end case;
   end Configure_Polarity;

   procedure Configure_Polarity (This         : in out MCP23x17_IO_Expander;
                                 Port         : MCP23x17_Port;
                                 Invert_Input : Boolean)
   is
   begin
      case Port is
      when A =>  Loc_IO_Write (This, I_Pol_A, (if Invert_Input then 16#FF# else 16#00#));
      when B =>  Loc_IO_Write (This, I_Pol_B, (if Invert_Input then 16#FF# else 16#00#));
      end case;
   end Configure_Polarity;

   function Input_Is_Inverted (This : MCP23x17_IO_Expander;
                               Pin  : MCP23x17_Pin) return Boolean
   is
   begin
      case Pin.Port is
      when A => return Read_Bit (This, I_Pol_A, Pin.Pin_Nr);
      when B => return Read_Bit (This, I_Pol_B, Pin.Pin_Nr);
      end case;
   end Input_Is_Inverted;

   ---------------
   -- Is_Output --
   ---------------

   function Is_Output (This : in out MCP23x17_IO_Expander;
                       Pin  : MCP23x17_Pin)
                       return Boolean
   is
   begin
      case Pin.Port is
      when A => return not Read_Bit (This, IO_Dir_A, Pin.Pin_Nr);
      when B => return not Read_Bit (This, IO_Dir_B, Pin.Pin_Nr);
      end case;
   end Is_Output;

   -----------------------
   -- Configure_Pull_Up --
   -----------------------

   procedure Configure_Pull_Up (This    : in out MCP23x17_IO_Expander;
                                Pin     : MCP23x17_Pin;
                                Pull_Up : Boolean)
   is
      Reg : constant Register_Address :=
           (if Pin.Port = A then GP_Pu_A else GP_Pu_B);
   begin
      Set_Bit (This, Reg, Pin.Pin_Nr, Pull_Up);
   end Configure_Pull_Up;

   procedure Configure_Pull_Up (This    : in out MCP23x17_IO_Expander;
                                Port    : MCP23x17_Port;
                                Pull_Up : Boolean)
   is
      Reg : constant Register_Address :=
           (if Port = A then GP_Pu_A else GP_Pu_B);
   begin
      Loc_IO_Write (This, Reg, (if Pull_Up then 16#FF# else 16#00#));
   end Configure_Pull_Up;

   -----------------
   -- Has_Pull_Up --
   -----------------

   function Has_Pull_Up (This : MCP23x17_IO_Expander;
                     Pin  : MCP23x17_Pin) return Boolean
   is
      Reg : constant Register_Address :=
           (if Pin.Port = A then GP_Pu_A else GP_Pu_B);
   begin
      return Read_Bit (This, Reg, Pin.Pin_Nr);
   end Has_Pull_Up;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (This  : MCP23x17_IO_Expander;
                    Pin   : MCP23x17_Pin) return Boolean
   is
      Reg : constant Register_Address :=
           (if Pin.Port = A then GP_IO_A else GP_IO_B);
      Val : UInt8;
      IOs : Port_IO_Array;
   begin
      Loc_IO_Read (This, Reg, Val);
      IOs := +Val;
      return IOs (Pin.Pin_Nr);
   end Is_Set;

   ---------
   -- Set --
   ---------

   procedure Set (This  : in out MCP23x17_IO_Expander;
                  Pin   : MCP23x17_Pin;
                  Value : Boolean := True)
   is
   begin
      case Pin.Port is
      when A => Set_Bit (This, GP_IO_A, Pin.Pin_Nr, Value);
      when B => Set_Bit (This, GP_IO_B, Pin.Pin_Nr, Value);
      end case;
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (This  : in out MCP23x17_IO_Expander;
                    Pin   : MCP23x17_Pin)
   is
   begin
      Set (This, Pin, False);
   end Clear;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (This  : in out MCP23x17_IO_Expander;
                     Pin   : MCP23x17_Pin)
   is
   begin
      Set (This, Pin, not Is_Set (This, Pin));
   end Toggle;

   ------------
   -- Get_IO --
   ------------

   --  Return the current status of all pins of a port in an array
   function Get_IO (This : in out MCP23x17_IO_Expander;
                    Port : MCP23x17_Port) return Port_IO_Array
   is
      Val : UInt8;
   begin
      if Port = A then
         Loc_IO_Read (This, GP_IO_A, Val);
      else
         Loc_IO_Read (This, GP_IO_B, Val);
      end if;

      return +Val;
   end Get_IO;

   --  Return the current status of all pins of a given port in an array
   procedure Get_IO (This : in out MCP23x17_IO_Expander;
                     Port : MCP23x17_Port;
                     IOs  : out Port_IO_Array)
   is
   begin
      IOs := Get_IO (This, Port);
   end Get_IO;

   --  Set the current status of selected pins with an array
   procedure Set_IO (This : in out MCP23x17_IO_Expander;
                     Port : MCP23x17_Port;
                     IOs  : Port_IO_Array)
   is
   begin
      case Port is
      when A => Loc_IO_Write (This, GP_IO_A, +IOs);
      when B => Loc_IO_Write (This, GP_IO_B, +IOs);
      end case;
   end Set_IO;

   -------------------
   -- As_GPIO_Point --
   -------------------

   function As_GPIO_Point (This : in out MCP23x17_IO_Expander;
                           Pin  : MCP23x17_Pin)
                          return not null HAL.GPIO.Any_GPIO_Point
   is
   begin
      case Pin.Port is
      when A =>
         This.A_Points (Pin.Pin_Nr) := (Device => This'Unchecked_Access,
                                        Port   => A,
                                        Pin_Nr => Pin.Pin_Nr);
         return This.A_Points (Pin.Pin_Nr)'Unchecked_Access;
      when B =>
         This.B_Points (Pin.Pin_Nr) := (Device => This'Unchecked_Access,
                                        Port   => B,
                                        Pin_Nr => Pin.Pin_Nr);
         return This.B_Points (Pin.Pin_Nr)'Unchecked_Access;
      end case;
   end As_GPIO_Point;

   ----------
   -- Mode --
   ----------

   overriding
   function Mode (This : MCP23x17_GPIO_Point) return HAL.GPIO.GPIO_Mode is
   begin
      return (if This.Device.Is_Output (Pin => (This.Port, This.Pin_Nr))
              then HAL.GPIO.Output else HAL.GPIO.Input);
   end Mode;

   --------------
   -- Set_Mode --
   --------------

   overriding
   procedure Set_Mode (This : in out MCP23x17_GPIO_Point;
                       Mode : HAL.GPIO.GPIO_Config_Mode)
   is
   begin
      This.Device.Configure_Mode (Pin    => (This.Port, This.Pin_Nr),
                                  Output => (Mode = HAL.GPIO.Output));
   end Set_Mode;

   -------------------
   -- Pull_Resistor --
   -------------------

   overriding
   function Pull_Resistor (This : MCP23x17_GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor
   is
   begin
      return (if This.Device.Has_Pull_Up ((This.Port, This.Pin_Nr)) then
                 HAL.GPIO.Pull_Up
              else
                 HAL.GPIO.Floating);
   end Pull_Resistor;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding
   procedure Set_Pull_Resistor (This : in out MCP23x17_GPIO_Point;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor)
   is
   begin
      This.Device.Configure_Pull_Up ((This.Port, This.Pin_Nr), Pull = HAL.GPIO.Pull_Up);
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : MCP23x17_GPIO_Point) return Boolean is
   begin
      return This.Device.Is_Set ((This.Port, This.Pin_Nr));
   end Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out MCP23x17_GPIO_Point) is
   begin
      This.Device.Set ((This.Port, This.Pin_Nr), True);
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out MCP23x17_GPIO_Point) is
   begin
      This.Device.Set ((This.Port, This.Pin_Nr), False);
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out MCP23x17_GPIO_Point) is
   begin
      This.Device.Toggle ((This.Port, This.Pin_Nr));
   end Toggle;

   --  set the module configuration
   procedure Set_Module_Configuration (This : in out MCP23x17_IO_Expander;
                                       Conf : Module_Configuration)
   is
      function "+" is new Ada.Unchecked_Conversion (Source => Module_Configuration,
                                                    Target => UInt8);
   begin
      Loc_IO_Write (This, IO_Con, +Conf);
   end Set_Module_Configuration;

   --  return the current module configuration.
   procedure Get_Module_Configuration (This : in out MCP23x17_IO_Expander;
                                       Conf : out Module_Configuration)
   is
      function "+" is new Ada.Unchecked_Conversion (Source => UInt8,
                                                    Target => Module_Configuration);
      C : UInt8;
   begin
      Loc_IO_Read (This, IO_Con, C);
      Conf := +C;
   end Get_Module_Configuration;

end MCP23x17;
