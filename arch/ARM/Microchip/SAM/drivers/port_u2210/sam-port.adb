------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with HAL;      use HAL;
with HAL.GPIO; use HAL.GPIO;

package body SAM.Port is

   function Pin_Bit (This : GPIO_Point'Class) return UInt32
     with Inline_Always;

   -------------
   -- Pin_Bit --
   -------------

   function Pin_Bit (This : GPIO_Point'Class) return UInt32
   is (Shift_Left (1, Natural (This.Pin)));

   ------------------
   -- Set_Function --
   ------------------

   procedure Set_Function (This : in out GPIO_Point;
                           Func :        Peripheral_Function)
   is
      PINCFG : PINCFG_Register renames
        This.Controller.Periph.PINCFG (This.Pin);

      Even  : constant Boolean := (This.Pin mod 2) = 0;
      Index : constant Integer := Integer (This.Pin) / 2;
   begin
      if Func = Disabled then
         PINCFG.PMUXEN := False;
      else
         PINCFG.PMUXEN := True;
         if Even then
            This.Controller.Periph.PMUX (Index).PMUXE := Func'Enum_Rep;
         else
            This.Controller.Periph.PMUX (Index).PMUXO := Func'Enum_Rep;
         end if;
      end if;
   end Set_Function;

   ----------
   -- Mode --
   ----------

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode is
   begin
      if (This.Controller.Periph.DIR and This.Pin_Bit) /= 0 then
         return Output;
      else
         return Input;
      end if;
   end Mode;

   --------------
   -- Set_Mode --
   --------------

   overriding
   procedure Set_Mode (This : in out GPIO_Point;
                       Mode : HAL.GPIO.GPIO_Config_Mode)
   is
      PINCFG : PINCFG_Register renames
        This.Controller.Periph.PINCFG (This.Pin);
   begin
      if Mode = Output then
         This.Controller.Periph.DIRSET := This.Pin_Bit;
      else
         This.Controller.Periph.DIRCLR := This.Pin_Bit;
         PINCFG.INEN := True;
      end if;
   end Set_Mode;

   -------------------
   -- Pull_Resistor --
   -------------------

   overriding
   function Pull_Resistor (This : GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor
   is

      PINCFG : PINCFG_Register renames
        This.Controller.Periph.PINCFG (This.Pin);

   begin
      if not PINCFG.PULLEN then
         return Floating;
      elsif (This.Controller.Periph.DATA_OUT and This.Pin_Bit) /= 0 then
         return Pull_Up;
      else
         return Pull_Down;
      end if;
   end Pull_Resistor;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding
   procedure Set_Pull_Resistor (This : in out GPIO_Point;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor)

   is
      PINCFG : PINCFG_Register renames
        This.Controller.Periph.PINCFG (This.Pin);

      Pin : constant UInt32 := This.Pin_Bit;
   begin
      if Pull = Floating then
         PINCFG.PULLEN := False;
      else
         PINCFG.PULLEN := True;
         if Pull = Pull_Up then
            This.Controller.Periph.OUTSET := Pin;
         else
            This.Controller.Periph.OUTCLR := Pin;
         end if;
      end if;
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : GPIO_Point) return Boolean is
   begin
      return (This.Controller.Periph.DATA_IN and This.Pin_Bit) /= 0;
   end Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out GPIO_Point) is
   begin
      This.Controller.Periph.OUTSET := This.Pin_Bit;
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out GPIO_Point) is
   begin
      This.Controller.Periph.OUTCLR := This.Pin_Bit;
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out GPIO_Point) is
   begin
      This.Controller.Periph.OUTTGL := This.Pin_Bit;
   end Toggle;

end SAM.Port;
