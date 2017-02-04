------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

package body Wire_Simulation is

   -----------
   -- Point --
   -----------

   function Point
     (This : in out Virtual_Wire;
      Id   : Positive)
      return Any_GPIO_Point
   is
   begin
      This.Points (Id).Wire := This'Unchecked_Access;
      return This.Points (Id)'Unchecked_Access;
   end Point;

   --------------
   -- Set_Mode --
   --------------

   overriding function Set_Mode
     (This : in out Wire_Point;
      Mode : GPIO_Config_Mode)
      return Boolean
   is
   begin
      This.Current_Mode := Input;
      if Mode = Output and then This.Wire.At_Least_One_Output then
         raise Invalid_Configuration;
      end if;
      This.Current_Mode := Mode;
      This.Wire.Update_Wire_State;
      return True;
   end Set_Mode;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding function Set_Pull_Resistor
     (This : in out Wire_Point;
      Pull : GPIO_Pull_Resistor)
      return Boolean
   is
   begin
      This.Current_Pull := Floating;

      if Pull = Pull_Down
        and then
          (This.Wire.At_Least_One_Pull_Up
           or else
           This.Wire.Default_Pull = Pull_Up)
      then
         raise Invalid_Configuration;
      elsif Pull = Pull_Up
        and then
          (This.Wire.At_Least_One_Pull_Down
           or else
           This.Wire.Default_Pull = Pull_Down)
      then
         raise Invalid_Configuration;
      else
         This.Current_Pull := Pull;
         This.Wire.Update_Wire_State;
         return True;
      end if;
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding function Set
     (This : Wire_Point)
      return Boolean
   is
   begin
      case This.Wire.State is
         when Unknown =>
            raise Unknown_State;
         when High =>
            return True;
         when Low =>
            return False;
      end case;
   end Set;

   ---------
   -- Set --
   ---------

   overriding procedure Set
     (This : in out Wire_Point)
   is
   begin
      This.Current_State := True;
      This.Wire.Update_Wire_State;
   end Set;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (This : in out Wire_Point)
   is
   begin
      This.Current_State := False;
      This.Wire.Update_Wire_State;
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding procedure Toggle
     (This : in out Wire_Point)
   is
   begin
      This.Current_State := not This.Current_State;
      This.Wire.Update_Wire_State;
   end Toggle;

   -----------------------
   -- Update_Wire_State --
   -----------------------

   procedure Update_Wire_State (This : in out Virtual_Wire) is
      Has_Pull_Up : constant Boolean :=
        This.At_Least_One_Pull_Up or This.Default_Pull = Pull_Up;
      Has_Pull_Down : constant Boolean :=
        This.At_Least_One_Pull_Down  or This.Default_Pull = Pull_Down;

      State : Wire_State := Unknown;
   begin
      pragma Assert ((Has_Pull_Down /= Has_Pull_Up) or else not Has_Pull_Up,
                     "Invalid config. This should've been caught before");

      if Has_Pull_Down then
         State := Low;
      elsif Has_Pull_Up then
         State := High;
      end if;

      for Pt of This.Points loop
         if Pt.Current_Mode = Output then
            if Pt.Current_State then
               State := High;
            else
               State := Low;
            end if;
            exit;
         end if;
      end loop;
      This.State := State;
   end Update_Wire_State;

   -------------------------
   -- At_Least_One_Output --
   -------------------------

   function At_Least_One_Output (This : Virtual_Wire) return Boolean is
     (for some Pt of This.Points => Pt.Current_Mode = Output);

   --------------------------
   -- At_Least_One_Pull_Up --
   --------------------------

   function At_Least_One_Pull_Up (This : Virtual_Wire) return Boolean is
     (for some Pt of This.Points => Pt.Current_Pull = Pull_Up);

   ----------------------------
   -- At_Least_One_Pull_Down --
   ----------------------------

   function At_Least_One_Pull_Down (This : Virtual_Wire) return Boolean is
     (for some Pt of This.Points => Pt.Current_Pull = Pull_Down);

end Wire_Simulation;
