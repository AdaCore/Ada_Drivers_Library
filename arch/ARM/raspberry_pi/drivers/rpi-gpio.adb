------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with System.Machine_Code;

with RPi.Regs.GPIO; use RPi.Regs.GPIO;

package body RPi.GPIO is

   procedure Wait_150;
   --  Waits 150 Cycles

   --------------
   -- Wait_150 --
   --------------

   procedure Wait_150
   is
      use System.Machine_Code;
   begin
      for J in 1 .. 150 loop
         Asm ("nop", Volatile => True);
      end loop;
   end Wait_150;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This     : GPIO_Point;
      Mode     : Pin_Mode)
   is
      Sel : AF_Selector;

   begin
      case Mode is
         when Mode_In =>
            Sel := Input;
         when Mode_Out =>
            Sel := Output;
         when Mode_AF0 =>
            Sel := AF0;
         when Mode_AF1 =>
            Sel := AF1;
         when Mode_AF2 =>
            Sel := AF2;
         when Mode_AF3 =>
            Sel := AF3;
         when Mode_AF4 =>
            Sel := AF4;
         when Mode_AF5 =>
            Sel := AF5;
      end case;

      if This.Pin in GPIO_Periph.GPFSEL0.FSel'Range then
         GPIO_Periph.GPFSEL0.FSel (This.Pin) := Sel;
      elsif This.Pin in GPIO_Periph.GPFSEL1.FSel'Range then
         GPIO_Periph.GPFSEL1.FSel (This.Pin) := Sel;
      elsif This.Pin in GPIO_Periph.GPFSEL2.FSel'Range then
         GPIO_Periph.GPFSEL2.FSel (This.Pin) := Sel;
      elsif This.Pin in GPIO_Periph.GPFSEL3.FSel'Range then
         GPIO_Periph.GPFSEL3.FSel (This.Pin) := Sel;
      elsif This.Pin in GPIO_Periph.GPFSEL4.FSel'Range then
         GPIO_Periph.GPFSEL4.FSel (This.Pin) := Sel;
      else
         GPIO_Periph.GPFSEL5.FSel (This.Pin) := Sel;
      end if;
   end Configure;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This     : GPIO_Points;
      Mode     : Pin_Mode)
   is
      Sel : AF_Selector;

   begin
      case Mode is
         when Mode_In =>
            Sel := Input;
         when Mode_Out =>
            Sel := Output;
         when Mode_AF0 =>
            Sel := AF0;
         when Mode_AF1 =>
            Sel := AF1;
         when Mode_AF2 =>
            Sel := AF2;
         when Mode_AF3 =>
            Sel := AF3;
         when Mode_AF4 =>
            Sel := AF4;
         when Mode_AF5 =>
            Sel := AF5;
      end case;

      for Point of This loop
         if Point.Pin in GPIO_Periph.GPFSEL0.FSel'Range then
            GPIO_Periph.GPFSEL0.FSel (Point.Pin) := Sel;
         elsif Point.Pin in GPIO_Periph.GPFSEL1.FSel'Range then
            GPIO_Periph.GPFSEL1.FSel (Point.Pin) := Sel;
         elsif Point.Pin in GPIO_Periph.GPFSEL2.FSel'Range then
            GPIO_Periph.GPFSEL2.FSel (Point.Pin) := Sel;
         elsif Point.Pin in GPIO_Periph.GPFSEL3.FSel'Range then
            GPIO_Periph.GPFSEL3.FSel (Point.Pin) := Sel;
         elsif Point.Pin in GPIO_Periph.GPFSEL4.FSel'Range then
            GPIO_Periph.GPFSEL4.FSel (Point.Pin) := Sel;
         else
            GPIO_Periph.GPFSEL5.FSel (Point.Pin) := Sel;
         end if;
      end loop;
   end Configure;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This     : GPIO_Point;
      Resistor : Pin_Resistance)
   is
   begin
      case Resistor is
         when Open_Drain =>
            GPIO_Periph.GPPUD.PUD := Off;
         when Pull_Up =>
            GPIO_Periph.GPPUD.PUD := Pull_Up;
         when Pull_Down =>
            GPIO_Periph.GPPUD.PUD := Pull_Down;
      end case;

      Wait_150;

      if This.Pin in GPIO_Periph.GPPUDCLK0.PUDCLK'Range then
         GPIO_Periph.GPPUDCLK0.PUDCLK (This.Pin) := True;
      else
         GPIO_Periph.GPPUDCLK1.PUDCLK (This.Pin) := True;
      end if;

      Wait_150;

      GPIO_Periph.GPPUD.PUD := Off;
      if This.Pin in GPIO_Periph.GPPUDCLK0.PUDCLK'Range then
         GPIO_Periph.GPPUDCLK0.PUDCLK (This.Pin) := False;
      else
         GPIO_Periph.GPPUDCLK1.PUDCLK (This.Pin) := False;
      end if;
   end Configure;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This     : GPIO_Points;
      Resistor : Pin_Resistance)
   is
   begin
      case Resistor is
         when Open_Drain =>
            GPIO_Periph.GPPUD.PUD := Off;
         when Pull_Up =>
            GPIO_Periph.GPPUD.PUD := Pull_Up;
         when Pull_Down =>
            GPIO_Periph.GPPUD.PUD := Pull_Down;
      end case;

      Wait_150;

      for Point of This loop
         if Point.Pin in GPIO_Periph.GPPUDCLK0.PUDCLK'Range then
            GPIO_Periph.GPPUDCLK0.PUDCLK (Point.Pin) := True;
         else
            GPIO_Periph.GPPUDCLK1.PUDCLK (Point.Pin) := True;
         end if;
      end loop;

      Wait_150;

      GPIO_Periph.GPPUD.PUD := Off;

      for Point of This loop
         if Point.Pin in GPIO_Periph.GPPUDCLK0.PUDCLK'Range then
            GPIO_Periph.GPPUDCLK0.PUDCLK (Point.Pin) := False;
         else
            GPIO_Periph.GPPUDCLK1.PUDCLK (Point.Pin) := False;
         end if;
      end loop;
   end Configure;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding function Set_Pull_Resistor
     (This : in out GPIO_Point;
      Pull : HAL.GPIO.GPIO_Pull_Resistor) return Boolean
   is
   begin
      case Pull is
         when HAL.GPIO.Floating =>
            Configure (This, Open_Drain);
         when HAL.GPIO.Pull_Up =>
            Configure (This, Pull_Up);
         when HAL.GPIO.Pull_Down =>
            Configure (This, Pull_Down);
      end case;

      return True;
   end Set_Pull_Resistor;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This    : GPIO_Point;
      Trigger : GPIO_Trigger)
   is
      Ren   : constant Boolean :=
                (Trigger and Trigger_Rising_Edge_Detect) /= 0;
      Fen   : constant Boolean :=
                (Trigger and Trigger_Falling_Edge_Detect) /= 0;
      Hen   : constant Boolean :=
                (Trigger and Trigger_High_Detect) /= 0;
      Len   : constant Boolean :=
                (Trigger and Trigger_Low_Detect) /= 0;
      A_Ren : constant Boolean :=
                (Trigger and Trigger_Async_Rising_Edge_Detect) /= 0;
      A_Fen : constant Boolean :=
                (Trigger and Trigger_Async_Falling_Edge_Detect) /= 0;

   begin
      if This.Pin in GPIO_Periph.GPREN0.REN'Range then
         GPIO_Periph.GPREN0.REN (This.Pin) := Ren;
         GPIO_Periph.GPFEN0.FEN (This.Pin) := Fen;
         GPIO_Periph.GPHEN0.HEN (This.Pin) := Hen;
         GPIO_Periph.GPLEN0.LEN (This.Pin) := Len;
         GPIO_Periph.GPAREN0.AREN (This.Pin) := A_Ren;
         GPIO_Periph.GPAFEN0.AFEN (This.Pin) := A_Fen;
      else
         GPIO_Periph.GPREN1.REN (This.Pin) := Ren;
         GPIO_Periph.GPFEN1.FEN (This.Pin) := Fen;
         GPIO_Periph.GPHEN1.HEN (This.Pin) := Hen;
         GPIO_Periph.GPLEN1.LEN (This.Pin) := Len;
         GPIO_Periph.GPAREN1.AREN (This.Pin) := A_Ren;
         GPIO_Periph.GPAFEN1.AFEN (This.Pin) := A_Fen;
      end if;
   end Configure;

   ---------
   -- Set --
   ---------

   overriding function Set (This : GPIO_Point) return Boolean
   is
   begin
      if This.Pin in GPIO_Periph.GPLEV0.LEV'Range then
         return GPIO_Periph.GPLEV0.LEV (This.Pin);
      else
         return GPIO_Periph.GPLEV1.LEV (This.Pin);
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   overriding procedure Set (This : in out GPIO_Point)
   is
   begin
      if This.Pin in GPIO_Periph.GPSET0.SET'Range then
         GPIO_Periph.GPSET0.SET (This.Pin) := True;
      else
         GPIO_Periph.GPSET1.SET (This.Pin) := True;
      end if;
   end Set;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (This : in out GPIO_Point)
   is
   begin
      if This.Pin in GPIO_Periph.GPCLR0.CLR'Range then
         GPIO_Periph.GPCLR0.CLR (This.Pin) := True;
      else
         GPIO_Periph.GPCLR1.CLR (This.Pin) := True;
      end if;
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding procedure Toggle (This : in out GPIO_Point)
   is
   begin
      if Set (This) then
         Clear (This);
      else
         Set (This);
      end if;
   end Toggle;

   --------------------
   -- Event_Detected --
   --------------------

   function Event_Detected (This : GPIO_Point) return Boolean
   is
   begin
      if This.Pin in GPIO_Periph.GPEDS0.EDS'Range then
         return GPIO_Periph.GPEDS0.EDS (This.Pin);
      else
         return GPIO_Periph.GPEDS1.EDS (This.Pin);
      end if;
   end Event_Detected;

   -----------------
   -- Clear_Event --
   -----------------

   procedure Clear_Event (This : GPIO_Point)
   is
   begin
      if This.Pin in GPIO_Periph.GPEDS0.EDS'Range then
         GPIO_Periph.GPEDS0.EDS (This.Pin) := True;
      else
         GPIO_Periph.GPEDS1.EDS (This.Pin) := True;
      end if;
   end Clear_Event;

end RPi.GPIO;
