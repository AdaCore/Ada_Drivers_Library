------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

with HAL.GPIO; use HAL.GPIO;

with Wire_Simulation; use Wire_Simulation;
with Ada.Text_IO;   use Ada.Text_IO;

procedure TC_Virtual_Wire is
   pragma Assertion_Policy (Assert => Check);

   No_Pull_Wire : Virtual_Wire (Default_Pull => Floating,
                                Max_Points   => 2);

   Pull_Up_Wire : Virtual_Wire (Default_Pull => Pull_Up,
                                Max_Points   => 2);

   Pull_Down_Wire : Virtual_Wire (Default_Pull => Pull_Down,
                                  Max_Points   => 2);

   Unref : Boolean with Unreferenced;
begin

   -- Default mode --

   pragma Assert (No_Pull_Wire.Point (1).Mode = Input,
                  "Default point mode should be input");

   -- State with only inputs and a wire pull resistor --

   pragma Assert (Pull_Up_Wire.Point (1).Set,
                  "Default state of pull up wire should be high");
   pragma Assert (not Pull_Down_Wire.Point (1).Set,
                  "Default state of pull down wire should be low");

   -- State with only inputs and a point pull resistor --

   pragma Assert (No_Pull_Wire.Point (1).Support (Pull_Up),
                  "It should be possible to change the pull resitor");
   No_Pull_Wire.Point (1).Set_Pull_Resistor (Pull_Up);

   pragma Assert (No_Pull_Wire.Point (1).Set,
                  "State of wire with one pull up point should be high");


   No_Pull_Wire.Point (1).Set_Pull_Resistor (Pull_Down);
   pragma Assert (not No_Pull_Wire.Point (1).Set,
                  "State of wire with one pull down point should be low");

   -- State with one input one output and no pull resistor --

   pragma Assert (No_Pull_Wire.Point (1).Support (Input),
                  "It should be possible to change the mode");
   No_Pull_Wire.Point (1).Set_Mode (Input);
   No_Pull_Wire.Point (1).Set_Pull_Resistor (Pull_Down);

   No_Pull_Wire.Point (2).Set_Mode (Output);
   No_Pull_Wire.Point (2).Set_Pull_Resistor (Floating);

   No_Pull_Wire.Point (2).Set;
   pragma Assert (No_Pull_Wire.Point (1).Set, "Should be high");
   No_Pull_Wire.Point (2).Clear;
   pragma Assert (not No_Pull_Wire.Point (1).Set, "Should be low");

   -- State with one input one output and point pull resistor --

   No_Pull_Wire.Point (1).Set_Mode (Input);
   No_Pull_Wire.Point (1).Set_Pull_Resistor (Pull_Up);

   No_Pull_Wire.Point (2).Set_Mode (Output);
   No_Pull_Wire.Point (2).Set_Pull_Resistor (Floating);

   No_Pull_Wire.Point (2).Set;
   pragma Assert (No_Pull_Wire.Point (1).Set, "Should be high");
   No_Pull_Wire.Point (2).Clear;
   pragma Assert (not No_Pull_Wire.Point (1).Set, "Should be low");

   No_Pull_Wire.Point (1).Set_Pull_Resistor (Pull_Down);

   No_Pull_Wire.Point (2).Set;
   pragma Assert (No_Pull_Wire.Point (1).Set, "Should be high");
   No_Pull_Wire.Point (2).Clear;
   pragma Assert (not No_Pull_Wire.Point (1).Set, "Should be low");

   -- Opposite pull on the same wire --
   declare
   begin
      Pull_Down_Wire.Point (1).Set_Pull_Resistor (Pull_Up);
   exception
      when Invalid_Configuration =>
         Put_Line ("Expected exception on oppposite pull (1)");
   end;

   declare
   begin
      Pull_Up_Wire.Point (1).Set_Pull_Resistor (Pull_Down);
   exception
      when Invalid_Configuration =>
         Put_Line ("Expected exception on oppposite pull (2)");
   end;

   -- Two output point on a wire --
   declare
   begin
      Pull_Up_Wire.Point (1).Set_Mode (Output);
      Pull_Up_Wire.Point (2).Set_Mode (Output);
   exception
      when Invalid_Configuration =>
         Put_Line ("Expected exception on multiple output points");
   end;

   -- Unknon state --
   declare
   begin
      No_Pull_Wire.Point (1).Set_Mode (Input);
      No_Pull_Wire.Point (1).Set_Pull_Resistor (Floating);
      No_Pull_Wire.Point (2).Set_Mode (Input);
      No_Pull_Wire.Point (2).Set_Pull_Resistor (Floating);
      Unref := No_Pull_Wire.Point (2).Set;
   exception
      when Unknown_State =>
         Put_Line ("Expected exception on unknown state");
   end;

end TC_Virtual_Wire;
