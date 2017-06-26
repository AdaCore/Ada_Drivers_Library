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

with nRF51.GPIO; use nRF51.GPIO;
with nRF51.ADC;  use nRF51.ADC;

package body MicroBit.IOs is

   Points : array (Pin_Id) of GPIO_Point :=
     (0  => MB_P0,
      1  => MB_P1,
      2  => MB_P2,
      3  => MB_P3,
      4  => MB_P4,
      5  => MB_P5,
      6  => MB_P6,
      7  => MB_P7,
      8  => MB_P8,
      9  => MB_P9,
      10 => MB_P10,
      11 => MB_P11,
      12 => MB_P12,
      13 => MB_P13,
      14 => MB_P14,
      15 => MB_P15,
      16 => MB_P16,
      17 => MB_P0,  --  There's no pin17, using P0 to fill in...
      18 => MB_P0,  --  There's no pin18, using P0 to fill in...
      19 => MB_P19,
      20 => MB_P20);

   type Pin_Mode is (None, Digital_In, Digital_Out, Analog_In, Analog_Out);

   Current_Mode : array (Pin_Id) of Pin_Mode := (others => None);

   ---------
   -- Set --
   ---------

   procedure Set
     (Pin : Pin_Id;
      Value : Boolean)
   is
      Pt   : GPIO_Point renames Points (Pin);
      Conf : GPIO_Configuration;
   begin
      if Current_Mode (Pin) /= Digital_Out then
         Conf.Mode         := Mode_Out;
         Conf.Resistors    := No_Pull;
         Conf.Input_Buffer := Input_Buffer_Connect;
         Conf.Sense        := Sense_Disabled;

         Pt.Configure_IO (Conf);
         Current_Mode (Pin) := Digital_Out;
      end if;

      if Value then
         Pt.Set;
      else
         Pt.Clear;
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   function Set
     (Pin : Pin_Id)
      return Boolean
   is
      Pt   : GPIO_Point renames Points (Pin);
      Conf : GPIO_Configuration;
   begin
      if Current_Mode (Pin) /= Digital_In then
         Conf.Mode         := Mode_In;
         Conf.Resistors    := No_Pull;
         Conf.Input_Buffer := Input_Buffer_Connect;
         Conf.Sense        := Sense_Disabled;

         Pt.Configure_IO (Conf);

         Current_Mode (Pin) := Digital_In;
      end if;

      return Pt.Set;
   end Set;

   -----------
   -- Write --
   -----------

   procedure Write
     (Pin : Pin_Id;
      Value : Analog_Value)
   is
      pragma Unreferenced (Value);
   begin
      if Current_Mode (Pin) /= Analog_Out then
         Current_Mode (Pin) := Analog_Out;
      end if;
      --  Generated stub: replace with real body!
      raise Program_Error with "Unimplemented procedure Write_Analog";
   end Write;

   ------------
   -- Analog --
   ------------

   function Analog
     (Pin : Pin_Id)
      return Analog_Value
   is
   begin
      if Current_Mode (Pin) /= Analog_In then
         Current_Mode (Pin) := Analog_In;
      end if;

      Start_Pin_Conversion (Pin   => (case Pin is
                                         when 0      => 4,
                                         when 1      => 3,
                                         when 2      => 2,
                                         when 3      => 5,
                                         when 4      => 6,
                                         when 10     => 7,
                                         when others => 0),
                            Input => Pin_One_Third,
                            Ref   => VDD_One_Third,
                            Res   => 10);
      return Analog_Value (Wait_For_Result);
   end Analog;

   -------------
   -- Touched --
   -------------

   function Touched
     (Pin : Pin_Id)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error with "Unimplemented function Is_Touched";
      return Touched (Pin => Pin);
   end Touched;

end MicroBit.IOs;
