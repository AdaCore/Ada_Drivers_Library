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

with HAL;

private with SAM_SVD.ADC;

package SAM.ADC is

   type ADC_Internal is private;

   type ADC_Device
     (Periph : not null access ADC_Internal)
   is tagged private;

   type Conversion_Resolution is (Res_12bit, Res_16bit, Res_10bit, Res_8bit);

   type Reference_Kind is (Internal_Bandgap, Half_VDDANA, VDDANA,
                           External_A, External_B, External_C);

   type Prescaler_Kind is (Pre_2, Pre_4, Pre_8, Pre_16,
                           Pre_32, Pre_64, Pre_128, Pre_256);

   procedure Configure (This              : in out ADC_Device;
                        Resolution        : Conversion_Resolution;
                        Reference         : Reference_Kind;
                        Prescaler         : Prescaler_Kind;
                        Free_Running      : Boolean;
                        Differential_Mode : Boolean);

   procedure Enable (This : in out ADC_Device);
   --  Enable the ADC channel

   procedure Disable (This : in out ADC_Device);
   --  Disable the ADC channel

   type Negative_Selection is (AIN0, AIN1, AIN2, AIN3, AIN4, AIN5, AIN6, AIN7,
                               GND);

   type Positive_Selection is (AIN0, AIN1, AIN2, AIN3, AIN4, AIN5, AIN6, AIN7,
                               AIN8, AIN9, AIN10, AIN11, AIN12, AIN13, AIN14,
                               AIN15, AIN16, AIN17, AIN18, AIN19, AIN20, AIN21,
                               AIN22, AIN23,
                               SCALEDCOREVCC, SCALEDVBAT, SCALEDIOVCC,
                               BANDGAP, PTAT, CTAT, DAC);

   procedure Set_Inputs (This     : in out ADC_Device;
                         Negative : Negative_Selection;
                         Positive : Positive_Selection);
   --  Set the negative and positive inputs for the conversion

   procedure Software_Start (This : in out ADC_Device);
   --  Start a conversion

   function Conversion_Done (This : in out ADC_Device) return Boolean;
   --  Return True if the conversion is done

   function Result (This : in out ADC_Device) return HAL.UInt16;
   --  Return the result of a conversion

private

   type ADC_Internal is new SAM_SVD.ADC.ADC_Peripheral;

   type ADC_Device
     (Periph : not null access ADC_Internal)
   is tagged null record;


   for Conversion_Resolution use (Res_12bit => 0,
                                  Res_16bit => 1,
                                  Res_10bit => 2,
                                  Res_8bit  => 3);

   for Reference_Kind use (Internal_Bandgap => 0,
                           Half_VDDANA      => 2,
                           VDDANA           => 3,
                           External_A       => 4,
                           External_B       => 5,
                           External_C       => 6);

--     type Prescaler_Kind is (Pre_2, Pre_4, Pre_8, Pre_16,
--                             Pre_32, Pre_64, Pre_128, Pre_256);

   for Negative_Selection use (AIN0 => 16#00#,
                               AIN1 => 16#01#,
                               AIN2 => 16#02#,
                               AIN3 => 16#03#,
                               AIN4 => 16#04#,
                               AIN5 => 16#05#,
                               AIN6 => 16#06#,
                               AIN7 => 16#07#,
                               GND  => 16#18#);

   for Positive_Selection use (AIN0          => 16#00#,
                               AIN1          => 16#01#,
                               AIN2          => 16#02#,
                               AIN3          => 16#03#,
                               AIN4          => 16#04#,
                               AIN5          => 16#05#,
                               AIN6          => 16#06#,
                               AIN7          => 16#07#,
                               AIN8          => 16#08#,
                               AIN9          => 16#09#,
                               AIN10         => 16#0A#,
                               AIN11         => 16#0B#,
                               AIN12         => 16#0C#,
                               AIN13         => 16#0D#,
                               AIN14         => 16#0E#,
                               AIN15         => 16#0F#,
                               AIN16         => 16#10#,
                               AIN17         => 16#11#,
                               AIN18         => 16#12#,
                               AIN19         => 16#13#,
                               AIN20         => 16#14#,
                               AIN21         => 16#15#,
                               AIN22         => 16#16#,
                               AIN23         => 16#17#,
                               SCALEDCOREVCC => 16#18#,
                               SCALEDVBAT    => 16#19#,
                               SCALEDIOVCC   => 16#1A#,
                               BANDGAP       => 16#1B#,
                               PTAT          => 16#1C#,
                               CTAT          => 16#1D#,
                               DAC           => 16#1E#);
end SAM.ADC;
