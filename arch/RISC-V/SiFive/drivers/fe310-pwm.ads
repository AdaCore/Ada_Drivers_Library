------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with FE310_SVD.PWM;

package FE310.PWM is

   type Internal_PWM is limited private;

   type PWM_Device (Periph : not null access Internal_PWM) is
     limited private;

   subtype Count_Value is FE310_SVD.PWM.COUNT_CNT_Field;

   function Count (This : PWM_Device) return Count_Value;

   procedure Set_Count (This  : in out PWM_Device;
                        Value : Count_Value);

   subtype Scaled_Value is FE310_SVD.PWM.SCALE_COUNT_CNT_Field;

   function Scaled_Counter (This : PWM_Device)
                            return Scaled_Value;

   -- Enable --

   procedure Enable_Continous (This : in out PWM_Device);
   procedure Enable_One_Shot (This : in out PWM_Device);

   procedure Disable (This : in out PWM_Device);

   -- Configuration --

   procedure Configure (This          : in out PWM_Device;
                        Scale         : FE310_SVD.PWM.CONFIG_SCALE_Field;
                        Sticky        : Boolean;
                        Reset_To_Zero : Boolean;
                        Deglitch      : Boolean);

   -- Comparators --

   subtype Comparator_ID is Natural range 0 .. 3;

   procedure Configure (This           : in out PWM_Device;
                        ID             : Comparator_ID;
                        Compare_Center : Boolean;
                        Compare_Gang   : Boolean);

   -- Compare Value --

   subtype Compare_Value is FE310_SVD.PWM.COMPARE_COMPARE_Field;

   procedure Set_Compare (This  : in out PWM_Device;
                          ID    : Comparator_ID;
                          Value : Compare_Value);

   function Compare (This : PWM_Device;
                     ID   : Comparator_ID)
                     return Compare_Value;

   -- Interrupts --

   function Interrupt_Pending (This : PWM_Device;
                               ID   : Comparator_ID)
                               return Boolean;

private

   type Internal_PWM is new FE310_SVD.PWM.PWM_Peripheral;

   type PWM_Device (Periph : not null access Internal_PWM) is
     limited null record;

end FE310.PWM;
