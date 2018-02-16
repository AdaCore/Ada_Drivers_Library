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

package body FE310.PWM is

   -----------
   -- Count --
   -----------

   function Count (This : PWM_Device) return Count_Value
   is (This.Periph.COUNT.CNT);

   ---------------
   -- Set_Count --
   ---------------

   procedure Set_Count (This  : in out PWM_Device;
                        Value : Count_Value)
   is
   begin
      This.Periph.COUNT.CNT := Value;
   end Set_Count;

   ----------------------
   -- Enable_Continous --
   ----------------------

   procedure Enable_Continous (This : in out PWM_Device) is
   begin
      This.Periph.CONFIG.ENALWAYS := True;
   end Enable_Continous;

   ---------------------
   -- Enable_One_Shot --
   ---------------------

   procedure Enable_One_Shot (This : in out PWM_Device) is
   begin
      This.Periph.CONFIG.ENONESHOT := True;
   end Enable_One_Shot;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out PWM_Device) is
   begin
      This.Periph.CONFIG.ENONESHOT := False;
      This.Periph.CONFIG.ENALWAYS := False;
   end Disable;

   --------------------
   -- Scaled_Counter --
   --------------------

   function Scaled_Counter
     (This : PWM_Device)
      return Scaled_Value
   is (This.Periph.SCALE_COUNT.CNT);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This          : in out PWM_Device;
      Scale         : FE310_SVD.PWM.CONFIG_SCALE_Field;
      Sticky        : Boolean;
      Reset_To_Zero : Boolean;
      Deglitch      : Boolean)
   is
   begin
      This.Periph.CONFIG.SCALE := Scale;
      This.Periph.CONFIG.STICKY := Sticky;
      This.Periph.CONFIG.ZEROCMP := Reset_To_Zero;
      This.Periph.CONFIG.DEGLITCH := Deglitch;
   end Configure;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This           : in out PWM_Device;
      ID             : Comparator_ID;
      Compare_Center : Boolean;
      Compare_Gang   : Boolean)
   is
   begin
      This.Periph.CONFIG.CMP_CENTER.Arr (ID) := Compare_Center;
      This.Periph.CONFIG.CMP_GANG.Arr (ID) := Compare_Gang;
   end Configure;

   -----------------
   -- Set_Compare --
   -----------------

   procedure Set_Compare
     (This  : in out PWM_Device;
      ID    : Comparator_ID;
      Value : Compare_Value)
   is
   begin
      case ID is
         when 0 => This.Periph.COMPARE0.COMPARE := Value;
         when 1 => This.Periph.COMPARE1.COMPARE := Value;
         when 2 => This.Periph.COMPARE2.COMPARE := Value;
         when 3 => This.Periph.COMPARE3.COMPARE := Value;
      end case;
   end Set_Compare;

   -------------
   -- Compare --
   -------------

   function Compare
     (This : PWM_Device;
      ID   : Comparator_ID)
      return Compare_Value
   is (case ID is
          when 0 => This.Periph.COMPARE0.COMPARE,
          when 1 => This.Periph.COMPARE1.COMPARE,
          when 2 => This.Periph.COMPARE2.COMPARE,
          when 3 => This.Periph.COMPARE3.COMPARE);

   -----------------------
   -- Interrupt_Pending --
   -----------------------

   function Interrupt_Pending
     (This : PWM_Device;
      ID   : Comparator_ID)
      return Boolean
   is (This.Periph.CONFIG.CMP_IP.Arr (ID));

end FE310.PWM;
