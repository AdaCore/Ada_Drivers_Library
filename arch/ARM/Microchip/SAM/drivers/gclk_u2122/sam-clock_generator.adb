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

with SAM_SVD.GCLK; use SAM_SVD.GCLK;

package body SAM.Clock_Generator is

   -------------------------
   -- Configure_Generator --
   -------------------------

   procedure Configure_Generator
     (Id             : Generator_Id;
      Source         : Clock_Source;
      DIV_Select     : Boolean;
      DIV            : UInt16;
      Run_In_Standby : Boolean;
      Output_Enable  : Boolean)
   is
      CTRL : GCLK_GENCTRL_Register := GCLK_Periph.GENCTRL (Integer (Id));
   begin

      CTRL.GENEN := True;
      CTRL.DIV := DIV;
      CTRL.DIVSEL := DIV_Select;
      CTRL.SRC := GENCTRL_SRCSelect'Enum_Val (Source'Enum_Rep);
      CTRL.RUNSTDBY := Run_In_Standby;
      CTRL.OE := Output_Enable;
      GCLK_Periph.GENCTRL (Integer (Id)) := CTRL;
   end Configure_Generator;

   -----------------------
   -- Disable_Generator --
   -----------------------

   procedure Disable_Generator (Id : Generator_Id) is
   begin
      GCLK_Periph.GENCTRL (Integer (Id)).GENEN := False;
   end Disable_Generator;

   ------------------------------
   -- Configure_Periph_Channel --
   ------------------------------

   procedure Configure_Periph_Channel
     (Periph : Peripheral_Id;
      Gen    : Generator_Id)
   is
      CTRL : GCLK_PCHCTRL_Register := GCLK_Periph.PCHCTRL (Integer (Periph));
   begin

      CTRL.GEN := PCHCTRL_GENSelect (Gen);
      CTRL.CHEN := True;

      GCLK_Periph.PCHCTRL (Integer (Periph)) := CTRL;
   end Configure_Periph_Channel;

   ----------------------------
   -- Disable_Periph_Channel --
   ----------------------------

   procedure Disable_Periph_Channel (Periph : Peripheral_Id) is
   begin
      GCLK_Periph.PCHCTRL (Integer (Periph)).CHEN := False;
   end Disable_Periph_Channel;

end SAM.Clock_Generator;
