------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with NRF51_SVD.CLOCK; use NRF51_SVD.CLOCK;
with nRF51.Tasks;     use nRF51.Tasks;

package body nRF51.Clock is

   --------------------------------------
   -- Set_High_Freq_External_Frequency --
   --------------------------------------

   procedure Set_High_Freq_External_Frequency (Freq : High_Freq_Ext_Freq) is
   begin
      CLOCK_Periph.XTALFREQ.XTALFREQ := (case Freq is
                                            when HFCLK_16MHz => XTALFREQ_XTALFREQ_Field_16Mhz,
                                            when HFCLK_32MHz => XTALFREQ_XTALFREQ_Field_16Mhz);
   end Set_High_Freq_External_Frequency;

   --------------------------
   -- Set_High_Freq_Source --
   --------------------------

   procedure Set_High_Freq_Source (Src : High_Freq_Source_Kind) is
   begin
      CLOCK_Periph.HFCLKSTAT.SRC := (case Src is
                                        when HFCLK_RC => Rc,
                                        when HFCLK_XTAL => Xtal);
   end Set_High_Freq_Source;

   ----------------------
   -- High_Freq_Source --
   ----------------------

   function High_Freq_Source return High_Freq_Source_Kind is
   begin
      case CLOCK_Periph.HFCLKSTAT.SRC is
         when Rc => return HFCLK_RC;
         when Xtal => return HFCLK_XTAL;
      end case;
   end High_Freq_Source;

   -----------------------
   -- High_Freq_Running --
   -----------------------

   function High_Freq_Running return Boolean is
   begin
      return CLOCK_Periph.HFCLKSTAT.STATE = Running;
   end High_Freq_Running;

   ---------------------
   -- Start_High_Freq --
   ---------------------

   procedure Start_High_Freq is
   begin
      Tasks.Trigger (Tasks.Clock_HFCLKSTART);
   end Start_High_Freq;

   --------------------
   -- Stop_High_Freq --
   --------------------

   procedure Stop_High_Freq is
   begin
      Tasks.Trigger (Tasks.Clock_HFCLKSTOP);
   end Stop_High_Freq;

   -------------------------
   -- Set_Low_Freq_Source --
   -------------------------

   procedure Set_Low_Freq_Source (Src : Low_Freq_Source_Kind) is
   begin
      CLOCK_Periph.LFCLKSRC.SRC := (case Src is
                                       when LFCLK_RC => Rc,
                                       when LFCLK_XTAL => Xtal,
                                       when LFCLK_SYNTH => Synth);
   end Set_Low_Freq_Source;

   ---------------------
   -- Low_Freq_Source --
   ---------------------

   function Low_Freq_Source return Low_Freq_Source_Kind is
   begin
      case CLOCK_Periph.LFCLKSTAT.SRC is
         when Rc => return LFCLK_RC;
         when Xtal => return LFCLK_XTAL;
         when Synth => return LFCLK_SYNTH;
      end case;
   end Low_Freq_Source;

   ----------------------
   -- Low_Freq_Running --
   ----------------------

   function Low_Freq_Running return Boolean is
   begin
      return CLOCK_Periph.LFCLKSTAT.STATE = Running;
   end Low_Freq_Running;

   --------------------
   -- Start_Low_Freq --
   --------------------

   procedure Start_Low_Freq is
   begin
      Tasks.Trigger (Tasks.Clock_LFCLKSTART);
   end Start_Low_Freq;

   -------------------
   -- Stop_Low_Freq --
   -------------------

   procedure Stop_Low_Freq is
   begin
      Tasks.Trigger (Tasks.Clock_LFCLKSTOP);
   end Stop_Low_Freq;

end nRF51.Clock;
