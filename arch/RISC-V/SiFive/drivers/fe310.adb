------------------------------------------------------------------------------
--                                                                          --
--          Copyright (C) 2017-2018, AdaCore and other contributors         --
--                                                                          --
--      See github.com/AdaCore/Ada_Drivers_Library/graphs/contributors      --
--                           for more information                           --
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

with FE310.Time;        use FE310.Time;
with FE310_SVD.OTP_Mem; use FE310_SVD.OTP_Mem;
with FE310_SVD.PRIC;    use FE310_SVD.PRIC;
with FE310_SVD.SPI;     use FE310_SVD.SPI;

package body FE310 is

   Crystal_Frequency : constant := 16_000_000;
   HFROSC_Frequency : constant := 72_000_000;  -- High frequency internal oscillator

   -------------------
   -- CPU_Frequency --
   -------------------

   function CPU_Frequency return UInt32 is
      Freq : UInt32;
   begin
      if PRIC_Periph.PLLCFG.SEL = Internal then
         Freq := HFROSC_Frequency / (UInt32 (PRIC_Periph.HFROSCCFG.DIV) + 1);
      else
         if PRIC_Periph.PLLCFG.REFSEL = Crystal then
            Freq := Crystal_Frequency;
         else
            Freq := HFROSC_Frequency;
         end if;

         if PRIC_Periph.PLLCFG.BYPASS = False then
            Freq := Freq / (UInt32 (PRIC_Periph.PLLCFG.R) + 1)
              * (2 * (UInt32 (PRIC_Periph.PLLCFG.F) + 1))
              / (2**(Natural (PRIC_Periph.PLLCFG.Q)));
         end if;

         if PRIC_Periph.PLLOUTDIV.DIV_BY_1 = False then
               Freq := Freq / (2 * (UInt32 (PRIC_Periph.PLLOUTDIV.DIV) + 1));
         end if;
      end if;

      return Freq;
   end CPU_Frequency;

   -----------------------------------------
   -- Load_Internal_Oscilator_Calibration --
   -----------------------------------------

   procedure Load_Internal_Oscilator_Calibration is
   begin
      PRIC_Periph.HFROSCCFG.TRIM := OTP_Mem_Periph.HFROSC_TRIM.VALUE - 1;
   end Load_Internal_Oscilator_Calibration;

   ----------------------------
   -- Use_Crystal_Oscillator --
   ----------------------------

   procedure Use_Crystal_Oscillator (Divider : PLL_Output_Divider := 1) is
   begin
      --  Use internal oscillator during switch
      PRIC_Periph.HFROSCCFG.DIV := 4;  --  Divide by 5, Freq = 14.4 MHz
      PRIC_Periph.HFROSCCFG.ENABLE := True;
      loop
         exit when PRIC_Periph.HFROSCCFG.READY;
      end loop;
      PRIC_Periph.PLLCFG.SEL := Internal;

      --  Start the crystal oscillator
      PRIC_Periph.HFXOSCCFG.ENABLE := True;
      loop
         exit when PRIC_Periph.HFXOSCCFG.READY;
      end loop;

      --  Configure the final divider
      if Divider = 1 then
         PRIC_Periph.PLLOUTDIV.DIV_BY_1 := True;
      else
         PRIC_Periph.PLLOUTDIV.DIV_BY_1 := False;
         PRIC_Periph.PLLOUTDIV.DIV := PLLOUTDIV_DIV_Field ((Divider / 2) - 1);
      end if;

      --  Switch to crystal oscillator
      PRIC_Periph.PLLCFG.REFSEL := Crystal;
      PRIC_Periph.PLLCFG.BYPASS := True;
      PRIC_Periph.PLLCFG.SEL := Pll;

      --  Disable internal oscillator
      PRIC_Periph.HFROSCCFG.ENABLE := False;
   end Use_Crystal_Oscillator;

   -----------------------------
   -- Use_Internal_Oscillator --
   -----------------------------

   procedure Use_Internal_Oscillator (Divider : Internal_Oscillator_Divider := 5) is
   begin
      PRIC_Periph.HFROSCCFG.DIV := HFROSCCFG_DIV_Field (Divider - 1);
      PRIC_Periph.HFROSCCFG.ENABLE := True;
      loop
         exit when PRIC_Periph.HFROSCCFG.READY;
      end loop;
      PRIC_Periph.PLLCFG.SEL := Internal;

      --  Disable crystal oscillator and PLL
      PRIC_Periph.HFXOSCCFG.ENABLE := False;
      PRIC_Periph.PLLCFG.BYPASS := True;
   end Use_Internal_Oscillator;

   ------------
   -- Use_PLL--
   ------------

   procedure Use_PLL (Reference : PLL_Reference;
                      Internal_Osc_Div : Internal_Oscillator_Divider  := 5;
                      R_Div : PLL_R;
                      F_Mul : PLL_F;
                      Q_Div : PLL_Q;
                      Output_Div : PLL_Output_Divider) is
   begin
      --  Use internal oscillator during switch
      PRIC_Periph.HFROSCCFG.DIV := HFROSCCFG_DIV_Field (Internal_Osc_Div - 1);
      PRIC_Periph.HFROSCCFG.ENABLE := True;
      loop
         exit when PRIC_Periph.HFROSCCFG.READY;
      end loop;
      PRIC_Periph.PLLCFG.SEL := Internal;

      if Reference = Crystal then
         --  Start the crystal oscillator
         PRIC_Periph.HFXOSCCFG.ENABLE := True;
         loop
            exit when PRIC_Periph.HFXOSCCFG.READY;
         end loop;
      else
         PRIC_Periph.HFXOSCCFG.ENABLE := False;
      end if;

      --  Configure the PLL
      PRIC_Periph.PLLCFG.REFSEL := PLLCFG_REFSEL_Field (Reference);
      PRIC_Periph.PLLCFG.R := PLLCFG_R_Field (R_Div - 1);
      PRIC_Periph.PLLCFG.F := PLLCFG_F_Field ((F_Mul / 2) - 1);
      PRIC_Periph.PLLCFG.Q := PLLCFG_Q_Field (PLL_Q'Enum_Rep (Q_Div));

      --  Configure the final divider
      if Output_Div = 1 then
         PRIC_Periph.PLLOUTDIV.DIV_BY_1 := True;
      else
         PRIC_Periph.PLLOUTDIV.DIV_BY_1 := False;
         PRIC_Periph.PLLOUTDIV.DIV := PLLOUTDIV_DIV_Field ((Output_Div / 2) - 1);
      end if;

      --  Start the PLL
      PRIC_Periph.PLLCFG.BYPASS := False;
      Delay_Us (150);

      loop
         exit when PRIC_Periph.PLLCFG.LOCK;
      end loop;

      --  Switch to PLL
      PRIC_Periph.PLLCFG.SEL := Pll;

      --  Disable internal oscillator if the crystal reference is used
      if Reference = Crystal then
         PRIC_Periph.HFROSCCFG.ENABLE := False;
      end if;

   end Use_PLL;

   ---------------------------------
   -- Set_SPI_Flash_Clock_Divider --
   ---------------------------------

   procedure Set_SPI_Flash_Clock_Divider (Divider : SPI_Clock_Divider) is
   begin
      QSPI0_Periph.SCKDIV.SCALE := (UInt12 (Divider) / 2) - 1;
   end Set_SPI_Flash_Clock_Divider;

   -----------------------------
   -- SPI_Flash_Clock_Divider --
   -----------------------------

   function SPI_Flash_Clock_Divider return SPI_Clock_Divider is
   begin
      return 2 * (Integer (QSPI0_Periph.SCKDIV.SCALE) + 1);
   end SPI_Flash_Clock_Divider;

end FE310;
