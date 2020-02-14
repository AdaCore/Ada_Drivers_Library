separate (nRF.Clock)
procedure Set_High_Freq_External_Frequency (Freq : High_Freq_Ext_Freq) is
begin
   CLOCK_Periph.XTALFREQ.XTALFREQ := (case Freq is
      when HFCLK_16MHz => Val_16Mhz,
      when HFCLK_32MHz => Val_32Mhz);
end Set_High_Freq_External_Frequency;
