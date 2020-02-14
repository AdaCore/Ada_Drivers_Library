separate (nRF.PPI)
procedure Disable_Channel (Chan : Channel_ID) is
   Arr : CHENCLR_CH_Field_Array := (others => Chenclr_Ch0_Field_Reset);
begin
   Arr (Chan) := Clear;
   PPI_Periph.CHENCLR.Arr := Arr;
end Disable_Channel;
