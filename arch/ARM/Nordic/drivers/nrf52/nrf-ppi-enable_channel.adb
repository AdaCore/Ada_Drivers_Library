separate (nRF.PPI)
procedure Enable_Channel (Chan : Channel_ID) is
   Arr : CHENSET_CH_Field_Array := (others => Chenset_Ch0_Field_Reset);
begin
   Arr (Chan) := Set;
   PPI_Periph.CHENSET.Arr := Arr;
end Enable_Channel;
