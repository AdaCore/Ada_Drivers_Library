separate(nRF.PPI)
procedure Remove_From_Group
  (Chan  : Channel_ID;
   Group : Group_ID)
is
begin
   PPI_Periph.CHG (Group).Arr (Chan) := Excluded;
end Remove_From_Group;
