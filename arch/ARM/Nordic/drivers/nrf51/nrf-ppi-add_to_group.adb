separate(nRF.PPI)
procedure Add_To_Group
  (Chan  : Channel_ID;
   Group : Group_ID)
is
begin
   PPI_Periph.CHG (Group).CH.Arr (Chan) := Included;
end Add_To_Group;
