separate(nRF.Interrupts)
procedure Set_Priority (Int : Interrupt_Name; Prio : Interrupt_Priority) is
begin
   Cortex_M.NVIC.Set_Priority (Int'Enum_Rep, Prio);
end Set_Priority;
