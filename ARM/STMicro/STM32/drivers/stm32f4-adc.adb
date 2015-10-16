package body STM32F4.ADC is

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Analog_To_Digital_Converter) is
   begin
      This.CR2.ADC_Enabled := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Analog_To_Digital_Converter) is
   begin
      This.CR2.ADC_Enabled := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CR2.ADC_Enabled);

   ----------------------
   -- Configure_Common --
   ----------------------

   procedure Configure_Common
     (Mode           : Multi_ADC_Mode_Selections := Independent;
      Prescalar      : ADC_Prescalars            := PCLK2_Div_2;
      DMA_Mode       : Multi_ADC_DMA_Modes       := Disabled;
      Sampling_Delay : Sampling_Delay_Selections := Sampling_Delay_5_Cycles)
   is
   begin
      Common.CR.Multi_ADC_Mode     := Mode;
      Common.CR.Sampling_Delay     := Sampling_Delay;
      Common.CR.Multi_ADC_DMA_Mode := DMA_Mode;
      Common.CR.ADC_Prescalar      := Prescalar;
   end Configure_Common;

end STM32F4.ADC;
