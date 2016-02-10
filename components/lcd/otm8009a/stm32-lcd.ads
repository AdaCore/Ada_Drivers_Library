with STM32.LTDC;
with STM32.LCDInit;

package STM32.LCD is new
  STM32.LTDC (LCD_Width            => STM32.LCDInit.LCD_WIDTH,
              LCD_Height           => STM32.LCDInit.LCD_HEIGHT,
              LCD_HSync            => STM32.LCDInit.HSYNC,
              LCD_HBP              => STM32.LCDInit.HBP,
              LCD_HFP              => STM32.LCDInit.HFP,
              LCD_VSYNC            => STM32.LCDInit.VSYNC,
              LCD_VBP              => STM32.LCDInit.VBP,
              LCD_VFP              => STM32.LCDInit.VFP,
              PLLSAI_N             => 384,
              PLLSAI_R             => 7,
              DivR                 => 2,
              Pre_LTDC_Initialize  => STM32.LCDInit.Pre_LTDC_Initialize,
              Post_LTDC_Initialize => STM32.LCDInit.Default_Postinit);

