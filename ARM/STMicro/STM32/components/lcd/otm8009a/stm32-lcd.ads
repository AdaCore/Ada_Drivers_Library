with STM32.LTDC;
with STM32.LCDInit;

package STM32.LCD is new
  STM32.LTDC (LCD_Width            => STM32.LCDInit.LCD_WIDTH,
              LCD_Height           => STM32.LCDInit.LCD_HEIGHT,
              LCD_HSync            => 120,
              LCD_HBP              => 120,
              LCD_HFP              => 110,
              LCD_VSYNC            => 12,
              LCD_VBP              => 12,
              LCD_VFP              => 12,
              PLLSAI_N             => 384,
              PLLSAI_R             => 7,
              DivR                 => 2,
              Pre_LTDC_Initialize  => STM32.LCDInit.Default_Preinit,
              Post_LTDC_Initialize => STM32.LCDInit.Default_Postinit);

