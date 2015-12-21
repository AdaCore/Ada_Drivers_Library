with STM32.LTDC;
with STM32.LCDInit;

package STM32.LCD is new
  STM32.LTDC (LCD_Width      => 480,
              LCD_Height     => 272,
              LCD_HSync      => 41,
              LCD_HBP        => 13,
              LCD_HFP        => 32,
              LCD_VSYNC      => 10,
              LCD_VBP        => 2,
              LCD_VFP        => 2,
              PLLSAI_R       => 4,
              DivR           => 4,
              Initialize_LCD => STM32.LCDInit.Initialize);
