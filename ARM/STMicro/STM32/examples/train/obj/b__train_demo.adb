pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__train_demo.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__train_demo.adb");
pragma Suppress (Overflow_Check);

package body ada_main is
   pragma Warnings (Off);

   E006 : Short_Integer; pragma Import (Ada, E006, "ada__real_time_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "system__tasking__protected_objects_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "system__tasking__protected_objects__multiprocessors_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "system__tasking__restricted__stages_E");
   E003 : Short_Integer; pragma Import (Ada, E003, "driver_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "last_chance_handler_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "stm32f4_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "bmp_fonts_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "bitmapped_drawing_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "stm32f4__adc_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "stm32f4__dac_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "stm32f4__exti_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "stm32f4__fmc_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "stm32f4__i2c_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "stm32f4__rcc_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "stm32f4__sdram_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "stm32f4__spi_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "stm32f4__ili9341_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "stm32f4__l3gd20_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "stm32f4__syscfg_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "stm32f42xxx_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "stm32f429_discovery_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "stm32f4__ltdc_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "screen_interface_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "railroad_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "stm32f4__touch_panel_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "trains_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "tracks_display_E");


   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");

   begin
      Main_Priority := 0;

      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E006 := E006 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E100 := E100 + 1;
      System.Tasking.Protected_Objects.Multiprocessors'Elab_Body;
      E104 := E104 + 1;
      System.Tasking.Restricted.Stages'Elab_Body;
      E098 := E098 + 1;
      Driver'Elab_Spec;
      STM32F4'ELAB_SPEC;
      E067 := E067 + 1;
      E066 := E066 + 1;
      E071 := E071 + 1;
      STM32F4.ADC'ELAB_SPEC;
      E107 := E107 + 1;
      E109 := E109 + 1;
      STM32F4.EXTI'ELAB_SPEC;
      E085 := E085 + 1;
      STM32F4.FMC'ELAB_SPEC;
      E123 := E123 + 1;
      STM32F4.RCC'ELAB_SPEC;
      E081 := E081 + 1;
      E113 := E113 + 1;
      E091 := E091 + 1;
      STM32F4.ILI9341'ELAB_SPEC;
      E089 := E089 + 1;
      STM32F4.SYSCFG'ELAB_SPEC;
      E083 := E083 + 1;
      E115 := E115 + 1;
      Stm32f42xxx'Elab_Spec;
      E121 := E121 + 1;
      Stm32f429_Discovery'Elab_Spec;
      E087 := E087 + 1;
      E077 := E077 + 1;
      E131 := E131 + 1;
      STM32F4.LTDC'ELAB_SPEC;
      E075 := E075 + 1;
      STM32F4.TOUCH_PANEL'ELAB_SPEC;
      E125 := E125 + 1;
      E073 := E073 + 1;
      Trains'Elab_Spec;
      E129 := E129 + 1;
      E127 := E127 + 1;
      Railroad'Elab_Body;
      E064 := E064 + 1;
      Driver'Elab_Body;
      E003 := E003 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_train_demo");

   procedure main is
      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      adainit;
      Ada_Main_Program;
   end;

--  BEGIN Object file/option list
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/train_demo.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/bmp_fonts.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/bitmapped_drawing.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-adc.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-dac.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-dma.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-exti.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-fmc.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-rcc.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-i2c.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-spi.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-ili9341.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-syscfg.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-l3gd20.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-gpio.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-timers.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-usarts.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f42xxx.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f429_discovery.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-sdram.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/last_chance_handler-stm32f429xx-sfp.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-ltdc.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/drawing.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/stm32f4-touch_panel.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/screen_interface.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/trains.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/tracks_display.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/railroad.o
   --   /home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/driver.o
   --   -L/home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/
   --   -L/home/emu/Dropbox/TUM/MA/Sandbox/bareboard/ARM/STMicro/STM32/examples/train/obj/
   --   -L/home/emu/Software/gnat/arm-eabi/lib/gnat/ravenscar-sfp-stm32f4/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
