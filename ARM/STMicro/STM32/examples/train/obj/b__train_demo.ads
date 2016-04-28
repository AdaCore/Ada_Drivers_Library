pragma Ada_95;
pragma Restrictions (No_Exception_Propagation);
with System;
package ada_main is
   pragma Warnings (Off);


   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2015 (20150428-49)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_train_demo" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure main;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#e85394f0#;
   pragma Export (C, u00001, "train_demoB");
   u00002 : constant Version_32 := 16#ba5fd9cc#;
   pragma Export (C, u00002, "driverB");
   u00003 : constant Version_32 := 16#3676f0ed#;
   pragma Export (C, u00003, "driverS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#48fa1cf9#;
   pragma Export (C, u00005, "ada__real_timeB");
   u00006 : constant Version_32 := 16#cf4a25c2#;
   pragma Export (C, u00006, "ada__real_timeS");
   u00007 : constant Version_32 := 16#9eacb62e#;
   pragma Export (C, u00007, "systemS");
   u00008 : constant Version_32 := 16#3cd52794#;
   pragma Export (C, u00008, "system__task_primitivesS");
   u00009 : constant Version_32 := 16#bf2e4f10#;
   pragma Export (C, u00009, "system__os_interfaceS");
   u00010 : constant Version_32 := 16#169bdef8#;
   pragma Export (C, u00010, "system__bbS");
   u00011 : constant Version_32 := 16#77957d55#;
   pragma Export (C, u00011, "system__bb__board_supportB");
   u00012 : constant Version_32 := 16#c731b4f5#;
   pragma Export (C, u00012, "system__bb__board_supportS");
   u00013 : constant Version_32 := 16#12319864#;
   pragma Export (C, u00013, "system__bb__parametersS");
   u00014 : constant Version_32 := 16#0e11b1f1#;
   pragma Export (C, u00014, "system__stm32f4S");
   u00015 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00015, "system__storage_elementsB");
   u00016 : constant Version_32 := 16#b36ffc2a#;
   pragma Export (C, u00016, "system__storage_elementsS");
   u00017 : constant Version_32 := 16#03f412ee#;
   pragma Export (C, u00017, "system__unsigned_typesS");
   u00018 : constant Version_32 := 16#ad398267#;
   pragma Export (C, u00018, "system__stm32f4__rccB");
   u00019 : constant Version_32 := 16#8e9d494b#;
   pragma Export (C, u00019, "system__stm32f4__rccS");
   u00020 : constant Version_32 := 16#9117cde0#;
   pragma Export (C, u00020, "system__machine_codeS");
   u00021 : constant Version_32 := 16#cf75f556#;
   pragma Export (C, u00021, "system__bb__cpu_primitivesB");
   u00022 : constant Version_32 := 16#3522fd47#;
   pragma Export (C, u00022, "system__bb__cpu_primitivesS");
   u00023 : constant Version_32 := 16#5e68f5cc#;
   pragma Export (C, u00023, "ada__exceptionsB");
   u00024 : constant Version_32 := 16#fa0f3719#;
   pragma Export (C, u00024, "ada__exceptionsS");
   u00025 : constant Version_32 := 16#4c80e781#;
   pragma Export (C, u00025, "system__bb__threadsB");
   u00026 : constant Version_32 := 16#111ee653#;
   pragma Export (C, u00026, "system__bb__threadsS");
   u00027 : constant Version_32 := 16#5d9b2ea0#;
   pragma Export (C, u00027, "system__bb__protectionB");
   u00028 : constant Version_32 := 16#7cbd1653#;
   pragma Export (C, u00028, "system__bb__protectionS");
   u00029 : constant Version_32 := 16#89e68bc8#;
   pragma Export (C, u00029, "system__bb__threads__queuesB");
   u00030 : constant Version_32 := 16#d0270049#;
   pragma Export (C, u00030, "system__bb__threads__queuesS");
   u00031 : constant Version_32 := 16#f22a3c08#;
   pragma Export (C, u00031, "system__bb__cpu_primitives__multiprocessorsB");
   u00032 : constant Version_32 := 16#71f00eda#;
   pragma Export (C, u00032, "system__bb__cpu_primitives__multiprocessorsS");
   u00033 : constant Version_32 := 16#84f3a776#;
   pragma Export (C, u00033, "system__multiprocessorsB");
   u00034 : constant Version_32 := 16#790901e2#;
   pragma Export (C, u00034, "system__multiprocessorsS");
   u00035 : constant Version_32 := 16#6033a23f#;
   pragma Export (C, u00035, "interfacesS");
   u00036 : constant Version_32 := 16#8d26105b#;
   pragma Export (C, u00036, "interfaces__cS");
   u00037 : constant Version_32 := 16#06112067#;
   pragma Export (C, u00037, "system__bb__timeB");
   u00038 : constant Version_32 := 16#54dfd416#;
   pragma Export (C, u00038, "system__bb__timeS");
   u00039 : constant Version_32 := 16#b5993228#;
   pragma Export (C, u00039, "ada__tagsB");
   u00040 : constant Version_32 := 16#40d12d63#;
   pragma Export (C, u00040, "ada__tagsS");
   u00041 : constant Version_32 := 16#a56b3ef3#;
   pragma Export (C, u00041, "system__secondary_stackB");
   u00042 : constant Version_32 := 16#17bf9668#;
   pragma Export (C, u00042, "system__secondary_stackS");
   u00043 : constant Version_32 := 16#a7e82125#;
   pragma Export (C, u00043, "system__bb__interruptsB");
   u00044 : constant Version_32 := 16#b67d8be2#;
   pragma Export (C, u00044, "system__bb__interruptsS");
   u00045 : constant Version_32 := 16#edf7a093#;
   pragma Export (C, u00045, "system__bb__timing_eventsB");
   u00046 : constant Version_32 := 16#4015fc61#;
   pragma Export (C, u00046, "system__bb__timing_eventsS");
   u00047 : constant Version_32 := 16#ec2092fc#;
   pragma Export (C, u00047, "system__multiprocessors__fair_locksB");
   u00048 : constant Version_32 := 16#a70e2885#;
   pragma Export (C, u00048, "system__multiprocessors__fair_locksS");
   u00049 : constant Version_32 := 16#9621e181#;
   pragma Export (C, u00049, "system__multiprocessors__spin_locksB");
   u00050 : constant Version_32 := 16#9ac42bf1#;
   pragma Export (C, u00050, "system__multiprocessors__spin_locksS");
   u00051 : constant Version_32 := 16#c0ab394a#;
   pragma Export (C, u00051, "system__parametersB");
   u00052 : constant Version_32 := 16#5752e571#;
   pragma Export (C, u00052, "system__parametersS");
   u00053 : constant Version_32 := 16#a12451c2#;
   pragma Export (C, u00053, "system__task_primitives__operationsB");
   u00054 : constant Version_32 := 16#f0ec23ba#;
   pragma Export (C, u00054, "system__task_primitives__operationsS");
   u00055 : constant Version_32 := 16#168b7360#;
   pragma Export (C, u00055, "system__taskingB");
   u00056 : constant Version_32 := 16#8496e0f9#;
   pragma Export (C, u00056, "system__taskingS");
   u00057 : constant Version_32 := 16#e0fce7f8#;
   pragma Export (C, u00057, "system__task_infoB");
   u00058 : constant Version_32 := 16#1eb60b2f#;
   pragma Export (C, u00058, "system__task_infoS");
   u00059 : constant Version_32 := 16#0f8eba36#;
   pragma Export (C, u00059, "system__tasking__debugB");
   u00060 : constant Version_32 := 16#8e39484d#;
   pragma Export (C, u00060, "system__tasking__debugS");
   u00061 : constant Version_32 := 16#13da4b8a#;
   pragma Export (C, u00061, "ada__real_time__delaysB");
   u00062 : constant Version_32 := 16#6fcba83e#;
   pragma Export (C, u00062, "ada__real_time__delaysS");
   u00063 : constant Version_32 := 16#61d1085b#;
   pragma Export (C, u00063, "railroadB");
   u00064 : constant Version_32 := 16#0cc58064#;
   pragma Export (C, u00064, "railroadS");
   u00065 : constant Version_32 := 16#4a3a9430#;
   pragma Export (C, u00065, "bmp_fontsB");
   u00066 : constant Version_32 := 16#d7a121e2#;
   pragma Export (C, u00066, "bmp_fontsS");
   u00067 : constant Version_32 := 16#89f08baa#;
   pragma Export (C, u00067, "stm32f4S");
   u00068 : constant Version_32 := 16#6da90ea8#;
   pragma Export (C, u00068, "drawingB");
   u00069 : constant Version_32 := 16#b6d6a2b1#;
   pragma Export (C, u00069, "drawingS");
   u00070 : constant Version_32 := 16#5f036ea7#;
   pragma Export (C, u00070, "bitmapped_drawingB");
   u00071 : constant Version_32 := 16#ab5eb066#;
   pragma Export (C, u00071, "bitmapped_drawingS");
   u00072 : constant Version_32 := 16#7047a04a#;
   pragma Export (C, u00072, "screen_interfaceB");
   u00073 : constant Version_32 := 16#30c39f65#;
   pragma Export (C, u00073, "screen_interfaceS");
   u00074 : constant Version_32 := 16#93b7b8a6#;
   pragma Export (C, u00074, "stm32f4__ltdcB");
   u00075 : constant Version_32 := 16#b22a6182#;
   pragma Export (C, u00075, "stm32f4__ltdcS");
   u00076 : constant Version_32 := 16#9915c086#;
   pragma Export (C, u00076, "stm32f4__sdramB");
   u00077 : constant Version_32 := 16#448d406c#;
   pragma Export (C, u00077, "stm32f4__sdramS");
   u00078 : constant Version_32 := 16#f2b89a61#;
   pragma Export (C, u00078, "stm32f4__gpioB");
   u00079 : constant Version_32 := 16#e0808d2f#;
   pragma Export (C, u00079, "stm32f4__gpioS");
   u00080 : constant Version_32 := 16#22288aaa#;
   pragma Export (C, u00080, "stm32f4__rccB");
   u00081 : constant Version_32 := 16#89695593#;
   pragma Export (C, u00081, "stm32f4__rccS");
   u00082 : constant Version_32 := 16#0c3076a5#;
   pragma Export (C, u00082, "stm32f4__syscfgB");
   u00083 : constant Version_32 := 16#fda6d162#;
   pragma Export (C, u00083, "stm32f4__syscfgS");
   u00084 : constant Version_32 := 16#5a32d722#;
   pragma Export (C, u00084, "stm32f4__extiB");
   u00085 : constant Version_32 := 16#8d52403a#;
   pragma Export (C, u00085, "stm32f4__extiS");
   u00086 : constant Version_32 := 16#efae8842#;
   pragma Export (C, u00086, "stm32f429_discoveryB");
   u00087 : constant Version_32 := 16#502fe330#;
   pragma Export (C, u00087, "stm32f429_discoveryS");
   u00088 : constant Version_32 := 16#6c702914#;
   pragma Export (C, u00088, "stm32f4__ili9341B");
   u00089 : constant Version_32 := 16#854b1ad7#;
   pragma Export (C, u00089, "stm32f4__ili9341S");
   u00090 : constant Version_32 := 16#4ccc5d93#;
   pragma Export (C, u00090, "stm32f4__spiB");
   u00091 : constant Version_32 := 16#3efd2206#;
   pragma Export (C, u00091, "stm32f4__spiS");
   u00092 : constant Version_32 := 16#a34e0368#;
   pragma Export (C, u00092, "ada__interruptsB");
   u00093 : constant Version_32 := 16#426c174d#;
   pragma Export (C, u00093, "ada__interruptsS");
   u00094 : constant Version_32 := 16#0a6637d7#;
   pragma Export (C, u00094, "system__interruptsB");
   u00095 : constant Version_32 := 16#48c99e36#;
   pragma Export (C, u00095, "system__interruptsS");
   u00096 : constant Version_32 := 16#4d5ecdbf#;
   pragma Export (C, u00096, "system__tasking__restrictedS");
   u00097 : constant Version_32 := 16#4767effe#;
   pragma Export (C, u00097, "system__tasking__restricted__stagesB");
   u00098 : constant Version_32 := 16#57e13518#;
   pragma Export (C, u00098, "system__tasking__restricted__stagesS");
   u00099 : constant Version_32 := 16#1982f60e#;
   pragma Export (C, u00099, "system__tasking__protected_objectsB");
   u00100 : constant Version_32 := 16#a15edde4#;
   pragma Export (C, u00100, "system__tasking__protected_objectsS");
   u00101 : constant Version_32 := 16#63260eca#;
   pragma Export (C, u00101, "system__tasking__protected_objects__single_entryB");
   u00102 : constant Version_32 := 16#d8cc26ef#;
   pragma Export (C, u00102, "system__tasking__protected_objects__single_entryS");
   u00103 : constant Version_32 := 16#ffa17f64#;
   pragma Export (C, u00103, "system__tasking__protected_objects__multiprocessorsB");
   u00104 : constant Version_32 := 16#1e3d54f2#;
   pragma Export (C, u00104, "system__tasking__protected_objects__multiprocessorsS");
   u00105 : constant Version_32 := 16#df6423ea#;
   pragma Export (C, u00105, "ada__interrupts__namesS");
   u00106 : constant Version_32 := 16#ed2ae690#;
   pragma Export (C, u00106, "stm32f4__adcB");
   u00107 : constant Version_32 := 16#48cdf2aa#;
   pragma Export (C, u00107, "stm32f4__adcS");
   u00108 : constant Version_32 := 16#8db74a79#;
   pragma Export (C, u00108, "stm32f4__dacB");
   u00109 : constant Version_32 := 16#56eae3e9#;
   pragma Export (C, u00109, "stm32f4__dacS");
   u00110 : constant Version_32 := 16#31af94e4#;
   pragma Export (C, u00110, "stm32f4__dmaB");
   u00111 : constant Version_32 := 16#f0d30da5#;
   pragma Export (C, u00111, "stm32f4__dmaS");
   u00112 : constant Version_32 := 16#c594cd93#;
   pragma Export (C, u00112, "stm32f4__i2cB");
   u00113 : constant Version_32 := 16#c23eb9a9#;
   pragma Export (C, u00113, "stm32f4__i2cS");
   u00114 : constant Version_32 := 16#6064e817#;
   pragma Export (C, u00114, "stm32f4__l3gd20B");
   u00115 : constant Version_32 := 16#80cfed84#;
   pragma Export (C, u00115, "stm32f4__l3gd20S");
   u00116 : constant Version_32 := 16#3fe3dfc9#;
   pragma Export (C, u00116, "stm32f4__timersB");
   u00117 : constant Version_32 := 16#87899d16#;
   pragma Export (C, u00117, "stm32f4__timersS");
   u00118 : constant Version_32 := 16#2572bdfe#;
   pragma Export (C, u00118, "stm32f4__usartsB");
   u00119 : constant Version_32 := 16#067226c3#;
   pragma Export (C, u00119, "stm32f4__usartsS");
   u00120 : constant Version_32 := 16#ed46d23f#;
   pragma Export (C, u00120, "stm32f42xxxB");
   u00121 : constant Version_32 := 16#64070490#;
   pragma Export (C, u00121, "stm32f42xxxS");
   u00122 : constant Version_32 := 16#0774cab4#;
   pragma Export (C, u00122, "stm32f4__fmcB");
   u00123 : constant Version_32 := 16#27607f3b#;
   pragma Export (C, u00123, "stm32f4__fmcS");
   u00124 : constant Version_32 := 16#d68c4072#;
   pragma Export (C, u00124, "stm32f4__touch_panelB");
   u00125 : constant Version_32 := 16#69aa30e5#;
   pragma Export (C, u00125, "stm32f4__touch_panelS");
   u00126 : constant Version_32 := 16#4ac2fd54#;
   pragma Export (C, u00126, "tracks_displayB");
   u00127 : constant Version_32 := 16#b1190612#;
   pragma Export (C, u00127, "tracks_displayS");
   u00128 : constant Version_32 := 16#4cccf5b6#;
   pragma Export (C, u00128, "trainsB");
   u00129 : constant Version_32 := 16#1767ea1b#;
   pragma Export (C, u00129, "trainsS");
   u00130 : constant Version_32 := 16#dbdd7b6f#;
   pragma Export (C, u00130, "last_chance_handlerB");
   u00131 : constant Version_32 := 16#44fd462a#;
   pragma Export (C, u00131, "last_chance_handlerS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  interfaces.c%s
   --  system%s
   --  ada.exceptions%s
   --  ada.exceptions%b
   --  system.bb%s
   --  system.bb.protection%s
   --  system.machine_code%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  ada.tags%s
   --  system.task_info%s
   --  system.task_info%b
   --  system.unsigned_types%s
   --  system.stm32f4%s
   --  system.stm32f4.rcc%s
   --  system.bb.parameters%s
   --  system.stm32f4.rcc%b
   --  system.bb.cpu_primitives%s
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.bb.cpu_primitives.multiprocessors%s
   --  system.bb.cpu_primitives.multiprocessors%b
   --  system.bb.interrupts%s
   --  system.bb.board_support%s
   --  system.bb.board_support%b
   --  system.bb.time%s
   --  system.bb.threads%s
   --  system.bb.threads.queues%s
   --  system.bb.threads.queues%b
   --  system.bb.threads%b
   --  system.bb.interrupts%b
   --  system.bb.cpu_primitives%b
   --  system.bb.protection%b
   --  system.bb.timing_events%s
   --  system.bb.timing_events%b
   --  system.multiprocessors.spin_locks%s
   --  system.multiprocessors.spin_locks%b
   --  system.multiprocessors.fair_locks%s
   --  system.bb.time%b
   --  system.os_interface%s
   --  system.multiprocessors.fair_locks%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.secondary_stack%s
   --  system.tasking%b
   --  ada.tags%b
   --  system.secondary_stack%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.real_time.delays%s
   --  ada.real_time.delays%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.protected_objects.multiprocessors%s
   --  system.tasking.protected_objects.multiprocessors%b
   --  system.tasking.protected_objects.single_entry%s
   --  system.tasking.protected_objects.single_entry%b
   --  system.tasking.restricted%s
   --  system.tasking.restricted.stages%s
   --  system.tasking.restricted.stages%b
   --  system.interrupts%s
   --  system.interrupts%b
   --  ada.interrupts%s
   --  ada.interrupts%b
   --  ada.interrupts.names%s
   --  driver%s
   --  last_chance_handler%s
   --  train_demo%b
   --  stm32f4%s
   --  bmp_fonts%s
   --  bmp_fonts%b
   --  bitmapped_drawing%s
   --  bitmapped_drawing%b
   --  stm32f4.adc%s
   --  stm32f4.adc%b
   --  stm32f4.dac%s
   --  stm32f4.dac%b
   --  stm32f4.dma%s
   --  stm32f4.dma%b
   --  stm32f4.exti%s
   --  stm32f4.exti%b
   --  stm32f4.fmc%s
   --  stm32f4.fmc%b
   --  stm32f4.gpio%s
   --  stm32f4.i2c%s
   --  stm32f4.rcc%s
   --  stm32f4.rcc%b
   --  stm32f4.i2c%b
   --  stm32f4.sdram%s
   --  stm32f4.spi%s
   --  stm32f4.spi%b
   --  stm32f4.ili9341%s
   --  stm32f4.ili9341%b
   --  stm32f4.l3gd20%s
   --  stm32f4.syscfg%s
   --  stm32f4.syscfg%b
   --  stm32f4.l3gd20%b
   --  stm32f4.gpio%b
   --  stm32f4.timers%s
   --  stm32f4.timers%b
   --  stm32f4.usarts%s
   --  stm32f4.usarts%b
   --  stm32f42xxx%s
   --  stm32f42xxx%b
   --  stm32f429_discovery%s
   --  stm32f429_discovery%b
   --  stm32f4.sdram%b
   --  last_chance_handler%b
   --  stm32f4.ltdc%s
   --  stm32f4.ltdc%b
   --  screen_interface%s
   --  drawing%s
   --  drawing%b
   --  railroad%s
   --  stm32f4.touch_panel%s
   --  stm32f4.touch_panel%b
   --  screen_interface%b
   --  trains%s
   --  trains%b
   --  tracks_display%s
   --  tracks_display%b
   --  railroad%b
   --  driver%b
   --  END ELABORATION ORDER


end ada_main;
