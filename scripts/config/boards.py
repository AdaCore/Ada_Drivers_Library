#! /usr/bin/env python


def list_of_boards():
    return ["Custom_Board",
            'STM32F407_Discovery',
            'STM32F429_Discovery',
            'STM32F469_Discovery',
            'STM32F746_Discovery',
            'STM32F769_Discovery',
            'STM32_H405',
            'NUCLEO_F446ZE',
            'Crazyflie',
            'Feather_STM32F405',
            'OpenMV2',
            "MicroBit",
            "NRF52_DK",
            "HiFive1",
            "HiFive1_rev_B",
            "Unleashed",
            'Native']


def load_board_config(config):
    board = config.get_config("Board")
    origin = 'board definition'

    # The NUCLEO-F446ZE has an STM32F446ZE microcontroller. However the
    # 446 is not supported yet, so we use the 407 that is very similar.
    if board == "STM32F407_Discovery" or board == "NUCLEO_F446ZE":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'STMicro', origin)
        config.pre_define('Device_Family', 'STM32F4', origin)
        config.pre_define('Device_Name', 'STM32F407VGTx', origin)
        config.pre_define('High_Speed_External_Clock', '8000000', origin)
        config.pre_define('Has_ZFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'stm32f4', origin)

        if board == "STM32F407_Discovery":
            config.add_source_dir('boards/stm32_common/stm32f407disco/',
                                  origin)
        else:
            config.add_source_dir('boards/stm32_common/nucleo_f446ze/', origin)

        config.add_source_dir('boards/stm32_common/common/', origin)

    elif board == "STM32F429_Discovery":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'STMicro', origin)
        config.pre_define('Device_Family', 'STM32F4', origin)
        config.pre_define('Device_Name', 'STM32F429ZITx', origin)
        config.pre_define('High_Speed_External_Clock', '8000000', origin)
        config.pre_define('Has_ZFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'stm32f429disco', origin)
        config.add_source_dir('boards/stm32_common/stm32f429disco/', origin)
        config.add_source_dir('boards/stm32_common/common/', origin)
        config.add_source_dir('boards/stm32_common/dma2d/', origin)
        config.add_source_dir('boards/stm32_common/ltdc/', origin)
        config.add_source_dir('boards/stm32_common/sdram/', origin)

    elif board == "STM32F469_Discovery":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'STMicro', origin)
        config.pre_define('Device_Family', 'STM32F4', origin)
        config.pre_define('Device_Name', 'STM32F469NIHx', origin)
        config.pre_define('High_Speed_External_Clock', '8000000', origin)
        config.pre_define('Has_ZFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'stm32f469disco', origin)
        config.add_source_dir('boards/stm32_common/stm32f469disco/', origin)
        config.add_source_dir('boards/stm32_common/common/', origin)
        config.add_source_dir('boards/stm32_common/dma2d/', origin)
        config.add_source_dir('boards/stm32_common/otm8009a', origin)
        config.add_source_dir('boards/stm32_common/sdcard/', origin)
        config.add_source_dir('boards/stm32_common/sdram/', origin)

    elif board == "STM32F746_Discovery":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'STMicro', origin)
        config.pre_define('Device_Family', 'STM32F7', origin)
        config.pre_define('Device_Name', 'STM32F746NGHx', origin)
        config.pre_define('High_Speed_External_Clock', '25000000', origin)
        config.pre_define('Has_ZFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'stm32f746disco', origin)
        config.add_source_dir('boards/stm32_common/stm32f746disco/', origin)
        config.add_source_dir('boards/stm32_common/common/', origin)
        config.add_source_dir('boards/stm32_common/dma2d/', origin)
        config.add_source_dir('boards/stm32_common/ltdc', origin)
        config.add_source_dir('boards/stm32_common/sdcard/', origin)
        config.add_source_dir('boards/stm32_common/sdram/', origin)

    elif board == "STM32F769_Discovery":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'STMicro', origin)
        config.pre_define('Device_Family', 'STM32F7', origin)
        config.pre_define('Device_Name', 'STM32F769NIHx', origin)
        config.pre_define('High_Speed_External_Clock', '25000000', origin)
        config.pre_define('Has_ZFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'stm32f769disco', origin)
        config.add_source_dir('boards/stm32_common/stm32f769disco/', origin)
        config.add_source_dir('boards/stm32_common/common/', origin)
        config.add_source_dir('boards/stm32_common/dma2d/', origin)
        config.add_source_dir('boards/stm32_common/otm8009a', origin)
        config.add_source_dir('boards/stm32_common/sdcard/', origin)
        config.add_source_dir('boards/stm32_common/sdram/', origin)

    elif board == "STM32_H405":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'STMicro', origin)
        config.pre_define('Device_Family', 'STM32F4', origin)
        config.pre_define('Device_Name', 'STM32F405RGTx', origin)
        config.pre_define('High_Speed_External_Clock', '8_000_000', origin)
        config.pre_define('Has_ZFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'stm32f4', origin)
        config.add_source_dir('boards/stm32_h405/src/', origin)

    elif board == "Crazyflie":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'STMicro', origin)
        config.pre_define('Device_Family', 'STM32F4', origin)
        config.pre_define('Device_Name', 'STM32F405RGTx', origin)
        config.pre_define('High_Speed_External_Clock', '8000000', origin)
        config.pre_define('Has_ZFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'stm32f4', origin)
        config.add_source_dir('boards/crazyflie/src/', origin)

    elif board == "Feather_STM32F405":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'STMicro', origin)
        config.pre_define('Device_Family', 'STM32F4', origin)
        config.pre_define('Device_Name', 'STM32F405RGTx', origin)
        config.pre_define('High_Speed_External_Clock', '12000000', origin)
        config.pre_define('Has_ZFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'feather_stm32f405', origin)
        config.add_source_dir('boards/feather_stm32f405/src/', origin)

    elif board == "OpenMV2":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'STMicro', origin)
        config.pre_define('Device_Family', 'STM32F4', origin)
        config.pre_define('Device_Name', 'STM32F427VGTx', origin)
        config.pre_define('High_Speed_External_Clock', '12000000', origin)
        config.pre_define('Has_ZFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'openmv2', origin)
        config.add_source_dir('boards/OpenMV2/src/', origin)

    elif board == "MicroBit":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'Nordic', origin)
        config.pre_define('Device_Family', 'nRF51', origin)
        config.pre_define('Device_Name', 'nRF51822xxAA', origin)
        config.pre_define('Has_ZFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'False', origin)
        config.pre_define('Runtime_Name_Suffix', 'cortex-m0', origin)
        config.add_source_dir('boards/MicroBit/src/', origin)

    elif board == "NRF52_DK":
        config.pre_define('Architecture', 'ARM', origin)
        config.pre_define('Vendor', 'Nordic', origin)
        config.pre_define('Device_Family', 'nRF52', origin)
        config.pre_define('Device_Name', 'nRF52832xxAA', origin)
        config.pre_define('Has_ZFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'False', origin)
        config.pre_define('Runtime_Name_Suffix', 'cortex-m4f', origin)
        config.add_source_dir('boards/NRF52_DK/src/', origin)

    elif board == "HiFive1":
        config.pre_define('Architecture', 'RISC-V', origin)
        config.pre_define('Vendor', 'SiFive', origin)
        config.pre_define('Device_Family', 'FE3', origin)
        config.pre_define('Device_Name', 'FE310', origin)
        config.pre_define('Has_ZFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'False', origin)
        config.pre_define('Runtime_Name_Suffix', 'rv32imc', origin)
        config.add_memory('rom', 'board_flash', '0x20400000', '512M')
        config.add_source_dir('boards/HiFive1/src/', origin)

    elif board == "HiFive1_rev_B":
        config.pre_define('Architecture', 'RISC-V', origin)
        config.pre_define('Vendor', 'SiFive', origin)
        config.pre_define('Device_Family', 'FE3', origin)
        config.pre_define('Device_Name', 'FE310', origin)
        config.pre_define('Has_ZFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'False', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'False', origin)
        config.pre_define('Runtime_Name_Suffix', 'rv32imc', origin)
        config.add_memory('rom', 'board_flash', '0x20010000', '32M')
        config.add_source_dir('boards/HiFive1/src/', origin)

    elif board == "Unleashed":
        config.pre_define('Architecture', 'RISC-V', origin)
        config.pre_define('Vendor', 'SiFive', origin)
        config.pre_define('Device_Family', 'U5', origin)
        config.pre_define('Device_Name', 'U540', origin)
        config.pre_define('Has_ZFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_SFP_Runtime', 'True', origin)
        config.pre_define('Has_Ravenscar_Full_Runtime', 'True', origin)
        config.pre_define('Runtime_Name_Suffix', 'unleashed', origin)
        config.add_source_dir('boards/Unleashed/src/', origin)

    elif board == "Native":
        config.pre_define('Architecture', 'Native', origin)
        config.add_source_dir('boards/native/src/', origin)
    else:
        print("Unknown board %s." % board)
