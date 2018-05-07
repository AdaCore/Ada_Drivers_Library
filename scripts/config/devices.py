#! /usr/bin/env python

import sys


def list_of_devices(config):
    family = config.get_config("Device_Family")
    if family == "STM32F4":
        return ['STM32F407VGTx',
                'STM32F405RGTx',
                'STM32F427VGTx',
                'STM32F429ZITx',
                'STM32F469NIHx']
    elif family == "STM32F7":
        return ['STM32F746NGHx',
                'STM32F769NIHx']
    elif family == "nRF51":
        return ['nRF51822xxAA']
    elif family == "FE3":
        return ['FE310']
    else:
        print "fatal error, unknown family '%s'" % family
        sys.exit(1)


def list_of_vendors(config):
    arch = config.get_config("Architecture")
    if arch == "ARM":
        return ["STMicro", "Nordic"]
    elif arch == "RISC-V":
        return ["SiFive"]
    elif arch == "Native":
        return []
    else:
        print "fatal error, unknown architecture '%s'" % arch
        sys.exit(1)


def list_of_families(config):
    vendor = config.get_config("Vendor")
    if vendor == "STMicro":
        return ["STM32F4", "STM32F7"]
    elif vendor == "Nordic":
        return ["nRF51"]
    elif vendor == "SiFive":
        return ['FE3']
    else:
        print "fatal error, unknown vendor '%s'" % vendor
        sys.exit(1)


def load_device_config(config):
    mcu = config.get_config("Device_Name")
    origin = 'MCU definition'

    src = []
    if mcu == 'STM32F407VGTx' or mcu == 'STM32F405RGTx':
        src += ['arch/ARM/STM32/devices/stm32f40x/',
                'arch/ARM/STM32/svd/stm32f40x',
                'arch/ARM/STM32/drivers/',
                'arch/ARM/STM32/drivers/dma/',
                'arch/ARM/STM32/drivers/dma_interrupts/',
                'arch/ARM/STM32/drivers/crc_stm32f4/',
                'arch/ARM/STM32/drivers/i2c_stm32f4',
                'arch/ARM/STM32/drivers/power_control_stm32f4',
                'arch/ARM/STM32/drivers/uart_stm32f4/',
                'arch/ARM/STM32/drivers/sd/',
                'arch/ARM/STM32/drivers/sd/sdio/']

    elif mcu == 'STM32F427VGTx':
        src += ['arch/ARM/STM32/devices/stm32f42x/',
                'arch/ARM/STM32/svd/stm32f429x',
                'arch/ARM/STM32/drivers/',
                'arch/ARM/STM32/drivers/dma/',
                'arch/ARM/STM32/drivers/dma_interrupts/',
                'arch/ARM/STM32/drivers/fmc/',
                'arch/ARM/STM32/drivers/dma2d/',
                'arch/ARM/STM32/drivers/crc_stm32f4/',
                'arch/ARM/STM32/drivers/i2c_stm32f4',
                'arch/ARM/STM32/drivers/power_control_stm32f4',
                'arch/ARM/STM32/drivers/uart_stm32f4/',
                'arch/ARM/STM32/drivers/sd/',
                'arch/ARM/STM32/drivers/sd/sdio/']

    elif mcu == 'STM32F429ZITx':
        src += ['arch/ARM/STM32/devices/stm32f42x/',
                'arch/ARM/STM32/svd/stm32f429x',
                'arch/ARM/STM32/drivers/',
                'arch/ARM/STM32/drivers/dma/',
                'arch/ARM/STM32/drivers/dma_interrupts/',
                'arch/ARM/STM32/drivers/fmc/',
                'arch/ARM/STM32/drivers/dma2d/',
                'arch/ARM/STM32/drivers/crc_stm32f4/',
                'arch/ARM/STM32/drivers/i2c_stm32f4',
                'arch/ARM/STM32/drivers/power_control_stm32f4',
                'arch/ARM/STM32/drivers/uart_stm32f4/',
                'arch/ARM/STM32/drivers/ltdc/',
                'arch/ARM/STM32/drivers/sd/',
                'arch/ARM/STM32/drivers/sd/sdio/']

    elif mcu == 'STM32F469NIHx':
        src += ['arch/ARM/STM32/devices/stm32f46_79x/',
                'arch/ARM/STM32/svd/stm32f46_79x/',
                'arch/ARM/STM32/drivers/',
                'arch/ARM/STM32/drivers/dma/',
                'arch/ARM/STM32/drivers/dma_interrupts/',
                'arch/ARM/STM32/drivers/dsi/',
                'arch/ARM/STM32/drivers/fmc/',
                'arch/ARM/STM32/drivers/dma2d/',
                'arch/ARM/STM32/drivers/crc_stm32f4/',
                'arch/ARM/STM32/drivers/i2c_stm32f4',
                'arch/ARM/STM32/drivers/power_control_stm32f4',
                'arch/ARM/STM32/drivers/uart_stm32f4/',
                'arch/ARM/STM32/drivers/ltdc/',
                'arch/ARM/STM32/drivers/sai/',
                'arch/ARM/STM32/drivers/sd/',
                'arch/ARM/STM32/drivers/sd/sdio/']

    elif mcu == 'STM32F746NGHx':
        src += ['arch/ARM/STM32/devices/stm32f7x/',
                'arch/ARM/STM32/svd/stm32f7x/',
                'arch/ARM/STM32/drivers/',
                'arch/ARM/STM32/drivers/dma/',
                'arch/ARM/STM32/drivers/dma_interrupts/',
                'arch/ARM/STM32/drivers/fmc/',
                'arch/ARM/STM32/drivers/dma2d/',
                'arch/ARM/STM32/drivers/i2c_stm32f7',
                'arch/ARM/STM32/drivers/power_control_stm32f7',
                'arch/ARM/STM32/drivers/ltdc/',
                'arch/ARM/STM32/drivers/sai/',
                'arch/ARM/STM32/drivers/sd/',
                'arch/ARM/STM32/drivers/sd/sdmmc/']

    elif mcu == 'STM32F769NIHx':
        src += ['arch/ARM/STM32/devices/stm32f7x9/',
                'arch/ARM/STM32/svd/stm32f7x9/',
                'arch/ARM/STM32/drivers/',
                'arch/ARM/STM32/drivers/dma_stm32f769/',
                'arch/ARM/STM32/drivers/dma_interrupts/',
                'arch/ARM/STM32/drivers/fmc/',
                'arch/ARM/STM32/drivers/dma2d/',
                'arch/ARM/STM32/drivers/i2c_stm32f7',
                'arch/ARM/STM32/drivers/power_control_stm32f7',
                'arch/ARM/STM32/drivers/ltdc/',
                'arch/ARM/STM32/drivers/dsi/',
                'arch/ARM/STM32/drivers/sai/',
                'arch/ARM/STM32/drivers/sd/',
                'arch/ARM/STM32/drivers/sd/sdmmc/']

    elif mcu.startswith('nRF51'):
        src += ['arch/ARM/Nordic/devices/',
                'arch/ARM/Nordic/drivers/',
                'arch/ARM/Nordic/svd/nrf51/']

    elif mcu == 'FE310':
        src += ['arch/RISC-V/SiFive/svd/FE310/',
                'arch/RISC-V/SiFive/devices/FE310/',
                'arch/RISC-V/SiFive/drivers/']

    else:
        print "Unknown MCU device %s." % mcu

    for d in src:
        config.add_source_dir(d, origin)
