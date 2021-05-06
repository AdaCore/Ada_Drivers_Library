#! /usr/bin/env python

import sys
from config.peripherals import *


class U540(SOC):
    def __init__(self):
        super(U540, self).__init__("SiFive")
        self.add(SiFiveGPIO_0(0x10060000, 0x1000, '0', 16))

        self.add(SiFiveSPI_0(0x10040000, 0x1000, '0'))
        self.add(SiFiveSPI_0(0x10041000, 0x1000, '1'))
        self.add(SiFiveSPI_0(0x10050000, 0x1000, '2'))

        self.add(SiFivePWM_0(0x10020000, 0x1000, '0'))
        self.add(SiFivePWM_0(0x10021000, 0x1000, '1'))

        self.add(SiFiveUART_0(0x10010000, 0x1000, '0'))
        self.add(SiFiveUART_0(0x10011000, 0x1000, '1'))


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
    elif family == "nRF52":
        return ['nRF52832xxAA', 'nRF52833xxAA']
    elif family == "FE3":
        return ['FE310']
    elif family == "U5":
        return ['U540']
    else:
        print("fatal error, unknown family '%s'" % family)
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
        print("fatal error, unknown architecture '%s'" % arch)
        sys.exit(1)


def list_of_families(config):
    vendor = config.get_config("Vendor")
    if vendor == "STMicro":
        return ["STM32F4", "STM32F7"]
    elif vendor == "Nordic":
        return ["nRF51", "nRF52"]
    elif vendor == "SiFive":
        return ['FE3', 'U5']
    else:
        print("fatal error, unknown vendor '%s'" % vendor)
        sys.exit(1)


def load_device_config(config, source_dir):
    mcu = config.get_config("Device_Name")
    origin = 'MCU definition'
    dev = None
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
        src += ['arch/ARM/Nordic/devices/nrf51',
                'arch/ARM/Nordic/drivers/nrf_common',
                'arch/ARM/Nordic/drivers/nrf51',
                'arch/ARM/Nordic/svd/nrf51/']

        config.pre_define('Number_Of_Interrupts', 32, origin)

        if mcu.endswith ('AB'):
            config.add_memory('rom', 'flash', '0x00000000', '128K')
        else:
            config.add_memory('rom', 'flash', '0x00000000', '256K')


        if mcu.endswith ('AC'):
            config.add_memory('ram', 'ram', '0x20000000', '32K')
        else:
            config.add_memory('ram', 'ram', '0x20000000', '16K')

    elif mcu.startswith('nRF52'):
        src += ['arch/ARM/Nordic/devices/nrf52',
                'arch/ARM/Nordic/drivers/nrf_common',
                'arch/ARM/Nordic/drivers/nrf52',
                'arch/ARM/Nordic/svd/nrf52/']

        config.pre_define('Number_Of_Interrupts', 128, origin)

        if mcu.startswith('nRF52832'):
            if mcu.endswith ('AA'):
                config.add_memory('rom', 'flash', '0x00000000', '512K')
                config.add_memory('ram', 'ram', '0x20000000', '64K')
            elif mcu.endswith ('AB'):
                config.add_memory('rom', 'flash', '0x00000000', '256K')
                config.add_memory('ram', 'ram', '0x20000000', '32K')
        elif mcu.startswith('nRF52833'):
            config.add_memory('rom', 'flash', '0x00000000', '512K')
            config.add_memory('ram', 'ram', '0x20000000', '128K')

    elif mcu == 'FE310':
        src += ['arch/RISC-V/SiFive/svd/FE310/',
                'arch/RISC-V/SiFive/devices/FE310/',
                'arch/RISC-V/SiFive/drivers/']
        config.add_memory('ram', 'RAM', '0x80000000', '16K')

    elif mcu == 'U540':
        dev = U540()

    else:
        print("Unknown MCU device %s." % mcu)

    if dev:
        dev.write_device_spec(source_dir)
        src += dev.source_dirs()

    for d in src:
        config.add_source_dir(d, origin)
