#! /usr/bin/env python

import sys
import os
import json
from peripherals import *


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
    elif family == "SAMD51":
        return ['ATSAMD51G18A', 'ATSAMD51G19A', 'ATSAMD51J18A', 'ATSAMD51J19A',
                'ATSAMD51J20A', 'ATSAMD51N19A', 'ATSAMD51N20A', 'ATSAMD51P19A',
                'ATSAMD51P20A']
    elif family == "FE3":
        return ['FE310']
    elif family == "U5":
        return ['U540']
    else:
        print "fatal error, unknown family '%s'" % family
        sys.exit(1)


def list_of_vendors(config):
    arch = config.get_config("Architecture")
    if arch == "ARM":
        return ["STMicro", "Nordic", "Microchip"]
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
    elif vendor == "Microchip":
        return ["SAMD51"]
    elif vendor == "SiFive":
        return ['FE3', 'U5']
    else:
        print "fatal error, unknown vendor '%s'" % vendor
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
        src += ['arch/ARM/Nordic/devices/',
                'arch/ARM/Nordic/drivers/',
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

    elif mcu == 'FE310':
        src += ['arch/RISC-V/SiFive/svd/FE310/',
                'arch/RISC-V/SiFive/devices/FE310/',
                'arch/RISC-V/SiFive/drivers/']
        config.add_memory('ram', 'RAM', '0x80000000', '16K')

    elif mcu == 'U540':
        dev = U540()

    elif mcu.startswith('ATSAMD51'):
        device_dir = os.path.join('arch/ARM/Microchip/SAM/devices/SAMD51/', mcu)

        root_dir = os.path.dirname(os.path.realpath(__file__))
        root_dir = os.path.join(root_dir, '..', '..')
        with open(os.path.join(root_dir, device_dir, 'device_info.json'), 'r') as f:
            info = json.load(f)

        print(json.dumps(info))

        config.add_memory(info['memories'][info['boot_mem']]['type'],
                          info['boot_mem'],
                          info['memories'][info['boot_mem']]['start'],
                          info['memories'][info['boot_mem']]['size'])

        for mem in info['memories']:
            if mem != info['boot_mem']:
                config.add_memory(info['memories'][mem]['type'],
                                  mem,
                                  info['memories'][mem]['start'],
                                  info['memories'][mem]['size'])

        for driver in info['drivers']:
            src += ['arch/ARM/Microchip/SAM/drivers/' + driver]

        src += ['arch/ARM/Microchip/SAM/drivers/samd51_clock_setup']
        src += ['arch/ARM/Microchip/SAM', device_dir]

    else:
        print "Unknown MCU device %s." % mcu

    if dev:
        dev.write_device_spec(source_dir)
        src += dev.source_dirs()

    for d in src:
        config.add_source_dir(d, origin)
