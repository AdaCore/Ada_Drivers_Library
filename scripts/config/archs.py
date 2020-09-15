#! /usr/bin/env python


def list_of_archs(config):
    return ["ARM", "RISC-V", "Native"]


def load_cpu_config(config):
    cpu = config.get_config("CPU_Core")
    origin = 'arch definition'

    if cpu == "ARM Cortex-M0":
        config.add_source_dir('arch/ARM/cortex_m/src', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/cm0', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/nocache', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/nvic_cm0', origin)

    elif cpu == "ARM Cortex-M4F":
        config.add_source_dir('arch/ARM/cortex_m/src', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/cm4f', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/fpu', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/nocache', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/nvic_cm4_cm7', origin)

    elif cpu == "ARM Cortex-M7F":
        config.add_source_dir('arch/ARM/cortex_m/src', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/cm7', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/fpu', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/cache', origin)
        config.add_source_dir('arch/ARM/cortex_m/src/nvic_cm4_cm7', origin)

    elif cpu == "RISC-V32":
        config.add_source_dir('arch/RISC-V/src/', origin)
        config.add_source_dir('arch/RISC-V/src/rv32', origin)

    elif cpu == "RISC-V64":
        config.add_source_dir('arch/RISC-V/src/', origin)
        config.add_source_dir('arch/RISC-V/src/rv64', origin)

    else:
        print("Unknown CPU core %s." % cpu)
