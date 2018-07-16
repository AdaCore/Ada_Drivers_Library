#! /usr/bin/env python2

import argparse
import os
import os.path
import subprocess
import sys
import distutils.spawn

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))


def run_program(*argv):
    print "$ %s" % " ".join(argv)
    p = subprocess.Popen(argv, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    stdout, stderr = p.communicate()

    try:
        stdout = stdout.decode('ascii')
    except UnicodeError:
        return 'stdout is not ASCII'

    try:
        stderr = stderr.decode('ascii')
    except UnicodeError:
        return 'stderr is not ASCII'

    return (p.returncode, stdout, stderr)


def gprbuild(project_file, debug=False):
    extra_args = []

    extra_args = extra_args + ["-XADL_BUILD=" +
                               ("Debug" if debug else "Production")]

    print "Building '%s'" % project_file

    # Build the project
    returncode, stdout, stderr = run_program(
        'gprbuild', '-j0', '-p', '-q', '-s', '-P', project_file, *extra_args
    )

    print stdout

    if returncode:
        print 'Build error (gprbuild returned {}):\n{}'.format(
            returncode, stderr
        )

    # Clean to avoid error in the next build with a different run-time or
    # compile switches.
    run_program('gprclean', "-P",  project_file, *extra_args)

    return returncode


# Run-time profiles
RAVENSCAR_FULL = ["ravenscar-full"]
RAVENSCAR_SFP = ["ravenscar-sfp"]
BOTH_RAVENSCAR = RAVENSCAR_FULL + RAVENSCAR_SFP

# There's no ZFP value in the RTS variable so far so we'll use an empty string
# for now.
ZFP = [""]

STM_DRIVERS = "/arch/ARM/STM32/driver_demos/"

# List of project to compile
projects = [
            # STM32F429 Discovery
            "/boards/stm32f429_discovery/stm32f429_discovery_full.gpr",
            "/boards/stm32f429_discovery/stm32f429_discovery_sfp.gpr",
            "/examples/STM32F429_Discovery/draw_stm32f429disco.gpr",
            "/examples/STM32F429_Discovery/blinky_f429disco.gpr",
            "/examples/STM32F429_Discovery/dma2d_stm32f429disco.gpr",
            "/examples/STM32F429_Discovery/serial_ports_f429disco.gpr",


            # STM32F4 DISCO
            "/boards/stm32f407_discovery/stm32f407_discovery_full.gpr",
            "/boards/stm32f407_discovery/stm32f407_discovery_sfp.gpr",
            "/examples/STM32F4_DISCO/accelerometer/accelerometer.gpr",
            "/examples/STM32F4_DISCO/simple_audio/simple_audio.gpr",
            "/examples/STM32F4_DISCO/filesystem/filesystem.gpr",
            "/examples/STM32F4_DISCO/blinky_f4disco.gpr",
            "/examples/STM32F4_DISCO/serial_ports_f4disco.gpr",

            # STM32F469 Discovery
            "/boards/stm32f469_discovery/stm32f469_discovery_full.gpr",
            "/boards/stm32f469_discovery/stm32f469_discovery_sfp.gpr",
            "/examples/STM32F469_Discovery/dma2d_stm32f469disco.gpr",
            "/examples/STM32F469_Discovery/draw_stm32f469disco.gpr",
            "/examples/STM32F469_Discovery/hello_world_tasking_f469disco.gpr",


            # STM32F746 Discovery
            "/boards/stm32f746_discovery/stm32f746_discovery_full.gpr",
            "/boards/stm32f746_discovery/stm32f746_discovery_sfp.gpr",
            "/examples/STM32F746_Discovery/dma2d_stm32f746disco.gpr",
            "/examples/STM32F746_Discovery/draw_stm32f746disco.gpr",
            "/examples/STM32F746_Discovery/blinky_f7disco.gpr",

            # STM32F769 Discovery
            "/boards/stm32f769_discovery/stm32f769_discovery_full.gpr",
            "/boards/stm32f769_discovery/stm32f769_discovery_sfp.gpr",
            "/examples/STM32F769_Discovery/dma2d_stm32f769disco.gpr",
            "/examples/STM32F769_Discovery/draw_stm32f769disco.gpr",

            # OpenMV2
            "/boards/OpenMV2/openmv2_full.gpr",
            "/boards/OpenMV2/openmv2_sfp.gpr",
            "/examples/OpenMV2/openmv2_example.gpr",
            "/examples/OpenMV2/openmv2_example.gpr",
            "/examples/OpenMV2/openmv2_example.gpr",

            # MicroBit
            "/boards/MicroBit/microbit_zfp.gpr",
            "/examples/MicroBit/text_scrolling/text_scrolling.gpr",
            "/examples/MicroBit/buttons/buttons.gpr",
            "/examples/MicroBit/digital_in/digital_in.gpr",
            "/examples/MicroBit/music/music.gpr",
            "/examples/MicroBit/analog_in/analog_in.gpr",
            "/examples/MicroBit/analog_out/analog_out.gpr",
            "/examples/MicroBit/digital_out/digital_out.gpr",
            "/examples/MicroBit/BLE_beacon/BLE_beacon.gpr",

            # STM32 driver examples
            STM_DRIVERS + "/demo_adc_dma/demo_adc_dma.gpr",
            STM_DRIVERS + "/demo_adc_interrupts/demo_adc_interrupts.gpr",
            STM_DRIVERS + "/demo_adc_polling/demo_adc_polling.gpr",
            STM_DRIVERS + "/demo_adc_timer_dma/demo_adc_timer_dma.gpr",
            STM_DRIVERS + "/demo_adc_timer_triggered/demo_adc_timer_triggered.gpr",
            STM_DRIVERS + "/demo_crc/demo_crc.gpr",
            STM_DRIVERS + "/demo_dac_basic/demo_dac_basic.gpr",
            STM_DRIVERS + "/demo_dac_dma/demo_dac_dma.gpr",
            STM_DRIVERS + "/demo_dma_mem_to_mem/demo_dma_mem_to_mem.gpr",
            STM_DRIVERS + "/demo_dma_mem_to_peripheral/demo_usart_dma_continuous.gpr",
            STM_DRIVERS + "/demo_gpio_direct_leds/demo_gpio.gpr",
            STM_DRIVERS + "/demo_independent_watchdog/demo_iwdg.gpr",
            STM_DRIVERS + "/demo_L3GD20_dataready_int/demo_l3gd20_dataready_int.gpr",
            STM_DRIVERS + "/demo_L3GD20_fifo_int/demo_l3gd20_fifo_int.gpr",
            STM_DRIVERS + "/demo_L3GD20_polling/demo_l3gd20.gpr",
            STM_DRIVERS + "/demo_LIS3DSH_pwm/demo_lis3dsh_pwm.gpr",
            STM_DRIVERS + "/demo_LIS3DSH_tilt/demo_lis3dsh_tilt.gpr",
            STM_DRIVERS + "/demo_rng/demo_rng.gpr",
            STM_DRIVERS + "/demo_timer_interrupts_basic/demo_basic_timer_interrupts.gpr",
            STM_DRIVERS + "/demo_timer_interrupts_multichannel/demo_timer_interrupts_multichannel.gpr",
            STM_DRIVERS + "/demo_timer_pwm/demo_timer_pwm.gpr",
            STM_DRIVERS + "/demo_timer_quad_encoder/demo_timer_quad_encoder.gpr",
            STM_DRIVERS + "/demo_usart_interrupts/demo_usart_interrupts.gpr",
            STM_DRIVERS + "/demo_usart_polling/demo_usart_polling.gpr",
            ]

# Check if RISC-V32 compiler is available
if distutils.spawn.find_executable("riscv32-elf-gnatls"):

    # Add RISC-V32 projects
    projects += ["/examples/HiFive1/hifive1_example.gpr"]

parser = argparse.ArgumentParser('Compile all the Ada_Drivers_Library examples')

parser.add_argument(
    'pattern', nargs='*',
    help='List of patterns to filter the set of examples to build'
)


def main(args):
    # Check if we can actually detect a build failure
    ret = gprbuild("This_Project_Doesnt_Exist", debug=False)
    if not ret:
        print "Build failure is not detected"
        sys.exit(1)

    ret = 0
    for prj in projects:

        # Check filter pattern, if any
        if args.pattern and not any(pat in prj for pat in args.pattern):
            continue

        ret = ret or gprbuild(ROOT_DIR + prj, debug=True)
        ret = ret or gprbuild(ROOT_DIR + prj, debug=False)

    if ret:
        sys.exit(1)


if __name__ == '__main__':
    main(parser.parse_args())
