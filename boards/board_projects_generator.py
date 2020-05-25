#!/usr/bin/env python3

from subprocess import check_call


BOARDS = {
    'Crazyflie':           { 'rts_profiles': ['ravenscar-sfp', 'ravenscar-full']},
    'HiFive1':             { 'rts_profiles': ['zfp']},
    'HiFive1_rev_B':       { 'rts_profiles': ['zfp']},
    'Unleashed':           { 'rts_profiles': ['zfp', 'ravenscar-sfp', 'ravenscar-full']},
    'MicroBit':            { 'rts_profiles': ['zfp']},
    'NRF52_DK':            { 'rts_profiles': ['zfp']},
    'Native':              { 'rts_profiles': ['none']},
    'OpenMV2':             { 'rts_profiles': ['ravenscar-sfp', 'ravenscar-full']},
    'STM32F407_Discovery': { 'rts_profiles': ['ravenscar-sfp', 'ravenscar-full']},
    'STM32F429_Discovery': { 'rts_profiles': ['ravenscar-sfp', 'ravenscar-full']},
    'STM32F469_Discovery': { 'rts_profiles': ['ravenscar-sfp', 'ravenscar-full']},
    'STM32F746_Discovery': { 'rts_profiles': ['ravenscar-sfp', 'ravenscar-full']},
    'STM32F769_Discovery': { 'rts_profiles': ['ravenscar-sfp', 'ravenscar-full']},
    'NUCLEO_F446ZE':       { 'rts_profiles': ['ravenscar-sfp', 'ravenscar-full']},
    'Feather_STM32F405':   { 'rts_profiles': ['ravenscar-sfp', 'ravenscar-full']},
    }


FOLDERS = {'Crazyflie': 'crazyflie',
           'HiFive1': 'HiFive1',
           'HiFive1_rev_B': 'HiFive1_rev_B',
           'Unleashed': 'Unleashed',
           'MicroBit': 'MicroBit',
	   'NRF52_DK': 'NRF52_DK',
           'Native': 'native',
           'OpenMV2': 'OpenMV2',
           'STM32F407_Discovery': 'stm32f407_discovery',
           'STM32F429_Discovery': 'stm32f429_discovery',
           'STM32F469_Discovery': 'stm32f469_discovery',
           'STM32F746_Discovery': 'stm32f746_discovery',
           'STM32F769_Discovery': 'stm32f769_discovery',
           'NUCLEO_F446ZE':       'nucleo_f446ze',
           'Feather_STM32F405':   'feather_stm32f405'}

USE_STARTUP_GEN = ['HiFive1', 'HiFive1_rev_B', 'MicroBit', 'NRF52_DK']

def gen_project(board_name, rts):
    assert board_name is not None, "board is undefined"
    assert board_name in BOARDS, "%s is undefined" % board_name

    if rts == 'zfp':
        suffix = 'ZFP'
    elif rts == 'ravenscar-sfp':
        suffix = 'SFP'
    elif rts == 'ravenscar-full':
        suffix = 'Full'
    elif rts == 'none':
        suffix = None
    else:
        assert False, "Unexpected runtime %s" % rts

    if suffix is not None:
        project_name = '%s_%s' % (board_name, suffix)
        object_dir_name = "obj/" + suffix.lower()
        source_dir_name = "src/" + suffix.lower()
    else:
        project_name = board_name
        object_dir_name = "obj"
        source_dir_name = "config_src"

    args = ["python3",
            "../scripts/project_wizard.py",
            "--script-mode",
            "-d", FOLDERS[board_name],
            "-p", project_name,
            "-s", source_dir_name,
            "-o", object_dir_name,
            "Board=%s" % board_name,
            "Runtime_Profile=%s" % rts,
            "Use_Startup_Gen=" + ("True" if board_name in USE_STARTUP_GEN else "False")]
    check_call(args)


if __name__ == "__main__":
    for b in BOARDS:
        print("\n=== %s ===" % b)
        for rts in BOARDS[b]['rts_profiles']:
            print("\n=== %s" % rts)
            gen_project(b, rts)
