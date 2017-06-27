#!/usr/bin/env python

import re
import os
import sys

BOARDS = {
    'Crazyflie': {
        'rts_board':             'stm32f4',
        'rts_profiles':          ['ravenscar-sfp', 'ravenscar-full'],
        'target':                'arm-eabi',
        'mcu':                   'arch/ARM/STM32/stm32f40x',
        'source_dirs':           ['src']},
    'HiFive1': {
        'rts_board':             'hifive1',
        'rts_profiles':          ['zfp'],
        'target':                'riscv32-unknown-elf',
        'mcu':                   'arch/RISC-V/SiFive/FE310',
        'source_dirs':           ['src']},
    'MicroBit': {
        'rts_board':             'microbit',
        'rts_profiles':          ['zfp'],
        'target':                'arm-eabi',
        'mcu':                   'arch/ARM/Nordic/nrf51',
        'source_dirs':           ['src']},
    'Native': {
        'source_dirs':           ['src']},
    'OpenMV2': {
        'rts_board':             'openmv2',
        'rts_profiles':          ['ravenscar-sfp', 'ravenscar-full'],
        'target':                'arm-eabi',
        'mcu':                   'arch/ARM/STM32/stm32f427x',
        'source_dirs':           ['src']},
    'STM32F407_Discovery': {
        'rts_board':             'stm32f4',
        'rts_profiles':          ['ravenscar-sfp', 'ravenscar-full'],
        'target':                'arm-eabi',
        'mcu':                   'arch/ARM/STM32/stm32f40x',
        'source_dirs':           ['../stm32_common/stm32f407disco',
                                  '../stm32_common/common']},
    'STM32F429_Discovery': {
        'rts_board':             'stm32f429disco',
        'rts_profiles':          ['ravenscar-sfp', 'ravenscar-full'],
        'target':                'arm-eabi',
        'mcu':                   'arch/ARM/STM32/stm32f42x',
        'source_dirs':           ['../stm32_common/stm32f429disco',
                                  '../stm32_common/common',
                                  '../stm32_common/dma2d',
                                  '../stm32_common/ltdc',
                                  '../stm32_common/sdram']},
    'STM32F469_Discovery': {
        'rts_board':             'stm32f469disco',
        'rts_profiles':          ['ravenscar-sfp', 'ravenscar-full'],
        'target':                'arm-eabi',
        'mcu':                   'arch/ARM/STM32/stm32f46_79x',
        'source_dirs':           ['../stm32_common/stm32f469disco',
                                  '../stm32_common/common',
                                  '../stm32_common/dma2d',
                                  '../stm32_common/otm8009a',
                                  '../stm32_common/sdcard',
                                  '../stm32_common/sdram']},
    'STM32F746_Discovery': {
        'rts_board':             'stm32f746disco',
        'rts_profiles':          ['ravenscar-sfp', 'ravenscar-full'],
        'target':                'arm-eabi',
        'mcu':                   'arch/ARM/STM32/stm32f7x',
        'source_dirs':           ['../stm32_common/stm32f746disco',
                                  '../stm32_common/common',
                                  '../stm32_common/dma2d',
                                  '../stm32_common/ltdc',
                                  '../stm32_common/sdcard',
                                  '../stm32_common/sdram']},
    'STM32F769_Discovery': {
        'rts_board':             'stm32f769disco',
        'rts_profiles':          ['ravenscar-sfp', 'ravenscar-full'],
        'target':                'arm-eabi',
        'mcu':                   'arch/ARM/STM32/stm32f7x9',
        'source_dirs':           ['../stm32_common/stm32f769disco',
                                  '../stm32_common/common',
                                  '../stm32_common/dma2d',
                                  '../stm32_common/otm8009a',
                                  '../stm32_common/sdcard',
                                  '../stm32_common/sdram']},
    }


FOLDERS = {'Crazyflie': 'crazyflie',
           'HiFive1': 'HiFive1',
           'MicroBit': 'MicroBit',
           'Native': 'native',
           'OpenMV2': 'OpenMV2',
           'STM32F407_Discovery': 'stm32f407_discovery',
           'STM32F429_Discovery': 'stm32f429_discovery',
           'STM32F469_Discovery': 'stm32f469_discovery',
           'STM32F746_Discovery': 'stm32f746_discovery',
           'STM32F769_Discovery': 'stm32f769_discovery'}


def gen_project(board_name, rts):
    assert board_name is not None, "board is undefined"
    assert board_name in BOARDS, "%s is undefined" % board_name

    if rts == 'zfp':
        suffix = 'ZFP'
    elif rts == 'ravenscar-sfp':
        suffix = 'SFP'
    elif rts == 'ravenscar-full':
        suffix = 'Full'
    elif rts is None:
        suffix = None
    else:
        assert False, "Unexpected runtime %s" % rts

    if suffix is not None:
        project_name = '%s_%s' % (board_name, suffix)
    else:
        project_name = board_name

    board = BOARDS[board_name]
    board_folder = FOLDERS[board_name]

    # Generate the project's dependencies
    cnt = '--  **AUTOMATICALLY GENERATED** Do not edit !!\n'
    cnt += '--  Please see board_projects_generator.py\n'
    cnt += '--  and edit this script instead.\n'
    cnt += '\n'
    cnt += 'with "../config";\n'
    if 'mcu' in board:
        cnt += 'with "../../%s";\n' % board['mcu']
    cnt += 'with "../../components/components";\n'
    cnt += 'with "../../middleware/middleware";\n'
    if 'rts_profiles' in board:
        add_ravenscar_support = True
        if rts is None:
            for profile in board['rts_profiles']:
                if 'ravenscar' not in profile:
                    add_ravenscar_support = False
                    break
        elif 'ravenscar' not in rts:
            add_ravenscar_support = False
        if add_ravenscar_support:
            cnt += 'with "../../middleware/ravenscar_support";\n'
    if 'additional_dependency' in board:
        deps = board['additional_dependency']
        if deps is not None:
            for dep in deps:
                cnt += 'with "../../%s";\n' % dep

    cnt += '\n'
    cnt += 'library project %s is\n' % project_name
    cnt += '\n'

    #  Do not use a RTS Profile for the native project
    if 'rts_profiles' in board:
        runtimes = board['rts_profiles']
        if len(runtimes) == 1:
            # Only one rts defined for the board, do not make it a user option
            rts = runtimes[0]
        if rts is not None:
            # Runtime profile is forced for this project
            assert rts in runtimes, "invalid rts %s for %s" % (rts, board_name)
            cnt += '   RTS_Profile := "%s";\n' % rts
        else:
            # Runtime profile is for the user to choose
            cnt += '   type RTS_Profile_Type is ("%s");\n' % \
                   '", "'.join(runtimes)
            cnt += '   RTS_Profile : RTS_Profile_Type :=\n'
            cnt += '     external ("RTS_Profile", "%s");\n' % \
                   runtimes[0]
        cnt += '\n'

    # definition of the Target and runtime
    if 'target' in board and board['target'] is not None:
        assert 'rts_board' in board, "undefined 'rts_board' for %s" % board_name
        cnt += '   for Target use "%s";\n' % board['target']
        cnt += '   for Runtime ("Ada") use RTS_Profile & "-%s";\n' % \
               board['rts_board']
        cnt += '\n'

    # Object subdirectories.
    if board_name == 'Native':
        # native target
        cnt += '   Obj_Suffix := "native";\n'
    else:
        cnt += '   Obj_Suffix := Project\'Runtime ("Ada");\n'
    cnt += '\n'
    cnt += '   for Create_Missing_Dirs use "True";\n'
    cnt += '   for Library_Name use "%s";\n' % board_name.lower()
    cnt += '   for Library_Dir use "lib/" & Obj_Suffix;\n'
    cnt += '   for Object_Dir use "obj/" & Obj_Suffix;\n'
    cnt += '\n'
    cnt += '   for Source_Dirs use\n'
    cnt += '     ("%s");\n' % '",\n      "'.join(board['source_dirs'])
    cnt += '\n'
    cnt += '   package Builder is\n'
    cnt += '      for Switches ("Ada") use\n'
    if board_name == 'Native':
        cnt += '        ("-s");\n'
    else:
        cnt += '        ("--RTS=" & Project\'Runtime("Ada"), "-s");\n'
    cnt += '   end Builder;\n'
    cnt += '\n'
    cnt += '   package Compiler renames Config.Compiler;\n'
    cnt += '\n'
    cnt += 'end %s;\n' % project_name

    print "creating %s.gpr" % project_name.lower()
    with open('%s/%s.gpr' % (board_folder, project_name.lower()), 'w') as fp:
        fp.write(cnt)


if __name__ == "__main__":
    for b in BOARDS:
        gen_project(b, None)
        if 'rts_profiles' in BOARDS[b] and len(BOARDS[b]['rts_profiles']) > 1:
            for rts in BOARDS[b]['rts_profiles']:
                gen_project(b, rts)
