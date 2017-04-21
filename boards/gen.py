#!/usr/bin/env python

BOARDS = {'Crazyflie': ['ravenscar-sfp', 'ravenscar-full'],
          'HiFive1': ['zfp'],
          'MicroBit': ['zfp'],
          'Native': None,
          'OpenMV2': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F407Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F429Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F469Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F746Disco': ['ravenscar-sfp', 'ravenscar-full'],
          'STM32F769Disco': ['ravenscar-sfp', 'ravenscar-full']}

FOLDERS = {'Crazyflie': 'crazyflie',
           'HiFive1': 'hifive1',
           'MicroBit': 'microbit',
           'Native': 'native',
           'OpenMV2': 'openmv2',
           'STM32F407Disco': 'stm32f407_discovery',
           'STM32F429Disco': 'stm32f429_discovery',
           'STM32F469Disco': 'stm32f469_discovery',
           'STM32F746Disco': 'stm32f746_discovery',
           'STM32F769Disco': 'stm32f769_discovery'}


def gen_project(board, rts):
    assert board is not None, "board is undefined"
    project_name = FOLDERS[board].title()

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
        project_name = '%s_%s' % (project_name, suffix)

    cnt = 'aggregate library project %s is\n' % project_name
    cnt += '\n'
    lower = board.lower()
    cnt += '   Board := "%s";\n\n' % lower

    if BOARDS[board] is not None:
        #  Do not use a RTS Profile for the native project
        if len(BOARDS[board]) == 1:
            # Only one rts defined for the board, do not make it a user option
            rts = BOARDS[board][0]
        if rts is not None:
            # Runtime profile is forced for this project
            cnt += '   RTS_Profile := "%s";\n' % rts
        else:
            # Runtime profile is for the user to choose
            cnt += '   type RTS_Profile_Type is ("%s");\n' % \
                   '", "'.join(BOARDS[board])
            cnt += '   RTS_Profile : RTS_Profile_Type :=\n'
            cnt += '     external ("RTS_Profile", "%s");\n' % \
                   BOARDS[board][0]
        cnt += '\n'

    cnt += '   type Build_Type is ("Debug", "Production");\n'
    cnt += '   Build : Build_Type := external ("PLATFORM_BUILD", "Production");\n'
    cnt += '\n'

    # definition of the Target
    if lower == 'native':
        pass
    else:
        cnt += '   for Target use "arm-eabi";\n'
    # definition of Runtime ("Ada")
    if lower == 'native':
        pass
    elif lower in ('crazyflie', 'stm32f407disco'):
        # Both use the stm32f4 runtime
        cnt += '   for Runtime ("Ada") use RTS_Profile & "-stm32f4";\n'
    else:
        cnt += '   for Runtime ("Ada") use RTS_Profile & "-" & Board;\n'
    cnt += '\n'

    # Object subdirectories.
    if lower == 'native':
        cnt += '   Obj_Suffix := "native-" & Build;\n'
    else:
        cnt += '   Obj_Suffix := Board & "/" & RTS_Profile & "/" & Build;\n'
    cnt += '\n'
    cnt += '   for Library_Name use Board;\n'
    cnt += '   for Library_Dir use "lib/" & Obj_Suffix;\n'
    cnt += '\n'
    cnt += '   for external ("Obj_Suffix") use Obj_Suffix;\n'
    if lower != 'native':
        cnt += '   for external ("RTS_Profile") use RTS_Profile;\n'

    cnt += '   for Project_Path use \n'
    cnt += '     (Project\'Project_Dir, Project\'Project_Dir & "..");\n'
    cnt += '   for Project_Files use ("%s/board.gpr");\n' % FOLDERS[board]
    cnt += '\n'
    cnt += 'end %s;\n' % project_name

    print "creating %s.gpr" % project_name.lower()
    with open('%s.gpr' % project_name.lower(), 'w') as fp:
        fp.write(cnt)

if __name__ == "__main__":
    for b in BOARDS:
        gen_project(b, None)
        if BOARDS[b] is not None and len(BOARDS[b]) > 1:
            for rts in BOARDS[b]:
                gen_project(b, rts)
