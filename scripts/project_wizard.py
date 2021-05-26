#! /usr/bin/env python3

import os
import argparse
import config
import config.user_input.console
from build_all_examples import run_program

from config.boards import list_of_boards, load_board_config
from config.devices import list_of_devices, list_of_vendors, \
     list_of_families, load_device_config
from config.archs import list_of_archs, load_cpu_config

script_dir = os.path.dirname(__file__)
ADL_root_dir = os.path.abspath(os.path.join(script_dir, ".."))
cwd = os.getcwd()

interactive = True
use_default = True


def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)


def board_config(config):
    config.query_enum_key("Board", list_of_boards(), "Custom_Board")

    if config.get_config("Board") != "Custom_Board":
        load_board_config(config)


def mcu_config(config, source_dir):

    config.query_enum_key("Architecture", list_of_archs(config))

    if config.get_config("Architecture") != "Native":
        config.query_enum_key("Vendor", list_of_vendors(config))
        config.query_enum_key("Device_Family", list_of_families(config))
        config.query_enum_key("Device_Name", list_of_devices(config))

        device = config.get_config("Device_Name")

        if device.startswith("STM32F4"):
            core = "ARM Cortex-M4F"
        elif device.startswith("STM32F7"):
            core = "ARM Cortex-M7F"
        elif device.startswith("nRF51"):
            core = "ARM Cortex-M0"
        elif device.startswith("nRF52"):
            core = "ARM Cortex-M4F"
        elif device.startswith("FE3"):
            core = "RISC-V32"
        elif device.startswith("U5"):
            core = "RISC-V64"

        config.set_config_string_key("CPU_Core", core, "mcu definition")

        family = config.get_config("Device_Family")

        if family.startswith("STM32"):
            config.query_integer_key("High_Speed_External_Clock",
                                     1000000, 32000000)
        elif family.startswith("FE3"):
            config.query_integer_key("hifive1_uart_root")
            config.query_bool_key("qemu_sifive_test_exit")

        load_cpu_config(config)
        load_device_config(config, source_dir)

        config.query_integer_key("Number_Of_Interrupts", default = 0)

    # TDOD: Is the SVD mapping available?
    # TODO: Does the user want to generate it?


def linker_script_config(config):
    config.query_bool_key('Use_Startup_Gen', default="False")

    if config.get_config('Use_Startup_Gen') == "True":
        config.query_bool_key('Has_Custom_Memory_Area_1', default="False")

        cnt = 1
        while config.get_config('Has_Custom_Memory_Area_%d' % cnt) == "True":
            config.query_enum_key('Custom_Mem_%d_Kind' % cnt, ['ROM', 'RAM'])
            config.query_integer_key('Custom_Mem_%d_Addr' % cnt, 0)
            config.query_integer_key('Custom_Mem_%d_Size' % cnt, 0)
            config.query_string_key('Custom_Mem_%d_Name' % cnt,
                                    default="custom_mem_%d" % cnt)

            config.add_memory(config.get_config('Custom_Mem_%d_Kind' % cnt),
                              config.get_config('Custom_Mem_%d_Name' % cnt),
                              config.get_config('Custom_Mem_%d_Addr' % cnt),
                              config.get_config('Custom_Mem_%d_Size' % cnt))

            cnt += 1
            config.query_bool_key('Has_Custom_Memory_Area_%d' % cnt,
                                  default="False")

    if len(config.memory_names()) > 0:
        config.query_string_key('Boot_Memory', default=config.memory_names()[0])


def middleware_config(config):
    config.query_integer_key('Max_Path_Length', 0, default="1024")
    config.query_integer_key('Max_Mount_Points', 0, default="2")
    config.query_integer_key('Max_Mount_Name_Length', 0, default="128")

    origin = "middleware config"
    config.add_source_dir('middleware/src/filesystem', origin)
    config.add_source_dir('middleware/src/BLE', origin)
    config.add_source_dir('middleware/src/utils', origin)
    config.add_source_dir('middleware/src/audio', origin)
    config.add_source_dir('middleware/src/monitor', origin)
    config.add_source_dir('middleware/src/bitmap', origin)
    config.add_source_dir('middleware/src/command_line', origin)
    config.add_source_dir('middleware/src/sdmmc', origin)
    config.add_source_dir('middleware/src/neopixel', origin)

    runtime_profile = config.get_config("Runtime_Profile")

    if runtime_profile is None:
        runtime_profile = ''

    if runtime_profile.startswith("ravenscar"):
        config.add_source_dir('middleware/src/ravenscar-common', origin)


def components_config(config):
    config.add_source_dir('components/src/**', "components config")


def runtime_config(config):
    config.query_string_key('Has_ZFP_Runtime', default="False")
    config.query_string_key('Has_Ravenscar_SFP_Runtime', default="False")
    config.query_string_key('Has_Ravenscar_Full_Runtime', default="False")

    profiles = []
    if config.get_config("Has_Ravenscar_Full_Runtime") == "True":
        profiles.append("ravenscar-full")
    if config.get_config("Has_Ravenscar_SFP_Runtime") == "True":
        profiles.append("ravenscar-sfp")
    if config.get_config("Has_ZFP_Runtime") == "True":
        profiles.append("zfp")

    if len(profiles) > 0:
        config.query_enum_key('Runtime_Profile',  profiles,
                              default=profiles[0])

        config.query_string_key('Runtime_Name_Suffix')

        suffix = config.get_config("Runtime_Name_Suffix")
        profile = config.get_config("Runtime_Profile")
        config.query_string_key('Runtime_Name', default=profile + "-" + suffix)


def ADL_configuration(config, project_directory, project_name,
                      source_dir_name, object_dir_name):

    relative_ADL_root_path = os.path.relpath(ADL_root_dir, project_directory)
    source_dir = os.path.join(project_directory, source_dir_name)
    ada_config_path = os.path.join(source_dir, "adl_config.ads")
    gpr_path = os.path.join(project_directory, "%s.gpr" % project_name.lower())

    ensure_dir(source_dir)

    # HAL is always incuded
    config.add_source_dir('hal/src/', "HAL config")

    board_config(config)
    mcu_config(config, source_dir)
    runtime_config(config)
    linker_script_config(config)
    middleware_config(config)
    components_config(config)

    print("The configuration is now finished.")
    print("Let's generate some files:")


    #  GPR file
    gpr = "--  This project file was generated by the " + \
          "Ada_Drivers_Library project wizard script\n"
    gpr += "library project %s is\n" % project_name

    gpr += """
   type Build_Type is ("Debug", "Production");
   Build : Build_Type := external ("ADL_BUILD", "Debug");

   type Build_Checks_Type is ("Disabled", "Enabled");
   Build_Checks : Build_Checks_Type := external ("ADL_BUILD_CHECKS", "Disabled");

   --  Target architecture
   Target := Project'Target;

   --  Callgraph info is not available on all architectures
   Callgraph_Switch := ();
   case Target is
      when "riscv32-unknown-elf" => null;
      when others => Callgraph_Switch := ("-fcallgraph-info=su");
   end case;

   Build_Checks_Switches := ();
   case Build_Checks is
      when "Disabled" => null;
      when others =>
         Build_Checks_Switches :=
           ("-gnaty", "-gnatyM120", "-gnatyO", --  Style checks
            "-gnatwe"); --  Warnings as errors
   end case;

   package Compiler is
      case Build is
         when "Production" =>
            for Default_Switches ("Ada") use
              ("-O3",     -- Optimization
               "-gnatp",  -- Supress checks
               "-gnatn"); -- Enable inlining
         when "Debug" =>
            for Default_Switches ("Ada") use
              ("-O0",    -- No optimization
               "-gnata") -- Enable assertions
              & Callgraph_Switch;
      end case;

      for Default_Switches ("ada") use Compiler'Default_Switches ("Ada") &
        Callgraph_Switch &
        Build_Checks_Switches &
        ("-g",       -- Debug info
         "-gnatwa",  -- All warnings
         "-gnatw_A", -- Turn off warnings for anonymous allocators
         "-gnatQ",   -- Don't quit. Generate ALI and tree files even if illegalities
         "-gnatw.X", -- Disable warnings for No_Exception_Propagation
         "-ffunction-sections", -- Create a linker section for each function
         "-fdata-sections");  -- Create a linker section for each data
   end Compiler;


"""
    if config.get_config ("Use_Startup_Gen") == "True":
        gpr += '   for Languages use ("Ada", "Asm_CPP");\n'
    else:
        gpr += '   for Languages use ("Ada");\n'

    gpr += "   for Create_Missing_Dirs use \"True\";\n"
    gpr += "   for Object_Dir use \"%s_\" & Build;\n" % object_dir_name
    gpr += "   for Library_Dir use \"%s_lib_\" & Build;\n" % object_dir_name
    gpr += "   for Library_Kind use \"static\";\n"
    gpr += "   for Library_Name use \"ada_drivers_library\";\n"

    if config.get_config ("Use_Startup_Gen") == "True":
        gpr += '\n   Linker_Switches := ' + \
               '("-T", Project\'Project_dir & "/%s/link.ld");\n' % source_dir_name
    else:
        gpr += '\n   Linker_Switches := ();\n'

    gpr += config.gpr_configuration(relative_ADL_root_path, source_dir_name)
    gpr += "end %s;\n" % project_name
    print(" -> Writing gprbuild project file.")
    with open(gpr_path, "w") as f:
        f.write(gpr)

    # Ada config package
    print(" -> Writing the Ada configuration file.")

    ada = "--  This package was generated by the " + \
          "Ada_Drivers_Library project wizard script\n"
    ada += "package ADL_Config is\n"
    ada += config.ada_configuration()
    ada += "end ADL_Config;\n"
    with open(ada_config_path, "w") as f:
        f.write(ada)

    config.print_remaining_pre_defined()

    # Run startup-gen
    if config.get_config ("Use_Startup_Gen") == "True":
        print(" -> Generating startup files.")
        run_program ('startup-gen',
                     '-P', gpr_path,
                     '-l', os.path.join(source_dir, "link.ld"),
                     '-s', os.path.join(source_dir, "crt0.S"))

    print("Your Ada Drivers Library project is now ready to use.")


description = \
"""Welcome to the Ada Drivers Library (ADL) project wizard. This script will
ask you some questions to define the ADL configuration of your project. It will
then generate the different files required to use ADL based on this
configuration."""

parser = argparse.ArgumentParser(description=description)

parser.add_argument('definitions', nargs='*',
                    help="""Pre defined config key=value. Use this to give predefined answer to the
                    configuration question. For instant Board=STM32F407_Discovery""")

parser.add_argument('-i', '--input-config', type=argparse.FileType('r'),
                    action='append',
                    help="""Load pre defined config keys from a file. The file must containt couples of
                    key=value, only one per line and nothing else.""")

parser.add_argument('--script-mode', action="store_true",
                    help="""Enable script mode. In script mode there is no input required from the user.
                    Configuration values will be taken from pre-defined values
                    (--input-config or command line definitions), or default
                    value if any. The configuration will fail if there are no
                    pre-defined or default value for a key. This mode can be
                    used to automatically generated ADL project file from a
                    known configuration.""")

parser.add_argument('--dont-use-defaults', action="store_true",
                    help="""When this switch is used in script mode, the configuration will not take into
                    account default values. This means that the configuration
                    will fail if a key doesn't have a pre-defined value from
                    --input-config or command line definitions. This can be
                    used to automatically detect discrepency when upgrading to
                    a new version of ADL, for instance if a new config key was
                    added.""")


parser.add_argument("-d", "--project-dir",
                    help="""Directory where the project will be created (default is Ada_Drivers_Library).
                    Use '.' to create the project in the current directory.""",
                    default="Ada_Drivers_Library")

parser.add_argument("-p", "--project-name",
                    help="""Name of the project file that will be generated (default is
                    Ada_Drivers_Library)""",
                    default="Ada_Drivers_Library")

parser.add_argument("-s", "--source-dir",
                    help="""Name of the source directory for you ADL project (default is src). This is
                    where the Ada confiuration file will be written. You can
                    also use this directory to add you own package
                    specification and/or package implementation to the ADL
                    project. The source directory will be created inside the
                    project directory""",
                    default="src")

parser.add_argument("-o", "--object-dir",
                    help="""Name of the object output directory that will be used for your ADL project
                    (default is obj). The object directory will be created
                    inside the project directory""",
                    default="obj")

args = parser.parse_args()

print(description)

interactive = not args.script_mode
use_default = not args.dont_use_defaults

ADL_config = config.Database(interactive,
                             use_default,
                             config.user_input.console)

if args.input_config is not None:
    for f in args.input_config:
        ADL_config.parse_input_config(f)

if args.definitions is not None:
    for d in args.definitions:
        key, value = d.partition("=")[::2]
        print("Add pre definition '%s' => '%s' from command line" % \
              (key, value))

        ADL_config.pre_define(key, value, "command line")

ensure_dir(args.project_dir)

ADL_configuration(ADL_config,
                  os.path.join(cwd, args.project_dir),
                  args.project_name,
                  args.source_dir,
                  args.object_dir)
