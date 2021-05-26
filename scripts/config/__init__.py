#! /usr/bin/env python3

import sys
from config.validation import Int_Validation, Float_Validation, \
     Enum_Validation, String_Validation, Bool_Validation


class Database:

    def __init__(self, interactive, use_default, user_input):
        self.pre_defined_params = {}
        self.configuration = {}
        self.memories = []
        self.src_dirs = []
        self.interactive = interactive
        self.use_default = use_default
        self.user_input = user_input

    def pre_define(self, key, value, origin):
        self.pre_defined_params[key] = {'value': value,
                                        'origin': origin}

    def print_remaining_pre_defined(self):
        for key in self.pre_defined_params:
            origin = self.pre_defined_params[key]['origin']
            print("warning: key '%s' from %s " % (key, origin) + \
                  "was not used in the configuration")

    def add_to_configuration(self, key, kind, value, origin):
        self.configuration[key] = {'value':  value,
                                   'kind':   kind,
                                   'origin': origin}

    def get_config(self, key):
        if key in self.configuration:
            return self.configuration[key]['value']
        else:
            return None

    def ada_configuration(self):
        out = ""
        for key in sorted(self.configuration):
            origin = self.configuration[key]['origin']
            value = self.configuration[key]['value']
            kind = self.configuration[key]['kind']
            if kind == "int" or kind == "float":
                out += "   %-30s : constant         := %-20s -- From %s\n" % \
                      (key, str(value) + ";", origin)
            elif kind == "enum" or kind == "string":
                out += "   %-30s : constant String  := %-20s -- From %s\n" % \
                      (key, "\"" + str(value) + "\";", origin)
            elif kind == "bool":
                out += "   %-30s : constant Boolean := %-20s -- From %s\n" % \
                      (key, str(value) + ';', origin)
            else:
                print("fatal error, unknown kind '%s' for config key '%s'" % \
                      (kind, key))
                sys.exit(1)

        return out

    def gpr_configuration(self, src_root_dir, extra_source_dir):
        out = ""
        arch = self.get_config("Architecture")
        runtime = self.get_config("Runtime_Name")

        if arch is not None and arch != "Native":
            if arch == "ARM":
                target = "arm-eabi"
            elif arch == "RISC-V":
                target = "riscv32-elf"

            out += "   for Target use \"%s\";\n" % target

        if runtime is not None:
            out += "   for Runtime (\"Ada\") use \"%s\";\n" % runtime

        out += "\n"

        # Device configuration
        if arch is not None and arch != "Native":
            out += "   package Device_Configuration is\n"
            out += '      for CPU_Name use "%s";\n' % self.get_config("CPU_Core");
            out += '      for Number_Of_Interrupts use "%d";\n' % self.get_config("Number_Of_Interrupts");

            for cnt in range(self.get_config("Number_Of_Interrupts")):
                out += '      for Interrupt ("%d") use "%s";\n' % (cnt, "adl_irq")

            if len(self.memory_names()):
               out += '\n      for Memories use ("' + '", "'.join(self.memory_names())
               out += '");\n'

               for mem in self.memories:
                   out += '\n      for Mem_Kind ("%(name)s") use "%(kind)s";\n' % (mem)
                   out += '      for Address  ("%(name)s") use "%(addr)s";\n' % (mem)
                   out += '      for Size     ("%(name)s") use "%(size)s";\n' % (mem)

               out += '\n      for Boot_Memory use "%s";\n' % self.get_config("Boot_Memory")

            for key in ['hifive1_uart_root', 'qemu_sifive_test_exit']:
                if key in self.configuration:
                    out += '      for User_Tag ("%s") use "%s";\n' % (key, self.configuration[key]['value'])

            out += '   end Device_Configuration;\n\n'

        # Config keys and values
        for key in sorted(self.configuration):
            origin = self.configuration[key]['origin']
            value = self.configuration[key]['value']
            out += "   %-30s := %-20s -- From %s\n" % \
                   (key, '"' + str(value) + '";', origin)

        out += "\n   --  Project source directories\n"
        out += "   Src_Dirs_Root := \"%s\";\n" % src_root_dir
        out += "   for Source_Dirs use (\n"
        for src in self.src_dirs:
            out += "    Src_Dirs_Root & \"/%s\", -- From %s\n" % \
                   (src['value'], src['origin'])
        out += "     \"%s/\");\n" % extra_source_dir

        return out

    def ask_the_user(self, key, validation, default):
        origin = "user input"
        kind = validation.kind()

        if kind == "int":
            value = self.user_input.query_int(key,
                                              validation.min_value,
                                              validation.max_value,
                                              default)
        elif kind == "enum":
            value = self.user_input.query_choice(key,
                                                 validation.list_of_values,
                                                 default)
        elif kind == "string":
            value = self.user_input.query_string(key, default)
        elif kind == "float":
            value = self.user_input.query_float(key,
                                                validation.min_value,
                                                validation.max_value,
                                                default)
        elif kind == "bool":
            value = self.user_input.query_bool(key, default)

        self.add_to_configuration(key,
                                  validation.kind(),
                                  value,
                                  origin)

    def use_pre_defined_param(self, key, validation):
        value = self.pre_defined_params[key]['value']
        origin = self.pre_defined_params[key]['origin']

        if validation(value):
            self.add_to_configuration(key, validation.kind(), value, origin)
            print("For key '%s', take value '%s' from %s" % \
                  (key, value, origin))
            del self.pre_defined_params[key]
        else:
            print("Invalid value '%s' for key '%s' from %s" % \
                  (value, key, origin))
            print("Valid values are : %s" % str(validation))
            sys.exit(1)

    def query_key(self, key, validation=None, default=None):
        if key in self.pre_defined_params:
            self.use_pre_defined_param(key, validation)
        elif self.interactive:
            self.ask_the_user(key, validation, default)
        elif default is not None:
            if self.use_default:
                print("For key '%s', use default value '%s'" % (key, default))
                self.add_to_configuration(key,
                                          validation.kind(),
                                          default,
                                          "default value")
            else:
                print("fatal error, user doesn't want to use default " + \
                      "value for key '%s'" % key)
                sys.exit(1)
        else:
            print("fatal error, user input required for key '%s'" % key)
            sys.exit(1)

    def query_integer_key(self,
                          key,
                          min_value=None,
                          max_value=None,
                          default=None):
        self.query_key(key,
                       validation=Int_Validation(min_value, max_value),
                       default=default)

    def query_float_key(self,
                        key,
                        min_value=None,
                        max_value=None,
                        default=None):
        self.query_key(key,
                       validation=Float_Validation(min_value, max_value),
                       default=default)

    def query_enum_key(self, key, list_of_values, default=None):
        self.query_key(key,
                       validation=Enum_Validation(list_of_values),
                       default=default)

    def query_string_key(self, key, default=None):
        self.query_key(key, validation=String_Validation(), default=default)

    def query_bool_key(self, key, default=None):
        self.query_key(key, validation=Bool_Validation(), default=default)

    def set_config_string_key(self, key, value, origin):
        self.add_to_configuration(key, 'string', value, origin)

    def set_config_key_int(self, key, value, origin):
        self.add_to_configuration(key, 'int', value, origin)

    def parse_input_config(self, file):
        print("loading configuration keys from: '%s'" % file.name)
        line_cnt = 0
        for line in file:
            line_cnt += 1
            key, val = line.partition("=>")[::2]
            key = key.strip()
            val = val.strip()
            if key and val:
                print("Add pre definition '%s' => '%s' from file '%s'" % \
                      (key, val, file.name))
                self.pre_define(key, val, "%s:%d" % (file.name, line_cnt))
            else:
                print("invalid input configuration line at %s:%d" % \
                      (file.name, line_cnt))

    # Memory layout config #

    def add_memory(self, kind, name, addr, size):
        self.memories += [{'kind': kind,
                           'name': name,
                           'addr': addr,
                           'size': size}]

    def memory_names(self):
        list = []
        for mem in self.memories:
            list.append(mem['name'])
        return list

    def default_rom_area(self):
        for mem in self.memories:
            if mem['kind'] == 'ROM':
                return mem['name']
        return None

    def default_ram_area(self):
        for mem in self.memories:
            if mem['kind'] == 'RAM':
                return mem['name']
        return None

    # Source directories config #

    def add_source_dir(self, relative_path, origin):
        self.src_dirs.append({'value':  relative_path,
                              'origin': origin})
