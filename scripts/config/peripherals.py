import os

class Peripheral(object):
    def __init__(self):
        self._base_addr = 0
        self._size = 0
        self._interrupts = []
        self._src = []
        self._deps = []

    def add_interrupt(self, id):
        self._interrpts.append(id)

    def set_reg(self, base_addr, size):
        self._base_addr = base_addr
        self._size = size

    def add_source_dir(self, dir):
        self._src.append(dir)

    def source_dirs(self):
        return self._src

    def add_dependency(self, dep):
        self._deps.append(dep)

    def dependencies(self):
        return self._deps


class SOC(object):
    def __init__(self, root_package):
        self._root_package = root_package
        self._periphs = []
        self._src = []
        self._deps = []

    def add(self, p):
        self._periphs.append(p)
        self._src += p.source_dirs()

    def source_dirs(self):
        # return list of uniq source dirs
        return list(set(self._src))

    def dependencies(self):
        return self._deps

    def generate_device_spec(self):
        out = ""
        deps = []
        for p in self._periphs:
            deps += p.dependencies()
        deps = list(set(deps))

        for d in deps:
            out += "with %s; use %s;\n" % (d, d)

        out += "\npackage %s.Device is\n\n" % self._root_package
        for p in self._periphs:
            out += p.generate_device_spec()
        out += "end %s.Device;\n" % self._root_package
        return out

    def write_device_spec(self, source_dir):
        filename = "sifive-device.ads"
        with open(os.path.join(source_dir, filename), "w") as file:
            file.write(self.generate_device_spec());


class SiFive(Peripheral):
    def __init__(self):
        super(SiFive, self).__init__()
        self.add_source_dir('arch/RISC-V/SiFive/')


class SiFiveGPIO_0(SiFive):
    def __init__(self, base_addr, size, port_id, pin_nbr):
        super(SiFiveGPIO_0, self).__init__()

        self._pin_nbr = pin_nbr
        self._port_id = port_id

        self.set_reg(base_addr, size)
        self.add_source_dir('arch/RISC-V/SiFive/drivers/gpio0')
        self.add_dependency("SiFive.GPIO")

    def generate_device_spec(self):
        out = "   -- GPIO%s --\n\n" % self._port_id
        out += "   GPIO%s : aliased GPIO_Controller (%d);\n\n" \
               % (self._port_id, self._base_addr)

        for X in range(self._pin_nbr):
            out += "   P%s : aliased GPIO_Point (GPIO%s'Access, %d);\n" \
                   % (self._port_id + str(X), self._port_id, X)

        return out + "\n"


class SiFiveSPI_0(SiFive):
    def __init__(self, base_addr, size, port_id):
        super(SiFiveSPI_0, self).__init__()

        self._port_id = port_id

        self.set_reg(base_addr, size)
        self.add_source_dir('arch/RISC-V/SiFive/drivers/spi0')
        self.add_dependency("SiFive.SPI")

    def generate_device_spec(self):
        out = "   -- QSPI%s --\n\n" % self._port_id
        out += "   QSPI%s : aliased SPI_Controller (%d);\n" \
               % (self._port_id, self._base_addr)
        return out + "\n"


class SiFivePWM_0(SiFive):
    def __init__(self, base_addr, size, port_id):
        super(SiFivePWM_0, self).__init__()

        self._port_id = port_id

        self.set_reg(base_addr, size);
        self.add_source_dir('arch/RISC-V/SiFive/drivers/pwm0');
        self.add_dependency("SiFive.PWM")
        self.add_dependency("System")

    def generate_device_spec(self):
        out = "   -- PWM%s --\n\n" % self._port_id
        out += "   PWM%s_Internal : aliased SiFive.PWM.Internal_PWM\n" % self._port_id
        out += "      with Import, Address => System'To_Address (%d);\n" % (self._base_addr)
        out += "   PWM%s : aliased SiFive.PWM.PWM_Device (PWM%s_Internal'Access);\n"\
               % (self._port_id, self._port_id)
        return out + "\n"


class SiFiveUART_0(SiFive):
    def __init__(self, base_addr, size, port_id):
        super(SiFiveUART_0, self).__init__()

        self._port_id = port_id

        self.set_reg(base_addr, size);
        self.add_source_dir('arch/RISC-V/SiFive/drivers/uart0');
        self.add_dependency("SiFive.UART")
        self.add_dependency("System")

    def generate_device_spec(self):
        out = "   -- UART%s --\n\n" % self._port_id
        out += "   UART%s : aliased SiFive.UART.UART_Device (%d);\n" \
               % (self._port_id, self._base_addr)
        return out + "\n"
