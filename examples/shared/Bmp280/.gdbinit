# This command file will cause a Cortex-M3 or -M4 board to automatically 
# reset immediately after a GDB "load" command executes.  Note that GPS
# issues that command as part of the Debug->Init menu invocation.  Manual
# "load" command invocations will also trigger the action.
#
# The reset is achieved by writing to the "Application Interrupt and Reset
# Control" register located at address 0xE000ED0C.  
#
# Both the processor and the peripherals can be reset by writing a value
# of 0x05FA0004. That value will write to the SYSRESETREQ bit.  If you want
# to avoid resetting the peripherals, change the value to 0x05FA0001.  That
# value will write to the VECTRESET bit.  Do *not* use a value that sets both
# bits.
#
# In both cases, any on-board debug hardware is not reset.
#
# See the book "The Definitive Guide to the ARM Cortex-M3 and Cortex-M4
# Processors" by Joseph Yiu, 3rd edition, pp 262-263 for further details.

define hookpost-load
echo Resetting the processor and peripherals...\n
set *0xE000ED0C := 0x05FA0004
echo Reset complete\n
end