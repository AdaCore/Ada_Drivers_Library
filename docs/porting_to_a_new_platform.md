# Porting to a new platform

## Porting the run-time

Porting will always start with the run-time, it is a prerequisite for any Ada
application.

Porting a run-time will be a different task depending on the CPU architecture
and the run-time profile that you need.

If your CPU architecture is already supported, for instance ARM Cortex-M4, the
porting effort will be limited to the BSP: startup code, linker script, clock
configuration and interrupt names.

If the CPU architecture is not already supported, for instance AVR, there
will be more work: context switch, handling interrupts, providing a clock and
timing services, etc. It may also require some compiler development.

There are three different run-time profiles (ZFP, Ravescar-SFP, Ravenscar-Full)
with increasing set of features:

 - ZFP (stands for Zero FootPrint): The minimum required to run an Ada program

 - Ravenscar-SFP (stands for Small FootPrint): ZFP + the minimum required for Ada
   Ravenscar tasking support (task, protected objects, interrupt handling,
   clock and timming).

 - Ravenscar-Full: Ravenscar-SFP + Ada exception propagation and standard
   library packages like containers.

If you are only interested in the ZFP profile (no tasking) then porting will be
much simpler. Usually only the startup code an linker script need to be
changed. Note that for the moment, the platform agnostic parts of
`Ada_Drivers_Library` (middleware and components) works on all run-time
profiles starting with ZFP.

Please go to [AdaCore/bb-runtimes](https://github.com/AdaCore/bb-runtimes) for
detailed instructions on how to port run-times.

## Porting Ada_Drivers_Library

This section is still in a "TODO" state.
