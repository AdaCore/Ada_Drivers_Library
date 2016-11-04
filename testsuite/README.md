Ada_Drivers_Library testsuite
=============================

Without nice emulator support, testing bare-board code is hard. The goal of
this testsuite is to leverage the native support packages in this repository to
test services and components that rely on native implementations for HAL
interfaces.


How to run the testsuite
------------------------

First, make sure you have a Python 3 interpreter available, and then run:

    ./run.py

The standard output report should be obvious to read. In order to restrict the
set of executed tests, run instead:

    ./run.py foo bar

This will execute all tests that have either ``foo`` or ``bar`` in their name

If Valgrind is available, add a ``--valgrind`` switch to detect memory issues
such as invalid operations or leaks.


How to write testcases
----------------------

Every subdirectory in ``tests/`` that contains a ``tc.gpr`` file is a testcase.
Each testcase embeds one or more test drivers (i.e. Ada programs) that run test
code and write to their standard output to demonstrate that some feature is
correctly implemented. For each test driver X, the project file must build an
executable as ``bin/X`` and there must be a ``X.out`` file next to the
``tc.gpr`` project file that states what the test driver output should be for
the test to pass.
