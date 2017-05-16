# Filesystem example

This filesystem example set currently runs only under a unix host. Most of the
other examples in this repository will run under either Windows or Linux,
however this set requires a special build of st-util which is not yet available
for Windows, and the example code looks back to the host computer and expects
to see a unix file system.

The demo will parse the file on the host and print some basic information about
the file in the same terminal window used to run st-util and then exits.

1. stlink and st-util
----------
If this command
```
st-util --semihosting
```
does not generate an error about an unknown option then your st-util is new
enough. If you get the error, please visit github.com/texane/stlink for the
latest source code.

2. The disk_8_partitions.img file
----------

This example requires a file named disk_8_partitions.img. This is the target
file about which the application will return some basic information. Copy the
disk_8_partitions.img from testsuite/tests/disk_partitions/ in the /tmp/
directory on your host computer before you run the code.

3. The basics
----------
a) In a shell window run the command
```
st-util --semihosting
```
b) Build the demo and load it onto your stm32 board. 

c) Run the demo on the board and check the outputs in the shell window that you
   used for st-util.




