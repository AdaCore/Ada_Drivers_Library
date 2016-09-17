# Filesystem example

This filesystem example set currently runs only under a unix host. Most of the other examples in this repository will run under either Windows or Linux, however this set requires a special build of st-util which is not yet available for Windows, and the demo code looks back to the host and expects to see a unix file system. 

The demo will parse the file on the host and print some basic information about the file in the same terminal window used to run st-util and then exits.

1. stlink and st-util
----------
If this command 
```
st-util --semihosting
```
does not generate an error about an unknown option then your st-util is new enough. If you get the error, please visit github.com/texane/stlink for the latest source code.

2. The disk_8_partitions.img file
----------
This demo is delivered with a file named disk_8_partitions.img. This is the target file about which the application will return some basic information. Place the disk_8_partitions.img in hte /tmp/ directory on your host computer before you run the code.


