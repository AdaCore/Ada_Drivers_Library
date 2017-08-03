# File system
## Accessing the file system

The user interface to read and write files in `Ada_Drivers_Library` is located
in the `File_IO` package.

At initialization, there is no file system available, so you have to mount one
before using the operations described below. Please read the section `Mounting
a file system` to learn how to add a file system.

### Opening a file

To open a file, use the `Open` function:

```ada=
   FD : File_Descriptor;
begin
   if Open (FD, "/host/tmp/file.txt", Read_Only) /= OK then
      --  Error handling...
   end if;
```

You can open the file in `Read_Only`, `Write_Only` or `Read_Write` mode.

### Reading from a file

Once the file descriptor is open in `Read_Only` or `Read_Write` mode you can
use the `Read` function to get data from the file:

```ada=

   Data : HAL.UInt8_Array (1 .. 10);
begin
   if Read (FD, Data'Address, Data'Length) /= Data'Length then
      --  Error handling...
   end if;
```

This uses the `'Address` attribute to get the address of the data in memory.
You also have to specify the size of the data in number of bytes. The return
value is the number of bytes actually read.

A a safer and cleaner alternative is to use the generic function `Generic_Read`
and instantiate it for the type of data that you want to read:

```ada=
   type Custom_Type is [...];

   function Read is new Generic_Read (Custom_Type);
   Data : Custom_Type;
begin
   if Read (FD, Data) /= OK then
      --  Error handling...
   end if;
```

### Writing to a file

Writing operations are very similar to the reading operations:

```ada=

   Data : HAL.UInt8_Array (1 .. 10) := (others => 0);
begin
   if Write (FD, Data'Address, Data'Length) /= Data'Length then
      --  Error handling...
   end if;
```

And the generic version:

```ada=
   type Custom_Type is [...];

   function Write is new Generic_Write (Custom_Type);
   Data : Custom_Type;
begin
   if Write (FD, Data) /= OK then
      --  Error handling...
   end if;
```

### Seek and Offset

You can use the `Offset` and `Seek` function to manipulate the file descriptor
offset.

`Offset` will give you the current value of the Offset.

```ada=
   Current_Offset : File_Size;
begin
   Current_Offset := Offset (FD);
```

The function `Seek` is used to change the offset:

```ada=
   Amount : File_Size := 10;
begin
   if Seek (FD, Forward, Amount) /= OK then
      --  Error handling...
   end if;
```

There are 4 seek modes available:
 - `Forward`: Increase the offset by the given `Amount` 
 - `backward`: Decrease the offset by the given `Amount` 
 - `From_Start`: Set to offset to `Amount`
 - `From_End`: Set the offset to the size of the file minus `Amount`

### Flush

Use `Flush` to force all buffered data to be written on the file.

### Close

Use `Close` to close the file descriptor and free the associated resources.

## Mounting a file system

To mount a file system you will need a file system driver. Please find below
detailed instructions on how to instantiate and mount the different file
systems available in `Ada_Drivers_Library`.

```Ada=
   if Mount_Volume ("mount_point", A_File_System_Driver) /= OK then
      --  Error handling...
   end
```

You can now access file in the volume, for example `/mount_point/tmp/file.txt`.

## Mounting a drive

It is also possible to mount a disk drive that has one or more file system.
Please note that for the moment only FAT file systems are supported.

In `Ada_Drivers_Library` disk drive are accessed with a `Block_Driver` interface.

```Ada=
   if Mount_Drive ("mount_point", A_Block_Driver) /= OK then
      --  Error handling...
   end if;
```

## File system drivers available

### File Allocation Table (FAT) drives

`Work in progress...`

### ARM Semihosting file system

The ARM semihosting file system driver uses the [ARM semihosting
interface](http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0471c/Bgbjhiea.html)
to provide a bridge to the host file system.

It is implemented in the package `Semihosting.Filesystem`.

#### Limitation

Given the semihosting operation available, it is not possible to implement a
complete file system driver, for instance listing files in a directory. However
this implementation allows you to open, read and write files on the host
computer.

#### How to mount an ARM semihosting file system

To use the ARM semihosting file system you first have to declare it:

```ada=

with Semihosting.Filesystem; use Semihosting.Filesystem;

package body My_Package is

  Semihosting_FS : aliased SHFS;
  
```

and finally, mount the file system:

```ada=
   if Mount_Volume ("host", Semihosting_FS'Access) /= OK then
      --  Error handling
   end if;
```

You can now use the `File_IO` package to access the host file system. 

For instance, opening `/host/tmp/test.txt` with `File_IO.Open` will actually
open `/tmp/test.txt` on the host machine.

### Native file system

The native file system is used on non embedded platform (Windows, Linux, etc.)
to provide access to the OS file system through the common interface of
`Ada_Drivers_Library`. This is mostly useful for testing purposes.

It is implemented in the package `Semihosting.Filesystem`.

#### How to mount a native file system

To use the native file system you first have to declare it:

```ada=

with Filesystem.Native; use Filesystem.Native;

package body My_Package is

  Native_FS : aliased Native_FS_Driver;
  
```

then use the `Create` function to specify which directory on the host file
system will be the root of the Native file system driver:

```ada=
   if Native_FS.Create ("/home/username") /= OK then
      --  Error handling
   end if;
```

and finally, mount the file system:

```ada=
   if Mount_Volume ("mount_point", FS'Access) /= OK then
      --  Error handling
   end if;
```

You can now use the `File_IO` package to access the native file system.

For instance, opening `/mount_point/test.txt` with `File_IO.Open` will actually
open `/home/username/test.txt` on your machine.

### Writing a new driver

You may want to write your own file system driver because you use a custom
format or if you want add support for a format that is not available in
`Ada_Drivers_Library`.

File system drivers have to implement the `HAL.Filesystem.Filesystem_Driver`
interface.

If your file system is located on a disk drive or any other type of mass
storage device, the driver must use the `HAL.Block_Driver` interface to make
sure that the code can be re-used on different drives. The FAT driver is an
example of that.
