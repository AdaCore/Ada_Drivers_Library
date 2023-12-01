# Flexible static memory controller (FSMC)

This component of STM32F40x/41x processors allows for the management
of static memory, flash memory, and PC Cards. Unlike the FMC in more
advanced MCU models, dynamic memory is not supported. Each type of
memory has its address bank. The controller offers flexible settings
for various operating modes. For instance, extended modes can have
different delays set for read and write operations. It is recommended
to refer to the STM32 Reference Manual (RM0090) for a detailed description
of possible modes and corresponding settings.

* [Datasheet](https://www.st.com/resource/en/reference_manual/dm00031020-stm32f405-415-stm32f407-417-stm32f427-437-and-stm32f429-439-advanced-arm-based-32-bit-mcus-stmicroelectronics.pdf) - STM32 Reference Manual (RM0090), FSMC chapter.

* [Configuration example](../../../../../boards/stm32_f4ve/src/display_ili9341.adb) - display_ili9341.adb:

## Example

Configuring Bank_1 for ILI9341 LCD controller:

* half word (16 bit access)
* with write enabled
* distinct read and write timings

```ada
STM32.FSMC.Configure
  (Bank_1 =>
     (1 =>  --  TFT is connected to sub-bank 1
        (Is_Set => True,
         Value  =>
           (Write_Enable  => True,
            Bus_Width     => STM32.FSMC.Half_Word,
            Memory_Type   => STM32.FSMC.SRAM,
            Bus_Turn      => 15,  --  90ns
            Data_Setup    => 57, --  342ns
            Address_Setup => 0,
            Extended      =>
              (STM32.FSMC.Mode_A,
               Write_Bus_Turn      => 3,  --  18ns
               Write_Data_Setup    => 2,  --  12ns
               Write_Address_Setup => 0),
            others        => <>)),
      others => <>));
```