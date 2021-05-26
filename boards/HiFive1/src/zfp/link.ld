
/* This is a RISC-V specific version of this file */

_DEFAULT_STACK_SIZE = 0x1000;

OUTPUT_ARCH("riscv")

ENTRY(_start);

MEMORY
{
  board_flash (rx) : ORIGIN = 0x20400000, LENGTH = 0x20000000
  RAM (rwx) : ORIGIN = 0x80000000, LENGTH = 0x4000
}

SECTIONS
{
  /***************/
  /* board_flash */
  /***************/
  .text :
  {
    KEEP (*(SORT_NONE(.start)))
    *(.text .text.* .gnu.linkonce.t*)
    *(.gnu.warning)
  } >  board_flash

  .eh_frame_hdr :
  {
      *(.eh_frame_hdr)
  } > board_flash

  .eh_frame :
  {
      __EH_FRAME__ = .;
      KEEP(*(.eh_frame))
      LONG(0);
  } > board_flash

  .gcc_except_table :
  {
      *(.gcc_except_table .gcc_except_table.*)
  } > board_flash

  .init_array :
  {
    PROVIDE_HIDDEN (__init_array_start = .);
    KEEP (*(SORT(.init_array.*)))
    KEEP (*(.init_array*))
    PROVIDE_HIDDEN (__init_array_end = .);
  } > board_flash

  .fini_array :
  {
    PROVIDE_HIDDEN (__fini_array_start = .);
    KEEP (*(SORT(.fini_array.*)))
    KEEP (*(.fini_array*))
    PROVIDE_HIDDEN (__fini_array_end = .);
  } > board_flash

  .rodata :
  {
    *(.rdata)
    *(.rodata .rodata.* .gnu.linkonce.r*)
    . = ALIGN(0x4);
    __rom_end = .;
  } > board_flash
  /**********************/
  /* End of board_flash */
  /**********************/

  /*******/
  /* RAM */
  /*******/
  .data :
  {
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)
  } > RAM AT > board_flash

  .srodata ALIGN(4) : ALIGN(4) /* Align both virtual and load addresses */
  {
    PROVIDE( __global_pointer$ = . + 0x800 );
    *(.srodata.cst16)
    *(.srodata.cst8)
    *(.srodata.cst4)
    *(.srodata.cst2)
    *(.srodata .srodata.*)
  } > RAM  AT > board_flash

  .sdata ALIGN(4) : ALIGN(4) /* Align both virtual and load addresses */
  {
    *(.sdata .sdata.*)
    *(.gnu.linkonce.s.*)
  } > RAM  AT > board_flash

  __data_end = .;

  /* Size of all data sections (.data, .srodata, .sdata) in number of 32bit
   * words.
   */
  __data_words = (__data_end - __data_start) >> 2;

  /* Base address of all data sections in ROM. The startup code copies these
   * sections from __data_load (in ROM) to __data_start (in RAM).
   */
  __data_load = LOADADDR(.data);

  .bss (NOLOAD): {
    . = ALIGN(0x8);
    __bss_start = .;

    *(.sbss*)
    *(.gnu.linkonce.sb.*)
    *(.bss .bss.*)
    *(.gnu.linkonce.b.*)
    *(COMMON)

    /* Interrupt stacks. Statically allocated in System.BB.Interrupts */
    __interrupt_stack_start = .;
    *(.interrupt_stacks)
    . = ALIGN(0x10); /* Align stack to 128 bits as required in the ABI */
    __interrupt_stack_end = .;

    __stack_start = .;
    . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
    . = ALIGN(0x10); /* Align stack to 128 bits as required in the ABI */
    __stack_end = .;

    _end = .;
    __heap_start = .;
    __heap_end = ORIGIN(RAM) + LENGTH(RAM);
    __bss_end = .;
  } > RAM
  __bss_words = (__bss_end - __bss_start) >> 2;
  /**************/
  /* End of RAM */
  /**************/

  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */
  /* DWARF 1 */
  .debug          0 : { *(.debug) }
  .line           0 : { *(.line) }
  /* GNU DWARF 1 extensions */
  .debug_srcinfo  0 : { *(.debug_srcinfo) }
  .debug_sfnames  0 : { *(.debug_sfnames) }
  /* DWARF 1.1 and DWARF 2 */
  .debug_aranges  0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }
  /* DWARF 2 */
  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }
  /* DWARF 3 */
  .debug_pubtypes 0 : { *(.debug_pubtypes) }
  .debug_ranges   0 : { *(.debug_ranges) }
  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
}