

/* This is a ARM specific version of this file */

/* This script replaces ld's default linker script, providing the
   appropriate memory map and output format. */

SEARCH_DIR(.)
__DYNAMIC  =  0;

_DEFAULT_STACK_SIZE = 2048;

ENTRY(_start);

MEMORY
{
  FLASH (rx) : ORIGIN = 0x0, LENGTH = 0x80000
  HSRAM (rwx) : ORIGIN = 0x20000000, LENGTH = 0x30000
  BKUPRAM (rwx) : ORIGIN = 0x47000000, LENGTH = 0x2000
}

/*
 * Boot memory (.text, .ro_data, interrupt vector): FLASH
 * Main RAM memory (.data, .bss, stacks, interrupt stacks): FLASH
 */

SECTIONS
{

  .text :
  {
    KEEP (*(.vectors))
    *(.text .text.* .gnu.linkonce.t*)
    *(.gnu.warning)
  } > FLASH

  .ARM.extab   : { *(.ARM.extab* .gnu.linkonce.armextab.*) } > FLASH
  PROVIDE_HIDDEN (__exidx_start = .);
  .ARM.exidx   : { *(.ARM.exidx* .gnu.linkonce.armexidx.*) } > FLASH
  PROVIDE_HIDDEN (__exidx_end = .);

  .rodata :
  {
    *(.lit)
    *(.rodata .rodata.* .gnu.linkonce.r*)
    . = ALIGN(0x4);
    __rom_end = .;
  } > FLASH

  __data_load = LOADADDR(.data);
  .data :
  {
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)

    /* Ensure that the end of the data section is always word aligned.
       Initial values are stored in 4-bytes blocks so we must guarantee
       that these blocks do not fall out the section (otherwise they are
       truncated and the initial data for the last block are lost). */

    . = ALIGN(0x4);
    __data_end = .;
  } > HSRAM AT> FLASH
  __data_words = (__data_end - __data_start) >> 2;



  __BKUPRAM_data_load = LOADADDR(.BKUPRAM_data);
  .BKUPRAM_data :
  {
    __BKUPRAM_data_start = .;
    *(.BKUPRAM_data .BKUPRAM_data.*)

    /* Ensure that the end of the data section is always word aligned.
       Initial values are stored in 4-bytes blocks so we must guarantee
       that these blocks do not fall out the section (otherwise they are
       truncated and the initial data for the last block are lost). */

    . = ALIGN(0x4);
    __BKUPRAM_data_end = .;
  } >  BKUPRAM AT> FLASH
  __BKUPRAM_data_words = (__BKUPRAM_data_end - __BKUPRAM_data_start) >> 2;


  .bss (NOLOAD): {
   . = ALIGN(0x8);
   __bss_start = .;

   *(.bss .bss.*)
   *(COMMON)

   . = ALIGN(0x8);    /* Align the stack to 64 bits */
   __bss_end = .;

   __interrupt_stack_start = .;
   *(.interrupt_stacks)
   . = ALIGN(0x8);
   __interrupt_stack_end = .;

   __stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x8);
   __stack_end = .;

   _end = .;
   __heap_start = .;
   __heap_end = ORIGIN(HSRAM) + LENGTH(HSRAM);
  } > HSRAM
  __bss_words = (__bss_end - __bss_start) >> 2;


  .BKUPRAM_bss (NOLOAD): {
   . = ALIGN(0x8);
   __BKUPRAM_bss_start = .;

   *(.BKUPRAM_bss .BKUPRAM_bss.*)
   __BKUPRAM_bss_end = .;
   __BKUPRAM_heap_start = .;
   __BKUPRAM_heap_end = ORIGIN(BKUPRAM) + LENGTH(BKUPRAM);
  } > BKUPRAM
  __BKUPRAM_bss_words = (__BKUPRAM_bss_end - __BKUPRAM_bss_start) >> 2;


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