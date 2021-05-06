	.syntax unified

#if defined(__SOFTFP__)
	.cpu cortex-m0
#else
        /* FPU support means at least cortex-m4 compatibility */
        .cpu cortex-m4
#endif

        .thumb

	.text
	.globl __vectors
	.p2align 9
	.section .vectors,"a"
__vectors:
	/* Cortex-M core interrupts */
	.word   __stack_end              /* stack top address */
	.word   Reset_Handler            /* 1 Reset */
	.word   NMI_Handler              /* 2 NMI. */
	.word   HardFault_Handler        /* 3 Hard fault. */
	.word   MemoryManagement_Handler /* 4 Mem manage. */
	.word   BusFault_Handler         /* 5 Bus fault. */
	.word   UsageFault_Handler       /* 6 Usage fault. */
	.word   Reserved_Handler         /* 7 reserved. */
	.word   Reserved_Handler         /* 8 reserved. */
	.word   Reserved_Handler         /* 9 reserved. */
	.word   Reserved_Handler         /* 10 reserved. */
	.word   SVC_Handler              /* 11 SVCall. */
	.word   DebugMon_Handler         /* 12 Breakpoint. */
	.word   Reserved_Handler         /* 13 reserved. */
	.word   PendSV_Handler           /* 14 PendSV. */
	.word   SysTick_Handler          /* 15 Systick. */
	/* MCU interrupts */
        .word __adl_irq_handler /* 0 */
        .word __adl_irq_handler /* 1 */
        .word __adl_irq_handler /* 2 */
        .word __adl_irq_handler /* 3 */
        .word __adl_irq_handler /* 4 */
        .word __adl_irq_handler /* 5 */
        .word __adl_irq_handler /* 6 */
        .word __adl_irq_handler /* 7 */
        .word __adl_irq_handler /* 8 */
        .word __adl_irq_handler /* 9 */
        .word __adl_irq_handler /* 10 */
        .word __adl_irq_handler /* 11 */
        .word __adl_irq_handler /* 12 */
        .word __adl_irq_handler /* 13 */
        .word __adl_irq_handler /* 14 */
        .word __adl_irq_handler /* 15 */
        .word __adl_irq_handler /* 16 */
        .word __adl_irq_handler /* 17 */
        .word __adl_irq_handler /* 18 */
        .word __adl_irq_handler /* 19 */
        .word __adl_irq_handler /* 20 */
        .word __adl_irq_handler /* 21 */
        .word __adl_irq_handler /* 22 */
        .word __adl_irq_handler /* 23 */
        .word __adl_irq_handler /* 24 */
        .word __adl_irq_handler /* 25 */
        .word __adl_irq_handler /* 26 */
        .word __adl_irq_handler /* 27 */
        .word __adl_irq_handler /* 28 */
        .word __adl_irq_handler /* 29 */
        .word __adl_irq_handler /* 30 */
        .word __adl_irq_handler /* 31 */
        .word __adl_irq_handler /* 32 */
        .word __adl_irq_handler /* 33 */
        .word __adl_irq_handler /* 34 */
        .word __adl_irq_handler /* 35 */
        .word __adl_irq_handler /* 36 */
        .word __adl_irq_handler /* 37 */
        .word __adl_irq_handler /* 38 */
        .word __adl_irq_handler /* 39 */
        .word __adl_irq_handler /* 40 */
        .word __adl_irq_handler /* 41 */
        .word __adl_irq_handler /* 42 */
        .word __adl_irq_handler /* 43 */
        .word __adl_irq_handler /* 44 */
        .word __adl_irq_handler /* 45 */
        .word __adl_irq_handler /* 46 */
        .word __adl_irq_handler /* 47 */
        .word __adl_irq_handler /* 48 */
        .word __adl_irq_handler /* 49 */
        .word __adl_irq_handler /* 50 */
        .word __adl_irq_handler /* 51 */
        .word __adl_irq_handler /* 52 */
        .word __adl_irq_handler /* 53 */
        .word __adl_irq_handler /* 54 */
        .word __adl_irq_handler /* 55 */
        .word __adl_irq_handler /* 56 */
        .word __adl_irq_handler /* 57 */
        .word __adl_irq_handler /* 58 */
        .word __adl_irq_handler /* 59 */
        .word __adl_irq_handler /* 60 */
        .word __adl_irq_handler /* 61 */
        .word __adl_irq_handler /* 62 */
        .word __adl_irq_handler /* 63 */
        .word __adl_irq_handler /* 64 */
        .word __adl_irq_handler /* 65 */
        .word __adl_irq_handler /* 66 */
        .word __adl_irq_handler /* 67 */
        .word __adl_irq_handler /* 68 */
        .word __adl_irq_handler /* 69 */
        .word __adl_irq_handler /* 70 */
        .word __adl_irq_handler /* 71 */
        .word __adl_irq_handler /* 72 */
        .word __adl_irq_handler /* 73 */
        .word __adl_irq_handler /* 74 */
        .word __adl_irq_handler /* 75 */
        .word __adl_irq_handler /* 76 */
        .word __adl_irq_handler /* 77 */
        .word __adl_irq_handler /* 78 */
        .word __adl_irq_handler /* 79 */
        .word __adl_irq_handler /* 80 */
        .word __adl_irq_handler /* 81 */
        .word __adl_irq_handler /* 82 */
        .word __adl_irq_handler /* 83 */
        .word __adl_irq_handler /* 84 */
        .word __adl_irq_handler /* 85 */
        .word __adl_irq_handler /* 86 */
        .word __adl_irq_handler /* 87 */
        .word __adl_irq_handler /* 88 */
        .word __adl_irq_handler /* 89 */
        .word __adl_irq_handler /* 90 */
        .word __adl_irq_handler /* 91 */
        .word __adl_irq_handler /* 92 */
        .word __adl_irq_handler /* 93 */
        .word __adl_irq_handler /* 94 */
        .word __adl_irq_handler /* 95 */
        .word __adl_irq_handler /* 96 */
        .word __adl_irq_handler /* 97 */
        .word __adl_irq_handler /* 98 */
        .word __adl_irq_handler /* 99 */
        .word __adl_irq_handler /* 100 */
        .word __adl_irq_handler /* 101 */
        .word __adl_irq_handler /* 102 */
        .word __adl_irq_handler /* 103 */
        .word __adl_irq_handler /* 104 */
        .word __adl_irq_handler /* 105 */
        .word __adl_irq_handler /* 106 */
        .word __adl_irq_handler /* 107 */
        .word __adl_irq_handler /* 108 */
        .word __adl_irq_handler /* 109 */
        .word __adl_irq_handler /* 110 */
        .word __adl_irq_handler /* 111 */
        .word __adl_irq_handler /* 112 */
        .word __adl_irq_handler /* 113 */
        .word __adl_irq_handler /* 114 */
        .word __adl_irq_handler /* 115 */
        .word __adl_irq_handler /* 116 */
        .word __adl_irq_handler /* 117 */
        .word __adl_irq_handler /* 118 */
        .word __adl_irq_handler /* 119 */
        .word __adl_irq_handler /* 120 */
        .word __adl_irq_handler /* 121 */
        .word __adl_irq_handler /* 122 */
        .word __adl_irq_handler /* 123 */
        .word __adl_irq_handler /* 124 */
        .word __adl_irq_handler /* 125 */
        .word __adl_irq_handler /* 126 */
        .word __adl_irq_handler /* 127 */

   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__common_int_handler

	.text

   .weak      __unknown_interrupt_handler
   .thumb_set __unknown_interrupt_handler,__common_int_handler

.macro weak_handler name
	.thumb_func
.weak \name
.type \name, %function
\name:
0:	b 0b
	.size \name, . - \name
.endm

weak_handler NMI_Handler
weak_handler HardFault_Handler
weak_handler MemoryManagement_Handler
weak_handler BusFault_Handler
weak_handler UsageFault_Handler
weak_handler Reserved_Handler
weak_handler SVC_Handler
weak_handler DebugMon_Handler
weak_handler PendSV_Handler
weak_handler SysTick_Handler
weak_handler __common_int_handler

        /*********/
        /* .data */
        /*********/
        .section .data.argv
argv_str:
        .ascii  "main\0"

        .align 4
argv:
        .word argv_str
        .word 0

        /*****************/
        /* Reset_Handler */
        /*****************/

	.text
	.thumb_func
	.globl Reset_Handler

Reset_Handler:

	/* Set the stack pointer */
	ldr	r1,=__stack_end
	mov     sp, r1

	/* Copy .data */
	.thumb_func
_startup_copy_data:
	ldr	r0,=__data_start
	ldr	r1,=__data_words
	ldr	r2,=__data_load
	cmp     r1,#0
	beq     1f
0:	ldr	r4,[r2]
	str	r4,[r0]
        adds    r2,#4
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_copy_data, . - _startup_copy_data

	/* Clear .bss */
	.thumb_func
_startup_clear_bss:
	ldr	r0,=__bss_start
	ldr	r1,=__bss_words
	movs	r2,#0
	cmp     r1,#0
	beq     1f
0:	str	r2,[r0]
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_clear_bss, . - _startup_clear_bss

#if !defined(__SOFTFP__)
        /**************/
        /* Enable FPU */
        /**************/

        movw     r0,#0xED88
        movt     r0,#0xE000
        ldr      r1,[r0]
        orr      r1,r1,#(0xF << 20)
        str      r1,[r0]

        dsb
        isb
#endif

        /* Call static constructors */
.weak __libc_init_array
        ldr     r5,=__libc_init_array
        cmp     r5,#0
        beq     .skip_libc_init
        blx     r5
.skip_libc_init:

        /* Call main, with argc, argv */
        movs    r0,#1
        ldr	r1,=argv
        bl	main

        /* Save main's return value */
        mov r4, r0

        /* static destructors */
.weak __libc_fini_array
        ldr     r5,=__libc_fini_array
        cmp     r5,#0
        beq     .skip_libc_fini
        blx     r5
.skip_libc_fini:

        /* Restore main's return value */
        mov r0, r4

        bl	_exit
        bl	Reset_Handler
	.size Reserved_Handler, . - Reset_Handler