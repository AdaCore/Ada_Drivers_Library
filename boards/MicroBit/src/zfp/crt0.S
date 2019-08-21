	.syntax unified
	.cpu cortex-m0
	.thumb

	.text
	.globl __vectors
	.p2align 9
	.section .vectors,"a"
__vectors:
	/* Cortex-M core interrupts */
	.word   __stack_end          /* stack top address */
	.word   _start               /* 1 Reset */
	.word   fault                /* 2 NMI. */
	.word   fault                /* 3 Hard fault. */
	.word   fault                /* 4 Mem manage. */
	.word   fault                /* 5 Bus fault. */
	.word   fault                /* 6 Usage fault. */
	.word   fault                /* 7 reserved. */
	.word   fault                /* 8 reserved. */
	.word   fault                /* 9 reserved. */
	.word   fault                /* 10 reserved. */
	.word   __gnat_sv_call_trap  /* 11 SVCall. */
	.word   __gnat_bkpt_trap     /* 12 Breakpoint. */
	.word   fault                /* 13 reserved. */
	.word   __gnat_pend_sv_trap  /* 14 PendSV. */
	.word   __gnat_sys_tick_trap /* 15 Systick. */
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

   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap
   .weak      __adl_irq_handler
   .thumb_set __adl_irq_handler,__gnat_irq_trap

	.text

   .weak      __unknown_interrupt_handler
   .thumb_set __unknown_interrupt_handler,__gnat_irq_trap

	.thumb_func
.weak __gnat_irq_trap
.type __gnat_irq_trap, %function
__gnat_irq_trap:
0:	b 0b
	.size __gnat_irq_trap, . - __gnat_irq_trap

	.thumb_func
.weak __gnat_sv_call_trap
.type __gnat_sv_call_trap, %function
__gnat_sv_call_trap:
0:	b 0b
	.size __gnat_sv_call_trap, . - __gnat_sv_call_trap

	.thumb_func
.weak __gnat_pend_sv_trap
.type __gnat_pend_sv_trap, %function
__gnat_pend_sv_trap:
0:	b 0b
	.size __gnat_pend_sv_trap, . - __gnat_pend_sv_trap

	.thumb_func
.weak __gnat_sys_tick_trap
.type __gnat_sys_tick_trap, %function
__gnat_sys_tick_trap:
0:	b 0b
	.size __gnat_sys_tick_trap, . - __gnat_sys_tick_trap

	.thumb_func
fault:	b fault


	.text
	.thumb_func
	.globl _start

_start:

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

	bl	main

	bl	_exit

hang:   b .
