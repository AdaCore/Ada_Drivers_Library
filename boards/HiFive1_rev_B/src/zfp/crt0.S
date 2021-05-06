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

        /**********/
        /* _start */
        /**********/

        .section .start
        .globl _start
        .type _start,@function

_start:
.option push
.option norelax
        la gp, __global_pointer$
.option pop
        la sp, __stack_end


        /* Load data section */
        .type _startup_copy_data,@function
_startup_copy_data:
        la a0, __data_load
        la a1, __data_start
        la a2, __data_end
        bgeu a1, a2, 2f
1:
        lw t0, (a0)
        sw t0, (a1)
        addi a0, a0, 4
        addi a1, a1, 4
        bltu a1, a2, 1b
2:
        .size _startup_copy_data, . - _startup_copy_data


        /* Clear bss section */
        .type _startup_clear_bss,@function
_startup_clear_bss:
        la a0, __bss_start
        la a1, __bss_end
        bgeu a0, a1, 2f
1:
        sw zero, (a0)
        addi a0, a0, 4
        bltu a0, a1, 1b
2:
        .size _startup_clear_bss, . - _startup_clear_bss


        /* Call static constructors */
.weak __libc_init_array
        la t0, __libc_init_array
        beq t0, zero, .skip_libc_init
        jalr t0
.skip_libc_init:

        /* Call main, with argc, argv  */
        la a0, 1
        la a1, argv
        call main

        /* Save main's return value */
        mv t0, a0

        /* static destructors */
.weak __libc_fini_array
        la t0, __libc_fini_array
        beq t0, zero, .skip_libc_fini
        jalr t0
.skip_libc_fini:

        /* Restore main's return value */
        mv a0, t0

        call __gnat_exit
2:      j 2b




        .globl __gnat_exit
        .type __gnat_exit,@function
        .globl _abort
        .type abort,@function
abort:
__gnat_exit:
        /* Write to the SiFive Test device on QEMU to shutdown */
        li t0, 0x5555
        li t1, 0x100000
        sw t0, (t1)

        j __gnat_exit

        /* Weak alias _exit to __gnat_exit */
        .weak      _exit
        .set _exit,__gnat_exit


        .globl putchar
        .type putchar,@function
putchar:
        li t1, 268513280
        li t1, 0x10013000
        li t2, 0x80000000
.loop:
        lw t3, 0(t1)
        and t3, t3, t2
        bnez t3, .loop

        sb a0, 0(t1)
        ret
