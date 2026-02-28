.text
    .globl _start, syscall5, syscall6

    _start:
        ld x10, x2
        addi x11, x2, 8
        andi x2, -16
        jal x1, main
        addi x17, x0, 93
        ecall

    syscall5:
        add x17, x0, x15
        ecall
        jalr x0, x1

    syscall6:
        add x17, x0, x16
        ecall
        jalr x0, x1
