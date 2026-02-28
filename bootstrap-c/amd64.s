.intel_syntax noprefix

.text
    .globl _start, syscall5, syscall6

    _start:
        xor rbp, rbp
        pop rdi
        mov rsi, rsp
        and rsp, -16
        call main
        mov rdi, rax
        mov rax, 60
        syscall
        ret

    syscall5:
        mov rax, rdi
        mov rdi, rsi
        mov rsi, rdx
        mov rdx, rcx
        mov r10, r8
        mov r8, r9
        syscall
        ret

    syscall6:
        mov rax, rdi
        mov rdi, rsi
        mov rsi, rdx
        mov rdx, rcx
        mov r10, r8
        mov r8, r9
        mov r9, [rsp + 8]
        syscall
        ret
