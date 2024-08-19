# S-Assembler

S-Assembler (Sasm) is a self-hosting RiscV assembler.
A rust version is located in the `bootstrap` folder and a self-hosting version
is in the `sasm` directory.

Here is an example:

```sasm
(import (syscalls *))
(defcon msg "hello, world!\n")
;; write(stdout, msg, msg.len)
(addi x10 x0 STDOUT)
(la x11 msg)
(addi x12 x0 (len msg))
(addi x17 x0 SYS_WRITE)
(ecall)

;; exit(0)
(add x10 x0 x0)
(addi x17 x0 SYS_EXIT)
(ecall)
```
