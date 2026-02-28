#!/usr/bin/env sh
#gcc -s -O2 -static -nostdlib -fno-unwind-tables -fno-asynchronous-unwind-tables -fdata-sections -Wl,--gc-sections -Wa,--noexecstack -fno-builtin -Wall -Werror sasm.c amd64.s -o sasm-c
gcc -s -O2 -static -nostdlib -fno-unwind-tables -fno-asynchronous-unwind-tables -fdata-sections -Wl,--gc-sections -Wa,--noexecstack -fno-builtin sasm.c amd64.s -o sasm-c
