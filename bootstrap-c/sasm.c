typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;
typedef char i8;
typedef short i16;
typedef int i32;
typedef long i64;

#define NULL 0

#include "syscalls.c"
#include "alloc.c"
#include "util.c"
#include "sasm_strings.c"
#include "symbol_table.c"

typedef struct ArrayBufu32_struct {
    u32* data;
    u64 len;
    u64 cap;
} ArrayBufu32;

typedef struct ArrayBufu8_struct {
    u8* data;
    u64 len;
    u64 cap;
} ArrayBufu8;

typedef struct Rewrite_struct {
    u64 program_pos;
    // (data_pos << 1) | rodata?
    u64 data_pos;
} Rewrite;

typedef struct RewriteArray_struct {
    Rewrite* data;
    u64 len;
    u64 cap;
} RewriteArray;

u64 read_file(char* filename, u8** input, u8** input_end);
u64 elf(ArrayBufu32 program, ArrayBufu8 data, ArrayBufu8 rodata, RewriteArray rewrites, u64 out);

void print_usage(u8* prog) {
    u8* usage1 = "USAGE: ";
    u64 usage1_len = sizeof(usage1) - 1;
    write(STDOUT, usage1, usage1_len);
    u64 prog_len = 0;
    while (prog[prog_len] != 0) {
        prog_len++;
    }
    write(STDOUT, prog, prog_len);
    u8* usage2 = " <INPUT> [OUTPUT]\n    -d: Include debug info.\n";
    u64 usage2_len = sizeof(usage2) - 1;
    write(STDOUT, usage2, usage2_len);
}

void args_flag_err(u8* flag) {
    u8* flag_err1 = "Unknown flag `";
    u64 flag_err1_len = sizeof(flag_err1) - 1;
    u8* flag_err2 = "`.\n";
    u64 flag_err2_len = sizeof(flag_err2) - 1;
    write(STDOUT, flag_err1, flag_err1_len);
    u64 flag_len = 0;
    while (flag[flag_len] != 0) {
        flag_len++;
    }
    write(STDOUT, flag, flag_len);
    write(STDOUT, flag_err2, flag_err2_len);
}

u64 main(u64 argc, u8* argv[]) {
    alloc_init();
    argc = argc - 1;
    u8* prog = argv[0];
    argv++;
    u8* input_file = NULL;
    u8* output_file = NULL;
    u8 debug = 0;
    u8 usage = 0;

    while (argc != 0) {
        argc = argc - 1;
        u8* tmp = argv[0];
        argv++;
        if (tmp[0] == '-') {
            if (tmp[1] == 'V' && tmp[2] == NULL) {
                u8* version_str = "S-Assembler v2.0 - The Arcadian State (C version)\nWritten by Hunter Praska\n";
                u64 version_str_len = sizeof(version_str) - 1;
                write(STDOUT, version_str, version_str_len);
                return 0;
            } else if (tmp[1] == 'd' && tmp[2] == NULL) {
                debug = 1;
            } else {
                args_flag_err(tmp);
                usage = 1;
                continue;
            }
        } else if (input_file != NULL) {
            if (output_file != NULL) {
                u8* args_err = "Too many arguments.\n";
                u64 args_err_len = sizeof(args_err) - 1;
                write(STDOUT, args_err, args_err_len);
                return 1;
            } else {
                output_file = tmp;
            }
        } else {
            input_file = tmp;
        }
    }

    if (input_file == NULL) {
        print_usage(prog);
        return 1;
    }

    if (output_file == NULL) {
        output_file = "bin.elf\0";
    }

    if (usage) {
        print_usage(prog);
        write(STDOUT, "\n", 1);
    }

    //symbol_table_init();
    u8* input = NULL;
    u8* input_end = NULL;
    u64 err = read_file(input_file, &input, &input_end);
    if (err != 0) {
        u8* msg = "Error: ";
        u64 msg_len = sizeof(msg) - 1;
        write(STDERR, msg, msg_len);
        write(STDERR, input+1, ((u64)input_end) - 1);
        free((u64)input);
        msg = ".\n";
        msg_len = sizeof(msg) - 1;
        write(STDERR, msg, msg_len);
        return 1;
    }

    u64 out = openat(AT_FDCWD, output_file, O_WRONLY|O_CREAT|O_TRUNC, 0777);
    if (out < 0) {
        return 2;
    }
    //u64 err = elf(out);
    close(out);
    // return err;
    return 0;
}

u64 read_file(char* filename, u8** input, u8** input_end) {
    u8* err = NULL;
    u64 err_len = 0;

    i64 file = openat(AT_FDCWD, filename, O_RDONLY|O_DIR, 0);
    if (file >= 0) {
        err = "\tExpected file, got directory `";
        err_len = sizeof(err) - 1;
        close(file);
        goto error_format;
    }
    file = openat(AT_FDCWD, filename, O_RDONLY, 0);
    if (file == ENOENT) {
        err = "\tNo such file `";
        err_len = sizeof(err) - 1;
        goto error_format;
    } else if (file < 0) {
        err = "\tUnable to open file `";
        err_len = sizeof(err) - 1;
        goto error_format;
    }

    u64 end = lseek(file, 0, SEEK_END);
    lseek(file, 0, SEEK_SET);
    *input = mmap(NULL, end, PROT_RW, MAP_PRIVATE, file, 0);
    if (0 >= *input) {
        err = "\tError reading file `";
        err_len = sizeof(err) - 1;
        goto error_format;
    }
    close(file);
    *input_end = *input + end;

    return 0;

error_format:
    u64 file_len = 0;
    while (filename[file_len] != 0) {
        file_len++;
    }

    u8* msg = (u8*)malloc(file_len + err_len + 1);
    strdup(msg, err, err_len);
    strdup(msg+err_len, filename, file_len);
    msg[err_len+file_len] = '`';


    *input = msg;
    *input_end = (u8*)(err_len + file_len + 1);
    return 1;
}

#define EI_NIDENT 16
typedef u64 Elf64Addr;
typedef u64 Elf64Off;
typedef u16 Elf64Half;
typedef u32 Elf64Word;
typedef u64 Elf64Xword;

#define ENTRY_LOCATION 0x400
#define DATA_LOCATION 0x600
#define RODATA_LOCATION 0x800
//#define program_offset 176
#define RISCV 0xf3

typedef struct ehdr_struct {
    u8 e_ident[EI_NIDENT];
    Elf64Half e_type;
    Elf64Half e_machine;
    Elf64Word e_version;
    Elf64Addr e_entry;
    Elf64Off e_phoff;
    Elf64Off e_shoff;
    Elf64Word e_flags;
    Elf64Half e_ehsize;
    Elf64Half e_phentsize;
    Elf64Half e_phnum;
    Elf64Half e_shentsize;
    Elf64Half e_shnum;
    Elf64Half e_shstrndx;
} Ehdr;

typedef struct phdr_struct {
    Elf64Word p_type;
    Elf64Word p_flags;
    Elf64Off p_offset;
    Elf64Addr p_vaddr;
    Elf64Addr p_paddr;
    Elf64Xword p_filesz;
    Elf64Xword p_memsz;
    Elf64Xword p_align;
} Phdr;

typedef struct shdr_struct {
    Elf64Word sh_name;
    Elf64Word sh_type;
    Elf64Xword sh_flags;
    Elf64Addr sh_addr;
    Elf64Off sh_offset;
    Elf64Xword sh_size;
    Elf64Word sh_link;
    Elf64Word sh_info;
    Elf64Xword sh_addralign;
    Elf64Xword sh_entsize;
} Shdr;

u64 elf(ArrayBufu32 program, ArrayBufu8 data, ArrayBufu8 rodata, RewriteArray rewrites, u64 out) {
    u8 err_flag = 0;
    if (program.len * 4 > 0x200000) {
        u8* err = "Code has exceeded 2MB limit.\n";
        u64 err_len = sizeof(err) - 1;
        write(STDOUT, err, err_len);
        err_flag = 1;
    }
    if (data.len > 0x200000) {
        u8* err = "Variable data (`defvar`) has exceeded 2MB limit.\n";
        u64 err_len = sizeof(err) - 1;
        write(STDOUT, err, err_len);
        err_flag = 1;
    }
    if (err_flag) {
        return 1;
    }
    u8 phdrs = 1;
    if (data.len > 0) {
        phdrs++;
    }
    if (rodata.len > 0) {
        phdrs++;
    }
    u64 program_offset = (phdrs * sizeof(Phdr)) + sizeof(Ehdr);
    Ehdr ehdr = {
        .e_ident = {127, 69, 76, 70, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        .e_type = 2,
        .e_machine = RISCV,
        .e_version = 1,
        .e_entry = ENTRY_LOCATION + program_offset,
        .e_phoff = sizeof(Ehdr),
        .e_shoff = 0,
        .e_flags = 0,
        .e_ehsize = sizeof(Ehdr),
        .e_phentsize = sizeof(Phdr),
        .e_phnum = phdrs,
        .e_shentsize = sizeof(Shdr),
        .e_shnum = 0,
        .e_shstrndx = 0,
    };
    write(out, (u8*)&ehdr, sizeof(Ehdr));

    if (program.len % 2 != 0) {
        program.data[program.len] = 0;
        program.len++;
    }

    u64 data_offset = program_offset + (program.len * 4);
    Phdr phdr = {
        .p_type = 1,
        .p_flags = 5,
        .p_offset = 0,
        .p_vaddr = ENTRY_LOCATION,
        .p_paddr = ENTRY_LOCATION,
        .p_filesz = data_offset,
        .p_memsz = data_offset,
        .p_align = 4096,
    };
    write(out, (u8*)&phdr, sizeof(Phdr));

    while (data.len % 8) {
        data.data[data.len] = 0;
        data.len++;
    }

    if (data.len != 0) {
        phdr.p_flags = 6;
        phdr.p_offset = data_offset;
        phdr.p_vaddr = DATA_LOCATION + data_offset;
        phdr.p_paddr = DATA_LOCATION + data_offset;
        phdr.p_filesz = data.len;
        phdr.p_memsz = data.len;
        write(out, (u8*)&phdr, sizeof(Phdr));
    }

    u64 rodata_offset = data_offset + data.len;
    if (rodata.len != 0) {
        phdr.p_flags = 4;
        phdr.p_offset = rodata_offset;
        phdr.p_vaddr = RODATA_LOCATION + rodata_offset;
        phdr.p_paddr = RODATA_LOCATION + rodata_offset;
        phdr.p_filesz = rodata.len;
        phdr.p_memsz = rodata.len;
        write(out, (u8*)&phdr, sizeof(Phdr));
    }

    u64 data_pos = DATA_LOCATION + data_offset;
    u64 rodata_pos = RODATA_LOCATION + rodata_offset;
    if (rewrites.len != 0) {
        u64 len = 0;
        while (len < rewrites.len) {
            u64 rodatap = rewrites.data[len].data_pos & 1;
            u64 data_offset = rewrites.data[len].data_pos >> 1;
            u64 offset = data_pos + data_offset;
            if (rodatap) {
                offset = rodata_pos + data_offset;
            }
            u32 imm20 = (offset + 0x800) >> 12;
            u32 imm12 = offset & 0xfff;

            u32 lui = program.data[rewrites.data[len].program_pos];
            lui = lui | (imm20 << 12);
            program.data[rewrites.data[len].program_pos] = lui;

            u32 addi = program.data[rewrites.data[len].program_pos+1];
            addi = addi | (imm12 << 20);
            program.data[rewrites.data[len].program_pos+1] = addi;

            len++;
        }

    }

    write(out, (u8*)program.data, program.len * 4);
    write(out, data.data, data.len);
    write(out, rodata.data, rodata.len);

    return 0;
}
