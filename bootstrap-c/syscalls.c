#define STDIN 0
#define STDOUT 1
#define STDERR 2

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define AT_FDCWD -100
#define O_RDONLY 0
#define O_WRONLY 1
#define O_CREAT 64
#define O_TRUNC 512
#define O_DIR 65536

#define ENOTDIR 20
#define ENOENT 2

#define PROT_READ 1
#define PROT_WRITE 2
#define PROT_RW 3
#define MAP_PRIVATE 2

#define SYS_WRITE 1
#define SYS_CLOSE 3
#define SYS_LSEEK 8
#define SYS_MMAP 9
#define SYS_MUNMAP 11
#define SYS_BRK 12
#define SYS_EXIT 60
#define SYS_GETDENTS64 217 
#define SYS_OPENAT 257
#define SYS_STATX 332 

u64 syscall5(u64 code, u64 a1, u64 a2, u64 a3, u64 a4, u64 a5);
u64 syscall6(u64 code, u64 a1, u64 a2, u64 a3, u64 a4, u64 a5, u64 a6);

u64 exit(u64 code) {
    return syscall5(SYS_EXIT, code, 0, 0, 0, 0);
}

u64 brk(u64 new_brk) {
    return syscall5(SYS_BRK, new_brk, 0, 0, 0, 0);
}

u64 write(u64 fd, u8* string, u64 len) {
    return syscall5(SYS_WRITE, fd, (u64)string, len, 0, 0);
}

i64 openat(u64 dir, u8* filename, u64 flags, u64 addr) {
    return syscall5(SYS_OPENAT, dir, (u64)filename, flags, addr, 0);
}

i64 lseek(u64 fd, u64 tmp, u64 pos) {
    return syscall5(SYS_LSEEK, fd, tmp, pos, 0, 0);
}

u8* mmap(u64 pos, u64 size, u64 prot, u64 flags, u64 fd, u64 tmp) {
    return (u8*)syscall6(SYS_MMAP, pos, size, prot, flags, fd, tmp);
}

u64 munmap(u8* ptr, u64 size) {
    return syscall6(SYS_MUNMAP, (u64)ptr, size, 0, 0, 0, 0);
}

u64 close(u64 fd) {
    return syscall5(SYS_CLOSE, fd, 0, 0, 0, 0);
}
