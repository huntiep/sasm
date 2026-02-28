u64 HEAP_START = 0;
u64 HEAP_END = 0;

void alloc_init() {
    u64 tmp = brk(0);
    HEAP_START = tmp;
    HEAP_END = tmp;
}

#define MALLOC_ALIGN 7
#define MALLOC_SIZE_T_SIZE 8
u64 malloc(u64 size) {
    if (size == 0) {
        return 0;
    }

    size = size + MALLOC_SIZE_T_SIZE + MALLOC_ALIGN;
    size = size & -8;
    u64 start = HEAP_START;
    u64 end = HEAP_END;

    while (start < end) {
        u64 hdr = ((u64*)start)[0];
        if ((hdr & 1) != 0) {
            start = start + (hdr ^ 1);
        } else if (size > hdr) {
            if (start + hdr >= end) {
                break;
            }
            u64 next_hdr = ((u64*)(start + hdr))[0];
            if (next_hdr & 1 != 0) {
                start = start + hdr;
            } else {
                ((u64*)start)[0] = hdr + next_hdr;
            }
        } else {
           if (8 >= hdr - start) {
               goto done;
           } else {
               ((u64*)(start+size))[0] = hdr - start;
               goto done;
           }
        }
    }

    HEAP_END = brk(start + size);
    start -= size;

done:
    ((u64*)start)[0] = size | 1;
    return start + MALLOC_SIZE_T_SIZE;
}

u64 calloc(u64 size) {
    u64 ptr = malloc(size);
    size = (size + MALLOC_ALIGN) & -8;
    u64* ptr_tmp = (u64*)ptr;
    u64* ptr_end = (u64*)(ptr + size);
    while (ptr_tmp != ptr_end) {
        ptr_tmp[0] = 0;
        ptr_tmp++;
    }

    return ptr;
}

void free(u64 ptr) {
    u64 size = ((u64*)(ptr - MALLOC_SIZE_T_SIZE))[0];
    size = size & -2;
    ((u64*)(ptr - MALLOC_SIZE_T_SIZE))[0] = size;
}

u64 realloc(u64 old_ptr, u64 size) {
    u64 old_size = ((u64*)(old_ptr - MALLOC_SIZE_T_SIZE))[0];
    old_size = old_size - 9;
    size = (size + MALLOC_ALIGN) & -8;

    if (old_size >= size) {
        return old_ptr;
    }

    if ((old_ptr + old_size) == HEAP_END) {
        HEAP_END = brk(HEAP_END + size - old_size);
        ((u64*)(old_ptr - MALLOC_SIZE_T_SIZE))[0] = (size + MALLOC_SIZE_T_SIZE) | 1;
        return old_ptr;
    }
    u64 new_ptr = malloc(size);
    u64* new_ptr_tmp = (u64*)new_ptr;
    u64* old_ptr_tmp = (u64*)old_ptr;
    while (old_size != 0) {
        new_ptr_tmp[0] = old_ptr_tmp[0];
        new_ptr_tmp++;
        old_ptr_tmp++;
        old_size -= 8;
    }
    free(old_ptr);
    return new_ptr;
}
