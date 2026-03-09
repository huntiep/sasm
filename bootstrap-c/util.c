void strdup(u8* to, u8* from, u64 len) {
    if (len == 0) {
        return;
    }
    u8* end = from + len;
    while (from != end) {
        *to = *from;
        to++;
        from++;
    }
}

u64 str_eqp(u8* left, u64 left_len, u8* right, u64 right_len) {
    if (left_len != right_len) {
        return 0;
    }
    for (u64 i = 0; i < left_len; i++) {
        if (left[i] != right[i]) {
            return 0;
        }
    }
    return 1;
}

typedef struct SM16Entry_struct {
    u32 symbol;
    u64 value;
} SM16Entry;

typedef struct SymbolMap16_struct {
    SM16Entry* data;
    u64 entries;
    u64 cap;
} SymbolMap16;

u64 SM16_entry(SymbolMap16* self, u32 key) {
    u64 idx = key & (self->cap - 1);

    while(1) {
        if (idx == self->cap) {
            idx = 0;
        }
        if (self->data[idx].symbol == NULL) {
            return idx;
        } else if (self->data[idx].symbol == key) {
            return idx;
        }
        idx++;
    }
}

SM16Entry SM16_get(SymbolMap16* self, u32 key) {
    u64 idx = SM16_entry(self, key);
    SM16Entry e = {};
    if (self->data[idx].symbol != key) {
        return e;
    }
    e.symbol = 1;
    e.value = self->data[idx].value;
    return e;
}


u64 SM16_insert(SymbolMap16* self, u32 key, u64 value, u64 overwritep) {
    if (self->entries * 2 >= self->cap) {
        // resize
        SM16Entry* newmap = (SM16Entry*)calloc(self->cap * 2 * sizeof(SM16Entry));
        for (u64 i = 0; i < self->cap; i++) {
            if (self->data[i].symbol != 0) {
                u64 idx = self->data[i].symbol & ((self->cap * 2) - 1);
                while (newmap[idx].symbol != 0) {
                    idx++;
                    if (idx == (self->cap * 2)) {
                        idx = 0;
                    }
                }
                newmap[idx].symbol = self->data[i].symbol;
                newmap[idx].value = self->data[i].value;
            }
        }

        self->cap = self->cap * 2;
        free((u64)self->data);
        self->data = newmap;
    }

    u64 idx = SM16_entry(self, key);
    if (self->data[idx].symbol == 0) {
        self->data[idx].symbol = key;
        self->data[idx].value = value;
        self->entries++;
        return 0;
    } else if (overwritep) {
        // exists
        self->data[idx].value = value;
    }
    return 1;
}

typedef struct SM24_struct {
    u32 symbol;
    u64 value1;
    u64 value2;
} SM24Entry;

typedef struct SymbolMap24_struct {
    SM24Entry* data;
    u64 entries;
    u64 cap;
} SymbolMap24;

u64 SM24_entry(SymbolMap24* self, u32 key) {
    u64 idx = key & (self->cap - 1);

    while(1) {
        if (idx == self->cap) {
            idx = 0;
        }
        if (self->data[idx].symbol == NULL) {
            return idx;
        } else if (self->data[idx].symbol == key) {
            return idx;
        }
        idx++;
    }
}

SM24Entry SM24_get(SymbolMap24* self, u32 key) {
    u64 idx = SM24_entry(self, key);
    SM24Entry e = {};
    if (self->data[idx].symbol != key) {
        return e;
    }
    e.symbol = 1;
    e.value1 = self->data[idx].value1;
    e.value2 = self->data[idx].value2;
    return e;
}

u64 SM24_insert(SymbolMap24* self, u32 key, u64 value1, u64 value2, u64 overwritep) {
    if (self->entries * 2 >= self->cap) {
        // resize
        SM24Entry* newmap = (SM24Entry*)calloc(self->cap * 2 * sizeof(SM24Entry));
        for (u64 i = 0; i < self->cap; i++) {
            if (self->data[i].symbol != 0) {
                u64 idx = self->data[i].symbol & ((self->cap * 2) - 1);
                while (newmap[idx].symbol != 0) {
                    idx++;
                    if (idx == (self->cap * 2)) {
                        idx = 0;
                    }
                }
                newmap[idx].symbol = self->data[i].symbol;
                newmap[idx].value1 = self->data[i].value1;
                newmap[idx].value2 = self->data[i].value2;
            }
        }

        self->cap = self->cap * 2;
        free((u64)self->data);
        self->data = newmap;
    }

    u64 idx = SM24_entry(self, key);
    if (self->data[idx].symbol == 0) {
        self->data[idx].symbol = key;
        self->data[idx].value1 = value1;
        self->data[idx].value2 = value2;
        self->entries++;
        return 0;
    } else if (overwritep) {
        // exists
        self->data[idx].value1 = value1;
        self->data[idx].value2 = value2;
    }
    return 1;
}

void resize_array(u64* arr, u64 elem_size) {
    arr[2] = arr[2] * 2;
    arr[0] = realloc(arr[0], arr[2] * elem_size);
}

u64 format_int(u8* str, u64 num) {
    if (num == 0) {
        str[0] = '0';
        return 1;
    } else if (num < 0) {
        str[0] = '-';
        return 1 + format_int(str + 1, -num);
    }

    u8* tmp = str;
    u64 len = 0;
    while (num != 0) {
        str[len] = (num % 10) + '0';
        num = num / 10;
        len++;
    }
    u64 right = len - 1;
    u64 left = 0;
    while (left < right) {
        u8 tmp = str[left];
        str[left] = str[right];
        str[right] = tmp;
        left++;
        right--;
    }
    return len;
}

