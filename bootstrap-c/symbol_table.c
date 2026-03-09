typedef struct SString_struct {
    u8* data;
    u32 len;
} SymString;

typedef struct SymbolEntry_struct {
    u8* string;
    u32 len;
    u32 symbol;
} SymbolEntry;

typedef struct SymbolTable_struct {
    SymbolEntry* data;
    u64 len;
    u64 cap;
} SymbolTable;

SymbolTable SYMBOLS = { .data = NULL, .len = 0, .cap = 0 };

u32 string_to_symbol(u8* str, u32 str_len);
u32 __string_to_symbol(u8* str, u32 str_len, u32 old_sym);

void SymbolTableInit(void) {
    SYMBOLS.data = (SymbolEntry*)calloc(256 * sizeof(SymbolEntry));
    SYMBOLS.cap = 256;

    string_to_symbol(STRING_Register_X0, sizeof(STRING_Register_X0) - 1);
    string_to_symbol(STRING_Register_X1, sizeof(STRING_Register_X1) - 1);
    string_to_symbol(STRING_Register_X2, sizeof(STRING_Register_X2) - 1);
    string_to_symbol(STRING_Register_X3, sizeof(STRING_Register_X3) - 1);
    string_to_symbol(STRING_Register_X4, sizeof(STRING_Register_X4) - 1);
    string_to_symbol(STRING_Register_X5, sizeof(STRING_Register_X5) - 1);
    string_to_symbol(STRING_Register_X6, sizeof(STRING_Register_X6) - 1);
    string_to_symbol(STRING_Register_X7, sizeof(STRING_Register_X7) - 1);
    string_to_symbol(STRING_Register_X8, sizeof(STRING_Register_X8) - 1);
    string_to_symbol(STRING_Register_X9, sizeof(STRING_Register_X9) - 1);
    string_to_symbol(STRING_Register_X10, sizeof(STRING_Register_X10) - 1);
    string_to_symbol(STRING_Register_X11, sizeof(STRING_Register_X11) - 1);
    string_to_symbol(STRING_Register_X12, sizeof(STRING_Register_X12) - 1);
    string_to_symbol(STRING_Register_X13, sizeof(STRING_Register_X13) - 1);
    string_to_symbol(STRING_Register_X14, sizeof(STRING_Register_X14) - 1);
    string_to_symbol(STRING_Register_X15, sizeof(STRING_Register_X15) - 1);
    string_to_symbol(STRING_Register_X16, sizeof(STRING_Register_X16) - 1);
    string_to_symbol(STRING_Register_X17, sizeof(STRING_Register_X17) - 1);
    string_to_symbol(STRING_Register_X18, sizeof(STRING_Register_X18) - 1);
    string_to_symbol(STRING_Register_X19, sizeof(STRING_Register_X19) - 1);
    string_to_symbol(STRING_Register_X20, sizeof(STRING_Register_X20) - 1);
    string_to_symbol(STRING_Register_X21, sizeof(STRING_Register_X21) - 1);
    string_to_symbol(STRING_Register_X22, sizeof(STRING_Register_X22) - 1);
    string_to_symbol(STRING_Register_X23, sizeof(STRING_Register_X23) - 1);
    string_to_symbol(STRING_Register_X24, sizeof(STRING_Register_X24) - 1);
    string_to_symbol(STRING_Register_X25, sizeof(STRING_Register_X25) - 1);
    string_to_symbol(STRING_Register_X26, sizeof(STRING_Register_X26) - 1);
    string_to_symbol(STRING_Register_X27, sizeof(STRING_Register_X27) - 1);
    string_to_symbol(STRING_Register_X28, sizeof(STRING_Register_X28) - 1);
    string_to_symbol(STRING_Register_X29, sizeof(STRING_Register_X29) - 1);
    string_to_symbol(STRING_Register_X30, sizeof(STRING_Register_X30) - 1);
    string_to_symbol(STRING_Register_X31, sizeof(STRING_Register_X31) - 1);
    string_to_symbol(STRING_Instr_add, sizeof(STRING_Instr_add) - 1);
    string_to_symbol(STRING_Instr_sub, sizeof(STRING_Instr_sub) - 1);
    string_to_symbol(STRING_Instr_xor, sizeof(STRING_Instr_xor) - 1);
    string_to_symbol(STRING_Instr_or, sizeof(STRING_Instr_or) - 1);
    string_to_symbol(STRING_Instr_and, sizeof(STRING_Instr_and) - 1);
    string_to_symbol(STRING_Instr_sll, sizeof(STRING_Instr_sll) - 1);
    string_to_symbol(STRING_Instr_srl, sizeof(STRING_Instr_srl) - 1);
    string_to_symbol(STRING_Instr_sra, sizeof(STRING_Instr_sra) - 1);
    string_to_symbol(STRING_Instr_slt, sizeof(STRING_Instr_slt) - 1);
    string_to_symbol(STRING_Instr_sltu, sizeof(STRING_Instr_sltu) - 1);
    string_to_symbol(STRING_Instr_mul, sizeof(STRING_Instr_mul) - 1);
    string_to_symbol(STRING_Instr_div, sizeof(STRING_Instr_div) - 1);
    string_to_symbol(STRING_Instr_rem, sizeof(STRING_Instr_rem) - 1);
    string_to_symbol(STRING_Instr_addi, sizeof(STRING_Instr_addi) - 1);
    string_to_symbol(STRING_Instr_subi, sizeof(STRING_Instr_subi) - 1);
    string_to_symbol(STRING_Instr_xori, sizeof(STRING_Instr_xori) - 1);
    string_to_symbol(STRING_Instr_ori, sizeof(STRING_Instr_ori) - 1);
    string_to_symbol(STRING_Instr_andi, sizeof(STRING_Instr_andi) - 1);
    string_to_symbol(STRING_Instr_slti, sizeof(STRING_Instr_slti) - 1);
    string_to_symbol(STRING_Instr_sltiu, sizeof(STRING_Instr_sltiu) - 1);
    string_to_symbol(STRING_Instr_srai, sizeof(STRING_Instr_srai) - 1);
    string_to_symbol(STRING_Instr_slli, sizeof(STRING_Instr_slli) - 1);
    string_to_symbol(STRING_Instr_srli, sizeof(STRING_Instr_srli) - 1);
    string_to_symbol(STRING_Instr_lb, sizeof(STRING_Instr_lb) - 1);
    string_to_symbol(STRING_Instr_lh, sizeof(STRING_Instr_lh) - 1);
    string_to_symbol(STRING_Instr_lw, sizeof(STRING_Instr_lw) - 1);
    string_to_symbol(STRING_Instr_ld, sizeof(STRING_Instr_ld) - 1);
    string_to_symbol(STRING_Instr_lbu, sizeof(STRING_Instr_lbu) - 1);
    string_to_symbol(STRING_Instr_lhu, sizeof(STRING_Instr_lhu) - 1);
    string_to_symbol(STRING_Instr_lwu, sizeof(STRING_Instr_lwu) - 1);
    string_to_symbol(STRING_Instr_sb, sizeof(STRING_Instr_sb) - 1);
    string_to_symbol(STRING_Instr_sh, sizeof(STRING_Instr_sh) - 1);
    string_to_symbol(STRING_Instr_sw, sizeof(STRING_Instr_sw) - 1);
    string_to_symbol(STRING_Instr_sd, sizeof(STRING_Instr_sd) - 1);
    string_to_symbol(STRING_Instr_beq, sizeof(STRING_Instr_beq) - 1);
    string_to_symbol(STRING_Instr_bne, sizeof(STRING_Instr_bne) - 1);
    string_to_symbol(STRING_Instr_blt, sizeof(STRING_Instr_blt) - 1);
    string_to_symbol(STRING_Instr_bge, sizeof(STRING_Instr_bge) - 1);
    string_to_symbol(STRING_Instr_bltu, sizeof(STRING_Instr_bltu) - 1);
    string_to_symbol(STRING_Instr_bgeu, sizeof(STRING_Instr_bgeu) - 1);
    string_to_symbol(STRING_Instr_jal, sizeof(STRING_Instr_jal) - 1);
    string_to_symbol(STRING_Instr_jalr, sizeof(STRING_Instr_jalr) - 1);
    string_to_symbol(STRING_Instr_lui, sizeof(STRING_Instr_lui) - 1);
    string_to_symbol(STRING_Instr_auipc, sizeof(STRING_Instr_auipc) - 1);
    string_to_symbol(STRING_Instr_ecall, sizeof(STRING_Instr_ecall) - 1);
    string_to_symbol(STRING_Instr_ebreak, sizeof(STRING_Instr_ebreak) - 1);
    string_to_symbol(STRING_Instr_la, sizeof(STRING_Instr_la) - 1);
    string_to_symbol(STRING_Instr_len, sizeof(STRING_Instr_len) - 1);
    string_to_symbol(STRING_Instr_PLUS, sizeof(STRING_Instr_PLUS) - 1);
    string_to_symbol(STRING_Instr_MINUS, sizeof(STRING_Instr_MINUS) - 1);
    string_to_symbol(STRING_Instr_import, sizeof(STRING_Instr_import) - 1);
    string_to_symbol(STRING_Instr_module, sizeof(STRING_Instr_module) - 1);
    string_to_symbol(STRING_Instr_CARAT, sizeof(STRING_Instr_CARAT) - 1);
    string_to_symbol(STRING_Instr_STAR, sizeof(STRING_Instr_STAR) - 1);
    string_to_symbol(STRING_Instr_include, sizeof(STRING_Instr_include) - 1);
    string_to_symbol(STRING_Instr_define, sizeof(STRING_Instr_define) - 1);
    string_to_symbol(STRING_Instr_defvar, sizeof(STRING_Instr_defvar) - 1);
    string_to_symbol(STRING_Instr_defcon, sizeof(STRING_Instr_defcon) - 1);
}

u32 string_to_symbol(u8* str, u32 str_len) {
    if (SYMBOLS.len * 2 >= SYMBOLS.cap) {
        SymbolEntry* old_table = SYMBOLS.data;
        SYMBOLS.data = (SymbolEntry*)calloc(SYMBOLS.cap * 2 * sizeof(SymbolEntry));
        SYMBOLS.cap = SYMBOLS.cap * 2;
        SYMBOLS.len = 0;

        for (u64 i = 0; i < SYMBOLS.cap / 2; i++) {
            if (old_table[i].string != NULL) {
                __string_to_symbol(old_table[i].string, old_table[i].len, old_table[i].symbol);
            }
        }

        free((u64)old_table);
    }
    return __string_to_symbol(str, str_len, 0);
}

u32 __string_to_symbol(u8* str, u32 str_len, u32 old_sym) {
    u64 offset = 14695981039346656037u;
    u64 FNV_PRIME = 1099511628211u;

    for (u64 i = 0; i < str_len; i++) {
        offset = offset ^ str[i];
        offset = offset * FNV_PRIME;
    }
    u64 pos = (SYMBOLS.cap - 1) & offset;

    while (1) {
        if (SYMBOLS.data[pos].string == NULL) {
            SYMBOLS.len++;
            u32 sym = SYMBOLS.len;
            if (old_sym == 0) {
                u8* new_str = (u8*)malloc(str_len);
                strdup(new_str, str, str_len);
                str = new_str;
            } else {
                sym = old_sym;
            }
            SYMBOLS.data[pos].string = str;
            SYMBOLS.data[pos].len = str_len;
            SYMBOLS.data[pos].symbol = sym;

            return sym;
        } else if (SYMBOLS.data[pos].len != str_len) {
            goto lookup_continue;
        } else {
            for (u64 i = 0; i < str_len; i++) {
                if (str[i] != SYMBOLS.data[pos].string[i]) {
                    goto lookup_continue;
                }
            }
            return SYMBOLS.data[pos].symbol;
        }

lookup_continue:
        pos++;
        if (pos == SYMBOLS.cap) {
            pos = 0;
        }
    }
}

SymString symbol_to_string(u32 sym) {
    if (sym == 0 || SYMBOLS.len < sym) {
        goto err;
    }

    for (u64 i = 0; i < SYMBOLS.cap; i++) {
        if (SYMBOLS.data[i].symbol == sym) {
            SymString ret = { .data = SYMBOLS.data[i].string, .len = SYMBOLS.data[i].len };
            return ret;
        }
    }

err:
    u8* err_msg = "INVARIANT BROKEN in `symbol->string()`\n";
    u64 err_len = sizeof(err_msg) - 1;
    write(STDERR, err_msg, err_len);
    exit(1);
}
