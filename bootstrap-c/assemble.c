u64 assemble(Asm* assembler, Tokenizer* tokenizer);

void assemble_print_err(Tokenizer* tokenizer, u8* msg, u64 msg_len, u8* note, u64 note_len) {
    u64 start = tokenizer->token_info.data[tokenizer->token_pos - 1].start;
    u32 line = tokenizer->token_info.data[tokenizer->token_pos - 1].line;
    Tokenizer_print_err(tokenizer, msg, msg_len, note, note_len, start, line);
}

SymString format_symbol(u32 sym, u8* m1, u64 m1_len, u8* m2, u64 m2_len) {
    SymString sym_string = symbol_to_string(sym);
    u8* fmt = (u8*)malloc(sym_string.len + m1_len + m2_len);
    strdup(fmt, m1, m1_len);
    strdup(fmt + m1_len, sym_string.data, sym_string.len);
    strdup(fmt + m1_len + sym_string.len, m2, m2_len);
    sym_string.data = fmt;
    sym_string.len += m1_len + m2_len;
    return sym_string;
}

void assemble_print_err_symbol(Tokenizer* tokenizer, u32 sym, u8* m1, u64 m1_len, u8* m2, u64 m2_len, u8* note, u64 note_len) {
    SymString fmt = format_symbol(sym, m1, m1_len, m2, m2_len);
    assemble_print_err(tokenizer, fmt.data, fmt.len, note, note_len);
    free((u64)fmt.data);
}

void assemble_print_err_int(Tokenizer* tokenizer, i32 imm, u8* m1, u64 m1_len, u8* m2, u64 m2_len) {
    u8* fmt = (u8*)malloc(m1_len + m2_len + 20);
    strdup(fmt, m1, m1_len);
    u64 imm_len = format_int(fmt + m1_len, imm);
    strdup(fmt + m1_len + imm_len, m2, m2_len);
    assemble_print_err(tokenizer, fmt, m1_len + imm_len + m2_len, NULL, 0);
    free((u64)fmt);
}

SymString assemble_path_to_string(ArrayBufu32 path) {
    if (path.len == 1) {
        SymString s = symbol_to_string(path.data[0]);
        u8* copy = (u8*)malloc(s.len);
        strdup(copy, s.data, s.len);
        s.data = copy;
        return s;
    }

    SymString s = {
        .data = (u8*)malloc(2),
        .len = 1,
    };
    // TODO: double check
    s.data[0] = '(';
    for (u64 i = 0; i < path.len; i++) {
        SymString sym = symbol_to_string(path.data[i]);
        s.data = (u8*)realloc((u64)s.data, s.len + sym.len + 2);
        strdup(s.data + s.len, sym.data, sym.len);
        s.len += sym.len + 1;
        s.data[s.len] = ' ';
    }
    s.data[s.len] = ')';
    return s;
}

SymString assemble_format_err_path(ArrayBufu32 path, u8* m1, u64 m1_len, u8* m2, u64 m2_len) {
    SymString pathstr = assemble_path_to_string(path);
    u8* fmt = (u8*)malloc(m1_len + m2_len + pathstr.len);
    strdup(fmt, m1, m1_len);
    strdup(fmt + m1_len, pathstr.data, pathstr.len);
    strdup(fmt + m1_len + pathstr.len, m2, m2_len);
    free((u64)pathstr.data);
    pathstr.data = fmt;
    pathstr.len += m1_len + m2_len;
    return pathstr;
}

void assemble_print_err_path(Tokenizer* tokenizer, ArrayBufu32 path, u8* m1, u64 m1_len, u8* m2, u64 m2_len, u8* note, u64 note_len) {
    SymString fmt = assemble_format_err_path(path, m1, m1_len, m2, m2_len);
    assemble_print_err(tokenizer, fmt.data, fmt.len, note, note_len);
    free((u64)fmt.data);
}

void assemble_add_label(Asm* self, Tokenizer* tokenizer, u32 label) {
    u64 idx = SM16_entry(&(self->module->labels), label);
    if (self->module->labels.data[idx].symbol == NULL) {
        u8* duplicate = "Duplicate label `";
        u64 duplicate_len = sizeof(duplicate) - 1;
        assemble_print_err_symbol(tokenizer, label, duplicate, duplicate_len, duplicate + duplicate_len - 1, 1, NULL, 0);
        return;
    }
    idx = SM24_entry(&(self->module->children), label);
    if (self->module->children.data[idx].symbol == NULL) {
        u8* conflict = "Label `";
        u8* conflict2 = "` conflicts with module/definition";
        assemble_print_err_symbol(tokenizer, label, conflict, sizeof(conflict) - 1, conflict2, sizeof(conflict2) - 1, NULL, 0);
        return;
    }
    SM16_insert(&(self->module->labels), label, self->module->code.len, 0);
}

void assemble_skip_opcode(Tokenizer* tokenizer) {
    u32 line = tokenizer->token_info.data[tokenizer->token_pos - 1].line;
    while(1) {
        Token t = tokenize(tokenizer);
        switch (t.tag) {
            case Token_RightParen:
                return;
            case NULL:
                u8* eof_msg = "Unclosed expression at end of file";
                assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
                return;
            case Token_LeftParen:
                u32 cur_line = tokenizer->token_info.data[tokenizer->token_pos - 1].line;
                if (cur_line == line) {
                    continue;
                }
                u8* lparen_msg = "Unclosed expression";
                tokenizer->token_pos--;
                assemble_print_err(tokenizer, lparen_msg, sizeof(lparen_msg) - 1, NULL, 0);
                return;
        }
    }
}

SymString assemble_get_string(Tokenizer* tokenizer, u64 start, u32 len) {
    SymString s = {
        .data = (u8*)malloc(len + 1),
        .len = 0,
    };
    u64 end = start + len;
    while (start != end) {
        u8 ch = tokenizer->input.data[start];
        if (ch == '\\') {
            start++;
            if (start == end) {
                u8* escape_msg = "Unfinished escape code";
                assemble_print_err(tokenizer, escape_msg, sizeof(escape_msg) - 1, NULL, 0);
                return s;
            }
            ch = tokenizer->input.data[start];
            switch (ch) {
                case 'r':
                    ch = '\r';
                case 'n':
                    ch = '\n';
                case 't':
                    ch = '\t';
                case '0':
                    ch = 0;
            }
        }
        s.data[s.len] = ch;
        s.len++;
        start++;
    }
    return s;
}

u32 assemble_unwrap_ident(Tokenizer* tokenizer) {
    Token t = tokenize(tokenizer);
    switch (t.tag) {
        case NULL:
            u8* eof_msg = "Unexpected EOF in definition";
            assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
            return 0;
        case Token_RightParen:
            u8* paren_msg = "Definition must have an identifier and value";
            assemble_print_err(tokenizer, paren_msg, sizeof(paren_msg) - 1, NULL, 0);
            return 0;
        case Token_Symbol:
            return t.value;
        default:
            u8* ident_msg = "Expected identifier in definition";
            assemble_print_err(tokenizer, ident_msg, sizeof(ident_msg) - 1, NULL, 0);
            return 0;
    }
}

void assemble_handle_include(Asm* self, Tokenizer* tokenizer) {
    Token t = tokenize(tokenizer);
    switch (t.tag) {
        case Token_String:
            break;
        case NULL:
            u8* eof_msg = "Unexpected EOF in `include!`";
            assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
            return;
        case Token_RightParen:
            u8* parenthesis_msg = "Unexpected parenthesis";
            assemble_print_err(tokenizer, parenthesis_msg, sizeof(parenthesis_msg) - 1, NULL, 0);
            return;
        default:
            u8* string_msg = "`include!` file must be a string";
            assemble_print_err(tokenizer, string_msg, sizeof(string_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            return;
    }
    SymString path = assemble_get_string(tokenizer, t.value, t.len);
    if (str_eqp(path.data, path.len, tokenizer->filename.data, tokenizer->filename.len)) {
        free((u64)path.data);
        u8* recursive_msg = "Recursive include";
        assemble_print_err(tokenizer, recursive_msg, sizeof(recursive_msg) - 1, NULL, 0);
        assemble_skip_opcode(tokenizer);
        return;
    }
    t = tokenize(tokenizer);
    switch (t.tag) {
        case Token_RightParen:
            break;
        case NULL:
            u8* eof_msg = "Unexpected EOF in `include!`";
            assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
            break;
        case Token_String:
            u8* multiple_msg = "Each file needs its own include statement";
            assemble_print_err(tokenizer, multiple_msg, sizeof(multiple_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            break;
        default:
            u8* expr_msg = "Unexpected expression in include statement";
            assemble_print_err(tokenizer, expr_msg, sizeof(expr_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            break;
    }
    // room for null terminator
    path.data = (u8*)realloc((u64)path.data, path.len + 1);
    u8* input = NULL;
    u64 input_len = NULL;
    u64 err = read_file(path.data, &input, &input_len);
    if (err != 0) {
        u8* read_msg = "Error executing `include!`";
        assemble_print_err(tokenizer, read_msg, sizeof(read_msg) - 1, input, input_len);
        free((u64)input);
        free((u64)path.data);
        return;
    }
    Tokenizer* new_tokenizer = Tokenizer_new(input, input_len, path.data, path.len);
    while(1) {
        u64 err = assemble(self, new_tokenizer);
        if (err == 0) {
            break;
        }
        u8* msg = "Unexpected closing parenthesis";
        assemble_print_err(new_tokenizer, msg, sizeof(msg) - 1, NULL, 0);
    }
    free((u64)new_tokenizer->filename.data);
    munmap(new_tokenizer->input.data, new_tokenizer->input.cap);
    tokenizer->err |= new_tokenizer->err;
    Tokenizer_free(new_tokenizer);
}

u64 assemble_in_scopep(Asm* self, u32 ident) {
    Module* mod = self->module;
    while (1) {
        SM24Entry e = SM24_get(&(mod->children), ident);
        if (e.symbol != 0) {
            return 1;
        } else if (mod->filep != 0) {
            return 0;
        } else {
            mod = mod->parent;
        }
    }
}

void assemble_handle_define(Asm* self, Tokenizer* tokenizer) {
    u8* empty_msg = "Definition `";

    u32 ident = assemble_unwrap_ident(tokenizer);
    if (ident == NULL) {
        return;
    }
    Token t = tokenize(tokenizer);
    switch (t.tag) {
        case Token_Integer:
        case Token_Char:
            break;
        case NULL:
            u8* eof_msg = "Unexpected EOF in definition `";
            u8* eof_msg2 = "` at end of file";
            assemble_print_err_symbol(tokenizer, ident, eof_msg, sizeof(eof_msg) - 1, eof_msg2, sizeof(eof_msg2) - 1, NULL, 0);
            return;
        case Token_RightParen:
            u8* empty_msg2 = "` must have a value";
            assemble_print_err_symbol(tokenizer, ident, empty_msg, sizeof(empty_msg) - 1, empty_msg2, sizeof(empty_msg2) - 1, NULL, 0);
            return;
        case Token_String:
        case Token_Pound:
            u8* constant_msg = "` must be a constant";
            u8* constant_note = "\tNote: Define is only for build_time constants, did you mean `defcon`/`defvar`?";
            assemble_print_err_symbol(tokenizer, ident, empty_msg, sizeof(empty_msg) - 1, constant_msg, sizeof(constant_msg) - 1, constant_note, sizeof(constant_note) - 1);
            goto ret;
        default:
            assemble_print_err_symbol(tokenizer, ident, empty_msg, sizeof(empty_msg) - 1, constant_msg, sizeof(constant_msg) - 1, NULL, 0);
            goto ret;
    }
    if (assemble_in_scopep(self, ident) != 0) {
        u8* conflict_msg = "` conflicts with existing module/definition in scope";
        assemble_print_err_symbol(tokenizer, ident, empty_msg, sizeof(empty_msg) - 1, conflict_msg, sizeof(conflict_msg) - 1, NULL, 0);
    } else {
        SM24_insert(&(self->module->children), ident, UNIT_CONSTANT, t.value, 0);
    }

ret:
    t = tokenize(tokenizer);
    if (t.tag != Token_RightParen) {
        if (t.tag != NULL) {
            tokenizer->token_pos -= 1;
        }
        u8* unclosed_msg = "Unclosed definition";
        assemble_print_err(tokenizer, unclosed_msg, sizeof(unclosed_msg) - 1, NULL, 0);
        if (t.tag != NULL) {
            u32 line = tokenizer->token_info.data[tokenizer->token_pos].line;
            if (tokenizer->token_info.data[tokenizer->token_pos + 1].line == line) {
                tokenizer->token_pos += 1;
                assemble_skip_opcode(tokenizer);
            }
        }
    }
}

// TODO
u64 assemble_handle_defcon_var(Asm* self, Tokenizer* tokenizer) {
    return 0;
}

// TODO
u64 assemble_handle_module(Asm* self, Tokenizer* tokenizer) {
    return 0;
}

// TODO
u64 assemble_handle_import(Asm* self, Tokenizer* tokenizer) {
    return 0;
}

SM24Entry assemble_follow_path(Asm* self, Tokenizer* tokenizer, u32* path, u64 path_len) {
    if (path_len == 1) {
        if (path[0] == SYMBOL_Instr_CARAT || path[0] == SYMBOL_Instr_STAR) {
            u8* single_msg = "Path cannot consist of just `^`/`*`";
            assemble_print_err(tokenizer, single_msg, sizeof(single_msg) - 1, NULL, 0);
            SM24Entry ret = {};
            return ret;
        }
    }
    Module* mod = self->module;
    u64 i = 0;
    while(i != path_len) {
        if (path[i] != SYMBOL_Instr_CARAT) {
            break;
        }
        i++;
        mod = mod->parent;
        if (mod == NULL) {
            u8* root_msg = "Path jumped past root in expression";
            assemble_print_err(tokenizer, root_msg, sizeof(root_msg) - 1, NULL, 0);
            SM24Entry ret = {};
            return ret;
        }
    }

    while(i != path_len) {
        if (path[i] == SYMBOL_Instr_CARAT) {
            u8* carat_msg = "Bad path, can only move up (`^`) at beginning of path in expression";
            assemble_print_err(tokenizer, carat_msg, sizeof(carat_msg) - 1, NULL, 0);
            SM24Entry ret = {};
            return ret;
        } else if (path[i] == SYMBOL_Instr_STAR) {
            u8* star_msg = "Bad path, only import statments can use `*` in expression";
            assemble_print_err(tokenizer, star_msg, sizeof(star_msg) - 1, NULL, 0);
            SM24Entry ret = {};
            return ret;
        }
        SM24Entry e = SM24_get(&(mod->children), path[i]);
        i++;

        if (e.symbol == NULL) {
            if (mod->filep) {
                tokenizer->err = 1;
                SM24Entry ret = {};
                return ret;
            }
            mod = mod->parent;
            i--;
            continue;
        }
        if ((e.value1 & UNIT_MASK) != UNIT_MODULE) {
            if (i != path_len) {
                SM24Entry ret = {};
                return ret;
            }
            return e;
        } else {
            mod = (Module*)e.value2;
        }
    }
    SM24Entry e = {
        .symbol = path[path_len - 1],
        .value1 = UNIT_MODULE,
        .value2 = (u64)mod,
    };
    return e;
}

ArrayBufu32 assemble_unwrap_path(Tokenizer* tokenizer) {
    tokenizer->token_pos -= 1;
    ArrayBufu32 path = {
        .data = (u32*)malloc(16),
        .len = 0,
        .cap = 4,
    };
    Token t = tokenize(tokenizer);
    if (t.tag == Token_Symbol) {
        path.data[0] = t.value;
        path.len++;
        return path;
    }

    while (1) {
        t = tokenize(tokenizer);
        if (t.tag == NULL) {
            u8* eof_msg = "Unexpeceted EOF in path";
            assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
            path.len = 0;
            return path;
        } else if (t.tag == Token_Symbol) {
            if (path.len == path.cap) {
                resize_array((u64*)&path, 4);
            }
            path.data[path.len] = t.value;
            path.len++;
        } else if (t.tag == Token_RightParen) {
            break;
        } else {
            u8* eof_msg = "Unexpeceted token in path";
            assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            path.len = 0;
            return path;
        }
    }
    if (path.len == 1) {
        u8* warn_msg = "Unnecessary parenthesis in path";
        u64 err = tokenizer->err;
        assemble_print_err(tokenizer, warn_msg, sizeof(warn_msg) - 1, NULL, 0);
        tokenizer->err = err;
    }
    return path;
}

SymString assemble_unwrap_array(Tokenizer* tokenizer) {
    SymString s = {};
    Token t = tokenize(tokenizer);
    switch (t.tag) {
        case NULL:
            u8* pound_eof_msg = "Unexpected `#` at end of file";
            assemble_print_err(tokenizer, pound_eof_msg, sizeof(pound_eof_msg) - 1, NULL, 0);
            return s;
        case Token_RightParen:
            u8* pound_incomplete_msg = "Unexpected `#` in definition, definition incomplete";
            assemble_print_err(tokenizer, pound_incomplete_msg, sizeof(pound_incomplete_msg) - 1, NULL, 0);
            return s;
        case Token_LeftParen:
            break;
        default:
            u8* pound_unexpected_msg = "Unexpected `#` in definition";
            assemble_print_err(tokenizer, pound_unexpected_msg, sizeof(pound_unexpected_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            return s;
    }

    s.data = (u8*)malloc(8);
    u64 cap = 8;
    while (1) {
        t = tokenize(tokenizer);
        u8 b = 0;
        switch (t.tag) {
            case NULL:
                // eof
                u8* eof_msg =  "Unexpected EOF in array literal";
                assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
                free((u64)s.data);
                s.data = NULL;
                s.len = 0;
                return s;
            case Token_Symbol:
                u8* var_msg =  "Array literals must consist of u8 integers, cannot use variables";
                assemble_print_err(tokenizer, var_msg, sizeof(var_msg) - 1, NULL, 0);
                continue;
            case Token_Integer:
                i64 v = t.value;
                if (v < 0 || v >= 256) {
                    u8* oor_msg =  "Array literals must consist of u8 integers, `";
                    u8* oor_msg2 =  "` is out of range";
                    assemble_print_err_int(tokenizer, v, oor_msg, sizeof(oor_msg) - 1, oor_msg2, sizeof(oor_msg2) - 1);
                }
                b = t.value;
                break;
            case Token_Char:
                b = t.value;
                break;
            case Token_RightParen:
                return s;
            default:
                u8* other_msg =  "Array literals must consist of u8 integers";
                assemble_print_err(tokenizer, other_msg, sizeof(other_msg) - 1, NULL, 0);
                continue;
        }
        if (s.len == cap) {
            s.data = (u8*)realloc((u64)s.data, cap * 2);
            cap = cap * 2;
        }
        s.data[s.len] = b;
        s.len++;
    }
}

i32 assemble_unwrap_imm(Asm* self, Tokenizer* tokenizer) {
    Token t = tokenize(tokenizer);
    switch (t.tag) {
        case NULL:
            u8* eof_msg = "Unexpected EOF in expression";
            assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
            return 0;
        case Token_RightParen:
            u8* rparen_msg = "Expected immediate in expression";
            assemble_print_err(tokenizer, rparen_msg, sizeof(rparen_msg) - 1, NULL, 0);
            tokenizer->token_pos -= 1;
            return 0;
        case Token_Char:
        case Token_Integer:
            return t.value;
        case Token_Symbol:
            u32 path[1] = {t.value};
            SM24Entry res = assemble_follow_path(self, tokenizer, path, 1);
            if (res.symbol == NULL) {
                u8* symcon_msg = "Variable `";
                u8* pathun_msg = "` not defined/imported at use";
                assemble_print_err_symbol(tokenizer, t.value, symcon_msg, sizeof(symcon_msg) - 1, pathun_msg, sizeof(pathun_msg) - 1, NULL, 0);
                return 0;
            } else if ((res.value1 & UNIT_MASK) != UNIT_CONSTANT) {
                u8* symcon_msg = "Variable `";
                u8* symcon_msg2 = "` must be a constant in expression";
                assemble_print_err_symbol(tokenizer, t.value, symcon_msg, sizeof(symcon_msg) - 1, symcon_msg2, sizeof(symcon_msg2) - 1, NULL, 0);
                return 0;
            }
            return res.value2;
        case Token_LeftParen:
            break;
        default:
            u8* other_msg = "Expected immediate in expression";
            assemble_print_err(tokenizer, other_msg, sizeof(other_msg) - 1, NULL, 0);
            return 0;
    }

    t = tokenize(tokenizer);
    switch (t.tag) {
        case NULL:
            u8* path_eof_msg =  "Expected path in expression, got EOF";
            assemble_print_err(tokenizer, path_eof_msg, sizeof(path_eof_msg) - 1, NULL, 0);
            return 0;
        case Token_RightParen:
            u8* empty_msg =  "Empty path in expression";
            assemble_print_err(tokenizer, empty_msg, sizeof(empty_msg) - 1, NULL, 0);
            return 0;
        case Token_Symbol:
            if (t.value == SYMBOL_Instr_len) {
                break;
            }
        default:
            goto path;
    }
    // len
    t = tokenize(tokenizer);
    u64 len = 0;
    switch (t.tag) {
        case NULL:
            break;
        case Token_Symbol:
        case Token_LeftParen:
            ArrayBufu32 path = assemble_unwrap_path(tokenizer);
            if (path.len == 0) {
                free((u64)path.data);
                u8* lenempty_msg = "Empty path in `len` expression";
                assemble_print_err(tokenizer, lenempty_msg, sizeof(lenempty_msg) - 1, NULL, 0);
                break;
            }
            SM24Entry e = assemble_follow_path(self, tokenizer, path.data, path.len);
            if ((e.value1 & UNIT_MASK) != UNIT_BYTES) {
                u8* lenun_msg = "Global `";
                u8* lenun_msg2 = "` not defined/imported at use";
                assemble_print_err_path(tokenizer, path, lenun_msg, sizeof(lenun_msg) - 1, lenun_msg2, sizeof(lenun_msg2) - 1, NULL, 0);
                free((u64)path.data);
                break;
            }
            len = e.value1 >> 4;
            free((u64)path.data);
            break;
        case Token_String:
            SymString str = assemble_get_string(tokenizer, t.value, t.len);
            len = str.len;
            free((u64)str.data);
            break;
        case Token_Pound:
            SymString arr = assemble_unwrap_array(tokenizer);
            len = arr.len;
            if (arr.data != NULL) {
                free((u64)arr.data);
            }
            break;
        default:
            u8* lenpath_msg = "Expected path as argument in `len` expression";
            assemble_print_err(tokenizer, lenpath_msg, sizeof(lenpath_msg) - 1, NULL, 0);
            break;
    }
    // len-closer
    t = tokenize(tokenizer);
    switch (t.tag) {
        case NULL:
            u8* leneof_msg = "Unexpected EOF in `len` expression";
            assemble_print_err(tokenizer, leneof_msg, sizeof(leneof_msg) - 1, NULL, 0);
            break;
        case Token_RightParen:
            break;
        case Token_LeftParen:
            tokenizer->token_pos -= 1;
        default:
            u8* lenargs_msg = "Too many arguments or missing parenthesis in `len` expression";
            assemble_print_err(tokenizer, lenargs_msg, sizeof(lenargs_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            break;
    }
    return len;

path:
    tokenizer->token_pos -= 1;
    ArrayBufu32 path = assemble_unwrap_path(tokenizer);
    if (path.len == 0) {
        free((u64)path.data);
        return 0;
    }
    SM24Entry e = assemble_follow_path(self, tokenizer, path.data, path.len);
    if (e.symbol == 0) {
        u8* symcon_msg = "Variable `";
        u8* pathun_msg = "` not defined/imported at use";
        assemble_print_err_path(tokenizer, path, symcon_msg, sizeof(symcon_msg) - 1, pathun_msg, sizeof(pathun_msg) - 1, NULL, 0);
        free((u64)path.data);
        return 0;
    }
    if ((e.value1 & UNIT_MASK) != UNIT_CONSTANT) {
        u8* symcon_msg = "Variable `";
        u8* pathcon_msg = "` must be a constant";
        assemble_print_err_path(tokenizer, path, symcon_msg, sizeof(symcon_msg) - 1, pathcon_msg, sizeof(pathcon_msg) - 1, NULL, 0);
        free((u64)path.data);
        return 0;
    }
    free((u64)path.data);
    return e.value2;
}

u32 assemble_unwrap_register(Tokenizer* tokenizer) {
    Token t = tokenize(tokenizer);
    switch (t.tag) {
        case Token_Symbol:
            u32 reg = t.value - 1;
            if (reg >= SYMBOL_Register_X31) {
                u8* ident_msg = "Expected register in instruction, got identifier `";
                u64 ident_msg_len = sizeof(ident_msg) - 1;
                assemble_print_err_symbol(tokenizer, t.value, ident_msg, ident_msg_len, ident_msg + ident_msg_len - 1, 1, NULL, 0);
                return 0;
            }
            return reg;
        case NULL:
            u8* eof_msg = "Expected register in instruction, got EOF";
            assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
            return 0;
        case Token_RightParen:
        case Token_LeftParen:
            tokenizer->token_pos -= 1;
        default:
            u8* expected_msg = "Expected register in instruction";
            assemble_print_err(tokenizer, expected_msg, sizeof(expected_msg) - 1, NULL, 0);
            return 0;
    }
}

typedef struct Offset_struct {
    u32 reg;
    i32 imm;
} Offset;

Offset assemble_unwrap_offset(Asm* self, Tokenizer* tokenizer) {
    Offset ret = {};
    Token t = tokenize(tokenizer);
    switch (t.tag) {
        case NULL:
            u8* eof_msg = "Unexpected EOF in expression";
            assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
            return ret;
        case Token_Symbol:
            tokenizer->token_pos -= 1;
            ret.reg = assemble_unwrap_register(tokenizer);
            return ret;
        case Token_LeftParen:
            break;
        default:
            u8* reg_offset_msg = "Expected register/offset in expression";
            assemble_print_err(tokenizer, reg_offset_msg, sizeof(reg_offset_msg) - 1, NULL, 0);
            if (t.tag == Token_RightParen) {
                tokenizer->token_pos -= 1;
            }
            return ret;
    }
    t = tokenize(tokenizer);
    u8 neg = 0;
    switch (t.tag) {
        case NULL:
            u8* offset_eof_msg = "Unexpected EOF in expression offset";
            assemble_print_err(tokenizer, offset_eof_msg, sizeof(offset_eof_msg) - 1, NULL, 0);
            return ret;
        case Token_RightParen:
            u8* empty_msg = "Empty offset in expression";
            assemble_print_err(tokenizer, empty_msg, sizeof(empty_msg) - 1, NULL, 0);
            return ret;
        case Token_Symbol:
            if (t.value == SYMBOL_Instr_MINUS) {
                neg = 1;
            } else if (t.value != SYMBOL_Instr_PLUS) {
                u8* symbol_msg = "Expected `+`/`-`, got `";
                u8* symbol_msg2 = "` in expression offset";
                assemble_print_err_symbol(tokenizer, t.value, symbol_msg, sizeof(symbol_msg) - 1, symbol_msg2, sizeof(symbol_msg2) - 1, NULL, 0);
                assemble_skip_opcode(tokenizer);
                return ret;
            }
            break;
        default:
            u8* expr_msg = "Expected `+`/`-` in expression offset";
            assemble_print_err(tokenizer, expr_msg, sizeof(expr_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            return ret;
    }

    t = tokenize(tokenizer);
    switch (t.tag) {
        case NULL:
            u8* offset_eof_msg = "Unexpected EOF in expression offset";
            assemble_print_err(tokenizer, offset_eof_msg, sizeof(offset_eof_msg) - 1, NULL, 0);
            return ret;
        case Token_RightParen:
            tokenizer->token_pos -= 1;
            u8* reg_imm_msg = "Expected register and immediate in expression offset";
            assemble_print_err(tokenizer, reg_imm_msg, sizeof(reg_imm_msg) - 1, NULL, 0);
            tokenizer->token_pos += 1;
            return ret;
        case Token_Symbol:
            if (t.value <= SYMBOL_Register_X31) {
                ret.reg = t.value - 1;
                ret.imm = assemble_unwrap_imm(self, tokenizer);
                break;
            }
        case Token_Integer:
        case Token_Char:
        case Token_LeftParen:
            tokenizer->token_pos -= 1;
            ret.imm = assemble_unwrap_imm(self, tokenizer);
            ret.reg = assemble_unwrap_register(tokenizer);
            break;
        default:
            tokenizer->token_pos -= 1;
            assemble_print_err(tokenizer, reg_imm_msg, sizeof(reg_imm_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            return ret;
    }

    // close
    t = tokenize(tokenizer);
    switch (t.tag) {
        case NULL:
            u8* close_msg = "Expected closing parenthesis in expression offset";
            assemble_print_err(tokenizer, close_msg, sizeof(close_msg) - 1, NULL, 0);
            break;
        case Token_RightParen:
            break;
        default:
            tokenizer->token_pos -= 1;
            assemble_print_err(tokenizer, close_msg, sizeof(close_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            break;
    }

    if (neg == 1) {
        ret.imm = -ret.imm;
    }
    return ret;
}

Offset assemble_unwrap_label(Asm* self, Tokenizer* tokenizer, u64 branchp) {
    Offset ret = {};
    Token t = tokenize(tokenizer);
    switch (t.tag) {
        case NULL:
            u8* eof_msg = "Expected label in instruction, got EOF";
            assemble_print_err(tokenizer, eof_msg, sizeof(eof_msg) - 1, NULL, 0);
            return ret;
        case Token_LeftParen:
            break;
        case Token_Symbol:
            SM16Entry e = SM16_get(&(self->module->labels), t.value);
            if (e.symbol == 0) {
                if (self->module->jumps.len == self->module->jumps.cap) {
                    resize_array((u64*)(&(self->module->jumps)), sizeof(Jump));
                }
                self->module->jumps.data[self->module->jumps.len].symbol = t.value;
                self->module->jumps.data[self->module->jumps.len].pos = (self->module->code.len << 1) | branchp;
                self->module->jumps.len++;
            } else {
                ret.reg = t.value;
                ret.imm = self->module->code.len - e.value;
            }
            return ret;
        case Token_RightParen:
        default:
            u8* other_msg = "Expected label in instruction";
            assemble_print_err(tokenizer, other_msg, sizeof(other_msg) - 1, NULL, 0);
            if (t.tag == Token_RightParen) {
                tokenizer->token_pos -= 1;
            }
            return ret;
    }

    // path
    ArrayBufu32 path = assemble_unwrap_path(tokenizer);
    if (path.len == 0) {
        free((u64)path.data);
        u8* path_msg = "Expected non-empty path in instruction";
        assemble_print_err(tokenizer, path_msg, sizeof(path_msg) - 1, NULL, 0);
        return ret;
    }
    if (self->module->refs.len == self->module->refs.cap) {
        resize_array((u64*)(&(self->module->refs)), sizeof(Ref));
    }
    self->module->refs.data[self->module->refs.len].pos = (self->module->code.len << 1) | branchp;
    self->module->refs.data[self->module->refs.len].path.data = (u8*)path.data;
    self->module->refs.data[self->module->refs.len].path.len = path.len;
    self->module->refs.len++;
    return ret;
}

u8 funct3_table[] = {0, 0, 4, 6, 7, 1, 5, 5, 2, 3, 0, 4, 6,    // r
                     0, 0, 4, 6, 7, 2, 3, 5, 1, 5,             // i
                     0, 1, 2, 3, 4, 5, 6,                      // i2
                     0, 1, 2, 3,                               // s
                     0, 1, 4, 5, 6, 7};                        // b
u8 funct7_table[] = {0, 0x20, 0, 0, 0, 0, 0, 0x20, 0, 0, 0x1, 0x1, 0x1};
// TODO
void assemble_handle_opcode(Asm* self, Tokenizer* tokenizer, u32 opcode) {
    u32 instr = 0;
    if (opcode < SYMBOL_Instr_add) {
        u8* reg_msg = "Cannot use register `";
        u8* reg_msg2 = "` as opcode";
        assemble_print_err_symbol(tokenizer, opcode, reg_msg, sizeof(reg_msg) - 1, reg_msg2, sizeof(reg_msg2) - 1, NULL, 0);
        assemble_skip_opcode(tokenizer);
        return;
    } else if (opcode < LAST_R) {
        u32 instr = 0b0110011;
        u32 funct3 = funct3_table[opcode - SYMBOL_Instr_add];
        instr = instr | (funct3 << 12);
        u32 funct7 = funct7_table[opcode - SYMBOL_Instr_add];
        instr = instr | (funct7 << 25);
        u32 to = assemble_unwrap_register(tokenizer);
        instr = instr | (to << 7);
        u32 r1 = assemble_unwrap_register(tokenizer);
        instr = instr | (r1 << 15);
        u32 r2 = assemble_unwrap_register(tokenizer);
        instr = instr | (r2 << 20);
    } else if (opcode < LAST_I) {
        instr = 0b0010011;
        u32 funct3 = funct3_table[opcode - SYMBOL_Instr_add];
        instr = instr | (funct3 << 12);
        u32 to = assemble_unwrap_register(tokenizer);
        instr = instr | (to << 7);
        u32 r1 = assemble_unwrap_register(tokenizer);
        instr = instr | (r1 << 15);
        i32 imm = assemble_unwrap_imm(self, tokenizer);
        if (opcode == SYMBOL_Instr_subi) {
            imm = -imm;
        } else if (opcode >= SYMBOL_Instr_srai) {
            u32 shift = imm;
            if (shift >= 64) {
                u8* iin_msg =  "Immediate `";
                u8* iin_shift_msg =  "` out of range [0, 64)";
                assemble_print_err_int(tokenizer, imm, iin_msg, sizeof(iin_msg) - 1, iin_shift_msg, sizeof(iin_shift_msg) - 1);
                imm = 0;
            }
            if (opcode == SYMBOL_Instr_srai) {
                instr = instr | (0x20 << 5);
            }
        }

        if (imm < -2048 | imm >= 2048) {
            u8* iin_msg =  "Immediate `";
            u8* iin_msg2 =  "` out of range [-2048, 2048)";
            assemble_print_err_int(tokenizer, imm, iin_msg, sizeof(iin_msg) - 1, iin_msg2, sizeof(iin_msg2) - 1);
            imm = 0;
        }
        instr = instr | (imm << 20);
    } else if (opcode < LAST_I2) {
        instr = 0b0000011;
        u32 funct3 = funct3_table[opcode - SYMBOL_Instr_add];
        instr = instr | (funct3 << 12);
        u32 to = assemble_unwrap_register(tokenizer);
        instr = instr | (to << 7);
        Offset off = assemble_unwrap_offset(self, tokenizer);
        instr = instr | (off.reg << 15);
        if (off.imm < -2048 | off.imm >= 2048) {
            u8* i2in_msg =  "Offset `";
            u8* i2in_msg2 =  "` out of range [-2048, 2048)";
            assemble_print_err_int(tokenizer, off.imm, i2in_msg, sizeof(i2in_msg) - 1, i2in_msg2, sizeof(i2in_msg2) - 1);
            off.imm = 0;
        }
        instr = instr | (off.imm << 20);
    } else if (opcode < LAST_S) {
        instr = 0b0100011;
        u32 funct3 = funct3_table[opcode - SYMBOL_Instr_add];
        instr = instr | (funct3 << 12);
        Offset off = assemble_unwrap_offset(self, tokenizer);
        instr = instr | (off.reg << 15);
        if (off.imm < -2048 | off.imm >= 2048) {
            u8* sin_msg =  "Offset `";
            u8* sin_msg2 =  "` out of range [-2048, 2048)";
            assemble_print_err_int(tokenizer, off.imm, sin_msg, sizeof(sin_msg) - 1, sin_msg2, sizeof(sin_msg2) - 1);
            off.imm = 0;
        }
        instr = instr | (((off.imm & 0b11111) >> 5) << 25);
        u32 from = assemble_unwrap_register(tokenizer);
        instr = instr | (from << 20);
    } else if (opcode < LAST_B) {
        instr = 0b1100011;
        u32 funct3 = funct3_table[opcode - SYMBOL_Instr_add];
        instr = instr | (funct3 << 12);
        u32 left = assemble_unwrap_register(tokenizer);
        instr = instr | (left << 15);
        u32 right = assemble_unwrap_register(tokenizer);
        instr = instr | (right << 20);
        Offset label = assemble_unwrap_label(self, tokenizer, 1);
        if (label.reg != 0) {
            if (label.imm >= (1 << 12) || label.imm < -(1 << 12)) {
                u8* bin_msg = "Branch to `";
                u8* bin_msg2 = "` too far";
                assemble_print_err_symbol(tokenizer, label.reg, bin_msg, sizeof(bin_msg) - 1, bin_msg2, sizeof(bin_msg2) - 1, NULL, 0);
                label.imm = 0;
            }
            u32 imm = label.imm & 0x1e;
            imm = imm | ((label.imm >> 11) & 1);
            instr = instr | (imm << 7);
            imm = (label.imm >> 6) & 0x40;
            imm = imm | ((label.imm >> 5) & 0x3f);
            instr = instr | (imm << 25);
        }
    } else if (opcode == SYMBOL_Instr_jal) {
        instr = 0b1100011;
        u32 to = assemble_unwrap_register(tokenizer);
        instr = instr | (to << 7);
        Offset label = assemble_unwrap_label(self, tokenizer, 0);
        if (label.reg != 0) {
            if (label.imm >= (1 << 20) || label.imm < -(1 << 20)) {
                u8* jal_msg =  "Jump to `";
                u8* jal_msg2 =  "` too far";
                assemble_print_err_symbol(tokenizer, label.reg, jal_msg, sizeof(jal_msg) - 1, jal_msg2, sizeof(jal_msg2) - 1, NULL, 0);
                label.imm = 0;
            }
            u32 imm = (label.imm & 0x100000) >> 1;
            imm = imm | (((label.imm >> 1) & 0x3ff) << 9);
            imm = imm | (((label.imm >> 11) & 1) << 8);
            imm = imm | ((label.imm >> 12) & 0xff);
            instr = instr | (imm << 12);
        }
    } else if (opcode == SYMBOL_Instr_jalr) {
        instr = 0b1100111;
        u32 to = assemble_unwrap_register(tokenizer);
        instr = instr | (to << 7);
        Offset off = assemble_unwrap_offset(self, tokenizer);
        instr = instr | (off.reg << 15);
        if (off.imm < -2048 || off.imm >= 2048) {
            u8* jalr_msg = "Offset `";
            u8* jalr_msg2 = "` out of range [-2048, 2048)";
            assemble_print_err_int(tokenizer, off.imm, jalr_msg, sizeof(jalr_msg) - 1, jalr_msg2, sizeof(jalr_msg2) - 1);
            off.imm = 0;
        }
        instr = instr | (off.imm << 20);
    } else if (opcode < SYMBOL_Instr_ecall) {
        // lui/auipc
        if (opcode == SYMBOL_Instr_auipc) {
            instr = 0b0010111;
        } else {
            instr = 0b0110111;
        }
        u32 to = assemble_unwrap_register(tokenizer);
        instr = instr | (to << 7);
        i32 imm = assemble_unwrap_imm(self, tokenizer);
        if (imm >= (1 << 19) || imm < -(1 << 19)) {
            u8* lui_msg = "Immediate `";
            u8* lui_msg2 = "` outof range [-524288, 524288)";
            assemble_print_err_int(tokenizer, imm, lui_msg, sizeof(lui_msg) - 1, lui_msg2, sizeof(lui_msg2) - 1);
        } else {
            instr = instr | (imm << 12);
        }
    } else if (opcode == SYMBOL_Instr_ecall) {
        instr = 0b1110011;
    } else if (opcode == SYMBOL_Instr_ebreak) {
        instr = 0b1110011;
        instr = instr | (1 << 12);
    } else if (opcode == SYMBOL_Instr_la) {
        u32 to = assemble_unwrap_register(tokenizer);
        Token t = tokenize(tokenizer);
        switch (t.tag) {
            case NULL:
                // la-eof
            case Token_RightParen:
                // la-rparen
            case Token_Symbol:
            case Token_LeftParen:
                break;
            default:
                // la-other
        }
        ArrayBufu32 path = assemble_unwrap_path(tokenizer);
        if (path.len == 0) {
            // la-empty
            free((u64)path.data);
            u8* lenempty_msg = "Empty path in `len` expression";
            assemble_print_err(tokenizer, lenempty_msg, sizeof(lenempty_msg) - 1, NULL, 0);
        } else if (path.len == 1 && SYMBOL_Register_X31 >= path.data[0]) {
            // la-reg
        }
        SM24Entry e = assemble_follow_path(self, tokenizer, path.data, path.len);
        if ((e.value1 & UNIT_MASK) != UNIT_BYTES) {
            // la-global
        }
        free((u64)path.data);
        // TODO
    } else {
        u8* unknown_err = "Unknown opcode `";
        u64 unknown_err_len = sizeof(unknown_err) - 1;
        assemble_print_err_symbol(tokenizer, opcode, unknown_err, unknown_err_len, unknown_err + unknown_err_len - 1, 1, NULL, 0);
        assemble_skip_opcode(tokenizer);
        return;
    }

    if (self->module->code.len == self->module->code.cap) {
        resize_array((u64*)(&(self->module->code)), 1);
    }
    self->module->code.data[self->module->code.len] = (instr & 0xff);
    instr = instr >> 8;
    self->module->code.data[self->module->code.len + 1] = (instr & 0xff);
    instr = instr >> 8;
    self->module->code.data[self->module->code.len + 2] = (instr & 0xff);
    instr = instr >> 8;
    self->module->code.data[self->module->code.len + 3] = (instr & 0xff);
    self->module->code.len += 4;

    Token t = tokenize(tokenizer);
    switch (t.tag) {
        case Token_RightParen:
            break;
        case NULL:
            u8* unclosed_msg = "Unclosed instruction at end of file";
            assemble_print_err(tokenizer, unclosed_msg, sizeof(unclosed_msg) - 1, NULL, 0);
            break;
        case Token_LeftParen:
            tokenizer->token_pos -= 1;
            u8* lparen_msg = "Unclosed instruction";
            assemble_print_err(tokenizer, lparen_msg, sizeof(lparen_msg) - 1, NULL, 0);
            break;
        default:
            u8* too_many_msg = "Too many arguments or missing parenthesis in instruction";
            assemble_print_err(tokenizer, too_many_msg, sizeof(too_many_msg) - 1, NULL, 0);
            assemble_skip_opcode(tokenizer);
            break;
    }
}

u64 assemble(Asm* assembler, Tokenizer* tokenizer) {
    while (1) {
        Token t = tokenize(tokenizer);
        switch (t.tag) {
            case Token_LeftParen:
                break;
            case NULL:
                return 0;
            case Token_Symbol:
                assemble_add_label(assembler, tokenizer, t.value);
                continue;
            case Token_RightParen:
                (tokenizer->token_pos)--;
                return 1;
            default:
                u8* expr_err = "Expression must be a label or an S-Expression";
                assemble_print_err(tokenizer, expr_err, sizeof(expr_err) - 1, NULL, 0);
                continue;
        }

        // Sexpr
        t = tokenize(tokenizer);
        switch (t.tag) {
            case Token_Symbol:
                break;
            case NULL:
                u8* opening_err = "Unexpected opening parenthesis";
                assemble_print_err(tokenizer, opening_err, sizeof(opening_err) - 1, NULL, 0);
                return 0;
            case Token_RightParen:
                u8* empty_err = "Empty expression ending";
                assemble_print_err(tokenizer, empty_err, sizeof(empty_err) - 1, NULL, 0);
                continue;
            default:
                u8* expr_value_err = "Unexpected value in expression";
                assemble_print_err(tokenizer, expr_value_err, sizeof(expr_value_err) - 1, NULL, 0);
                assemble_skip_opcode(tokenizer);
                continue;
        }

        switch (t.value) {
            case SYMBOL_Instr_include:
                assemble_handle_include(assembler, tokenizer);
                break;
            case SYMBOL_Instr_define:
                assemble_handle_define(assembler, tokenizer);
                break;
            case SYMBOL_Instr_defcon:
            case SYMBOL_Instr_defvar:
                assemble_handle_defcon_var(assembler, tokenizer);
                break;
            case SYMBOL_Instr_module:
                assemble_handle_module(assembler, tokenizer);
                break;
            case SYMBOL_Instr_import:
                assemble_handle_import(assembler, tokenizer);
                break;
            default:
                assemble_handle_opcode(assembler, tokenizer, t.value);
                break;
        }
    }
    return 0;
}

// TODO
void assemble_finish(Asm* assembler) {
}
