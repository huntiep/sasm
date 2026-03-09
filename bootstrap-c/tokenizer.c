void Tokenizer_newline(Tokenizer* self) {
    if (self->lines.len == self->lines.cap) {
        resize_array((u64*)(&self->lines), sizeof(Line));
    }

    u64 start = 0;
    if (self->lines.len != 0) {
        start = self->lines.data[self->lines.len - 1].end;
    }
    if (start != self->input.len) {
        Line line = { .start = start, .end = self->input.len };
        self->lines.data[self->lines.len] = line;
        self->lines.len++;
    }
}

void Tokenizer_print_err(Tokenizer* self, u8* msg, u64 msg_len, u8* note, u64 note_len, u64 start, u64 line) {
    write(STDERR, msg, msg_len);
    u8* at = " at ";
    write(STDERR, at, sizeof(at) - 1);
    write(STDERR, self->filename.data, self->filename.len);
    u8* colon = ": ";
    write(STDERR, colon, sizeof(colon) - 1);
    u8 int_str[24];
    u64 int_str_len = format_int(int_str, line + 1);
    write(STDERR, int_str, int_str_len);
    write(STDERR, colon, 1);

    u64 line_start = start;
    if (self->lines.len != 0) {
        u64 i = 0;
        line_start = self->lines.data[self->lines.len - 1].end;
        while (i < self->lines.len) {
            if (start < self->lines.data[i].end) {
                line_start = self->lines.data[i].start;
                break;
            }
            i++;
        }
    }
    int_str_len = format_int(int_str, line_start + 1);
    write(STDERR, int_str, int_str_len);
    u8* period = ".\n";
    write(STDERR, period, sizeof(period) - 1);

    if (note != NULL) {
        write(STDERR, note, note_len);
        // \n
        write(STDERR, period + 1, 1);
    }

    int_str_len = format_int(int_str, line + 1);
    write(STDERR, int_str, int_str_len);
    write(STDERR, colon, sizeof(colon) - 1);

    line_start = 0;
    if (self->lines.len > 0) {
        line_start = self->lines.data[self->lines.len - 1].end;
        if (line < self->lines.len) {
            line_start = self->lines.data[line].start;
        }
    }

    u64 line_len = self->input.cap - line_start;
    if (start != self->input.cap) {
        line_len = start;
        while (line_len != self->input.cap) {
            line_len++;
            if (self->input.data[line_len] == '\n') {
                break;
            }
        }
        line_len = line_len - line_start;
    }
    write(STDERR, self->input.data + line_start, line_len);


    write(STDERR, period + 1, 1);
    self->err = 1;
}

void Tokenizer_comment(Tokenizer* self, u64 start, u64 line) {
    while (self->input.len != self->input.cap) {
        if (self->input.data[self->input.len] == '\n') {
            return;
        }
        self->input.len++;
    }
}

void Tokenizer_block_comment(Tokenizer* self, u64 start, u64 line) {
    self->input.len++;
    u64 nested = 0;
    while (self->input.len != self->input.cap) {
        u8 ch = self->input.data[self->input.len];
        self->input.len++;

        if (ch == '\n') {
            Tokenizer_newline(self);
        } else if (ch == '#') {
            if (self->input.len == self->input.cap) {
                goto err;
            }
            ch = self->input.data[self->input.len];
            if (ch == '|') {
                self->input.len++;
                nested++;
            }
        } else if (ch == '|') {
            if (self->input.len == self->input.cap) {
                goto err;
            }
            ch = self->input.data[self->input.len];
            if (ch == '#') {
                self->input.len++;
                if (nested == 0) {
                    return;
                }
                nested--;
            }
        }
    }
err:
    u8* unclosed_msg = "Unclosed block comment";
    Tokenizer_print_err(self, unclosed_msg, sizeof(unclosed_msg), NULL, 0, start, line);
}

Token Tokenizer_push_token(Tokenizer* self, u64 start, u64 line, Token_variant tag, u64 value) {
    if (self->token_info.len == self->token_info.cap) {
        resize_array((u64*)(&self->token_info), sizeof(TokenInfoT));
    }
    self->token_info.data[self->token_info.len].line = line;
    self->token_info.data[self->token_info.len].start = start;
    self->token_info.data[self->token_info.len].len = self->input.len - start;
    self->token_info.len++;

    if (self->tokens.len == self->tokens.cap) {
        resize_array((u64*)(&self->tokens), sizeof(Token));
    }

    self->tokens.data[self->tokens.len].tag = tag;
    self->tokens.data[self->tokens.len].value = value;
    Token ret = { .tag = tag, .value = value };
    if (tag == Token_String) {
        self->tokens.data[self->tokens.len].len = self->input.len - value - 1;
        ret.len = self->input.len - value - 1;
    }
    return ret;
}

Token Tokenizer_string(Tokenizer* self, u64 start, u64 line) {
    u64 value = start + 1;
    while (self->input.len != self->input.cap) {
        u8 ch = self->input.data[self->input.len];
        self->input.len++;
        if (ch == '\\') {
            if (self->input.len != self->input.cap) {
                break;
            }
            ch = self->input.data[self->input.len];
            self->input.len++;
            switch (ch) {
                case 'r':
                case 'n':
                case 't':
                case '0':
                case '\\':
                case '"':
                    continue;
                case '\n':
                    Tokenizer_newline(self);
                default:
                    u8* invalid_msg = "Invalid string escape code `\\\0`";
                    u64 invalid_msg_len = sizeof(invalid_msg) - 1;
                    invalid_msg[invalid_msg_len - 2] = ch;
                    Tokenizer_print_err(self, invalid_msg, invalid_msg_len, NULL, 0, start, line);
            }
        } else if (ch == '\n') {
            Tokenizer_newline(self);
        } else if (ch == '"') {
            return Tokenizer_push_token(self, start, line, Token_String, value);
        }
    }

    u8* unclosed_msg = "Unclosed string beginning";
    Tokenizer_print_err(self, unclosed_msg, sizeof(unclosed_msg) - 1, NULL, 0, start, line);
    // TODO: double check
    if (line < self->lines.len) {
        start = self->lines.data[self->lines.len - 1].end;
    }
    return Tokenizer_push_token(self, start, line, Token_String, value);
}

Token Tokenizer_literal(Tokenizer* self, u64 start, u64 line) {
    u8* unclosed_msg = "Unclosed character literal";

    if (self->input.len == self->input.cap) {
        return Tokenizer_push_token(self, start, line, Token_Pound, NULL);
    } else if (self->input.data[self->input.len] == '|') {
        Tokenizer_block_comment(self, start, line);
        Token t = {};
        return t;
    } else if (self->input.data[self->input.len] != '\'') {
        return Tokenizer_push_token(self, start, line, Token_Pound, NULL);
    }
    self->input.len++;
    if (self->input.len == self->input.cap) {
        Tokenizer_print_err(self, unclosed_msg, sizeof(unclosed_msg) - 1, NULL, 0, start, line);
        Token t = {};
        return t;
    }

    u8 ch = self->input.data[self->input.len];
    self->input.len++;
    if (ch == '\'') {
        u8* empty_msg = "Empty character literal";
        Tokenizer_print_err(self, empty_msg, sizeof(empty_msg) - 1, NULL, 0, start, line);
        return Tokenizer_push_token(self, start, line, Token_Char, NULL);
    } else if (ch == '\n') {
        Tokenizer_newline(self);
    } else if (ch == '\\') {
        if (self->input.len == self->input.cap) {
            Tokenizer_print_err(self, unclosed_msg, sizeof(unclosed_msg) - 1, NULL, 0, start, line);
        }
        ch = self->input.data[self->input.len];
        self->input.len++;
        switch (ch) {
            case '\\':
            case '\'':
                break;
            case 'r':
                ch = '\r';
                break;
            case 'n':
                ch = '\n';
                break;
            case 't':
                ch = '\t';
                break;
            case '0':
                ch = '\0';
                break;
            case '\n':
                Tokenizer_newline(self);
            default:
                u8* escape_msg = "Bad escape sequence `\\\0` in character literal";
                escape_msg[22] = ch;
                Tokenizer_print_err(self, escape_msg, sizeof(escape_msg) - 1, NULL, 0, start, line);
        }
    }

    if (self->input.len == self->input.cap || self->input.data[self->input.len] != '\'') {
        Tokenizer_print_err(self, unclosed_msg, sizeof(unclosed_msg) - 1, NULL, 0, start, line);
        return Tokenizer_push_token(self, start, line, Token_Char, ch);
    }

    self->input.len++;
    return Tokenizer_push_token(self, start, line, Token_Char, ch);
}

Token Tokenizer_push_symbol(Tokenizer* self, u64 start, u64 line) {
    u32 sym = string_to_symbol(self->input.data + start, self->input.len - start);
    return Tokenizer_push_token(self, start, line, Token_Symbol, sym);
}

Token Tokenizer_identifier(Tokenizer* self, u64 start, u64 line) {
    while (self->input.len != self->input.cap) {
        u8 ch = self->input.data[self->input.len];
        self->input.len++;
        switch (ch) {
            case '\r':
            case '\n':
            case '\t':
            case ' ':
            case '#':
            case '"':
            case '(':
            case ')':
            case ';':
                goto after;
        }
    }
after:
        self->input.len--;
        return Tokenizer_push_symbol(self, start, line);
}

Token Tokenizer_ambiguous(Tokenizer* self, u64 start, u64 line) {
    while (self->input.len != self->input.cap) {
        u8 ch = self->input.data[self->input.len];
        switch (ch) {
            case '\r':
            case '\n':
            case '\t':
            case ' ':
            case '#':
            case '"':
            case '(':
            case ')':
            case ';':
                goto after;
        }
        self->input.len++;
        switch (ch) {
            case '0' ... '9':
            case 'a' ... 'f':
            case 'F' ... 'F':
            case '+':
            case '-':
            case '_':
            case 'o':
            case 'x':
                continue;
            default:
                return Tokenizer_identifier(self, start, line);
        }
    }
after:
    u64 i = start;
    u64 neg = 0;
    switch (self->input.data[i]) {
        case '-':
            neg = 1;
        case '+':
            i++;
            if (i == self->input.len) {
                return Tokenizer_push_symbol(self, start, line);
            }
    }

    if (self->input.data[i] == '_') {
        return Tokenizer_push_symbol(self, start, line);
    }

    u64 value = 0;
    // 0 => decimal, 1 => binary, 2 => octal, 3 => hex
    u64 base = 0;
    if (self->input.data[i] == '0') {
        i++;
        if (i == self->input.len) {
            return Tokenizer_push_token(self, start, line, Token_Integer, value);
        }
        i++;
        switch (self->input.data[i - 1]) {
            case 'b':
                base = 1;
                break;
            case 'o':
                base = 2;
                break;
            case 'x':
                base = 3;
                break;
            default:
                base = 0;
                i -= 2;
                goto decimal;
        }
        while (1) {
            if (i == self->input.len) {
                return Tokenizer_push_symbol(self, start, line);
            }
            if (self->input.data[i] == '_') {
                i++;
            } else if (base == 0) {
                goto decimal;
            } else {
                break;
            }
        }

        if (base == 1) {
            // binary
            while (i != self->input.len) {
                u8 ch = self->input.data[i];
                i++;
                switch (ch) {
                    case '_':
                        continue;
                    case '0' ... '1':
                        value = value << 1;
                        value += (ch - '0');
                        break;
                    default:
                        return Tokenizer_push_symbol(self, start, line);
                }
            }
            return Tokenizer_push_token(self, start, line, Token_Integer, value);
        } else if (base == 2) {
            // octal
            while (i != self->input.len) {
                u8 ch = self->input.data[i];
                i++;
                switch (ch) {
                    case '_':
                        continue;
                    case '0' ... '7':
                        value = value << 3;
                        value += (ch - '0');
                        break;
                    default:
                        return Tokenizer_push_symbol(self, start, line);
                }
            }
            return Tokenizer_push_token(self, start, line, Token_Integer, value);
        } else if (base == 3) {
            // hex
            while (i != self->input.len) {
                u8 ch = self->input.data[i];
                i++;
                switch (ch) {
                    case '_':
                        continue;
                    case '0' ... '9':
                        value = value << 4;
                        value += (ch - '0');
                        break;
                    case 'a' ... 'f':
                        value = value << 4;
                        value += (ch - 'a' + 10);
                        break;
                    case 'A' ... 'F':
                        value = value << 4;
                        value += (ch - 'A' + 10);
                        break;
                    default:
                        return Tokenizer_push_symbol(self, start, line);
                }
            }
            return Tokenizer_push_token(self, start, line, Token_Integer, value);
        }
    }
decimal:
    while (i != self->input.len) {
        u8 ch = self->input.data[i];
        i++;
        switch (ch) {
            case '_':
                continue;
            case '0' ... '9':
                value *= 10;
                value += (ch - '0');
                break;
            default:
                return Tokenizer_push_symbol(self, start, line);
        }
    }
    return Tokenizer_push_token(self, start, line, Token_Integer, value);
}

Token tokenize(Tokenizer* self) {
    if (self->token_pos != self->tokens.len) {
        Token ret = self->tokens.data[self->token_pos];
        self->token_pos++;
        return ret;
    }

    while (self->input.len != self->input.cap) {
        u8 ch = self->input.data[self->input.len];
        u64 start = self->input.len;
        u64 line = self->lines.len;
        self->input.len++;

        switch (ch) {
            case '(':
                return Tokenizer_push_token(self, start, line, Token_LeftParen, NULL);
            case ')':
                return Tokenizer_push_token(self, start, line, Token_RightParen, NULL);
            case '"':
                return Tokenizer_string(self, start, line);
            case '#':
                Token t = Tokenizer_literal(self, start, line);
                if (t.tag == NULL) {
                    continue;
                }
                return t;
            case ';':
                Tokenizer_comment(self, start, line);
                continue;

            // Whitespace
            case ' ':
            case '\t':
            case '\r':
                continue;
            case '\n':
                Tokenizer_newline(self);
                continue;
            case '0' ... '9':
            case 'a' ... 'f':
            case 'F' ... 'F':
            case '+':
            case '-':
            case '_':
            case 'o':
            case 'x':
                return Tokenizer_ambiguous(self, start, line);
            default:
                return Tokenizer_identifier(self, start, line);
        }
    }
    Tokenizer_newline(self);
    Token ret = {};
    return ret;
}
