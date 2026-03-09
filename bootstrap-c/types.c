#define UNIT_MODULE 2
#define UNIT_BYTES 4
#define UNIT_CONSTANT 6
#define UNIT_IMPORT 8
#define UNIT_MASK 6

typedef struct String_struct {
    u8* data;
    u64 len;
} String;

typedef struct ArrayBufu8_struct {
    u8* data;
    u64 len;
    u64 cap;
} ArrayBufu8;

typedef struct ArrayBufu32_struct {
    u32* data;
    u64 len;
    u64 cap;
} ArrayBufu32;

typedef struct ArrayBufu64_struct {
    u64* data;
    u64 len;
    u64 cap;
} ArrayBufu64;

typedef enum {
    Token_LeftParen = 1,
    Token_RightParen = 2,
    Token_Symbol = 3,
    Token_Integer = 4,
    Token_Char = 5,
    Token_String = 6,
    Token_Pound = 7,
} Token_variant;

// TODO: size?
typedef struct Token_struct {
    Token_variant tag;
    u32 len;
    u64 value;
    /*
    union {
        u32 sym;
        u64 integer;
        u8 ch;
        struct { u8* ptr; u32 len; } string;
    } data;
    */
} Token;

typedef struct Tokens_struct {
    Token* data;
    u64 len;
    u64 cap;
} Tokens;

typedef struct TokenInfoT_struct {
    u32 line;
    u32 len;
    u64 start;
} TokenInfoT;

typedef struct TokenInfo_struct {
    TokenInfoT* data;
    u64 len;
    u64 cap;
} TokenInfo;

typedef struct Line_struct {
    u64 start;
    u64 end;
} Line;

typedef struct Lines_struct {
    Line* data;
    u64 len;
    u64 cap;
} Lines;

typedef struct Tokenizer_struct {
    ArrayBufu8 input;
    u64 token_pos;
    Tokens tokens;
    TokenInfo token_info;
    Lines lines;
    String filename;
    u64 err;
} Tokenizer;

Tokenizer* Tokenizer_new(u8* input, u64 input_len, u8* filename, u64 filename_len) {
    Tokenizer* ret = (Tokenizer*)malloc(sizeof(Tokenizer));
    ret->input.data = input;
    ret->input.len = 0;
    ret->input.cap = input_len;
    ret->filename.data = filename;
    ret->filename.len = filename_len;
    ret->token_pos = 0;
    ret->tokens.data = (Token*)malloc(128);
    ret->tokens.len = 0;
    ret->tokens.cap = 128/sizeof(Token);
    ret->token_info.data = (TokenInfoT*)malloc(128);
    ret->token_info.len = 0;
    ret->token_info.cap = 128/sizeof(TokenInfoT);
    ret->lines.data = (Line*)malloc(128);
    ret->lines.len = 0;
    ret->lines.cap = 128/sizeof(Line);
    ret->err = 0;
    return ret;
}

void Tokenizer_free(Tokenizer* self) {
    free((u64)self->tokens.data);
    free((u64)self->token_info.data);
    free((u64)self->lines.data);
    free((u64)self);
}

typedef struct Jump_struct {
    u32 symbol;
    // (pos << 1) | JumpType
    u64 pos;
} Jump;

typedef struct JumpArray_struct {
    Jump* data;
    u64 len;
    u64 cap;
} JumpArray;

typedef struct Ref_struct {
    String path;
    // (pos << 1) | JumpType
    u64 pos;
} Ref;

typedef struct RefArray_struct {
    Ref* data;
    u64 len;
    u64 cap;
} RefArray;

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

typedef struct Module_struct {
    struct Module_struct* parent;
    SymbolMap24 children;
    u32 filep;
    u32 location;
    String path;
    ArrayBufu8 code;
    SymbolMap16 labels;
    JumpArray jumps;
    RefArray refs;
    RewriteArray rewrites;
} Module;

Module* Module_new(Module* parent, u8 filep) {
    Module* ret = (Module*)malloc(sizeof(Module));
    ret->parent = parent;
    ret->filep = filep;
    ret->children.data = (SM24Entry*)calloc(16 * sizeof(SM24Entry));
    ret->children.entries = 0;
    ret->children.cap = 16;
    ret->code.data = (u8*)malloc(64);
    ret->code.len = 0;
    ret->code.cap = 64;
    ret->labels.data = (SM16Entry*)calloc(8 * sizeof(SM16Entry));
    ret->labels.entries = 0;
    ret->labels.cap = 8;
    ret->jumps.data = (Jump*)malloc(8 * sizeof(Jump));
    ret->jumps.len = 0;
    ret->jumps.cap = 8;
    ret->refs.data = (Ref*)malloc(8 * sizeof(Ref));
    ret->refs.len = 0;
    ret->refs.cap = 8;
    ret->rewrites.data = (Rewrite*)malloc(8 * sizeof(Rewrite));
    ret->rewrites.len = 0;
    ret->rewrites.cap = 8;
    return ret;
}

// TODO
u64 Module_finish(Module* self) {
    return 0;
}

typedef struct ImportFile_struct {
    String path;
    Module* module;
} ImportFile;

typedef struct ImportFiles_struct {
    ImportFile* data;
    u64 len;
    u64 cap;
} ImportFiles;

typedef struct Asm_struct {
    ArrayBufu64 modules;
    Module* module;
    ArrayBufu8 data;
    ArrayBufu8 rodata;
    ImportFiles import_files;
} Asm;

Asm* Asm_new(Tokenizer* tokenizer) {
    Asm* ret = (Asm*)malloc(sizeof(Asm));
    ret->modules.data = (u64*)malloc(64);
    ret->modules.len = 0;
    ret->modules.cap = 8;
    ret->data.data = (u8*)malloc(256);
    ret->data.len = 0;
    ret->data.cap = 256;
    ret->rodata.data = (u8*)malloc(256);
    ret->rodata.len = 0;
    ret->rodata.cap = 256;
    ret->import_files.data = (ImportFile*)malloc(5 * sizeof(ImportFile));
    ret->import_files.len = 0;
    ret->import_files.cap = 5;

    u64 root_path_len = tokenizer->filename.len;
    while (root_path_len != 0 && tokenizer->filename.data[root_path_len-1] != '/') {
        root_path_len--;
    }
    // room for null byte
    u8* root_path = (u8*)malloc(root_path_len + 1);
    strdup(root_path, tokenizer->filename.data, root_path_len);
    Module* mod = Module_new(NULL, 1);
    mod->path.data = root_path;
    mod->path.len = root_path_len;
    ret->modules.data[0] = (u64)mod;

    u8* modname_str = tokenizer->filename.data + root_path_len;
    u64 modname_len = tokenizer->filename.len;
    while (modname_len != 0 && modname_str[modname_len-1] != '.') {
        modname_len--;
    }
    //TODO: check if len is zero, means no file extension so use full name
    u32 modname = string_to_symbol(modname_str, modname_len);

    Module* mod2 = Module_new(mod, 1);
    ret->modules.data[1] = (u64)mod2;
    ret->modules.len = 2;
    ret->module = mod2;

    ret->import_files.data[0].path.data = tokenizer->filename.data;
    ret->import_files.data[0].path.len = tokenizer->filename.len;
    ret->import_files.data[0].module = mod2;
    ret->import_files.len = 1;

    SM24_insert(&(mod->children), modname, UNIT_MODULE, (u64)mod2, 1);
    return ret;
}
